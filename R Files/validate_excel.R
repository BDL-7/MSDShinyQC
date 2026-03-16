

# validate_excel.R
# Pre-flight validation for MSD experiment XLSX files.
# Must be sourced before process_excel_files() is called.
#
# Public API:
#   validate_msd_file(file_paths, mode)
#     file_paths — character vector of temp file paths (from fileInput$datapath)
#     mode       — "qc" (single file) | "append" (multiple files)
#
# Returns a list:
#   $ok       — TRUE if no blocking errors were found
#   $errors   — character vector of blocking error messages
#   $warnings — character vector of non-blocking warning messages


# Constants ----

# Sheets that must be present
REQUIRED_SHEETS <- c("Exp_Data_Tbl", "Experiment_Info")

# Columns that must exist in Exp_Data_Tbl (blocking if missing)
REQUIRED_COLS <- c(
  "Assay",                       
  "Sample Group",                
  "Sample",                      
  "Dilution",                    
  "Well",                        
  "Spot",     
  "Calc. Concentration",         
  "Calc. Conc. Mean",            
  "Concentration",               
  "Detection Range",
  "Signal",                      
  "Mean",                        
  "Std. Deviation",              
  "CV",                          
  "% Recovery",                  
  "% Recovery Mean",            
  "Calc. Conc. Std. Deviation",  
  "Calc. Conc. CV",              
  "Detection Limits: Calc. Low", 
  "Detection Limits: Calc. High",
  "Excluded",                    
  "Fit Statistic: RSquared",     
  "Plate Name"   
)

# Columns expected but whose absence is a warning, not a hard error
EXPECTED_COLS <- c(
  "CV",
  "Std. Deviation",
  "% Recovery",
  "Calc. Conc. Std. Deviation",
  "Detection Limits: Calc. Low",
  "Detection Limits: Calc. High"
)

# Experiment_Info section boundaries (1-indexed rows, 1-indexed cols)
SECTION_BOUNDS <- list(
  s1 = list(rows = 1:9,   cols = 1:2,  label = "Section 1 (experiment metadata, rows 1-9)"),
  s2 = list(rows = 13:20, cols = 1:3,  label = "Section 2 (reagents, rows 13-20)"),
  s3 = list(rows = 25:42, cols = 1:2,  label = "Section 3 (plate barcodes, rows 25-42)")
)


# Internal helpers ----

# Read only the header row of Exp_Data_Tbl (fast — avoids loading all data)
read_header <- function(path) {
  tryCatch(
    names(readxl::read_excel(path, sheet = "Exp_Data_Tbl", skip = 1, n_max = 0)),
    error = function(e) NULL
  )
}

# Read a cell range from Experiment_Info and return TRUE if it has any content
range_has_content <- function(path, rows, cols) {
  tryCatch({
    df <- readxl::read_excel(
      path,
      sheet     = "Experiment_Info",
      col_names = FALSE,
      skip      = rows[1] - 1,
      n_max     = length(rows)
    )
    df <- df[, cols, drop = FALSE]
    any(!is.na(df) & trimws(as.matrix(df)) != "")
  }, error = function(e) FALSE)
}

# Extract the Barcode1 token from a Plate Name column (3rd "_"-delimited token)
extract_barcode1 <- function(plate_names) {
  sub("^(?:[^_]*_){2}([^_]+).*", "\\1", trimws(plate_names))
}


# Validate a single file ----

validate_single_file <- function(path, original_name) {
  errors   <- character()
  warnings <- character()
  label    <- sprintf("'%s'", original_name)

  # 1. File is readable ----
  available_sheets <- tryCatch(
    readxl::excel_sheets(path),
    error = function(e) {
      errors <<- c(errors, sprintf("%s could not be opened as an Excel file: %s", label, e$message))
      NULL
    }
  )
  if (is.null(available_sheets)) {
    return(list(ok = FALSE, errors = errors, warnings = warnings, barcodes = character()))
  }

  # 2. Required sheets ----
  missing_sheets <- setdiff(REQUIRED_SHEETS, available_sheets)
  if (length(missing_sheets)) {
    errors <- c(errors, sprintf(
      "%s is missing required sheet(s): %s",
      label, paste(missing_sheets, collapse = ", ")
    ))
    # Cannot continue without the sheets — return early
    return(list(ok = FALSE, errors = errors, warnings = warnings, barcodes = character()))
  }

  # 3. Required columns in Exp_Data_Tbl ----
  col_names <- read_header(path)
  if (is.null(col_names)) {
    errors <- c(errors, sprintf("%s: could not read column headers from 'Exp_Data_Tbl'.", label))
  } else {
    missing_required <- setdiff(REQUIRED_COLS, col_names)
    if (length(missing_required)) {
      errors <- c(errors, sprintf(
        "%s is missing required column(s) in 'Exp_Data_Tbl': %s",
        label, paste(missing_required, collapse = ", ")
      ))
    }

    missing_expected <- setdiff(EXPECTED_COLS, col_names)
    if (length(missing_expected)) {
      warnings <- c(warnings, sprintf(
        "%s: expected but non-critical column(s) not found in 'Exp_Data_Tbl': %s",
        label, paste(missing_expected, collapse = ", ")
      ))
    }

    unknown_cols <- setdiff(col_names, c(REQUIRED_COLS, EXPECTED_COLS))
    if (length(unknown_cols)) {
      warnings <- c(warnings, sprintf(
        "%s: unrecognised column(s) in 'Exp_Data_Tbl' (will be passed through): %s",
        label, paste(unknown_cols, collapse = ", ")
      ))
    }
  }

  # 4. Experiment_Info section ranges ----
  for (sec in SECTION_BOUNDS) {
    if (!range_has_content(path, sec$rows, sec$cols)) {
      errors <- c(errors, sprintf(
        "%s: %s appears to be empty or missing in 'Experiment_Info'.",
        label, sec$label
      ))
    }
  }

  # 5. Extract barcodes for cross-file duplicate check ----
  barcodes <- tryCatch({
    df <- readxl::read_excel(path, sheet = "Exp_Data_Tbl", skip = 1,
                             col_types = "text", n_max = 500)
    if ("Plate Name" %in% names(df)) {
      unique(extract_barcode1(na.omit(df$`Plate Name`)))
    } else {
      character()
    }
  }, error = function(e) character())

  list(
    ok       = length(errors) == 0,
    errors   = errors,
    warnings = warnings,
    barcodes = barcodes
  )
}


# validate_msd_file ----
# Main entry point called from app.R before process_excel_files().

validate_msd_file <- function(file_paths, original_names, mode = c("qc", "append")) {
  mode <- match.arg(mode)

  all_errors   <- character()
  all_warnings <- character()
  all_barcodes <- list()

  # Per-file checks ----
  for (i in seq_along(file_paths)) {
    nm     <- if (length(original_names) >= i) original_names[i] else basename(file_paths[i])
    result <- validate_single_file(file_paths[i], nm)

    all_errors   <- c(all_errors,   result$errors)
    all_warnings <- c(all_warnings, result$warnings)
    all_barcodes[[nm]] <- result$barcodes
  }

  # Cross-file duplicate plate check (append mode only) ----
  if (mode == "append" && length(file_paths) > 1) {
    barcode_source <- unlist(lapply(seq_along(all_barcodes), function(i) {
      setNames(rep(names(all_barcodes)[i], length(all_barcodes[[i]])), all_barcodes[[i]])
    }))

    # Find barcodes that appear in more than one file
    dup_barcodes <- names(which(table(names(barcode_source)) > 1))

    if (length(dup_barcodes)) {
      for (bc in dup_barcodes) {
        files_with_bc <- unique(barcode_source[names(barcode_source) == bc])
        all_errors <- c(all_errors, sprintf(
          "Duplicate plate barcode '%s' found in multiple files: %s",
          bc, paste(files_with_bc, collapse = ", ")
        ))
      }
    }
  }

  list(
    ok       = length(all_errors) == 0,
    errors   = all_errors,
    warnings = all_warnings
  )
}
