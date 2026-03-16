

# process_excel_files.R
# Parse one or more MSD experiment XLSX files and return a structured list:
#   main_data    joined, cleaned experiment data ready for QC processing
#   s1_raw       experiment-level metadata (Section 1)
#   s2_raw       reagent / lot information (Section 2)
#   s3_processed per-plate barcodes and read times (Section 3)
#
# Requires: dplyr, tidyr, readxl, purrr, lubridate, stringi


suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(purrr)
  library(lubridate)
  library(stringi)
})


# Helpers ----

# Parse a date from various formats, including Excel serials.
# Returns a formatted character string by default; pass out = NULL for Date.
date_any <- function(x, out = "%m/%d/%Y") {
  x <- trimws(as.character(x))
  x[x %in% c("", ".", "NA", "N/A", "-")] <- NA_character_

  suppressWarnings(num <- as.numeric(x))
  d  <- as.Date(NA_real_, origin = "1970-01-01")
  ix <- !is.na(num)
  if (any(ix)) d[ix] <- as.Date(num[ix], origin = "1899-12-30")

  rest <- which(is.na(d) & !is.na(x))
  if (length(rest)) {
    d[rest] <- suppressWarnings(
      lubridate::parse_date_time(x[rest], orders = c("ymd", "mdy"), quiet = TRUE)
    )
  }

  if (is.null(out)) d else format(d, out)
}

# Normalise column names: Unicode ASCII, collapse whitespace, strip tibble suffixes.
normalize_names <- function(nm) {
  nm <- stringi::stri_trans_nfkc(nm)
  nm <- gsub("[\u200B\u200C\u200D\uFEFF]", "", nm)   # zero-width chars
  nm <- gsub("\u00A0", " ", nm, perl = TRUE)           # non-breaking space
  nm <- gsub("[[:space:]]+", " ", nm)
  nm <- trimws(nm)
  nm <- sub("\\.{3}\\d+$", "", nm)                     # tibble ...n suffix
  nm
}

# Reduce a name to alphanumeric only, used for fuzzy matching.
canon_key <- function(nm) gsub("[^A-Za-z0-9]+", "", nm)

# Ensure a single, canonical "Barcode1" column exists.
# Falls back to deriving it from the 3rd "_"-delimited token of Plate Name.
ensure_barcode1 <- function(df) {
  names(df) <- normalize_names(names(df))
  cand <- which(canon_key(names(df)) == "Barcode1")

  if (length(cand) == 0 && "Plate Name" %in% names(df)) {
    df$Barcode1 <- sub("^(?:[^_]*_){2}([^_]+).*", "\\1", df$`Plate Name`)
    cand <- which(names(df) == "Barcode1")
  }

  if (length(cand) >= 1) {
    if (length(cand) > 1) {
      df   <- df[, -cand[-length(cand)], drop = FALSE]
      cand <- which(canon_key(names(df)) == "Barcode1")
    }
    names(df)[cand[length(cand)]] <- "Barcode1"
  }

  df
}

# Move Barcode1 to the last column (stable, non-disruptive).
relocate_barcode1_last <- function(df) {
  if ("Barcode1" %in% names(df)) {
    dplyr::relocate(df, Barcode1, .after = dplyr::last_col())
  } else {
    df
  }
}


# process_excel_files ----
# Main entry point. Accepts a character vector of file paths.

process_excel_files <- function(file_paths) {

  withProgress(message = "Loading Excel Files", detail = "This may take a while...", value = 0.2, {

    RDS_DIR_local <- get0("RDS_DIR", inherits = TRUE, ifnotfound = file.path(getwd(), "RDS"))
    if (!dir.exists(RDS_DIR_local)) dir.create(RDS_DIR_local, recursive = TRUE)


    # Read experiment data (Exp_Data_Tbl sheet) ----

    readfile <- function(file) {
      tryCatch({
        df <- readxl::read_excel(file, sheet = "Exp_Data_Tbl", skip = 1, col_types = "text")
        if (nrow(df) == 0) stop("Empty data frame")
        df
      }, error = function(e) {
        warning(paste("Error reading file:", file, "-", e$message))
        NULL
      })
    }

    datalist <- Filter(Negate(is.null), lapply(file_paths, readfile))
    if (length(datalist) == 0) stop("No valid data found in any file.")
    data <- as.data.frame(do.call(rbind, datalist))


    # Read experiment info (Experiment_Info sheet) ----
    # Parses all three sections of the info sheet and returns them as a list.

    plate_read1 <- function(file) {
      sheet_data <- readxl::read_excel(file, sheet = "Experiment_Info")

      # Section 1 — experiment-level metadata (rows 1–9) ----
      s1_raw <- sheet_data[1:9, 1:2] |>
        dplyr::rename(Experiment = 1, Information = 2)

      # Convert "Experiment Date" from Excel serial if needed
      date_row <- s1_raw$Experiment == "Experiment Date"
      s1_raw$Information[date_row] <- format(
        as.Date(as.numeric(s1_raw$Information[date_row]), origin = "1900-01-01"),
        "%m/%d/%Y"
      )

      # Transposed subset used for the plate-level join
      s1_processed <- sheet_data[1:9, 1:2] |>
        dplyr::rename(x = 1, y = 2) |>
        dplyr::filter(x %in% c(
          "Experiment Name", "Experiment Date",
          "User ID", "Plate Type", "Project(s)"
        )) |>
        t() |>
        as.data.frame()
      colnames(s1_processed) <- s1_processed[1, ]
      filter_s1 <- s1_processed[-1, ]

      # Section 2 — reagents, lot numbers, expiry dates (rows 13–20) ----
      s2_raw <- sheet_data[13:20, 1:3] |>
        dplyr::rename(Reagent = 1, Lot = 2, ExpiryDate = 3) |>
        dplyr::mutate(ExpiryDate = date_any(ExpiryDate))

      # s2_processed is kept in memory and referenced at join time below
      s2_processed <- s2_raw

      # Section 3 — per-plate barcodes and read times ----
      # Fixed cell ranges for each plate position (G = IgG, A = IgA, M = IgM)
      p <- list(
        G1 = sheet_data[25:42,  1:2],  G2 = sheet_data[25:42,  4:5],
        G3 = sheet_data[25:42,  7:8],  G4 = sheet_data[25:42, 10:11],
        G5 = sheet_data[25:42, 13:14], G6 = sheet_data[25:42, 16:17],
        A1 = sheet_data[47:64,  1:2],  A2 = sheet_data[47:64,  4:5],
        A3 = sheet_data[47:64,  7:8],  A4 = sheet_data[47:64, 10:11],
        A5 = sheet_data[47:64, 13:14], A6 = sheet_data[47:64, 16:17],
        M1 = sheet_data[69:86,  1:2],  M2 = sheet_data[69:86,  4:5],
        M3 = sheet_data[69:86,  7:8],  M4 = sheet_data[69:86, 10:11],
        M5 = sheet_data[69:86, 13:14], M6 = sheet_data[69:86, 16:17]
      )

      # Drop plates that are entirely NA (not all plexes are used every run)
      p <- purrr::discard(p, ~all(is.na(unlist(.))))
      s3_raw_list <- p

      # Extract the 5 key rows from each plate section and prepend the plate name
      p <- purrr::map(p, ~ {
        .x |>
          dplyr::rename(x = 1, y = 2) |>
          dplyr::filter(x %in% c("Read Time", "Barcode1", "Barcode2", "Barcode3", "Serial #"))
      })
      for (nm in names(p)) {
        p[[nm]] <- rbind(data.frame(x = "Plate", y = nm), p[[nm]])
      }

      # Transpose to wide format and bind all plates into one data frame
      tp_list <- purrr::map(p, ~t(.x))
      tp_list <- lapply(tp_list, function(df) {
        colnames(df) <- df[1, ]
        df[-1, , drop = FALSE]
      })
      tp <- do.call(rbind, tp_list) |> as.data.frame(stringsAsFactors = FALSE)

      # Named list of per-plate long-format data frames for the QMD report
      s3_processed <- purrr::map(p, function(df) {
        colnames(df) <- c("Experiment", "Information")
        df |>
          dplyr::filter(!is.na(Information)) |>
          as.data.frame(stringsAsFactors = FALSE)
      })

      list(
        plate_data   = dplyr::bind_cols(tp, filter_s1),
        s1_raw       = s1_raw,
        s2_raw       = s2_raw,
        s2_processed = s2_processed,
        s3_raw_list  = s3_raw_list,
        s3_processed = s3_processed
      )
    }

    incProgress(0.1)
    plate_read2 <- lapply(file_paths, plate_read1)


    # Collect results across all files ----

    plates       <- lapply(plate_read2, `[[`, "plate_data") |> dplyr::bind_rows()
    s3_lists     <- lapply(plate_read2, `[[`, "s3_processed")
    s3_processed <- do.call(c, s3_lists)   # flatten to a single named list

    # Use metadata from the last file processed (consistent with original behaviour)
    s1_raw       <- plate_read2[[length(plate_read2)]]$s1_raw
    s2_raw       <- plate_read2[[length(plate_read2)]]$s2_raw
    s2_processed <- plate_read2[[length(plate_read2)]]$s2_processed


    # Build main data table ----

    # Split "Plate Name" into Experiment Name, Plate Letter, Barcode1
    data <- tidyr::separate(
      data, `Plate Name`,
      into   = c("Experiment Name", "Plate Letter", "Barcode1"),
      sep    = "_",
      remove = FALSE,
      extra  = "drop",
      fill   = "right"
    )

    names(data) <- normalize_names(names(data))

    # Derive Antibody from the letter prefix of Plate Letter (G/A/M)
    data <- data |>
      dplyr::mutate(Antibody = dplyr::case_when(
        startsWith(`Plate Letter`, "G") ~ "IgG",
        startsWith(`Plate Letter`, "A") ~ "IgA",
        startsWith(`Plate Letter`, "M") ~ "IgM",
        TRUE                            ~ "Other"
      ))

    # Drop stray "_" column that can appear after separate()
    if ("_" %in% names(data)) data <- data[, setdiff(names(data), "_"), drop = FALSE]

    # Remove BSA rows (calibration reagent, not a true analyte)
    if ("Assay" %in% names(data)) {
      data <- data[data$Assay != "BSA", , drop = FALSE]
    }

    # Prepare the plate metadata for joining on Barcode1
    names(plates) <- normalize_names(names(plates))
    plates <- plates |>
      dplyr::rename(ExperiNameSec1 = `Experiment Name`) |>
      dplyr::select(dplyr::any_of(c(
        "Barcode1", "Barcode2", "Barcode3", "Serial #",
        "ExperiNameSec1", "Read Time", "Experiment Date",
        "User ID", "Plate Type"
      )))

    # Ensure a canonical Barcode1 exists on both sides before joining
    data   <- ensure_barcode1(data)
    plates <- ensure_barcode1(plates)

    if (!"Barcode1" %in% names(data)) {
      stop("Could not materialise 'Barcode1' in `data`. Found: ",
           paste(grep("Barcode", names(data), value = TRUE), collapse = ", "))
    }
    if (!"Barcode1" %in% names(plates)) {
      stop("Could not materialise 'Barcode1' in `plates`. Found: ",
           paste(grep("Barcode", names(plates), value = TRUE), collapse = ", "))
    }

    data$Barcode1   <- as.character(data$Barcode1)
    plates$Barcode1 <- as.character(plates$Barcode1)

    data <- data |>
      dplyr::left_join(plates, by = "Barcode1") |>
      relocate_barcode1_last()


    # Numeric coercions and date formatting ----

    if ("Read Time" %in% names(data)) {
      data$date <- as.POSIXct(
        data$`Read Time`,
        format = "%m/%d/%Y %H:%M:%S",
        tz     = "America/New_York"
      )
    }

    num_cols <- c(
      "Calc. Concentration", "Concentration", "Calc. Conc. Mean",
      "Std. Deviation", "CV", "% Recovery", "% Recovery Mean",
      "Calc. Conc. Std. Deviation", "Calc. Conc. CV",
      "Detection Limits: Calc. Low", "Detection Limits: Calc. High",
      "Fit Statistic: RSquared"
    )
    for (cc in intersect(num_cols, names(data))) {
      suppressWarnings(
        data[[cc]] <- round(
          as.numeric(data[[cc]]),
          digits = ifelse(grepl("^%|RSquared", cc), 4, 2)
        )
      )
    }

    if ("User ID" %in% names(data)) {
      data$`User ID` <- toupper(data$`User ID`)
    }

    if ("Experiment Date" %in% names(data)) {
      suppressWarnings({
        data$`Experiment Date` <- format(
          as.Date(
            as.numeric(data$`Experiment Date`) + as.Date("1900-01-01") - 2,
            origin = "1900-01-01"
          ),
          "%m/%d/%Y"
        )
      })
    }


    # Add reagent lot and expiry columns from Section 2 ----
    # Each reagent becomes two columns: <Reagent>_Lot and <Reagent>_ExpiryDate

    if (
      is.data.frame(s2_processed) &&
      all(c("Reagent", "Lot", "ExpiryDate") %in% names(s2_processed))
    ) {
      for (i in seq_len(nrow(s2_processed))) {
        reagent <- s2_processed$Reagent[i]
        data[[paste0(reagent, "_Lot")]]        <- s2_processed$Lot[i]
        data[[paste0(reagent, "_ExpiryDate")]] <- s2_processed$ExpiryDate[i]
      }
    }


    # Return ----

    list(
      main_data    = data,
      s1_raw       = s1_raw,
      s2_raw       = s2_raw,
      s3_processed = s3_processed
    )

  }) # end withProgress
}


# Display formatting functions ----
# Transform DataTableFunc output into display-ready tables for the HTML report.
# Called in run_qc.R (render_hir_reports) before saving the display RDS that
# is passed to HTML.qmd as data_path.
#
# All three functions:
#   - Deduplicate to one row per Sample + Assay (first occurrence kept;
#     drops the second dilution level for display purposes).
#   - Select and rename exactly the columns requested for the HTML output.


format_controls_display <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)

  df |>
    dplyr::distinct(Sample, Assay, .keep_all = TRUE) |>
    dplyr::select(
      Assay,
      Sample,
      `Expected Conc (BAU/mL)`    = Conc,
      `Calc. Conc. Mean (BAU/mL)` = MeanLOQ,
      `Calc. Conc. CV`            = CVLOQ,
      `% Recovery Mean`           = Percent_Recovery_Mean,
      Analyte_QC,
      Sample_QC
    )
}


format_standards_display <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)

  df |>
    dplyr::distinct(Sample, Assay, .keep_all = TRUE) |>
    dplyr::select(
      Assay,
      Sample,
      `Expected Conc (BAU/mL)`    = Conc,
      `Calc. Conc. Mean (BAU/mL)` = MeanCalConc,
      `Calc. Conc. CV`            = CVCalConc,
      `% Recovery Mean`           = Percent_Recovery_Mean,
      `Std. Curve R^2`            = Fit_Statistic_RSquared,
      Analyte_QC,
      Sample_QC
    )
}


format_unknowns_display <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)

  df |>
    dplyr::distinct(Sample, Assay, .keep_all = TRUE) |>
    dplyr::mutate(Count = dplyr::row_number()) |>
    dplyr::select(
      Count,
      Sample,
      Assay,
      `Calc. Conc. Mean (BAU/mL)` = MeanLOQ,
      `Calc. Conc. CV`            = CVLOQ,
      `ULOQ/LLOQ: Call`           = Detection_Range_LOQ_Display,
      `Threshold Call`            = ThresCall_BAU,
      QC_LOQ,
      QC_DRMatching,
      Analyte_QC,
      Sample_QC
    )
}
