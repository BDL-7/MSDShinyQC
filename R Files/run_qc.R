# run_qc.R
# Encapsulates the full QC run triggered by the "Run Program" button.
# Called from observeEvent(input$runRApp) in app.R.
#
# Depends on: helpers.R, shiny_quarto_safety.R, DataTable*.R (sourced internally)


# build_plate_partitions ----
# Splits merged_data into per-antibody, per-plate-number partitions,
# runs DataTableFunc on each, saves the result to an RDS file, and
# returns a named list of vectors used by the rendering step.

build_plate_partitions <- function(merged_data, RDS_DIR, DataTableFunc, team = "HIR") {

  plateletters <- unique(merged_data[["Plate Letter"]])
  parts_list   <- list()
  outname_list <- list()
  ab_list      <- list()
  p_list       <- list()

  for (pl in plateletters) {
    ab_chars    <- unlist(strsplit(gsub("[0-9]", "", pl), ""))
    n           <- as.numeric(gsub("[^0-9]", "", pl))
    if (is.na(n) || n <= 0) next

    plate_data  <- dplyr::filter(merged_data, `Plate Letter` == pl)
    total_parts <- length(ab_chars) * n

    if (nrow(plate_data) < total_parts) {
      shinyalert::shinyalert(
        "Warning", paste("Not enough rows for plate letter:", pl), type = "warning"
      )
      next
    }

    parts <- split(
      plate_data,
      rep(seq_len(total_parts),
          each       = floor(nrow(plate_data) / total_parts),
          length.out = nrow(plate_data))
    )

    i <- 1
    for (ab in ab_chars) {
      for (p in seq_len(n)) {
        if (i > length(parts)) next
        part_data <- parts[[i]]; i <- i + 1
        if (nrow(part_data) == 0) next

                  result_tables <- DataTableFunc(part_data, team = team)
        outname       <- file.path(RDS_DIR, paste0(ab, p, ".rds"))
        saveRDS(result_tables, outname)

        parts_list[[length(parts_list) + 1]]     <- part_data
        outname_list[[length(outname_list) + 1]] <- outname
        ab_list[[length(ab_list) + 1]]           <- ab
        p_list[[length(p_list) + 1]]             <- p
      }
    }
  }

  # Remove duplicate plate targets (e.g. A1 / G1 appearing more than once)
  plate_ids <- paste0(unlist(ab_list), unlist(p_list))
  keep      <- !duplicated(plate_ids)

  list(
    parts_list   = parts_list[keep],
    outname_list = outname_list[keep],
    ab_list      = ab_list[keep],
    p_list       = p_list[keep]
  )
}


# render_hir_reports ----
# Renders one Quarto HTML report per plate partition and builds the reports
# manifest data frame. Returns the manifest (used to populate output$reportsTable).

render_hir_reports <- function(session, input, outname_list, ab_list, p_list,
                                RDS_DIR, REPORTS_DIR) {

  analyzed_on      <- format(Sys.time(), "%d%b%Y at %I:%M:%S %p")
  reports_manifest <- list()

  qmd_path <- find_qmd()
  if (is.null(qmd_path) || is.na(qmd_path)) stop("Could not locate HTML.qmd.")
  qmd_abs <- normalizePath(qmd_path, winslash = "/", mustWork = TRUE)
  qmd_dir <- dirname(qmd_abs)

  s1_raw_df <- session$userData$s1_raw
  exp_name  <- get_experiment_name(s1_raw_df)
  exp_date  <- get_experiment_date(s1_raw_df)
  exp_user  <- get_user_id(s1_raw_df)

  # Fallbacks when metadata is missing from the file
  if (is.na(exp_name) || !nzchar(exp_name)) {
    exp_name <- if (!is.null(input$filesforRApp$name) && nzchar(input$filesforRApp$name[1]))
      tools::file_path_sans_ext(basename(input$filesforRApp$name[1])) else "Experiment"
  }
  if (is.na(exp_user) || !nzchar(exp_user)) exp_user <- ""
  exp_date_chr <- if (!is.na(exp_date)) as.character(exp_date) else ""

  for (i in seq_along(outname_list)) {
    outname  <- outname_list[[i]]
    ab       <- ab_list[[i]]
    p        <- p_list[[i]]
    plate_id <- paste0(ab, p)

    tryCatch({
      if (!plate_id %in% names(session$userData$s3_processed)) {
        warning(sprintf("Plate ID '%s' not found in s3_processed — skipping.", plate_id))
        next
      }

      # Write per-plate RDS files for the QMD template
      s1_path <- file.path(RDS_DIR, paste0(plate_id, "_s1_raw.rds"))
      s2_path <- file.path(RDS_DIR, paste0(plate_id, "_s2_raw.rds"))
      s3_path <- file.path(RDS_DIR, paste0(plate_id, "_s3_processed.rds"))

      saveRDS(session$userData$s1_raw,                  s1_path)
      saveRDS(session$userData$s2_raw,                  s2_path)
      saveRDS(session$userData$s3_processed[[plate_id]], s3_path)

      out_name <- paste0(output_stem(exp_name, plate_id), ".html")

      # Build display-ready tables and save to a sidecar RDS.
      # The raw partition RDS (outname) is left intact for downstream use.
      # HTML.qmd receives the display path so it renders only the requested
      # columns at one dilution level per sample.
      raw_tables     <- readRDS(outname)
      display_tables <- list(
        controls  = format_controls_display(raw_tables$controls),
        standards = format_standards_display(raw_tables$standards),
        qc        = raw_tables$qc,
        unknowns  = format_unknowns_display(raw_tables$unknowns)
      )
      display_path <- sub("\\.rds$", "_display.rds", outname, ignore.case = TRUE)
      saveRDS(display_tables, display_path)

      res_path <- render_quarto_report_sync(
        qmd_path    = qmd_abs,
        output_file = out_name,
        params      = list(
          ab                = ab,
          plate             = plate_id,
          s1_raw_path       = s1_path,
          s2_raw_path       = s2_path,
          s3_processed_path = s3_path,
          data_path         = display_path,
          user_id           = exp_user,
          analyzed_on       = analyzed_on,
          experiment_name   = exp_name,
          experiment_date   = exp_date_chr
        ),
        execute_dir     = qmd_dir,
        embed_resources = TRUE
      )

      src_html  <- if (!is.null(res_path) && nzchar(res_path)) res_path else file.path(qmd_dir, out_name)
      if (!file.exists(src_html)) stop("Output HTML not found after render: ", src_html)

      dest_html <- file.path(REPORTS_DIR, out_name)
      if (!file.copy(src_html, dest_html, overwrite = TRUE)) {
        stop("Failed to copy HTML to reports directory: ", dest_html)
      }
      unlink(src_html)

      view_url <- web_path("reports", out_name)
      reports_manifest[[length(reports_manifest) + 1]] <- data.frame(
        Plate      = plate_id,
        Experiment = if (nzchar(exp_name)) exp_name else "(unknown)",
        View       = sprintf('<a href="%s" target="_blank" rel="noopener">View</a>',  view_url),
        Download   = sprintf('<a href="%s" download>Download</a>', view_url),
        stringsAsFactors = FALSE
      )

    }, error = function(e) {
      cat(sprintf("Error rendering plate %s: %s\n", plate_id, e$message))
      shinyalert::shinyalert(
        "Render Error",
        paste("Failed to render report for", plate_id, ":", e$message),
        type = "error"
      )
    })
  }

  if (length(reports_manifest) > 0) {
    do.call(rbind, reports_manifest)
  } else {
    data.frame(
      Plate = character(), Experiment = character(),
      View  = character(), Download   = character()
    )
  }
}


# run_qc ----
# Top-level orchestrator. Called directly from observeEvent(input$runRApp).

run_qc <- function(session, input, output, merged_data, RDS_DIR, REPORTS_DIR, R_FILES_DIR) {

  if (!dir.exists(RDS_DIR))     dir.create(RDS_DIR,     recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(REPORTS_DIR)) dir.create(REPORTS_DIR, recursive = TRUE, showWarnings = FALSE)

  showNotification("Generating QC data. Please wait...", type = "message")

  # Source the correct DataTable script for the chosen team
  # HIR and IVYBivalent both use the unified DataTable.R (team parameter handles differences)
  dt_file <- switch(input$team,
    "HIR"         = "DataTable.R",
    "IVYBivalent" = "DataTable.R",
    "Sam5k50k"    = "DataTable_Sam5K50K.R",
    "VRI"         = "DataTable_VRI_AllData.R",
    "DataTable.R"
  )
  source(file.path(R_FILES_DIR, dt_file), local = TRUE)

  # Build and save per-plate partitions
  partitions <- build_plate_partitions(merged_data, RDS_DIR, DataTableFunc, team = input$team)

  session$userData$reportParts <- partitions$parts_list
  session$userData$outnameList <- partitions$outname_list
  session$userData$abList      <- partitions$ab_list
  session$userData$pList       <- partitions$p_list

  # HIR: render Quarto HTML reports and populate the reports table
  if (identical(input$team, "HIR")) {
    showNotification("Generating HTML reports...", type = "message")

    reports_df <- render_hir_reports(
      session      = session,
      input        = input,
      outname_list = partitions$outname_list,
      ab_list      = partitions$ab_list,
      p_list       = partitions$p_list,
      RDS_DIR      = RDS_DIR,
      REPORTS_DIR  = REPORTS_DIR
    )

    session$userData$reports_df <- reports_df

    output$reportsTable <- DT::renderDataTable(
      DT::datatable(
        reports_df,
        escape   = FALSE,
        rownames = FALSE,
        options  = list(pageLength = 10, autoWidth = TRUE)
      )
    )

    showNotification("Reports ready. Use View / Download links above.", type = "message")

  } else {
    showNotification("QC complete. Use 'Download QC Excel' to export.", type = "message")
  }
}
