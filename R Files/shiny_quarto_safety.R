# shiny_quarto_safety.R
# Safe, hardened wrapper around quarto::quarto_render() for use inside a
# Shiny server. Validates Quarto availability, normalises all paths, and
# provides a synchronous render with optional console logging.


# Quarto availability check ----

.QUARTO_OK <- requireNamespace("quarto", quietly = TRUE)
.qp        <- tryCatch(quarto::quarto_path(), error = function(e) NULL)

if (is.null(.qp) || !nzchar(.qp) || !file.exists(.qp)) {
  stop(
    "Quarto CLI not found. ",
    "Install Quarto and ensure it is on PATH. ",
    "See: https://quarto.org/docs/get-started/"
  )
}


# Internal helpers ----

if (!exists("%||%")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x
  }
}

.is_abs_path <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(FALSE)
  grepl("^(/|[A-Za-z]:[\\\\/]|\\\\\\\\)", path)
}

normalize_abs_path <- function(path, base = getwd()) {
  if (is.null(path) || is.na(path) || !nzchar(path)) return(NA_character_)
  if (!.is_abs_path(path)) path <- file.path(base, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

normalize_abs_dir <- function(path, base = getwd()) {
  p <- normalize_abs_path(path, base = base)
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}


# render_quarto_report_sync ----
# Renders a .qmd file synchronously using the supplied parameter list.
# Returns the absolute path to the generated HTML file.
#
# Args:
#   qmd_path        Path to the .qmd template file.
#   output_file     Desired output filename (HTML). Defaults to index.html.
#   params          Named list passed to Quarto as execute_params.
#   execute_dir     Working directory for Quarto execution.
#   embed_resources If TRUE, embeds all assets in the HTML (self-contained).
#   log_file        Optional path to capture console output from Quarto.

render_quarto_report_sync <- function(
    qmd_path,
    output_file     = NULL,
    params          = list(),
    execute_dir     = getwd(),
    embed_resources = TRUE,
    log_file        = NULL,
    ...
) {

  if (!exists(".QUARTO_OK") || !.QUARTO_OK) {
    stop("Quarto is not installed or not on PATH. Install Quarto and try again.")
  }

  # Normalise paths ----
  execute_dir  <- normalize_abs_dir(execute_dir)
  qmd_path_abs <- normalize_abs_path(qmd_path, base = execute_dir)

  if (!file.exists(qmd_path_abs)) {
    stop(sprintf("QMD file not found: %s", qmd_path_abs))
  }

  # Absolutise RDS param paths ----
  if (length(params)) {
    rds_keys <- intersect(
      names(params),
      c("s1_raw_path", "s2_raw_path", "s3_processed_path", "data_path")
    )
    for (k in rds_keys) {
      if (!is.null(params[[k]]) && is.character(params[[k]])) {
        params[[k]] <- normalize_abs_path(params[[k]], base = execute_dir)
      }
    }
  }

  # Build render argument list ----
  args <- list(input = qmd_path_abs, execute_dir = execute_dir, quiet = FALSE)
  if (!is.null(output_file)) args$output_file    <- output_file
  if (length(params))        args$execute_params <- params

  # Optional console logging to file ----
  if (!is.null(log_file) && nzchar(log_file)) {
    log_file <- normalize_abs_path(log_file, base = execute_dir)
    con      <- file(log_file, open = "wt")
    sink(con, type = "output")
    sink(con, type = "message")
    on.exit({
      try(sink(type = "message"), silent = TRUE)
      try(sink(type = "output"),  silent = TRUE)
      try(close(con),             silent = TRUE)
    }, add = TRUE)
  }

  # Render ----
  out <- tryCatch(
    do.call(quarto::quarto_render, args),
    error = function(e) stop(sprintf("Quarto render failed: %s", conditionMessage(e)))
  )

  # Resolve and return output path ----
  if (is.null(out) || !nzchar(out) || !file.exists(out)) {
    candidate <- file.path(execute_dir, output_file %||% "index.html")
    if (!file.exists(candidate)) {
      stop(sprintf(
        "Render completed but output not found. Expected at: %s", candidate
      ))
    }
    out <- candidate
  }

  out
}
