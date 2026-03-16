

#--------------------------------------------#
#                App Directory               #
#--------------------------------------------#

R_FILES_DIR <- "R Files"
RDS_DIR     <- "RDS"
REPORTS_DIR <- "Reports"   

if (!dir.exists(R_FILES_DIR)) dir.create(R_FILES_DIR, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(RDS_DIR))     dir.create(RDS_DIR,     recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(REPORTS_DIR)) dir.create(REPORTS_DIR, recursive = TRUE, showWarnings = FALSE)

# Expose reports directory at /reports for View/Download links
shiny::addResourcePath("reports", normalizePath(REPORTS_DIR, winslash = "/", mustWork = FALSE))

# source("shiny_quarto_safety.R", local = TRUE)
source(file.path(R_FILES_DIR, "shiny_quarto_safety.R"),   local = TRUE)
web_path <- function(...) gsub("\\", "/", do.call(file.path, list(...)), fixed = TRUE)


# ---- Helper operators & filename helpers (non-destructive patch) ----
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x
}

if (!exists("output_basename")) {
  # Keep only first and last tokens of experiment name, joined by underscore.
  output_basename <- function(experiment_name) {
    if (is.null(experiment_name) || is.na(experiment_name)) return("Experiment")
    parts <- unlist(strsplit(as.character(experiment_name), "_+", perl = TRUE))
    parts <- parts[nzchar(parts)]
    n <- length(parts)
    if (n >= 2) paste(parts[1], parts[n], sep = "_") else if (n == 1) parts[1] else "Experiment"
  }
}


# ---- Filename helper: keep only first and last tokens ----
output_stem <- function(experiment_name, plate_id) {
  if (is.null(experiment_name) || is.na(experiment_name)) experiment_name <- "Experiment"
  parts <- unlist(strsplit(as.character(experiment_name), "_+", perl = TRUE))
  parts <- parts[nzchar(parts)]
  base  <- if (length(parts) >= 1) parts[1] else "Experiment"
  plate <- as.character(plate_id %||% "G1")  
  # if you have %||%, otherwise do if (is.null(...)) "G1"
  paste0(base, "_", plate)
}
