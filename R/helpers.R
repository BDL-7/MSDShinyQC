



# helpers.R
# Pure utility functions used across the app.
# No Shiny state — safe to call from anywhere.
# Sourced once at the top of server().


# Experiment metadata extractors ----
# Each function accepts s1_raw (the Section 1 data frame from process_excel_files)
# and handles both the row-wise (Experiment / Information) and column-wise layouts.

get_experiment_name <- function(s1) {
  if (is.null(s1)) return(NA_character_)
  if (all(c("Experiment", "Information") %in% names(s1))) {
    key <- trimws(s1$Experiment); val <- s1$Information
    hit <- which(grepl("experiment", key, TRUE) & grepl("name", key, TRUE))
    if (length(hit)) {
      v <- as.character(val[hit][nzchar(trimws(val[hit]))])
      if (length(v)) return(v[1])
    }
  }
  nm <- c("Experiment Name", "ExperiNameSec1", "Experiment_Name", "Experiment")
  nm <- nm[nm %in% names(s1)][1]
  if (!is.na(nm)) {
    v <- as.character(s1[[nm]]); v <- v[!is.na(v) & nzchar(trimws(v))]
    if (length(v)) return(v[1])
  }
  NA_character_
}

get_experiment_date <- function(s1) {
  if (is.null(s1)) return(NA)
  if (all(c("Experiment", "Information") %in% names(s1))) {
    key <- trimws(s1$Experiment); val <- s1$Information
    hit <- which(grepl("experiment", key, TRUE) & grepl("date|read time", key, TRUE))
    if (length(hit)) {
      v <- val[hit][nzchar(trimws(val[hit]))]
      if (length(v)) return(suppressWarnings(as.Date(
        v[1], tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d-%b-%y", "%m/%d/%y")
      )))
    }
  }
  nm <- c("Experiment Date", "Experiment_Date", "Read Time", "Read_Time")
  nm <- nm[nm %in% names(s1)][1]
  if (!is.na(nm)) {
    v <- s1[[nm]]
    if (inherits(v, "Date"))   return(v[1])
    if (inherits(v, "POSIXt")) return(as.Date(v[1]))
    return(suppressWarnings(as.Date(
      v[1], tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d-%b-%y", "%m/%d/%y")
    )))
  }
  NA
}

get_user_id <- function(s1) {
  if (is.null(s1)) return(NA_character_)
  if (all(c("Experiment", "Information") %in% names(s1))) {
    key <- trimws(s1$Experiment); val <- s1$Information
    hit <- which(grepl("user id|userid|operator|analyst", key, TRUE))
    if (length(hit)) {
      v <- as.character(val[hit][nzchar(trimws(val[hit]))])
      if (length(v)) return(v[1])
    }
  }
  nm <- c("User ID", "UserID", "Operator", "Analyst")
  nm <- nm[nm %in% names(s1)][1]
  if (!is.na(nm)) {
    v <- as.character(s1[[nm]]); v <- v[!is.na(v) & nzchar(trimws(v))]
    if (length(v)) return(v[1])
  }
  NA_character_
}


# sanitize_filename ----
# Strips non-filesystem-safe characters from a string for use in output filenames.

sanitize_filename <- function(x) {
  if (length(x) == 0 || is.null(x) || all(is.na(x))) return("Experiment")
  x <- as.character(x[1])
  if (!nzchar(x) || tolower(x) %in% c("na", "n/a", "nan", ".")) return("Experiment")
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- gsub("[^[:alnum:]_.-]+", "_", x, perl = TRUE)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) "Experiment" else x
}


# find_qmd ----
# Searches common locations for HTML.qmd and returns the first match.

find_qmd <- function() {
  candidates <- c(
    "HTML.qmd",
    file.path("app",    "HTML.qmd"),
    file.path(getwd(), "HTML.qmd"),
    file.path(getwd(), "app", "HTML.qmd")
  )
  candidates[file.exists(candidates)][1]
}


# show_validation_modal ----
# Renders a blocking Shiny modal from a validate_msd_file() result.
# Returns TRUE if no hard errors (processing may continue), FALSE otherwise.

show_validation_modal <- function(result, title = "File Validation") {

  if (result$ok && length(result$warnings) == 0) return(TRUE)

  error_html <- if (length(result$errors)) {
    tags$div(
      tags$h5(
        icon("circle-xmark"), " Errors \u2014 file cannot be processed",
        style = "color:#c0392b; margin-top:0;"
      ),
      tags$ul(lapply(result$errors, tags$li), style = "color:#c0392b;")
    )
  }

  warning_html <- if (length(result$warnings)) {
    tags$div(
      tags$h5(
        icon("triangle-exclamation"), " Warnings",
        style = "color:#e67e22; margin-top:12px;"
      ),
      tags$ul(lapply(result$warnings, tags$li), style = "color:#856404;")
    )
  }

  footer_btn <- if (result$ok) {
    tagList(
      modalButton("Cancel"),
      actionButton("validationProceed", "Proceed anyway", class = "btn btn-warning")
    )
  } else {
    modalButton("Close")
  }

  showModal(modalDialog(
    title     = title,
    error_html,
    warning_html,
    footer    = footer_btn,
    easyClose = FALSE,
    size      = "m"
  ))

  return(result$ok)
}
