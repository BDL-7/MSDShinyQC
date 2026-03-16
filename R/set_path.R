# set_path.R
# Build and assign project directory paths to the global environment.
# Called once at app startup via source() in app.R.


# Path builder ----

set_path <- function(path = getwd()) {
  # Guard against passing the function object instead of calling it
  if (is.function(path)) path <- path()
  stopifnot(is.character(path), length(path) == 1)

  # loc  = raw data folder
  # pdf  = generated PDF/HTML reports
  # exl  = generated Excel reports (same folder as pdf for consistency)
  list(
    loc = file.path(path, "Data 2025"),
    pdf = file.path(path, "Reports"),
    exl = file.path(path, "Reports")
  )
}


# Assign paths to global environment ----

paths          <- set_path(getwd())
.GlobalEnv$loc <- paths$loc
.GlobalEnv$pdf <- paths$pdf
.GlobalEnv$exl <- paths$exl

message("LOC path : ", .GlobalEnv$loc)
message("PDF path : ", .GlobalEnv$pdf)
message("EXL path : ", .GlobalEnv$exl)


# Create directories if missing ----

if (!dir.exists(.GlobalEnv$pdf)) {
  dir.create(.GlobalEnv$pdf, recursive = TRUE, showWarnings = FALSE)
  message("Created Reports directory at: ", .GlobalEnv$pdf)
}
