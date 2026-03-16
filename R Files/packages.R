# packages.R
# Load all required packages for the MSD Shiny QC application.
# Assumes packages are pre-installed (e.g. on Posit Connect).
# To install missing packages locally, uncomment the block in "Install" below.


# Package list ----

packages <- c(
  "bslib",
  "bs4Dash",
  "dplyr",
  "DT",
  "fs",
  "future",
  "ggplot2",
  "gt",
  "here",
  "kableExtra",
  "knitr",
  "lubridate",
  "openxlsx",
  "promises",
  "purrr",
  "quarto",
  "readr",
  "readxl",
  "rlang",
  "rsconnect",
  "shiny",
  "shinyalert",
  "shinycssloaders",
  "shinyjs",
  "shinythemes",
  "shinyWidgets",
  "stringi",
  "stringr",
  "tidyr",
  "tinytex",
  "tools",
  "writexl"
)


# Install missing packages (local development only) ----
# Uncomment when running locally; leave commented on Posit Connect.

# missing_pkgs <- setdiff(packages, rownames(installed.packages()))
# if (length(missing_pkgs) > 0) install.packages(missing_pkgs)


# Load and verify ----

loaded <- sapply(packages, require, character.only = TRUE, quietly = TRUE)

if (!all(loaded)) {
  message("Some packages failed to load: ", paste(names(loaded)[!loaded], collapse = ", "))
} else {
  message("All packages loaded successfully.")
}
