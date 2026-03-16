# MSD Shiny QC

MSD Shiny QC is an R Shiny dashboard for reviewing MSD assay Excel exports, validating the expected workbook structure, joining assay cutoff tables, and generating plate-level QC outputs.

The app currently supports multiple teams and workflows:

- `HIR`
- `IVYBivalent`
- `Sam5k50k`
- `VRI`

For `HIR`, the app renders per-plate HTML QC reports with view/download links in the dashboard. For the other teams, the app prepares QC output for Excel export.

## What the app does

The application is built around a fixed MSD workbook format and a two-tab workflow:

- `QC Report`
  - Upload one experiment `.xlsx` file
  - Validate workbook sheets and required columns
  - Parse experiment metadata, reagent metadata, and plate metadata
  - Join signal and MSD cutoff tables
  - Build merged raw data for review and download
  - Run team-specific QC logic
  - Generate HTML reports for `HIR` or downloadable QC Excel output for other teams
- `Append Files`
  - Upload multiple experiment `.xlsx` files
  - Optionally remove duplicate rows
  - Preview the appended table
  - Download the combined Excel output

## Required input files

### Experiment workbook

The main upload file must be an Excel workbook (`.xlsx`) with these required sheets:

- `Exp_Data_Tbl`
- `Experiment_Info`

The validation logic in [R/validate_excel.R](C:/Users/sty4/OneDrive%20-%20CDC/Serology/MSDShinyQC_GitHub/R/validate_excel.R) expects `Exp_Data_Tbl` to contain, at minimum, the following columns:

- `Assay`
- `Sample Group`
- `Sample`
- `Dilution`
- `Well`
- `Spot`
- `Calc. Concentration`
- `Calc. Conc. Mean`
- `Concentration`
- `Detection Range`
- `Signal`
- `Mean`
- `Std. Deviation`
- `CV`
- `% Recovery`
- `% Recovery Mean`
- `Calc. Conc. Std. Deviation`
- `Calc. Conc. CV`
- `Detection Limits: Calc. Low`
- `Detection Limits: Calc. High`
- `Excluded`
- `Fit Statistic: RSquared`
- `Plate Name`

The `Experiment_Info` sheet is expected to include these sections in the current layout:

- Section 1: experiment metadata
- Section 2: reagents, lot numbers, and expiry dates
- Section 3: plate barcodes and read times

### Cutoff workbook

The app also expects a cutoff workbook at:

- `Data/Signal and MSD Cutoffs.xlsx`

That workbook must contain these sheets:

- `Signal_Cut_Offs`
- `MSD_Cut_Offs`

These datasets are loaded in [R/msdsigdata.R](C:/Users/sty4/OneDrive%20-%20CDC/Serology/MSDShinyQC_GitHub/R/msdsigdata.R) and joined into the uploaded experiment data based on plate type, antibody, assay, and sample where applicable.

## Repo structure

Current repository layout:

```text
MSDShinyQC_GitHub/
|-- app.R
|-- appDirectory.R
|-- HTML.qmd
|-- PDF.qmd
|-- styles.css
|-- README.md
`-- R/
    |-- DataTable.R
    |-- DataTable_IVYBivalent.R
    |-- DataTable_Sam5K50K.R
    |-- DataTable_VRI.R
    |-- helpers.R
    |-- msdsigdata.R
    |-- packages.R
    |-- process_excel_files.R
    |-- run_qc.R
    |-- set_path.R
    |-- shiny_quarto_safety.R
    `-- validate_excel.R
```

Runtime-generated directories are created automatically by [appDirectory.R](C:/Users/sty4/OneDrive%20-%20CDC/Serology/MSDShinyQC_GitHub/appDirectory.R):

- `RDS/`
- `Reports/`

Additional paths defined in [R/set_path.R](C:/Users/sty4/OneDrive%20-%20CDC/Serology/MSDShinyQC_GitHub/R/set_path.R):

- `Data 2025/`
- `Reports/`

## How to run the app

### Prerequisites

- R installed locally
- Quarto installed and available to R
- Required R packages installed
- The cutoff workbook present at `Data/Signal and MSD Cutoffs.xlsx`

The package load list is maintained in [R/packages.R](C:/Users/sty4/OneDrive%20-%20CDC/Serology/MSDShinyQC_GitHub/R/packages.R). It includes packages such as:

- `shiny`
- `bs4Dash`
- `DT`
- `dplyr`
- `tidyr`
- `readxl`
- `openxlsx`
- `quarto`
- `kableExtra`
- `shinyWidgets`
- `shinyalert`

### Start the app from R

From the repository root:

```r
source("app.R")
shiny::runApp()
```

Or directly:

```r
shiny::runApp(".")
```

## Processing flow

The main app logic lives in [app.R](C:/Users/sty4/OneDrive%20-%20CDC/Serology/MSDShinyQC_GitHub/app.R).

When a user uploads a workbook and runs QC, the app does the following:

1. Validates the workbook structure with `validate_msd_file()`.
2. Parses the workbook with `process_excel_files()`.
3. Extracts:
   - Section 1 experiment metadata
   - Section 2 reagent and lot metadata
   - Section 3 plate-level metadata
4. Joins MSD and signal cutoff tables.
5. Builds the merged raw data table shown in the UI.
6. Selects the team-specific QC script.
7. Partitions the merged data by antibody and plate.
8. Saves intermediate RDS objects to `RDS/`.
9. Generates report outputs or downloadable QC data.

## How reports are generated

Report generation is orchestrated in [R/run_qc.R](C:/Users/sty4/OneDrive%20-%20CDC/Serology/MSDShinyQC_GitHub/R/run_qc.R).

For `HIR`:

- The app builds per-plate QC tables using `DataTableFunc`.
- Per-plate intermediate objects are written to `RDS/`.
- The app renders [HTML.qmd](C:/Users/sty4/OneDrive%20-%20CDC/Serology/MSDShinyQC_GitHub/HTML.qmd) with Quarto.
- Rendered HTML files are copied into `Reports/`.
- The dashboard displays `View` and `Download` links for each generated plate report.

Each HTML report includes:

- Experiment name
- Experiment date
- Plate ID
- User ID
- Generated timestamp
- Plate-level QC summary
- Experiment metadata
- Reagent and lot information
- Plate metadata summary
- Controls table
- Standards table
- Unknowns table

The repository also contains [PDF.qmd](C:/Users/sty4/OneDrive%20-%20CDC/Serology/MSDShinyQC_GitHub/PDF.qmd), which is a PDF report template for the same data model, although the current `HIR` flow renders HTML.

## Output files

The app can generate:

- Merged raw data downloads as `.xlsx`, `.csv`, or `.rds`
- QC Excel output for non-`HIR` teams
- HTML QC reports in `Reports/`
- Intermediate RDS files in `RDS/`

## Notes

- The current `README` reflects the repository as it exists now, including the `R/` source directory and current runtime paths.
- Generated directories such as `RDS/` and `Reports/` are not source code and should generally be excluded from version control in future cleanup.
