# DataTable.R
# Core QC logic for HIR and IVYBivalent team data.
# Accepts a single plate-partition data frame and a team identifier.
# Returns: list(controls, standards, qc, unknowns)
#
# team = "HIR"         — default HIR logic
# team = "IVYBivalent" — IVYBivalent-specific QC rules


DataTableFunc <- function(part_data, team = "HIR") {

  stopifnot(is.data.frame(part_data))


  # Helpers ----

  to_num <- function(x) suppressWarnings(as.numeric(ifelse(x %in% c(".", "NaN", "NA", ""), NA, x)))
  `%||%` <- function(x, y) if (is.null(x)) y else x


  # Team-specific LOQ label constants ----
  # All LOQ comparisons use these variables so label changes propagate everywhere.

  below_loq <- if (identical(team, "IVYBivalent")) "Below LLOQ" else "Below LOQ"
  above_loq <- if (identical(team, "IVYBivalent")) "Above ULOQ" else "Above LOQ"


  # Column validation ----

  must_have <- c("Sample", "Assay", "Plate Name", "Sample Group")
  missing   <- setdiff(must_have, names(part_data))
  if (length(missing)) {
    stop(sprintf("DataTableFunc: missing required columns: %s", paste(missing, collapse = ", ")))
  }


  # Data preparation & numeric coercions ----

  dt <- part_data |>
    dplyr::rename(
      Plate_Name                 = `Plate Name`,
      Sample_Group               = `Sample Group`,
      Calc_Conc_Mean_raw         = `Calc. Conc. Mean`,
      Calc_Conc_CV_raw           = `Calc. Conc. CV`,
      Calc_Concentration_raw     = `Calc. Concentration`,
      Concentration_raw          = Concentration,
      X_Recovery_Mean_raw        = `% Recovery Mean`,
      Fit_Statistic_RSquared_raw = `Fit Statistic: RSquared`
    ) |>
    dplyr::filter(!is.na(Sample), Sample != ".") |>
    dplyr::mutate(Sam = substr(Sample, 1, 4)) |>
    dplyr::filter(Sam != "Empt") |>
    dplyr::mutate(
      Calc_Conc_Mean  = to_num(Calc_Conc_Mean_raw),
      Calc_Conc_CV    = to_num(Calc_Conc_CV_raw),
      CalConc         = to_num(Calc_Concentration_raw),
      Conc            = to_num(Concentration_raw),          # used by IVYBivalent controlspos
      PeRecMean       = to_num(X_Recovery_Mean_raw),
      FStatRSquare    = to_num(Fit_Statistic_RSquared_raw),
      Dilution        = to_num(Dilution),
      LLOQ_Unadju_BAU = to_num(`LLOQ_Unadju_BAU`),
      ULOQ_Unadju_BAU = to_num(`ULOQ_Unadju_BAU`),
      Thres_BAU       = to_num(`Thres_BAU`),
      SLL             = to_num(`SLL`),
      SUL             = to_num(`SUL`)
    ) |>
    dplyr::mutate(
      lowerlimit    = LLOQ_Unadju_BAU,
      upperlimit    = ULOQ_Unadju_BAU,
      InwellCalConc = CalConc / Dilution,
      Thres70_BAU   = Thres_BAU * 0.7
    )


  # Detection range (workbench labels) ----

  dt <- dt |>
    dplyr::mutate(
      Detection_Range    = `Detection Range`,
      Detection_Range_WB = `Detection Range`,
      CalConc            = as.numeric(CalConc),
      CalcConc_InRange   = dplyr::case_when(
        Sample_Group == "Standards"                                               ~ CalConc,
        Sample_Group != "Standards" & Detection_Range_WB == "In Detection Range" ~ CalConc,
        TRUE                                                                      ~ NA_real_
      )
    )


  # LOQ classification ----
  # Step 1 initialise.
  # Step 2 map WB labels to LOQ labels (team-specific via below_loq / above_loq).
  # Step 3 refine "In Detection Range" using actual LOQ boundaries.

  dt <- dt |>
    dplyr::mutate(
      Detection_Range_LOQ  = NA_character_,
      CalcConc_InRange_LOQ = NA_real_
    ) |>
    dplyr::mutate(
      Detection_Range_LOQ = dplyr::case_when(
        Sample_Group != "Standards" & Detection_Range_WB == "In Detection Range"    ~ "In Detection Range",
        Sample_Group != "Standards" & Detection_Range_WB == "Below Fit Curve Range" ~ below_loq,
        Sample_Group != "Standards" & Detection_Range_WB == "Below Detection Range" ~ below_loq,
        Sample_Group != "Standards" & Detection_Range_WB == "Above Fit Curve Range" ~ above_loq,
        Sample_Group != "Standards" & Detection_Range_WB == "Above Detection Range" ~ above_loq,
        TRUE ~ Detection_Range_LOQ
      ),
      Detection_Range_LOQ = dplyr::case_when(
        Sample_Group != "Standards" &
          Detection_Range_WB == "In Detection Range" &
          lowerlimit < InwellCalConc & InwellCalConc < upperlimit ~ "In Detection Range",
        Sample_Group != "Standards" &
          Detection_Range_WB == "In Detection Range" &
          InwellCalConc > upperlimit                              ~ above_loq,
        Sample_Group != "Standards" &
          Detection_Range_WB == "In Detection Range" &
          InwellCalConc < lowerlimit                              ~ below_loq,
        TRUE ~ Detection_Range_LOQ
      ),
      CalcConc_InRange_LOQ = dplyr::case_when(
        Sample_Group != "Standards" & Detection_Range_LOQ == below_loq            ~ NA_real_,
        Sample_Group != "Standards" & Detection_Range_LOQ == above_loq            ~ NA_real_,
        Sample_Group != "Standards" & Detection_Range_LOQ == "."                  ~ NA_real_,
        Sample_Group != "Standards" & Detection_Range_LOQ == "In Detection Range" ~ CalcConc_InRange
      )
    )


  # Sample-assay key ----
  # Unique identifier combining sample, assay, and plate — used for dilution-pair matching.

  dt <- dt |>
    dplyr::mutate(sampleassay = paste(Sample, Assay, Plate_Name, sep = "_"))


  # QC decision lookup table ----
  # Low / High dilution LOQ state → QC_DRMatching verdict.
  # Uses team-specific LOQ labels via below_loq / above_loq.

  rules <- tibble::tribble(
    ~Low,                   ~High,                  ~QC_DRMatching,
    "In Detection Range",   "In Detection Range",   "PASS",
    above_loq,              "In Detection Range",   "PASS",
    below_loq,              "In Detection Range",   "FAIL",
    "In Detection Range",   above_loq,              "FAIL",
    "In Detection Range",   below_loq,              "PASS",
    above_loq,              above_loq,              "PASS",
    below_loq,              below_loq,              "PASS",
    above_loq,              below_loq,              "PASS",
    below_loq,              above_loq,              "FAIL"
  )


  # Dilution ranking & QC_DRMatching flag ----
  # Rank 1 = lowest dilution (Low), rank 2 = highest (High).

  dt_qc <- dt |>
    dplyr::group_by(sampleassay) |>
    dplyr::arrange(Dilution, .by_group = TRUE) |>
    dplyr::mutate(dil_rank = dplyr::row_number()) |>
    dplyr::select(sampleassay, dil_rank, Detection_Range_LOQ) |>
    tidyr::pivot_wider(
      names_from   = dil_rank,
      values_from  = Detection_Range_LOQ,
      names_prefix = "Dil_"
    ) |>
    dplyr::rename(Low = Dil_1, High = Dil_2) |>
    dplyr::ungroup() |>
    dplyr::left_join(rules, by = c("Low", "High"))

  dt <- dt |>
    dplyr::left_join(
      dplyr::select(dt_qc, sampleassay, QC_DRMatching),
      by = "sampleassay"
    )


  # Summary statistics (means & CVs) ----

  stats <- dt |>
    dplyr::group_by(Sample, Assay, Plate_Name) |>
    dplyr::summarise(
      MeanCalConc = mean(CalConc,              na.rm = TRUE),
      CVCalConc   = ifelse(
        is.na(MeanCalConc) | MeanCalConc == 0, NA_real_,
        stats::sd(CalConc, na.rm = TRUE) / MeanCalConc * 100
      ),
      MeanLOQ     = mean(CalcConc_InRange_LOQ, na.rm = TRUE),
      CVLOQ       = ifelse(
        is.na(MeanLOQ) | MeanLOQ == 0, NA_real_,
        stats::sd(CalcConc_InRange_LOQ, na.rm = TRUE) / MeanLOQ * 100
      ),
      Numobs      = sum(!is.na(CalcConc_InRange_LOQ)),
      Meaninrange = mean(CalcConc_InRange,     na.rm = TRUE),
      CVinrange   = ifelse(
        is.na(Meaninrange) | Meaninrange == 0, NA_real_,
        stats::sd(CalcConc_InRange, na.rm = TRUE) / Meaninrange * 100
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      MeanCalConc = round(MeanCalConc, 2),
      CVCalConc   = round(CVCalConc,   2),
      MeanLOQ     = round(MeanLOQ,     2),
      CVLOQ       = round(CVLOQ,       2),
      Meaninrange = round(Meaninrange, 2),
      CVinrange   = round(CVinrange,   2)
    )

  # Join stats; NaN → NA_real_ keeps columns numeric (avoids round() errors downstream)
  dt_mean <- dt |>
    dplyr::left_join(stats, by = c("Sample", "Assay", "Plate_Name")) |>
    dplyr::mutate(
      MeanCalConc = ifelse(is.nan(MeanCalConc), NA_real_, MeanCalConc),
      Meaninrange = ifelse(is.nan(Meaninrange), NA_real_, Meaninrange),
      MeanLOQ     = ifelse(is.nan(MeanLOQ),     NA_real_, MeanLOQ)
    )


  # Dataset splits ----

  controlsneg <- dt_mean |> dplyr::filter(Sample_Group == "Controls - Neg")
  controlspos <- dt_mean |> dplyr::filter(Sample_Group == "Controls")
  standards   <- dt_mean |> dplyr::filter(Sample_Group == "Standards")
  unknowns    <- dt_mean |> dplyr::filter(Sample_Group == "Unknowns")

  # Shared output column order for controls, standards, and unknowns.
  # "Conc" (raw Concentration from instrument) is included so the display
  # formatting functions in process_excel_files.R can label it "Expected Conc".
  output_cols <- c(
    "Assay", "Sample_Group", "Sample", "Plate_Name", "Dilution",
    "Conc",
    "upperlimit", "lowerlimit", "InwellCalConc", "Thres_BAU", "Thres70_BAU",
    "Detection_Range_WB", "Detection_Range_LOQ", "Detection_Range_LOQ_Display",
    "Percent_Recovery_Mean", "Fit_Statistic_RSquared",
    "Signal", "SUL", "SLL",
    "CalConc", "MeanCalConc", "CVCalConc",
    "QC_CalcConc", "QC_CalcConc30",
    "CalcConc_InRange", "Meaninrange", "CVinrange", "QC_InRange",
    "CalcConc_InRange_LOQ", "MeanLOQ", "CVLOQ", "Numobs",
    "ThresCall_BAU", "QC_LOQ", "QC_DRMatching", "Analyte_QC", "Sample_QC"
  )


  # Controls negative: Analyte_QC ----
  # Shared logic; only the LOQ label differs (handled by above_loq).

  controlsneg <- controlsneg |>
    dplyr::mutate(
      Analyte_QC = dplyr::case_when(
        Detection_Range_LOQ == above_loq ~ "FAIL",
        MeanLOQ > Thres_BAU              ~ "FAIL",
        TRUE                             ~ "PASS"
      )
    )


  # Controls negative: Sample_QC ----
  # HIR:         REPEAT if ≥ 2 Signal > SUL or any Signal ≥ 130 % of SUL.
  # IVYBivalent: REPEAT if fail percentage > 35 %; uses Mean column.

  if (identical(team, "IVYBivalent")) {

    controlsneg <- controlsneg |>
      dplyr::group_by(Assay, Sample, Plate_Name, `Plate Type`, Antibody) |>
      dplyr::mutate(
        above_SUL       = sum(Mean > SUL,         na.rm = TRUE),
        above130_SUL    = any(Mean >= 1.3 * SUL,  na.rm = TRUE),
        fail_count      = sum(Analyte_QC == "FAIL" | above130_SUL, na.rm = TRUE),
        total_count     = dplyr::n(),
        fail_percentage = (fail_count / total_count) * 100,
        Sample_QC       = ifelse(
          Sample_Group %in% "Controls - Neg" & fail_percentage > 35,
          "REPEAT", "PASS"
        )
      ) |>
      dplyr::ungroup()

  } else {

    controlsneg <- controlsneg |>
      dplyr::group_by(Sample) |>
      dplyr::mutate(
        Sample_QC = ifelse(
          Sample_Group %in% "Controls - Neg" &
            (sum(Signal > SUL, na.rm = TRUE) >= 2 | any(Signal >= 1.3 * SUL, na.rm = TRUE)),
          "REPEAT", "PASS"
        )
      ) |>
      dplyr::ungroup()

  }


  # Controls positive: Analyte_QC & Sample_QC ----
  # HIR:         LLOQ check uses CalConc; Sample_QC computed later in combined step.
  # IVYBivalent: LLOQ check uses Conc; Sample_QC computed here per-subset,
  #              grouped by Assay + Plate_Name (no Antibody).
  #              Note: condition is passcount >= flagcontrols (preserved from source).

  if (identical(team, "IVYBivalent")) {

    controlspos <- controlspos |>
      dplyr::mutate(
        Analyte_QC = dplyr::case_when(
          Conc == LLOQ_Unadju_BAU                                             ~ "PASS",
          PeRecMean < 70 | PeRecMean >= 130 | CVLOQ >= 20                    ~ "FAIL",
          PeRecMean >= 70 & PeRecMean < 130 & !is.na(CVLOQ) & CVLOQ < 20    ~ "PASS",
          TRUE                                                                ~ NA_character_
        )
      ) |>
      dplyr::group_by(Assay, Plate_Name) |>
      dplyr::mutate(
        flagcontrols = round(as.numeric(Plex_Count) * 3 * 0.78),
        passcount    = sum(Analyte_QC == "PASS" & Sample_Group %in% "Controls", na.rm = TRUE)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        Sample_QC = ifelse(passcount >= flagcontrols, "PASS", "REPEAT")
      )

  } else {

    # HIR: Sample_QC is recomputed in the combined controls step below
    controlspos <- controlspos |>
      dplyr::mutate(
        Analyte_QC = dplyr::case_when(
          CalConc == LLOQ_Unadju_BAU                                           ~ "PASS",
          PeRecMean < 70 | PeRecMean >= 130 | CVLOQ >= 20                     ~ "FAIL",
          PeRecMean >= 70 & PeRecMean < 130 & !is.na(CVLOQ) & CVLOQ < 20     ~ "PASS",
          TRUE                                                                 ~ NA_character_
        )
      )

  }


  # Controls — combined ----
  # Shared cleanup applied to the bound dataset.
  # HIR: Sample_QC recomputed here on the combined data (78 % passcount rule).
  # IVYBivalent: Sample_QC already set per-subset above; no combined recompute needed.

  controls <- dplyr::bind_rows(
    dplyr::mutate(controlsneg, Analyte_QC = as.character(Analyte_QC)),
    dplyr::mutate(controlspos, Analyte_QC = as.character(Analyte_QC))
  ) |>
    dplyr::mutate(
      Detection_Range_LOQ_Display = NA_character_,
      QC_CalcConc                 = NA_character_,
      QC_CalcConc30               = NA_character_,
      QC_InRange                  = NA_character_,
      ThresCall_BAU               = NA_character_,
      QC_LOQ                      = NA_character_,
      CalConc                     = as.numeric(CalConc),
      PeRecMean                   = ifelse(!is.na(PeRecMean), round(PeRecMean, 2), PeRecMean),
      MeanLOQ                     = ifelse(!is.na(MeanLOQ),   round(MeanLOQ,   2), MeanLOQ),
      CVLOQ                       = ifelse(!is.na(CVLOQ),     round(CVLOQ,     2), CVLOQ)
    )

  if (identical(team, "HIR")) {
    controls <- controls |>
      dplyr::group_by(Assay, Plate_Name, Antibody) |>
      dplyr::mutate(
        flagcontrols = round(as.numeric(Plex_Count) * 3 * 0.78),
        passcount    = sum(Analyte_QC == "PASS" & Sample_Group %in% "Controls", na.rm = TRUE),
        Sample_QC    = ifelse(passcount >= flagcontrols, "PASS", "REPEAT")
      ) |>
      dplyr::ungroup()
  }

  controls <- controls |>
    dplyr::rename(
      Percent_Recovery_Mean  = PeRecMean,
      Fit_Statistic_RSquared = FStatRSquare
    ) |>
    dplyr::select(dplyr::all_of(output_cols))


  # Standards ----
  # Shared setup; Analyte_QC criteria differ by team.
  #
  # HIR:
  #   S002–S006 — recovery 70–130 %, CV < 20 %, R² ≥ 0.99
  #   S007      — recovery 70–130 %, CV < 30 %, R² ≥ 0.99
  #
  # IVYBivalent:
  #   S002–S005 — recovery 70–130 %, CV < 20 %, R² ≥ 0.99
  #   S006/S007 — if Below Detection/Fit Curve Range → "Out of Range - Low";
  #               if pass criteria (CV < 30 %) → "PASS"; else "FAIL"

  standards <- standards |>
    dplyr::arrange(Plate_Name) |>
    dplyr::mutate(
      Detection_Range_LOQ_Display = NA_character_,
      QC_CalcConc                 = NA_character_,
      QC_CalcConc30               = NA_character_,
      QC_InRange                  = NA_character_,
      ThresCall_BAU               = NA_character_,
      QC_LOQ                      = NA_character_,
      PeRecMean                   = round(PeRecMean,    2),
      FStatRSquare                = round(FStatRSquare, 4),
      CalConc                     = as.numeric(CalConc)
    )

  if (identical(team, "IVYBivalent")) {

    standards <- standards |>
      dplyr::mutate(
        Analyte_QC = dplyr::case_when(

          # S002–S005: CV threshold 20 %
          Sample %in% c("S002","S003","S004","S005") &
            (is.na(PeRecMean) | PeRecMean < 70 | PeRecMean >= 130 |
               is.na(CVCalConc) | CVCalConc >= 20 |
               is.na(FStatRSquare) | FStatRSquare < 0.99)                         ~ "FAIL",

          Sample %in% c("S002","S003","S004","S005") &
            (!is.na(PeRecMean) & PeRecMean >= 70 & PeRecMean < 130 &
               !is.na(CVCalConc) & CVCalConc < 20 &
               !is.na(FStatRSquare) & FStatRSquare >= 0.99)                       ~ "PASS",

          # S006 & S007: below fit / detection range → "Out of Range - Low"
          # (these are excluded from the passcount used in Sample_QC)
          Sample %in% c("S006","S007") &
            Detection_Range_WB %in% c("Below Detection Range",
                                      "Below Fit Curve Range")                    ~ "Out of Range - Low",

          # S006 & S007: pass if recovery / CV / R² criteria met (CV threshold 30 %)
          Sample %in% c("S006","S007") &
            !is.na(PeRecMean) & PeRecMean >= 70 & PeRecMean < 130 &
            !is.na(CVCalConc) & CVCalConc < 30 &
            !is.na(FStatRSquare) & FStatRSquare >= 0.99                           ~ "PASS",

          # S006 & S007: otherwise FAIL
          Sample %in% c("S006","S007")                                            ~ "FAIL",

          # S001: pass if signal ≥ SLL, or recovery in range, or CV < 20 %
          Sample == "S001" &
            ((!is.na(Signal) & !is.na(SLL) & Signal >= SLL) |
               (!is.na(PeRecMean) & PeRecMean >= 70 & PeRecMean < 130) |
               (!is.na(CVCalConc) & CVCalConc < 20))                              ~ "PASS",

          Sample == "S001"                                                        ~ "FAIL",

          # S008: pass if signal ≤ SUL, CalConc missing, or CalConc > first CalConc
          Sample == "S008" &
            (Signal <= SUL | is.na(CalConc) | CalConc > CalConc[1])               ~ "PASS",

          Sample == "S008"                                                        ~ "FAIL",

          TRUE ~ NA_character_
        )
      )

  } else {

    # HIR
    standards <- standards |>
      dplyr::mutate(
        Analyte_QC = dplyr::case_when(

          # S002–S006: CV threshold 20 %
          Sample %in% c("S002","S003","S004","S005","S006") &
            (is.na(PeRecMean) | PeRecMean < 70 | PeRecMean >= 130 |
               is.na(CVCalConc) | CVCalConc >= 20 |
               is.na(FStatRSquare) | FStatRSquare < 0.99)                         ~ "FAIL",

          Sample %in% c("S002","S003","S004","S005","S006") &
            (!is.na(PeRecMean) & PeRecMean >= 70 & PeRecMean < 130 &
               !is.na(CVCalConc) & CVCalConc < 20 &
               !is.na(FStatRSquare) & FStatRSquare >= 0.99)                       ~ "PASS",

          # S007: CV threshold 30 %
          Sample == "S007" &
            (is.na(PeRecMean) | PeRecMean < 70 | PeRecMean >= 130 |
               is.na(CVCalConc) | CVCalConc >= 30 |
               is.na(FStatRSquare) | FStatRSquare < 0.99)                         ~ "FAIL",

          Sample == "S007" &
            (!is.na(PeRecMean) & PeRecMean >= 70 & PeRecMean < 130 &
               !is.na(CVCalConc) & CVCalConc < 30 &
               !is.na(FStatRSquare) & FStatRSquare >= 0.99)                       ~ "PASS",

          # S001: pass if signal ≥ SLL, or recovery in range, or CV < 20 %
          Sample == "S001" &
            ((!is.na(Signal) & !is.na(SLL) & Signal >= SLL) |
               (!is.na(PeRecMean) & PeRecMean >= 70 & PeRecMean < 130) |
               (!is.na(CVCalConc) & CVCalConc < 20))                              ~ "PASS",

          Sample == "S001"                                                        ~ "FAIL",

          # S008: pass if signal ≤ SUL, CalConc missing, or CalConc > first CalConc
          Sample == "S008" &
            (Signal <= SUL | is.na(CalConc) | CalConc > CalConc[1])               ~ "PASS",

          Sample == "S008"                                                        ~ "FAIL",

          TRUE ~ NA_character_
        )
      )

  }

  standards <- standards |>
    dplyr::group_by(Assay, Plate_Name, Antibody) |>
    dplyr::mutate(
      flagstandards = round(as.numeric(Plex_Count) * 3 * 0.78),
      passcount     = sum(Analyte_QC == "PASS", na.rm = TRUE),
      Sample_QC     = ifelse(passcount >= flagstandards, "PASS", "REPEAT")
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(
      Percent_Recovery_Mean  = PeRecMean,
      Fit_Statistic_RSquared = FStatRSquare
    ) |>
    dplyr::select(dplyr::all_of(output_cols))


  # Plate-level QC summary ----
  # PASS / REPEAT derived from standards and controls.
  # If either repeats, all unknowns are forced to REPEAT.

  controls  <- dplyr::mutate(controls,  dplyr::across(c(Analyte_QC, Sample_QC), as.character))
  standards <- dplyr::mutate(standards, dplyr::across(c(Analyte_QC, Sample_QC), as.character))

  s_rep    <- any(tolower(standards$Sample_QC %||% "") == "repeat", na.rm = TRUE)
  c_rep    <- any(tolower(controls$Sample_QC  %||% "") == "repeat", na.rm = TRUE)
  plate_qc <- if (s_rep || c_rep) "REPEAT" else "PASS"

  if (!is.null(unknowns)) {
    if (!"Sample_QC" %in% names(unknowns)) unknowns$Sample_QC <- NA_character_
    unknowns$Sample_QC <- if (s_rep || c_rep) "REPEAT" else "PASS"
  }

  tbl_qc <- data.frame(Plate_QC = plate_qc, stringsAsFactors = FALSE)


  # Unknowns ----
  # ThresCall_BAU and Detection_Range_LOQ_Display use team-specific LOQ labels
  # via below_loq / above_loq — no explicit branching needed here.

  unknowns <- unknowns |>
    dplyr::filter(Plate_Name != "MSD065A_2BM8NAM627") |>
    dplyr::mutate(
      flag                        = NA_integer_,
      ThresCall_BAU               = NA_character_,
      Detection_Range_LOQ_Display = NA_character_,
      Percent_Recovery_Mean       = NA_real_,
      Fit_Statistic_RSquared      = NA_real_,
      QC_CalcConc                 = NA_character_,
      QC_CalcConc30               = NA_character_,
      QC_InRange                  = NA_character_,
      QC_LOQ                      = NA_character_,
      Analyte_QC                  = NA_character_,
      CalConc                     = as.numeric(CalConc)
    ) |>
    dplyr::mutate(
      flag = dplyr::case_when(
        is.na(CalcConc_InRange)          ~ 1L,
        CalcConc_InRange < Thres70_BAU   ~ 1L,
        CalcConc_InRange >= Thres70_BAU  ~ 2L,
        TRUE ~ flag
      ),
      ThresCall_BAU = dplyr::case_when(
        MeanLOQ >= Thres_BAU                                                               ~ "POS",
        MeanLOQ <  Thres_BAU                                                               ~ "< TH",
        Detection_Range_LOQ == "In Detection Range" & is.na(MeanLOQ)                      ~ "CHECK",
        Detection_Range_LOQ %in% c("Above Fit Curve Range", above_loq) & is.na(MeanLOQ)  ~ "> ULOQ",
        Detection_Range_LOQ %in% c(below_loq, "Below Fit Curve Range") & is.na(MeanLOQ)  ~ "< LLOQ",
        TRUE ~ ThresCall_BAU
      ),
      Detection_Range_LOQ_Display = dplyr::case_when(
        Detection_Range_LOQ == above_loq & Numobs == 1 ~ "In Detection Range",
        Detection_Range_LOQ == below_loq & Numobs == 1 ~ "In Detection Range",
        TRUE ~ Detection_Range_LOQ
      ),
      QC_CalcConc = dplyr::case_when(
        CVCalConc >= 30 & MeanCalConc >= Thres70_BAU ~ "FAIL",
        TRUE                                         ~ "PASS"
      ),
      QC_CalcConc30 = dplyr::case_when(
        CVCalConc >= 30 ~ "FAIL",
        TRUE            ~ QC_CalcConc30
      ),
      QC_InRange = dplyr::case_when(
        CVinrange >= 30 & Meaninrange >= Thres70_BAU ~ "FAIL",
        TRUE                                         ~ "PASS"
      ),
      QC_LOQ = dplyr::case_when(
        CVLOQ >= 30 & MeanLOQ >= Thres70_BAU ~ "FAIL",
        TRUE                                 ~ "PASS"
      )
    ) |>
    dplyr::mutate(
      Sample_QC = dplyr::case_when(
        QC_LOQ == "FAIL"                                                   ~ "FAIL",
        QC_LOQ == "PASS" & QC_DRMatching == "FAIL" & QC_InRange == "FAIL" ~ "FAIL",
        QC_LOQ == "PASS" & QC_DRMatching == "FAIL" & QC_InRange == "PASS" ~ "FAIL",
        QC_LOQ == "PASS" & QC_DRMatching == "PASS" & QC_InRange == "FAIL" ~ "PASS",
        QC_LOQ == "PASS" & QC_DRMatching == "PASS" & QC_InRange == "PASS" ~ "PASS",
        TRUE ~ "PASS"
      )
    ) |>
    dplyr::select(dplyr::all_of(output_cols))


  # Return ----

  list(
    controls  = controls,
    standards = standards,
    qc        = tbl_qc,   # single-row: Plate_QC = "PASS" or "REPEAT"
    unknowns  = unknowns
  )
}
