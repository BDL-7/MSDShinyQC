

#--------------------------------------------#
#         Signal and MSD Cutoff data         #
#--------------------------------------------#


cutoff_xlsx <- file.path("Data", "Signal and MSD Cutoffs.xlsx")

# Read datasets
signalcutoffs <- readxl::read_excel(cutoff_xlsx, sheet = "Signal_Cut_Offs")
msdcutoffs    <- readxl::read_excel(cutoff_xlsx, sheet = "MSD_Cut_Offs")

# RDS folder
if (!dir.exists(RDS_DIR)) dir.create(RDS_DIR, 
                                     recursive = TRUE, 
                                     showWarnings = FALSE)
saveRDS(signalcutoffs, file.path(RDS_DIR, "signalcutoffs.rds"))
saveRDS(msdcutoffs,    file.path(RDS_DIR, "msdcutoffs.rds"))

#--------------------------------------------#
#                Helpers                     #
#--------------------------------------------#

msd_cutoff_filter <- function(msdcutoffs, plex, abType) {
  msdcutoffs %>%
    dplyr::filter(`Plate Type` %in% plex, Antibody %in% abType) %>%
    as.data.frame()
}

sig_cutoff_filter <- function(signalcutoffs, plex, abType) {
  signalcutoffs %>%
    dplyr::filter(`Plate Type` %in% plex, Antibody %in% abType) %>%
    as.data.frame()
}

#--------------------------------------------#
#                   END                      #
#--------------------------------------------#
