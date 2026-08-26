# test xtab error
# Erik.Lepppo@tetratech.com
# 20260826
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# saved data from server.R, calc BCG
# example data is MN Fish, small
# write.csv(df_results, "df_results.csv", row.names = FALSE)
# write.csv(df_metmemb, "df_metmemb.csv", row.names = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Data ----
dn_shiny <- file.path("inst", "apps", "MNcalc")
df_results <- read.csv(file.path(dn_shiny, "df_results.csv"))
df_metmemb <- read.csv(file.path(dn_shiny, "df_metmemb.csv"))


# server code----
df_metmemb_xtab <- df_results |>
  # cols to keep
  # dplyr::select(SampleID, BCG_Status2, NumFlags) |>
  # 20260824
  dplyr::select(SampleID, BCG_Status2) |>
  # join tables
  dplyr::left_join(y = df_metmemb |>
                     dplyr::select(SAMPLEID,
                                   INDEX_CLASS,
                                   METRIC_NAME,
                                   DESCRIPTION,
                                   METRIC_VALUE,
                                   LEVEL,
                                   MEMBERSHIP,
                                   METRIC_SORT) |>
                     # account for Rule 1 and 2 duplicates
                     distinct(),
                   by = dplyr::join_by(SampleID == SAMPLEID)) |>
  # pivot
  tidyr::pivot_wider(id_cols = c(INDEX_CLASS,
                                 SampleID,
                                 BCG_Status2,
                                 # NumFlags,
                                 METRIC_SORT,
                                 METRIC_NAME,
                                 DESCRIPTION,
                                 METRIC_VALUE),
                     names_from = LEVEL,
                     values_from = MEMBERSHIP,
                     names_sort = TRUE,
                     names_prefix = "L") |>
  dplyr::arrange(SampleID, METRIC_SORT)
