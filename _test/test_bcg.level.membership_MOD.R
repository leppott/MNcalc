# Test, modify level membership
# Erik.Leppo@tetratech.com
# 20241008
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use bug sample, Index_Class "bug4"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Packages----
library(BCGcalc)
library(readxl)
library(dplyr)
source(file.path("_test", "BCG.Level.Membership.R"))

# Import Rules----
df_rules <- readxl::read_excel(system.file("extdata/Rules.xlsx"
                                           , package = "BCGcalc")
                               , sheet = "Rules")

# df_rules <- readxl::read_excel(file.path("_test", "Rules_mod_MN_flip.xlsx"),
#                                sheet = "Rules")

df_rules %>%
  filter(INDEX_CLASS == "fish7") %>%
  View()

# # metrics (pre-calc) ----
# df_met_tst <- "BCG_2metval_all_b4_20050152.csv"
df_met_tst <- "BCG_2metval_all_f7_20130276.csv"
df_met_val_bugs <- read.csv(file.path("_test", df_met_tst))

# Calculate Metric Memberships
df_met_memb <- BCG.Metric.Membership(df_met_val_bugs, df_rules)
# df_met_memb <- read.csv(file.path("_test", "BCG_3metmemb.csv"))

# tst_met_memb <- df_met_memb %>%
#   mutate(MEMBERSHIP = case_when(EXC_RULE == "FLIPMINMAX" ~ - MEMBERSHIP,
#                                 .default = MEMBERSHIP)) %>%
#   head()

# View(df.lev)

# Calculate Level Memberships
df_lev_memb <- BCG.Level.Membership(df_met_memb, df_rules)

# Show results
View(df_lev_memb)


# levels and rules
df_rules_alt1 <- df_rules %>%
  filter(Index_Name == "MN_BCG") %>%
  filter(Rule_Type == "Rule1")

table(df_rules_alt1$INDEX_CLASS, df_rules_alt1$Level)

df_rules_alt2 <- df_rules %>%
  filter(Index_Name == "MN_BCG") %>%
  filter(Rule_Type == "Rule2")

table(df_rules_alt2$INDEX_CLASS, df_rules_alt2$Level)
