# Creator: Hyoungchul Kim
# Revised date: 2024/07/12
# Description: This is a run script for the whole project. 

# PACKAGES -----------------------------------------------------------------
library(here)

# PRELIMINARIES -------------------------------------------------------------
# Control which scripts run
concordance_01_hs_cpc_ksic <- 1
concordance_02_comtrade_hs_ksic <- 1

# RUN SCRIPTS ---------------------------------------------------------------

# Make concordance table from hs - cpc - ksic code.
if (concordance_01_hs_cpc_ksic) source(here("scripts", "concordance_01_hs_cpc_ksic.R"), encoding = "UTF-8")

# Use concordance table to transition hs code comtrade data to ksic industry code data.
if (concordance_02_comtrade_hs_ksic) source(here("scripts", "concordance_02_comtrade_hs_ksic.R"), encoding = "UTF-8")


