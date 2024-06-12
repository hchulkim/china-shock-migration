# Creator: Hyoungchul Kim
# Revised date: 2024/07/12
# Description: This is a run script for the whole project. 

# PACKAGES -----------------------------------------------------------------
library(here)

# PRELIMINARIES -------------------------------------------------------------
# Control which scripts run
run_01_ex_dataprep <- 1
run_02_ex_reg      <- 1
run_03_ex_table    <- 1
run_04_ex_graph    <- 1

concordance_01_hs_cpc_ksic <- 1

# RUN SCRIPTS ---------------------------------------------------------------

# Make concordance table from hs - cpc - ksic code.
if (concordance_01_hs_cpc_ksic) source(here("scripts", "concordance_01_hs_cpc_ksic.R"), encoding = "UTF-8")

