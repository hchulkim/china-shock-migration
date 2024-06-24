# Creator: Hyoungchul Kim
# Revised date: 2024/06/24
# Description: This is a run script for the whole project. 

# PACKAGES -----------------------------------------------------------------
library(here)

# PRELIMINARIES -------------------------------------------------------------
# Control which scripts run
concordance_01_hs_cpc_ksic <- 1
concordance_02_comtrade_hs_ksic <- 1
concordance_03_est_region_match <- 1
concordance_04_est_ksic89_ksic10 <- 1
concordance_06_exposure_construction <- 1 
concordance_07_commutingzone_match <- 1
concordance_08_population_region_matched <- 1

# RUN SCRIPTS ---------------------------------------------------------------

# Make concordance table from hs - cpc - ksic code.
if (concordance_01_hs_cpc_ksic) source(here("scripts", "concordance_01_hs_cpc_ksic.R"), encoding = "UTF-8")

# Use concordance table to transition hs code comtrade data to ksic industry code data.
if (concordance_02_comtrade_hs_ksic) source(here("scripts", "concordance_02_comtrade_hs_ksic.R"), encoding = "UTF-8")

# Use concordance table to match region data in est data.
if (concordance_03_est_region_match) source(here("scripts", "concordance_03_est_region_match.R"), encoding = "UTF-8")

# Use concordance table to match industry code ksic in est data to ksic10.
if (concordance_04_est_ksic89_ksic10) source(here("scripts", "concordance_04_est_ksic89_ksic10.R"), encoding = "UTF-8")

# Construct local exposure to trade shock (x and z variable)
if (concordance_06_exposure_construction) source(here("scripts", "concordance_06_exposure_construction.R"), encoding = "UTF-8")

# Concords region to commuting zone.
if (concordance_07_commutingzone_match ) source(here("scripts", "concordance_07_commutingzone_match .R"), encoding = "UTF-8")

# Construct pop growth var by sigungu and cz.
if (concordance_08_population_region_matche) source(here("scripts", "concordance_08_population_region_matche.R"), encoding = "UTF-8")


