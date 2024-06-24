
##############################
# creator: Hyoungchul Kim
# revised date: 06/24/2024
# description: This script matches migration data into regions for analysis. Due to change in geographic unit, we need to make sure the unit code is consistent throughout the whole year span. 
##############################

if (!require(pacman)) install.packages("pacman")
pacman::p_load(here, tidyverse)

#-------------------- 
# read in the necessary industry data and concordance table.
#-------------------- 

# read in the concordance for region stat----
region_stat <- readxl::read_excel(here("data", "region", "region_stat.xlsx"))

region_stat <- region_stat %>% mutate(across(everything(), as.character))

region_kosis <- readxl::read_excel(here("data", "region", "region_kosis.xlsx"))

region_kosis <- region_kosis %>% mutate(across(everything(), as.character))

region_kosis <- region_kosis %>% mutate(city_stat=str_sub(city_stat, 1, 5))

