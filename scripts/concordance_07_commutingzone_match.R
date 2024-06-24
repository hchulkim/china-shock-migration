
##############################
# creator: Hyoungchul Kim
# revised date: 06/24/2024
# description: This script makes commuting zone code. 
##############################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, gt, tidyverse)


# read in the sigungu code

cz <- read_rds(here("data","concordance", "region", "final_region_code.rds"))


cz_data <- readxl::read_excel(here("data", "concordance", "region", "cz_data.xlsx")) %>% 
  mutate(cz_code=as.character(cz_code))

cz <- cz %>% mutate(cz_code=ifelse(str_sub(iso, 1, 2) %in% c("11", "21", "22", "23", "25", "24", "26", "39"), str_sub(iso,1,2), iso))

cz <- cz %>% left_join(cz_data, by="cz_code")

cz %>% arrange(cz_id) %>% gt() %>%  
  gtsave(here("proc", "commuting_zone.html"))

cz %>% 
  write_rds(here("proc", "commuting_zone.rds"))

