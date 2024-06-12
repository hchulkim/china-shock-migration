if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, gt, tidyverse)


# read in the sigungu code

cz <- read_rds(here("concordance", "region", "final_region_code.rds"))


cz_data <- readxl::read_excel(here("concordance", "region", "cz_data.xlsx")) %>% 
  mutate(cz_code=as.character(cz_code))

cz <- cz %>% mutate(cz_code=ifelse(str_sub(iso, 1, 2) %in% c("11", "21", "22", "23", "25", "24", "26", "39"), str_sub(iso,1,2), iso))

cz <- cz %>% left_join(cz_data, by="cz_code")

cz %>% arrange(cz_id) %>% gt() %>%  
  gtsave(here("data", "commuting_zone", "commuting_zone.html"))

cz %>% 
  write_rds(here("data", "commuting_zone", "commuting_zone.rds"))

cz %>% 
  write_rds(here("data","final", "cz_code", "commuting_zone.rds"))

# also later match the data for eup region 