library(tidyverse)

est2000 <- read_rds("data/est/est2000.rds")
est2001 <- read_rds("data/est/est2001.rds")
est2010 <- read_rds("data/est/est2010.rds")
est2019 <- read_rds("data/est/est2019.rds")




check2000 <- est2000 %>% distinct(iso)
check2001 <- est2001 %>% distinct(iso)
check2010 <- est2010 %>% distinct(iso)
check2019 <- est2019 %>% distinct(iso)





migration1996_2000 <- read_rds("data/migration/migration1996_2000_refine.rds")
migration2001_2014 <- read_rds("data/migration/migration2001_2014_refine.rds")
migration2015_2019 <- read_rds("data/migration/migration2015_2019_refine.rds")


check_m1 <- migration1996_2000 %>% distinct(iso_d)
check_m2 <- migration2001_2014 %>% distinct(iso_d)
check_m3 <- migration2015_2019 %>% distinct(iso_d)




check %>% write_rds("data/final_region_code.rds")

