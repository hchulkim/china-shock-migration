if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(here, gt, data.table, sf, rmapshaper, tidyverse)



tb_all <-  read_rds("data/final/shock/shock_main_data2001_2019.rds")


tb_all <- tb_all %>% select(cz_o, cz_d, starts_with("imp"), starts_with("exp"))








# this code basically makes the bilateral migration code and merges it with the imp / exp shock to make the basic data for OLS and IV regression

# migration1 <- read_rds("data/final/migration/migration1996_2000_refine.rds")
migration2 <- read_rds("data/final/migration/migration2001_2014_refine.rds")
migration3 <- read_rds("data/final/migration/migration2015_2019_refine.rds")
migration4 <- haven::read_dta("data/final/migration/migration2020_refine.dta")

# add the city_label in the final region code

# region <- read_rds("data/final/final_region_code.rds")
# 
# region <- region %>% left_join(migration1 %>% select(iso=iso_o, city_label) %>% distinct(iso, .keep_all=T), by="iso")
# 
# region %>% write_rds("data/final/final_region_code.rds")

#read in the migration data and default region data

com <- read_rds("data/final/cz_code/commuting_zone.rds")

region <- read_rds("data/final/final_region_code.rds")

# set up the migration format

migration <- expand.grid(region %>% select(iso_o=iso) %>% pull(), region %>% select(iso_d=iso) %>% pull(), seq(2001, 2020, by=1)) %>% 
  rename(iso_o=Var1, iso_d=Var2, year=Var3) %>% 
  filter(iso_o!=iso_d)

migration <- migration %>% left_join(com, by=c("iso_o"="iso")) %>% 
  rename(cz_label_o=cz_label, cz_id_o=cz_id, region_label_o=region_label) %>% 
  select(-cz_code)


migration <- migration %>% left_join(com, by=c("iso_d"="iso")) %>% 
  rename(cz_label_d=cz_label, cz_id_d=cz_id, region_label_d=region_label) %>% 
  select(-cz_code)



# refine for migration2

migration2 <- migration2 %>% select(year, iso_o, iso_d, migrate=이동_총인구, migrate_female=이동_여인구, migrate_male=이동_남인구)

migration2 <- migration2 %>% group_by(year, iso_d, iso_o) %>% 
  summarise(migrate=sum(migrate, na.rm=T), migrate_female=sum(migrate_female, na.rm=T), migrate_male=sum(migrate_male, na.rm=T)) %>% ungroup()





# refine for migration3 and 4



migration3 <- migration3 %>% select(year, iso_o, iso_d, migrate=이동_총인구수, migrate_female=이동_여자인구수, migrate_male=이동_남자인구수)

migration3 <- migration3 %>% group_by(year, iso_d, iso_o) %>% 
  summarise(migrate=sum(migrate, na.rm=T), migrate_female=sum(migrate_female, na.rm=T), migrate_male=sum(migrate_male, na.rm=T)) %>% ungroup()









migration4 <- migration4 %>% select(year, iso_o, iso_d, migrate=이동_총인구수, migrate_female=이동_여자인구수, migrate_male=이동_남자인구수)

migration4 <- migration4 %>% group_by(year, iso_d, iso_o) %>% 
  summarise(migrate=sum(migrate, na.rm=T), migrate_female=sum(migrate_female, na.rm=T), migrate_male=sum(migrate_male, na.rm=T)) %>% ungroup()



migration_all <- migration2 %>% 
  bind_rows(migration3) %>% 
  bind_rows(migration4)

migration <- migration %>% left_join(migration_all, by=c("year", "iso_d", "iso_o"))


# 227*227*20 - 227*20=1026040 obs in sigungu.





## if NA, impute zero

migration <- migration %>% 
  mutate(migrate=ifelse(is.na(migrate), 0, migrate), migrate_female=ifelse(is.na(migrate_female), 0, migrate_female), migrate_male=ifelse(is.na(migrate_male), 0, migrate_male))




migration2001_2019 <- migration %>% 
  filter(year<2020) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate=sum(migrate, na.rm=T), migrate_female=sum(migrate_female, na.rm=T), migrate_male=sum(migrate_male, na.rm=T)) %>% ungroup()




migration2001_2019 <- migration2001_2019 %>% left_join(com %>% select(iso, cz_id), by=c("iso_o"="iso")) %>% rename(cz_o=cz_id)

migration2001_2019 <- migration2001_2019 %>% left_join(com %>% select(iso, cz_id), by=c("iso_d"="iso")) %>% rename(cz_d=cz_id)


tb_all_o <- tb_all %>% select(ends_with("o")) %>% distinct(cz_o, .keep_all = T)
tb_all_d <- tb_all %>% select(ends_with("d")) %>% distinct(cz_d, .keep_all = T)


migration2001_2019 <- migration2001_2019 %>% left_join(tb_all_o, by="cz_o")
migration2001_2019 <- migration2001_2019 %>% left_join(tb_all_d, by="cz_d")


migration2001_2019 %>% write_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")





# now read in population to do weighting

pop <- readxl::read_excel("data/final/pop.xlsx") %>% 
  mutate(across(starts_with("pop"), as.numeric), iso=str_sub(iso, 4, 8))


# read in the concordance for region stat----
region_stat <- readxl::read_excel("concordance/region/region_stat.xlsx")

region_stat <- region_stat %>% mutate(across(everything(), as.character))

region_kosis <- readxl::read_excel("concordance/region/region_kosis.xlsx")

region_kosis <- region_kosis %>% mutate(across(everything(), as.character))

region_kosis <- region_kosis %>% mutate(city_stat=str_sub(city_stat, 1, 5))

pop <- pop %>% mutate(iso=ifelse(iso=="44825", "70000", iso))

pop <- pop %>% mutate(iso=ifelse(iso=="43745", "80000", iso))

pop <- pop %>% left_join(region_kosis, by=c("iso"="city_kosis"))

pop <- pop %>% mutate(iso=city_stat) %>% select(-city_stat)


pop <- pop %>% select(-city_label)

pop <- pop %>% filter(!is.na(iso))



pop <- pop %>% left_join(region_stat, by=c("iso"="city_stat_raw"))

pop <- pop %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso))

pop <- pop %>% select(-city_stat)

pop <- pop %>% group_by(iso) %>% 
  summarise(across(starts_with("pop"), ~ sum(.x, na.rm = TRUE)))


migration2001_2019 <- migration2001_2019 %>% left_join(pop %>% select(iso, pop2001), by=c("iso_o"="iso")) %>% rename(pop2001_o=pop2001)

migration2001_2019 <- migration2001_2019 %>% left_join(pop %>% select(iso, pop2001), by=c("iso_d"="iso")) %>% rename(pop2001_d=pop2001)

migration2001_2019 <- migration2001_2019 %>% filter(cz_o!=34, cz_d!=34)


migration2001_2019 %>% write_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")







