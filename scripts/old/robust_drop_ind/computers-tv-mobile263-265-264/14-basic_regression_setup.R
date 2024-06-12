if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(here, gt, data.table, sf, rmapshaper, tidyverse, fixest, texreg, rmapshaper)


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



# save disaggregate data

migration %>% write_rds("data/final/migration/migration_sigungu_2001_2020.rds")




migration <- read_rds("data/final/migration/migration_sigungu_2001_2020.rds")

# group by iso


migration2001_2010 <- migration %>% 
  filter(year<=2010) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate=sum(migrate, na.rm=T), migrate_female=sum(migrate_female, na.rm=T), migrate_male=sum(migrate_male, na.rm=T)) %>% ungroup()
  

migration2011_2020 <- migration %>% 
  filter(year>=2011) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate=sum(migrate, na.rm=T), migrate_female=sum(migrate_female, na.rm=T), migrate_male=sum(migrate_male, na.rm=T)) %>% ungroup()

migration2001_2019 <- migration %>% 
  filter(year<2020) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate=sum(migrate, na.rm=T), migrate_female=sum(migrate_female, na.rm=T), migrate_male=sum(migrate_male, na.rm=T)) %>% ungroup()






#read in import export // ivs

imp <- read_rds("data/final/shock/imp_cz.rds")
exp <- read_rds("data/final/shock/exp_cz.rds")

imp2010 <- read_rds("data/final/shock/imp2010_cz.rds")
imp2019 <- read_rds("data/final/shock/imp2019_cz.rds")
exp2010 <- read_rds("data/final/shock/exp2010_cz.rds")
exp2019 <- read_rds("data/final/shock/exp2019_cz.rds")


# imp1994_iv <-read_rds("data/final/shock/imp1994_iv_cz.rds")
# exp1994_iv <-read_rds("data/final/shock/exp1994_iv_cz.rds")
# 
# imp1996_iv <-read_rds("data/final/shock/imp1996_iv_cz.rds")
# exp1996_iv <-read_rds("data/final/shock/exp1996_iv_cz.rds")

imp1999_1_iv <-read_rds("data/final/shock/imp1999_1_iv_cz.rds")
exp1999_1_iv <-read_rds("data/final/shock/exp1999_1_iv_cz.rds")

imp1999_2_iv <-read_rds("data/final/shock/imp1999_2_iv_cz.rds")
exp1999_2_iv <-read_rds("data/final/shock/exp1999_2_iv_cz.rds")

imp1999_iv <-read_rds("data/final/shock/imp1999_iv_cz.rds")
exp1999_iv <-read_rds("data/final/shock/exp1999_iv_cz.rds")

# imp2000_iv <-read_rds("data/final/shock/imp2000_iv_cz.rds")
# exp2000_iv <-read_rds("data/final/shock/exp2000_iv_cz.rds")
# 
# imp_iv <-read_rds("data/final/shock/imp_iv_cz.rds")
# exp_iv <-read_rds("data/final/shock/exp_iv_cz.rds")



#dependent var is sigungu format: add the shock onto it

com <- read_rds("data/final/cz_code/commuting_zone.rds")


migration2001_2010 <- migration2001_2010 %>% left_join(com %>% select(iso, cz_id), by=c("iso_o"="iso")) %>% rename(cz_o=cz_id)

migration2001_2010 <- migration2001_2010 %>% left_join(com %>% select(iso, cz_id), by=c("iso_d"="iso")) %>% rename(cz_d=cz_id)


migration2011_2020 <- migration2011_2020 %>% left_join(com %>% select(iso, cz_id), by=c("iso_o"="iso")) %>% rename(cz_o=cz_id)

migration2011_2020 <- migration2011_2020 %>% left_join(com %>% select(iso, cz_id), by=c("iso_d"="iso")) %>% rename(cz_d=cz_id)


migration2001_2010 <- migration2001_2010 %>% left_join(imp2010, by=c("cz_o"="cz")) %>% 
  rename(imp_shock_o=imp_shock)
migration2001_2010 <- migration2001_2010 %>% left_join(imp2010, by=c("cz_d"="cz")) %>% 
  rename(imp_shock_d=imp_shock)

migration2001_2010 <- migration2001_2010 %>% left_join(exp2010, by=c("cz_o"="cz")) %>% 
  rename(exp_shock_o=exp_shock)
migration2001_2010 <- migration2001_2010 %>% left_join(exp2010, by=c("cz_d"="cz")) %>% 
  rename(exp_shock_d=exp_shock)


migration2001_2010 <- migration2001_2010 %>% left_join(imp1999_1_iv, by=c("cz_o"="cz")) %>% 
  rename(imp_shock1999_iv_o=imp_shock)
migration2001_2010 <- migration2001_2010 %>% left_join(imp1999_1_iv, by=c("cz_d"="cz")) %>% 
  rename(imp_shock1999_iv_d=imp_shock)

migration2001_2010 <- migration2001_2010 %>% left_join(exp1999_1_iv, by=c("cz_o"="cz")) %>% 
  rename(exp_shock1999_iv_o=exp_shock)
migration2001_2010 <- migration2001_2010 %>% left_join(exp1999_1_iv, by=c("cz_d"="cz")) %>% 
  rename(exp_shock1999_iv_d=exp_shock)



migration2011_2020 <- migration2011_2020 %>% left_join(imp2019, by=c("cz_o"="cz")) %>% 
  rename(imp_shock_o=imp_shock)
migration2011_2020 <- migration2011_2020 %>% left_join(imp2019, by=c("cz_d"="cz")) %>% 
  rename(imp_shock_d=imp_shock)

migration2011_2020 <- migration2011_2020 %>% left_join(exp2019, by=c("cz_o"="cz")) %>% 
  rename(exp_shock_o=exp_shock)
migration2011_2020 <- migration2011_2020 %>% left_join(exp2019, by=c("cz_d"="cz")) %>% 
  rename(exp_shock_d=exp_shock)


migration2011_2020 <- migration2011_2020 %>% left_join(imp1999_2_iv, by=c("cz_o"="cz")) %>% 
  rename(imp_shock1999_iv_o=imp_shock)
migration2011_2020 <- migration2011_2020 %>% left_join(imp1999_2_iv, by=c("cz_d"="cz")) %>% 
  rename(imp_shock1999_iv_d=imp_shock)

migration2011_2020 <- migration2011_2020 %>% left_join(exp1999_2_iv, by=c("cz_o"="cz")) %>% 
  rename(exp_shock1999_iv_o=exp_shock)
migration2011_2020 <- migration2011_2020 %>% left_join(exp1999_2_iv, by=c("cz_d"="cz")) %>% 
  rename(exp_shock1999_iv_d=exp_shock)




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







migration2001_2010 <- migration2001_2010 %>% left_join(pop %>% select(iso, pop2001), by=c("iso_o"="iso")) %>% rename(pop_o=pop2001)

migration2001_2010 <- migration2001_2010 %>% left_join(pop %>% select(iso, pop2001), by=c("iso_d"="iso")) %>% rename(pop_d=pop2001)

migration2001_2010 <- migration2001_2010 %>% filter(cz_o!=34, cz_d!=34)




migration2011_2020 <- migration2011_2020 %>% left_join(pop %>% select(iso, pop2010), by=c("iso_o"="iso")) %>% rename(pop_o=pop2010)

migration2011_2020 <- migration2011_2020 %>% left_join(pop %>% select(iso, pop2010), by=c("iso_d"="iso")) %>% rename(pop_d=pop2010)

migration2011_2020 <- migration2011_2020 %>% filter(cz_o!=34, cz_d!=34)



migration2001_2010 <- migration2001_2010 %>% mutate(year=1)
migration2011_2020 <- migration2011_2020 %>% mutate(year=2)

migration <- migration2001_2010 %>% bind_rows(migration2011_2020)


migration <- migration %>% left_join(pop %>% select(iso, pop2001), by=c("iso_o"="iso")) %>% rename(popw_o=pop2001)

migration <- migration %>% left_join(pop %>% select(iso, pop2001), by=c("iso_d"="iso")) %>% rename(popw_d=pop2001)



# use cz version to estimate distance

# read in shp and match region

region_stat <- readxl::read_excel("concordance/region/region_stat.xlsx")

region_stat <- region_stat %>% mutate(across(everything(), as.character))


shp <- st_read("data/final/region/bnd_sigungu_00_2021_2021_4Q.shp") %>% st_simplify(dTolerance = 1000)

shp <- shp %>% mutate(SIGUNGU_CD=ifelse(str_sub(SIGUNGU_CD, 5, 5)!="0", paste0(str_sub(SIGUNGU_CD, 1, 4), "0"), SIGUNGU_CD))  %>% 
  group_by(SIGUNGU_CD) %>% summarise(geometry = st_union(geometry), .groups = "keep") %>% 
  group_by(SIGUNGU_CD) %>% summarise(geometry = st_combine(geometry))


shp <- shp %>% rename(iso=SIGUNGU_CD)

shp <- shp %>% left_join(region_stat, by=c("iso"="city_stat_raw"))

shp <- shp %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)


shp <- shp %>% group_by(iso) %>% summarise(geometry = st_union(geometry), .groups="keep") %>% 
  summarise(geometry = st_combine(geometry))




region <- read_rds("data/final/final_region_code.rds")

region <- expand.grid(region %>% select(iso) %>% pull(), region %>% select(iso) %>% pull()) %>% rename(iso_o=Var1, iso_d=Var2)



region <- region %>% left_join(shp, by=c("iso_o"="iso")) %>% rename(geometry_o=geometry)
region <- region %>% left_join(shp, by=c("iso_d"="iso")) %>% rename(geometry_d=geometry)


region <- region %>% mutate(centroid_o=st_centroid(geometry_o),
                            centroid_d=st_centroid(geometry_d))


region <- region %>% mutate(distance=st_distance(centroid_o, centroid_d, by_element = T))




region <- region %>% select(iso_o, iso_d, distance)




migration <- migration %>% left_join(region, by=c("iso_o", "iso_d"))



migration %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")



































# basic format for cz_id

# basic format for cz_id

imp0 <- expand.grid(1:33, 1:33) %>% rename(cz_o=Var1, cz_d=Var2) %>% filter(cz_o!=cz_d)


imp0 <- imp0 %>% left_join(migration2001_2019, by=c("cz_o"="cz_id_o", "cz_d"="cz_id_d"))

imp0 <- imp0 %>% left_join(imp, by=c("cz_o"="cz")) %>% rename(imp_shock_o=imp_shock)
imp0 <- imp0 %>% left_join(imp, by=c("cz_d"="cz")) %>% rename(imp_shock_d=imp_shock)

imp0 <- imp0 %>% left_join(exp, by=c("cz_o"="cz")) %>% rename(exp_shock_o=exp_shock)
imp0 <- imp0 %>% left_join(exp, by=c("cz_d"="cz")) %>% rename(exp_shock_d=exp_shock)



imp0 <- imp0 %>% left_join(imp1994_iv, by=c("cz_o"="cz")) %>% rename(imp_shock1994_iv_o=imp_shock)
imp0 <- imp0 %>% left_join(imp1994_iv, by=c("cz_d"="cz")) %>% rename(imp_shock1994_iv_d=imp_shock)

imp0 <- imp0 %>% left_join(imp1996_iv, by=c("cz_o"="cz")) %>% rename(imp_shock1996_iv_o=imp_shock)
imp0 <- imp0 %>% left_join(imp1996_iv, by=c("cz_d"="cz")) %>% rename(imp_shock1996_iv_d=imp_shock)

imp0 <- imp0 %>% left_join(imp1999_iv, by=c("cz_o"="cz")) %>% rename(imp_shock1999_iv_o=imp_shock)
imp0 <- imp0 %>% left_join(imp1999_iv, by=c("cz_d"="cz")) %>% rename(imp_shock1999_iv_d=imp_shock)

imp0 <- imp0 %>% left_join(imp2000_iv, by=c("cz_o"="cz")) %>% rename(imp_shock2000_iv_o=imp_shock)
imp0 <- imp0 %>% left_join(imp2000_iv, by=c("cz_d"="cz")) %>% rename(imp_shock2000_iv_d=imp_shock)

imp0 <- imp0 %>% left_join(imp_iv, by=c("cz_o"="cz")) %>% rename(imp_shock_iv_o=imp_shock)
imp0 <- imp0 %>% left_join(imp_iv, by=c("cz_d"="cz")) %>% rename(imp_shock_iv_d=imp_shock)




imp0 <- imp0 %>% left_join(exp1994_iv, by=c("cz_o"="cz")) %>% rename(exp_shock1994_iv_o=exp_shock)
imp0 <- imp0 %>% left_join(exp1994_iv, by=c("cz_d"="cz")) %>% rename(exp_shock1994_iv_d=exp_shock)

imp0 <- imp0 %>% left_join(exp1996_iv, by=c("cz_o"="cz")) %>% rename(exp_shock1996_iv_o=exp_shock)
imp0 <- imp0 %>% left_join(exp1996_iv, by=c("cz_d"="cz")) %>% rename(exp_shock1996_iv_d=exp_shock)

imp0 <- imp0 %>% left_join(exp1999_iv, by=c("cz_o"="cz")) %>% rename(exp_shock1999_iv_o=exp_shock)
imp0 <- imp0 %>% left_join(exp1999_iv, by=c("cz_d"="cz")) %>% rename(exp_shock1999_iv_d=exp_shock)

imp0 <- imp0 %>% left_join(exp2000_iv, by=c("cz_o"="cz")) %>% rename(exp_shock2000_iv_o=exp_shock)
imp0 <- imp0 %>% left_join(exp2000_iv, by=c("cz_d"="cz")) %>% rename(exp_shock2000_iv_d=exp_shock)

imp0 <- imp0 %>% left_join(exp_iv, by=c("cz_o"="cz")) %>% rename(exp_shock_iv_o=exp_shock)
imp0 <- imp0 %>% left_join(exp_iv, by=c("cz_d"="cz")) %>% rename(exp_shock_iv_d=exp_shock)






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

pop <- pop %>% left_join(com, by="iso")


pop <- pop %>% group_by(cz_id) %>%
  summarise(across(starts_with("pop"), ~ sum(.x, na.rm = TRUE)))


pop1 <- pop %>% select(cz_id, contains("2001"))


imp0 <- imp0 %>% left_join(pop1, by=c("cz_o"="cz_id")) %>% rename(pop2001_o=pop2001, pop2001_female_o=pop2001_female, pop2001_male_o=pop2001_male)
imp0 <- imp0 %>% left_join(pop1, by=c("cz_d"="cz_id")) %>% rename(pop2001_d=pop2001, pop2001_female_d=pop2001_female, pop2001_male_d=pop2001_male)







# use cz version to estimate distance

# read in shp and match region

com <- read_rds("data/final/cz_code/commuting_zone.rds")

region_stat <- readxl::read_excel("concordance/region/region_stat.xlsx")

region_stat <- region_stat %>% mutate(across(everything(), as.character))


shp <- st_read("data/final/region/bnd_sigungu_00_2021_2021_4Q.shp") %>% st_simplify(dTolerance = 1000)

shp <- shp %>% mutate(SIGUNGU_CD=ifelse(str_sub(SIGUNGU_CD, 5, 5)!="0", paste0(str_sub(SIGUNGU_CD, 1, 4), "0"), SIGUNGU_CD))  %>%
  group_by(SIGUNGU_CD) %>% summarise(geometry = st_union(geometry), .groups = "keep") %>%
  group_by(SIGUNGU_CD) %>% summarise(geometry = st_combine(geometry))


shp <- shp %>% rename(iso=SIGUNGU_CD)

shp <- shp %>% left_join(region_stat, by=c("iso"="city_stat_raw"))

shp <- shp %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)


shp <- shp %>% group_by(iso) %>% summarise(geometry = st_union(geometry), .groups="keep") %>%
  summarise(geometry = st_combine(geometry))

shp <- shp %>% left_join(com, by="iso") %>% rename(cz=cz_id)


shp <- shp %>% group_by(cz) %>% summarise(geometry = st_union(geometry), .groups="keep") %>%
  summarise(geometry = st_combine(geometry))


shp <- shp %>% filter(cz!=34)

expand.grid(1:33, 1:33) %>% rename(cz_o=Var1, cz_d=Var2) %>%
  filter(cz_o!=cz_d) %>% haven::write_dta("data/final/cz_dist.dta")


region <- expand.grid(1:33, 1:33) %>% rename(cz_o=Var1, cz_d=Var2) %>%
  filter(cz_o!=cz_d)

region <- region %>% left_join(shp, by=c("cz_o"="cz")) %>% rename(geometry_o=geometry)
region <- region %>% left_join(shp, by=c("cz_d"="cz")) %>% rename(geometry_d=geometry)


region <- region %>% mutate(centroid_o=st_centroid(geometry_o),
                            centroid_d=st_centroid(geometry_d))


region <- region %>% mutate(distance=st_distance(centroid_o, centroid_d, by_element = T))

check <- region %>%
  rowwise() %>%
  do(adjacent = ifelse(st_intersects(.$geometry_o, .$geometry_d), 1, 0)) %>%
  ungroup()


region <- region %>% bind_cols(check)

region <- region %>% mutate(adjacent=ifelse(is.na(adjacent), 0, 1))


region <- region %>% select(cz_o, cz_d, distance, adjacent)


# shp <- shp %>% ms_simplify(keep=0.35)


imp0 <- imp0 %>% left_join(region, by=c("cz_o", "cz_d"))






#save it
imp0 %>% write_rds("data/final/shock/shock_main_data2001_2019.rds")














