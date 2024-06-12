if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(here, gt, data.table, sf, rmapshaper, tidyverse)


# this code makes the employment portion for the import / export penetration in each region. we will make it for sigungu level, cz level, sido level for various analysis.





#for jpn -- chn

# start of the period and lagged value: 1994, 1996, 1999, 2000, 2001

est1994 <- read_rds("data/final/hs_cpc_ksic/est1994_refine.rds")
est1996 <- read_rds("data/final/hs_cpc_ksic/est1996_refine.rds")
est1999 <- read_rds("data/final/hs_cpc_ksic/est1999_refine.rds")
est2000 <- read_rds("data/final/hs_cpc_ksic/est2000_refine.rds")
est2001 <- read_rds("data/final/hs_cpc_ksic/est2001_refine.rds")
est2010 <- read_rds("data/final/hs_cpc_ksic/est2010_refine.rds")


#read in the import and export


jpn_from_chn_import_2010_2001 <-  haven::read_dta("data/final/hs_cpc_ksic/jpn_import2010_2001.dta")
jpn_from_chn_import_2019_2010 <-  haven::read_dta("data/final/hs_cpc_ksic/jpn_import2019_2010.dta")
jpn_from_chn_import_2019_2001 <-  haven::read_dta("data/final/hs_cpc_ksic/jpn_import2019_2001.dta")
jpn_to_chn_export_2010_2001 <-   haven::read_dta("data/final/hs_cpc_ksic/jpn_export2010_2001.dta")
jpn_to_chn_export_2019_2010 <-   haven::read_dta("data/final/hs_cpc_ksic/jpn_export2019_2010.dta")
jpn_to_chn_export_2019_2001 <-   haven::read_dta("data/final/hs_cpc_ksic/jpn_export2019_2001.dta")


#for cz version

# read in cz data

com <- read_rds("data/final/cz_code/commuting_zone.rds")

est1994 <- est1994 %>% left_join(com, by="iso") %>% rename(cz=cz_id)
est1996 <- est1996 %>% left_join(com, by="iso") %>% rename(cz=cz_id)
est1999 <- est1999 %>% left_join(com, by="iso") %>% rename(cz=cz_id)
est2000 <- est2000 %>% left_join(com, by="iso") %>% rename(cz=cz_id)
est2001 <- est2001 %>% left_join(com, by="iso") %>% rename(cz=cz_id)
est2010 <- est2010 %>% left_join(com, by="iso") %>% rename(cz=cz_id)

# for 1994

est1994 <- est1994 %>% group_by(cz, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% 
  ungroup()


est1994 <- est1994 %>% group_by(cz) %>% 
  mutate(cz_emp_all=sum(emp_all, na.rm=T))

est1994 <- est1994 %>% group_by(ksic10) %>% 
  mutate(ind_emp_all=sum(emp_all, na.rm=T))

# for 1996

est1996 <- est1996 %>% group_by(cz, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% 
  ungroup()


est1996 <- est1996 %>% group_by(cz) %>% 
  mutate(cz_emp_all=sum(emp_all, na.rm=T))

est1996 <- est1996 %>% group_by(ksic10) %>% 
  mutate(ind_emp_all=sum(emp_all, na.rm=T))

# for 1999

est1999 <- est1999 %>% group_by(cz, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% 
  ungroup()


est1999 <- est1999 %>% group_by(cz) %>% 
  mutate(cz_emp_all=sum(emp_all, na.rm=T))

est1999 <- est1999 %>% group_by(ksic10) %>% 
  mutate(ind_emp_all=sum(emp_all, na.rm=T))

# for 2000

est2000 <- est2000 %>% group_by(cz, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% 
  ungroup()


est2000 <- est2000 %>% group_by(cz) %>% 
  mutate(cz_emp_all=sum(emp_all, na.rm=T))

est2000 <- est2000 %>% group_by(ksic10) %>% 
  mutate(ind_emp_all=sum(emp_all, na.rm=T))

# for 2001

est2001 <- est2001 %>% group_by(cz, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% 
  ungroup()


est2001 <- est2001 %>% group_by(cz) %>% 
  mutate(cz_emp_all=sum(emp_all, na.rm=T), cz_emp_female=sum(emp_female, na.rm=T), cz_emp_male=sum(emp_male, na.rm=T))

est2001 <- est2001 %>% group_by(ksic10) %>% 
  mutate(ind_emp_all=sum(emp_all, na.rm=T), ind_emp_female=sum(emp_female, na.rm=T), ind_emp_male=sum(emp_male, na.rm=T))



# for 2010

est2010 <- est2010 %>% group_by(cz, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% 
  ungroup()


est2010 <- est2010 %>% group_by(cz) %>% 
  mutate(cz_emp_all=sum(emp_all, na.rm=T), cz_emp_female=sum(emp_female, na.rm=T), cz_emp_male=sum(emp_male, na.rm=T))

est2010 <- est2010 %>% group_by(ksic10) %>% 
  mutate(ind_emp_all=sum(emp_all, na.rm=T), ind_emp_female=sum(emp_female, na.rm=T), ind_emp_male=sum(emp_male, na.rm=T))



#take out the case where 0/0 occurs for cz_emp_all, ind_emp_all

est1994 <- est1994 %>% filter(cz_emp_all>0, ind_emp_all>0)
est1996 <- est1996 %>% filter(cz_emp_all>0, ind_emp_all>0)
est1999 <- est1999 %>% filter(cz_emp_all>0, ind_emp_all>0)
est2000 <- est2000 %>% filter(cz_emp_all>0, ind_emp_all>0)
est2001 <- est2001 %>% filter(cz_emp_all>0, ind_emp_all>0)
est2010 <- est2010 %>% filter(cz_emp_all>0, ind_emp_all>0)


# imp1994 <- est1994 %>% left_join(jpn_from_chn_import_2019_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# exp1994 <- est1994 %>% left_join(jpn_to_chn_export_2019_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# 
# imp1996 <- est1996 %>% left_join(jpn_from_chn_import_2019_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# exp1996 <- est1996 %>% left_join(jpn_to_chn_export_2019_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))

imp1999_1 <- est1999 %>% left_join(jpn_from_chn_import_2010_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
exp1999_1 <- est1999 %>% left_join(jpn_to_chn_export_2010_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))

imp1999_2 <- est1999 %>% left_join(jpn_from_chn_import_2019_2010, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
exp1999_2 <- est1999 %>% left_join(jpn_to_chn_export_2019_2010, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))

imp1999 <- est1999 %>% left_join(jpn_from_chn_import_2019_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
exp1999 <- est1999 %>% left_join(jpn_to_chn_export_2019_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))



# imp2000 <- est2000 %>% left_join(jpn_from_chn_import_2019_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# exp2000 <- est2000 %>% left_join(jpn_to_chn_export_2019_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# 
# imp <- est2001 %>% left_join(jpn_from_chn_import_2019_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# exp <- est2001 %>% left_join(jpn_to_chn_export_2019_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))


# finally, calculate imp shock and exp shock: we will just do it for emp_all (0 issue for emp_female)
# 
# imp1994 <- imp1994 %>% mutate(imp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
# exp1994 <- exp1994 %>% mutate(exp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
# 
# imp1996 <- imp1996 %>% mutate(imp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
# exp1996 <- exp1996 %>% mutate(exp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))

imp1999_1 <- imp1999_1 %>% mutate(imp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
exp1999_1 <- exp1999_1 %>% mutate(exp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))

imp1999_2 <- imp1999_2 %>% mutate(imp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
exp1999_2 <- exp1999_2 %>% mutate(exp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))

imp1999 <- imp1999 %>% mutate(imp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
exp1999 <- exp1999 %>% mutate(exp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
# 
# imp2000 <- imp2000 %>% mutate(imp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
# exp2000 <- exp2000 %>% mutate(exp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))

# 
# imp <- imp %>% mutate(imp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
# exp <- exp %>% mutate(exp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))


# aggregate it into cz
# 
# imp1994 <- imp1994 %>% group_by(cz) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
# exp1994 <- exp1994 %>% group_by(cz) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
# 
# imp1996 <- imp1996 %>% group_by(cz) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
# exp1996 <- exp1996 %>% group_by(cz) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))


imp1999_1 <- imp1999_1 %>% group_by(cz) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
exp1999_1 <- exp1999_1 %>% group_by(cz) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
imp1999_2 <- imp1999_2 %>% group_by(cz) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
exp1999_2 <- exp1999_2 %>% group_by(cz) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
imp1999 <- imp1999 %>% group_by(cz) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
exp1999 <- exp1999 %>% group_by(cz) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
# 
# imp2000 <- imp2000 %>% group_by(cz) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
# exp2000 <- exp2000 %>% group_by(cz) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
# 
# 
# imp <- imp %>% group_by(cz) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
# exp <- exp %>% group_by(cz) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))


#save cz version
# imp1994 %>% write_rds("data/final/shock/imp1994_iv_cz.rds")
# exp1994 %>% write_rds("data/final/shock/exp1994_iv_cz.rds")
# 
# imp1996 %>% write_rds("data/final/shock/imp1996_iv_cz.rds")
# exp1996 %>% write_rds("data/final/shock/exp1996_iv_cz.rds")




imp1999_1 %>% write_rds("data/final/shock/imp1999_1_iv_cz.rds")
exp1999_1 %>% write_rds("data/final/shock/exp1999_1_iv_cz.rds")
imp1999_2 %>% write_rds("data/final/shock/imp1999_2_iv_cz.rds")
exp1999_2 %>% write_rds("data/final/shock/exp1999_2_iv_cz.rds")
imp1999 %>% write_rds("data/final/shock/imp1999_iv_cz.rds")
exp1999 %>% write_rds("data/final/shock/exp1999_iv_cz.rds")
# 
# imp2000 %>% write_rds("data/final/shock/imp2000_iv_cz.rds")
# exp2000 %>% write_rds("data/final/shock/exp2000_iv_cz.rds")
# 
# 
# imp %>% write_rds("data/final/shock/imp_iv_cz.rds")
# exp %>% write_rds("data/final/shock/exp_iv_cz.rds")











# use cz version to draw the imp / exp map

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

shp <- shp %>% left_join(com, by="iso") %>% rename(cz=cz_id)


shp <- shp %>% group_by(cz) %>% summarise(geometry = st_union(geometry), .groups="keep") %>% 
  summarise(geometry = st_combine(geometry))



shp <- shp %>% ms_simplify(keep=0.35)


#join the data onto the map

shp_imp2010 <- shp %>% left_join(imp2010, by="cz")
shp_imp2019 <- shp %>% left_join(imp2019, by="cz")
shp_exp2010 <- shp %>% left_join(exp2010, by="cz")
shp_exp2019 <- shp %>% left_join(exp2019, by="cz")


#map them

shp_imp2010 %>% st_as_sf() %>% mutate(imp_shock_q=imp_shock %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=imp_shock_q))+
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_imp2010_iv.png", width=10, height=7, dpi=400)



shp_imp2019 %>% st_as_sf() %>% mutate(imp_shock_q=imp_shock %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=imp_shock_q))+
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_imp2019_iv.png", width=10, height=7, dpi=400)


shp_exp2010 %>% st_as_sf() %>% mutate(exp_shock_q=exp_shock %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=exp_shock_q))+
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_exp2010_iv.png", width=10, height=7, dpi=400)


shp_exp2019 %>% st_as_sf() %>% mutate(exp_shock_q=exp_shock %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=exp_shock_q))+
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_exp2019_iv.png", width=10, height=7, dpi=400)






#for world -- chn

# we just need 2001 and 2010 as we are doing start of the period

est2001 <- read_rds("data/final/hs_cpc_ksic/est2001_refine.rds")
est2010 <- read_rds("data/final/hs_cpc_ksic/est2010_refine.rds")


#read in the import and export


chn_from_world_import_2010_2001 <-  haven::read_dta("data/final/hs_cpc_ksic/kor_import2010_2001.dta")
chn_from_world_import_2019_2010 <-  haven::read_dta("data/final/hs_cpc_ksic/kor_import2019_2010.dta")
chn_to_world_export_2010_2001 <-   haven::read_dta("data/final/hs_cpc_ksic/kor_export2010_2001.dta")
chn_to_world_export_2019_2010 <-   haven::read_dta("data/final/hs_cpc_ksic/kor_export2019_2010.dta")



#for iso version

# for 2001

est2001 <- est2001 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% 
  ungroup()


est2001 <- est2001 %>% group_by(iso) %>% 
  mutate(iso_emp_all=sum(emp_all, na.rm=T), iso_emp_female=sum(emp_female, na.rm=T), iso_emp_male=sum(emp_male, na.rm=T))

est2001 <- est2001 %>% group_by(ksic10) %>% 
  mutate(ind_emp_all=sum(emp_all, na.rm=T), ind_emp_female=sum(emp_female, na.rm=T), ind_emp_male=sum(emp_male, na.rm=T))



# for 2010

est2010 <- est2010 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% 
  ungroup()


est2010 <- est2010 %>% group_by(iso) %>% 
  mutate(iso_emp_all=sum(emp_all, na.rm=T), iso_emp_female=sum(emp_female, na.rm=T), iso_emp_male=sum(emp_male, na.rm=T))

est2010 <- est2010 %>% group_by(ksic10) %>% 
  mutate(ind_emp_all=sum(emp_all, na.rm=T), ind_emp_female=sum(emp_female, na.rm=T), ind_emp_male=sum(emp_male, na.rm=T))



#take out the case where 0/0 occurs for iso_emp_all, ind_emp_all

est2001 <- est2001 %>% filter(iso_emp_all>0, ind_emp_all>0)
est2010 <- est2010 %>% filter(iso_emp_all>0, ind_emp_all>0)




imp2010 <- est2001 %>% left_join(chn_from_world_import_2010_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
imp2019 <- est2010 %>% left_join(chn_from_world_import_2019_2010, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))



exp2010 <- est2001 %>% left_join(chn_to_world_export_2010_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
exp2019 <- est2010 %>% left_join(chn_to_world_export_2019_2010, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))




# finally, calculate imp shock and exp shock: we will just do it for emp_all (0 issue for emp_female)

imp2010 <- imp2010 %>% mutate(imp_shock=(value*emp_all)/(iso_emp_all*ind_emp_all))
imp2019 <- imp2019 %>% mutate(imp_shock=(value*emp_all)/(iso_emp_all*ind_emp_all))
exp2010 <- exp2010 %>% mutate(exp_shock=(value*emp_all)/(iso_emp_all*ind_emp_all))
exp2019 <- exp2019 %>% mutate(exp_shock=(value*emp_all)/(iso_emp_all*ind_emp_all))


# save the disaggregate version: for safety

# imp2010 %>% write_rds("data/final/shock/imp2010.rds")
# imp2019 %>% write_rds("data/final/shock/imp2019.rds")
# exp2010 %>% write_rds("data/final/shock/exp2010.rds")
# exp2019 %>% write_rds("data/final/shock/exp2019.rds")



# aggregate it into iso

imp2010 <- imp2010 %>% group_by(iso) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
imp2019 <- imp2019 %>% group_by(iso) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
exp2010 <- exp2010 %>% group_by(iso) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
exp2019 <- exp2019 %>% group_by(iso) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))


#save iso version

imp2010 %>% write_rds("data/final/shock/imp2010_iv_world_iso.rds")
imp2019 %>% write_rds("data/final/shock/imp2019_iv_world_iso.rds")
exp2010 %>% write_rds("data/final/shock/exp2010_iv_world_iso.rds")
exp2019 %>% write_rds("data/final/shock/exp2019_iv_world_iso.rds")






#for sido version

est2001 <- read_rds("data/final/hs_cpc_ksic/est2001_refine.rds")
est2010 <- read_rds("data/final/hs_cpc_ksic/est2010_refine.rds")

est2001 <- est2001 %>% mutate(sido=str_sub(iso, 1, 2))
est2010 <- est2001 %>% mutate(sido=str_sub(iso, 1, 2))

# for 2001

est2001 <- est2001 %>% group_by(sido, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% 
  ungroup()


est2001 <- est2001 %>% group_by(sido) %>% 
  mutate(sido_emp_all=sum(emp_all, na.rm=T), sido_emp_female=sum(emp_female, na.rm=T), sido_emp_male=sum(emp_male, na.rm=T))

est2001 <- est2001 %>% group_by(ksic10) %>% 
  mutate(ind_emp_all=sum(emp_all, na.rm=T), ind_emp_female=sum(emp_female, na.rm=T), ind_emp_male=sum(emp_male, na.rm=T))



# for 2010

est2010 <- est2010 %>% group_by(sido, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% 
  ungroup()


est2010 <- est2010 %>% group_by(sido) %>% 
  mutate(sido_emp_all=sum(emp_all, na.rm=T), sido_emp_female=sum(emp_female, na.rm=T), sido_emp_male=sum(emp_male, na.rm=T))

est2010 <- est2010 %>% group_by(ksic10) %>% 
  mutate(ind_emp_all=sum(emp_all, na.rm=T), ind_emp_female=sum(emp_female, na.rm=T), ind_emp_male=sum(emp_male, na.rm=T))



#take out the case where 0/0 occurs for sido_emp_all, ind_emp_all

est2001 <- est2001 %>% filter(sido_emp_all>0, ind_emp_all>0)
est2010 <- est2010 %>% filter(sido_emp_all>0, ind_emp_all>0)




imp2010 <- est2001 %>% left_join(chn_from_world_import_2010_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
imp2019 <- est2010 %>% left_join(chn_from_world_import_2019_2010, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))



exp2010 <- est2001 %>% left_join(chn_to_world_export_2010_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
exp2019 <- est2010 %>% left_join(chn_to_world_export_2019_2010, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))




# finally, calculate imp shock and exp shock: we will just do it for emp_all (0 issue for emp_female)

imp2010 <- imp2010 %>% mutate(imp_shock=(value*emp_all)/(sido_emp_all*ind_emp_all))
imp2019 <- imp2019 %>% mutate(imp_shock=(value*emp_all)/(sido_emp_all*ind_emp_all))
exp2010 <- exp2010 %>% mutate(exp_shock=(value*emp_all)/(sido_emp_all*ind_emp_all))
exp2019 <- exp2019 %>% mutate(exp_shock=(value*emp_all)/(sido_emp_all*ind_emp_all))


# aggregate it into sido

imp2010 <- imp2010 %>% group_by(sido) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
imp2019 <- imp2019 %>% group_by(sido) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
exp2010 <- exp2010 %>% group_by(sido) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
exp2019 <- exp2019 %>% group_by(sido) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))


#save sido version

imp2010 %>% write_rds("data/final/shock/imp2010_iv_world_sido.rds")
imp2019 %>% write_rds("data/final/shock/imp2019_iv_world_sido.rds")
exp2010 %>% write_rds("data/final/shock/exp2010_iv_world_sido.rds")
exp2019 %>% write_rds("data/final/shock/exp2019_iv_world_sido.rds")




#for cz version

est2001 <- read_rds("data/final/hs_cpc_ksic/est2001_refine.rds")
est2010 <- read_rds("data/final/hs_cpc_ksic/est2010_refine.rds")

# read in cz data

com <- read_rds("data/final/cz_code/commuting_zone.rds")

est2001 <- est2001 %>% left_join(com, by="iso") %>% rename(cz=cz_id)
est2010 <- est2010 %>% left_join(com, by="iso") %>% rename(cz=cz_id)

# for 2001

est2001 <- est2001 %>% group_by(cz, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% 
  ungroup()


est2001 <- est2001 %>% group_by(cz) %>% 
  mutate(cz_emp_all=sum(emp_all, na.rm=T), cz_emp_female=sum(emp_female, na.rm=T), cz_emp_male=sum(emp_male, na.rm=T))

est2001 <- est2001 %>% group_by(ksic10) %>% 
  mutate(ind_emp_all=sum(emp_all, na.rm=T), ind_emp_female=sum(emp_female, na.rm=T), ind_emp_male=sum(emp_male, na.rm=T))



# for 2010

est2010 <- est2010 %>% group_by(cz, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% 
  ungroup()


est2010 <- est2010 %>% group_by(cz) %>% 
  mutate(cz_emp_all=sum(emp_all, na.rm=T), cz_emp_female=sum(emp_female, na.rm=T), cz_emp_male=sum(emp_male, na.rm=T))

est2010 <- est2010 %>% group_by(ksic10) %>% 
  mutate(ind_emp_all=sum(emp_all, na.rm=T), ind_emp_female=sum(emp_female, na.rm=T), ind_emp_male=sum(emp_male, na.rm=T))



#take out the case where 0/0 occurs for cz_emp_all, ind_emp_all

est2001 <- est2001 %>% filter(cz_emp_all>0, ind_emp_all>0)
est2010 <- est2010 %>% filter(cz_emp_all>0, ind_emp_all>0)




imp2010 <- est2001 %>% left_join(chn_from_world_import_2010_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
imp2019 <- est2010 %>% left_join(chn_from_world_import_2019_2010, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))



exp2010 <- est2001 %>% left_join(chn_to_world_export_2010_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
exp2019 <- est2010 %>% left_join(chn_to_world_export_2019_2010, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))




# finally, calculate imp shock and exp shock: we will just do it for emp_all (0 issue for emp_female)

imp2010 <- imp2010 %>% mutate(imp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
imp2019 <- imp2019 %>% mutate(imp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
exp2010 <- exp2010 %>% mutate(exp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
exp2019 <- exp2019 %>% mutate(exp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))


# aggregate it into cz

imp2010 <- imp2010 %>% group_by(cz) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
imp2019 <- imp2019 %>% group_by(cz) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
exp2010 <- exp2010 %>% group_by(cz) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
exp2019 <- exp2019 %>% group_by(cz) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))


#save cz version

imp2010 %>% write_rds("data/final/shock/imp2010_iv_world_cz.rds")
imp2019 %>% write_rds("data/final/shock/imp2019_iv_world_cz.rds")
exp2010 %>% write_rds("data/final/shock/exp2010_iv_world_cz.rds")
exp2019 %>% write_rds("data/final/shock/exp2019_iv_world_cz.rds")



# use cz version to draw the imp / exp map

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

shp <- shp %>% left_join(com, by="iso") %>% rename(cz=cz_id)


shp <- shp %>% group_by(cz) %>% summarise(geometry = st_union(geometry), .groups="keep") %>% 
  summarise(geometry = st_combine(geometry))



shp <- shp %>% ms_simplify(keep=0.35)


#join the data onto the map

shp_imp2010 <- shp %>% left_join(imp2010, by="cz")
shp_imp2019 <- shp %>% left_join(imp2019, by="cz")
shp_exp2010 <- shp %>% left_join(exp2010, by="cz")
shp_exp2019 <- shp %>% left_join(exp2019, by="cz")


#map them

shp_imp2010 %>% st_as_sf() %>% mutate(imp_shock_q=imp_shock %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=imp_shock_q))+
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_imp2010_iv_w.png", width=10, height=7, dpi=400)



shp_imp2019 %>% st_as_sf() %>% mutate(imp_shock_q=imp_shock %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=imp_shock_q))+
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_imp2019_iv_w.png", width=10, height=7, dpi=400)


shp_exp2010 %>% st_as_sf() %>% mutate(exp_shock_q=exp_shock %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=exp_shock_q))+
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_exp2010_iv_w.png", width=10, height=7, dpi=400)


shp_exp2019 %>% st_as_sf() %>% mutate(exp_shock_q=exp_shock %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=exp_shock_q))+
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_exp2019_iv_w.png", width=10, height=7, dpi=400)

