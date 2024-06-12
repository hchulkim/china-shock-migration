if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(here, gt, data.table, sf, rmapshaper, tidyverse)


# this code makes the employment portion for the import / export penetration in each region. we will make it for sigungu level, cz level, sido level for various analysis.

# we just need 2001 and 2010 as we are doing start of the period

est2001 <- read_rds("data/final/hs_cpc_ksic/est2001_refine.rds")
est2010 <- read_rds("data/final/hs_cpc_ksic/est2010_refine.rds")


#read in the import and export


kor_from_chn_import_2010_2001 <-  haven::read_dta("data/final/hs_cpc_ksic/kor_import2010_2001_drop2325.dta")
kor_from_chn_import_2019_2010 <-  haven::read_dta("data/final/hs_cpc_ksic/kor_import2019_2010_drop2325.dta")
kor_from_chn_import_2019_2001 <-  haven::read_dta("data/final/hs_cpc_ksic/kor_import2019_2001_drop2325.dta")
kor_to_chn_export_2010_2001 <-   haven::read_dta("data/final/hs_cpc_ksic/kor_export2010_2001_drop2325.dta")
kor_to_chn_export_2019_2010 <-   haven::read_dta("data/final/hs_cpc_ksic/kor_export2019_2010_drop2325.dta")
kor_to_chn_export_2019_2001 <-   haven::read_dta("data/final/hs_cpc_ksic/kor_export2019_2001_drop2325.dta")




# #for iso version
# 
# # for 2001
# 
# est2001 <- est2001 %>% group_by(iso, ksic10) %>% 
#   summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% 
#   ungroup()
# 
# 
# est2001 <- est2001 %>% group_by(iso) %>% 
#   mutate(iso_emp_all=sum(emp_all, na.rm=T), iso_emp_female=sum(emp_female, na.rm=T), iso_emp_male=sum(emp_male, na.rm=T))
# 
# est2001 <- est2001 %>% group_by(ksic10) %>% 
#   mutate(ind_emp_all=sum(emp_all, na.rm=T), ind_emp_female=sum(emp_female, na.rm=T), ind_emp_male=sum(emp_male, na.rm=T))
# 
# 
# 
# # for 2010
# 
# est2010 <- est2010 %>% group_by(iso, ksic10) %>% 
#   summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% 
#   ungroup()
# 
# 
# est2010 <- est2010 %>% group_by(iso) %>% 
#   mutate(iso_emp_all=sum(emp_all, na.rm=T), iso_emp_female=sum(emp_female, na.rm=T), iso_emp_male=sum(emp_male, na.rm=T))
# 
# est2010 <- est2010 %>% group_by(ksic10) %>% 
#   mutate(ind_emp_all=sum(emp_all, na.rm=T), ind_emp_female=sum(emp_female, na.rm=T), ind_emp_male=sum(emp_male, na.rm=T))
# 
# 
# 
# #take out the case where 0/0 occurs for iso_emp_all, ind_emp_all
# 
# est2001 <- est2001 %>% filter(iso_emp_all>0, ind_emp_all>0)
# est2010 <- est2010 %>% filter(iso_emp_all>0, ind_emp_all>0)
# 
# 
# 
# 
# imp2010 <- est2001 %>% left_join(kor_from_chn_import_2010_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# imp2019 <- est2010 %>% left_join(kor_from_chn_import_2019_2010, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# 
# 
# 
# exp2010 <- est2001 %>% left_join(kor_to_chn_export_2010_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# exp2019 <- est2010 %>% left_join(kor_to_chn_export_2019_2010, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# 
# 
# 
# imp <- est2001 %>% left_join(kor_from_chn_import_2019_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# 
# 
# 
# exp <- est2001 %>% left_join(kor_to_chn_export_2019_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# 
# 
# 
# # finally, calculate imp shock and exp shock: we will just do it for emp_all (0 issue for emp_female)
# 
# imp2010 <- imp2010 %>% mutate(imp_shock=(value*emp_all)/(iso_emp_all*ind_emp_all))
# imp2019 <- imp2019 %>% mutate(imp_shock=(value*emp_all)/(iso_emp_all*ind_emp_all))
# exp2010 <- exp2010 %>% mutate(exp_shock=(value*emp_all)/(iso_emp_all*ind_emp_all))
# exp2019 <- exp2019 %>% mutate(exp_shock=(value*emp_all)/(iso_emp_all*ind_emp_all))
# 
# 
# imp <- imp %>% mutate(imp_shock=(value*emp_all)/(iso_emp_all*ind_emp_all))
# exp <- exp %>% mutate(exp_shock=(value*emp_all)/(iso_emp_all*ind_emp_all))
# 
# # save the disaggregate version: for safety
# 
# imp %>% write_rds("data/final/shock/imp.rds")
# exp %>% write_rds("data/final/shock/exp.rds")
# 
# imp2010 %>% write_rds("data/final/shock/imp2010.rds")
# imp2019 %>% write_rds("data/final/shock/imp2019.rds")
# exp2010 %>% write_rds("data/final/shock/exp2010.rds")
# exp2019 %>% write_rds("data/final/shock/exp2019.rds")
# 
# 
# 
# # aggregate it into iso
# 
# imp2010 <- imp2010 %>% group_by(iso) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
# imp2019 <- imp2019 %>% group_by(iso) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
# exp2010 <- exp2010 %>% group_by(iso) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
# exp2019 <- exp2019 %>% group_by(iso) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
# 
# imp <- imp %>% group_by(iso) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
# exp <- exp %>%group_by(iso) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
# 
# 
# #save iso version
# 
# imp2010 %>% write_rds("data/final/shock/imp2010_iso.rds")
# imp2019 %>% write_rds("data/final/shock/imp2019_iso.rds")
# exp2010 %>% write_rds("data/final/shock/exp2010_iso.rds")
# exp2019 %>% write_rds("data/final/shock/exp2019_iso.rds")
# 
# 
# imp %>%  write_rds("data/final/shock/imp_iso.rds")
# exp %>% write_rds("data/final/shock/exp_iso.rds")


# #for sido version
# 
# est2001 <- read_rds("data/final/hs_cpc_ksic/est2001_refine.rds")
# est2010 <- read_rds("data/final/hs_cpc_ksic/est2010_refine.rds")
# 
# est2001 <- est2001 %>% mutate(sido=str_sub(iso, 1, 2))
# est2010 <- est2001 %>% mutate(sido=str_sub(iso, 1, 2))
# 
# # for 2001
# 
# est2001 <- est2001 %>% group_by(sido, ksic10) %>% 
#   summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% 
#   ungroup()
# 
# 
# est2001 <- est2001 %>% group_by(sido) %>% 
#   mutate(sido_emp_all=sum(emp_all, na.rm=T), sido_emp_female=sum(emp_female, na.rm=T), sido_emp_male=sum(emp_male, na.rm=T))
# 
# est2001 <- est2001 %>% group_by(ksic10) %>% 
#   mutate(ind_emp_all=sum(emp_all, na.rm=T), ind_emp_female=sum(emp_female, na.rm=T), ind_emp_male=sum(emp_male, na.rm=T))
# 
# 
# 
# # for 2010
# 
# est2010 <- est2010 %>% group_by(sido, ksic10) %>% 
#   summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% 
#   ungroup()
# 
# 
# est2010 <- est2010 %>% group_by(sido) %>% 
#   mutate(sido_emp_all=sum(emp_all, na.rm=T), sido_emp_female=sum(emp_female, na.rm=T), sido_emp_male=sum(emp_male, na.rm=T))
# 
# est2010 <- est2010 %>% group_by(ksic10) %>% 
#   mutate(ind_emp_all=sum(emp_all, na.rm=T), ind_emp_female=sum(emp_female, na.rm=T), ind_emp_male=sum(emp_male, na.rm=T))
# 
# 
# 
# #take out the case where 0/0 occurs for sido_emp_all, ind_emp_all
# 
# est2001 <- est2001 %>% filter(sido_emp_all>0, ind_emp_all>0)
# est2010 <- est2010 %>% filter(sido_emp_all>0, ind_emp_all>0)
# 
# 
# 
# 
# imp2010 <- est2001 %>% left_join(kor_from_chn_import_2010_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# imp2019 <- est2010 %>% left_join(kor_from_chn_import_2019_2010, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# 
# 
# 
# exp2010 <- est2001 %>% left_join(kor_to_chn_export_2010_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# exp2019 <- est2010 %>% left_join(kor_to_chn_export_2019_2010, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# 
# 
# imp <- est2001 %>% left_join(kor_from_chn_import_2019_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# 
# exp <- est2001 %>% left_join(kor_to_chn_export_2019_2001, by="ksic10") %>% 
#   mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
# 
# 
# 
# 
# # finally, calculate imp shock and exp shock: we will just do it for emp_all (0 issue for emp_female)
# 
# imp2010 <- imp2010 %>% mutate(imp_shock=(value*emp_all)/(sido_emp_all*ind_emp_all))
# imp2019 <- imp2019 %>% mutate(imp_shock=(value*emp_all)/(sido_emp_all*ind_emp_all))
# exp2010 <- exp2010 %>% mutate(exp_shock=(value*emp_all)/(sido_emp_all*ind_emp_all))
# exp2019 <- exp2019 %>% mutate(exp_shock=(value*emp_all)/(sido_emp_all*ind_emp_all))
# 
# imp <- imp %>% mutate(imp_shock=(value*emp_all)/(sido_emp_all*ind_emp_all))
# exp <- exp %>% mutate(exp_shock=(value*emp_all)/(sido_emp_all*ind_emp_all))
# 
# # aggregate it into sido
# 
# imp2010 <- imp2010 %>% group_by(sido) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
# imp2019 <- imp2019 %>% group_by(sido) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
# exp2010 <- exp2010 %>% group_by(sido) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
# exp2019 <- exp2019 %>% group_by(sido) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
# 
# imp <- imp %>% group_by(sido) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
# exp <- exp %>% group_by(sido) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
# 
# #save sido version
# 
# imp2010 %>% write_rds("data/final/shock/imp2010_sido.rds")
# imp2019 %>% write_rds("data/final/shock/imp2019_sido.rds")
# exp2010 %>% write_rds("data/final/shock/exp2010_sido.rds")
# exp2019 %>% write_rds("data/final/shock/exp2019_sido.rds")
# 
# imp %>% write_rds("data/final/shock/imp_sido.rds")
# exp %>% write_rds("data/final/shock/exp_sido.rds")






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




imp2010 <- est2001 %>% left_join(kor_from_chn_import_2010_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
imp2019 <- est2010 %>% left_join(kor_from_chn_import_2019_2010, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))



exp2010 <- est2001 %>% left_join(kor_to_chn_export_2010_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))
exp2019 <- est2010 %>% left_join(kor_to_chn_export_2019_2010, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))


imp <- est2001 %>% left_join(kor_from_chn_import_2019_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))

exp <- est2001 %>% left_join(kor_to_chn_export_2019_2001, by="ksic10") %>% 
  mutate(across(starts_with("value"), ~ ifelse(is.na(.x), 0, .x)))




# finally, calculate imp shock and exp shock: we will just do it for emp_all (0 issue for emp_female)

imp2010 <- imp2010 %>% mutate(imp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
imp2019 <- imp2019 %>% mutate(imp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
exp2010 <- exp2010 %>% mutate(exp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
exp2019 <- exp2019 %>% mutate(exp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))

imp <- imp %>% mutate(imp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))
exp <- exp %>% mutate(exp_shock=(value*emp_all)/(cz_emp_all*ind_emp_all))


# aggregate it into cz

imp2010 <- imp2010 %>% group_by(cz) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
imp2019 <- imp2019 %>% group_by(cz) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
exp2010 <- exp2010 %>% group_by(cz) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))
exp2019 <- exp2019 %>% group_by(cz) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))

imp <- imp %>% group_by(cz) %>% summarise(imp_shock=sum(imp_shock, na.rm=T))
exp <- exp %>% group_by(cz) %>% summarise(exp_shock=sum(exp_shock, na.rm=T))


#save cz version

imp2010 %>% write_rds("data/final/shock/imp2010_cz_drop2325.rds")
imp2019 %>% write_rds("data/final/shock/imp2019_cz_drop2325.rds")
exp2010 %>% write_rds("data/final/shock/exp2010_cz_drop2325.rds")
exp2019 %>% write_rds("data/final/shock/exp2019_cz_drop2325.rds")

imp %>% write_rds("data/final/shock/imp_cz_drop2325.rds")
exp %>% write_rds("data/final/shock/exp_cz_drop2325.rds")















