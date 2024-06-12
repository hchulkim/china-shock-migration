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


jpn_from_chn_import_2010_2001 <-  haven::read_dta("data/final/hs_cpc_ksic/jpn_import2010_2001_drop1315.dta")
jpn_from_chn_import_2019_2010 <-  haven::read_dta("data/final/hs_cpc_ksic/jpn_import2019_2010_drop1315.dta")
jpn_from_chn_import_2019_2001 <-  haven::read_dta("data/final/hs_cpc_ksic/jpn_import2019_2001_drop1315.dta")
jpn_to_chn_export_2010_2001 <-   haven::read_dta("data/final/hs_cpc_ksic/jpn_export2010_2001_drop1315.dta")
jpn_to_chn_export_2019_2010 <-   haven::read_dta("data/final/hs_cpc_ksic/jpn_export2019_2010_drop1315.dta")
jpn_to_chn_export_2019_2001 <-   haven::read_dta("data/final/hs_cpc_ksic/jpn_export2019_2001_drop1315.dta")


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




imp1999_1 %>% write_rds("data/final/shock/imp1999_1_iv_cz_drop1315.rds")
exp1999_1 %>% write_rds("data/final/shock/exp1999_1_iv_cz_drop1315.rds")
imp1999_2 %>% write_rds("data/final/shock/imp1999_2_iv_cz_drop1315.rds")
exp1999_2 %>% write_rds("data/final/shock/exp1999_2_iv_cz_drop1315.rds")
imp1999 %>% write_rds("data/final/shock/imp1999_iv_cz_drop1315.rds")
exp1999 %>% write_rds("data/final/shock/exp1999_iv_cz_drop1315.rds")
# 
# imp2000 %>% write_rds("data/final/shock/imp2000_iv_cz.rds")
# exp2000 %>% write_rds("data/final/shock/exp2000_iv_cz.rds")
# 
# 
# imp %>% write_rds("data/final/shock/imp_iv_cz.rds")
# exp %>% write_rds("data/final/shock/exp_iv_cz.rds")








