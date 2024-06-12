if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(sf, fixest, texreg, tidyverse)


# read in main data and take out migrate

# tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds") %>% select(-c("migrate", "migrate_female", "migrate_male"))
# tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds") %>% select(-c("migrate", "migrate_female", "migrate_male"))
# 
# tb_all %>% write_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")
# 
# tb_two  %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

# read in the migration data

migration2001_2014 <-  read_rds("data/final/migration/migration2001_2014_hetero_refine.rds")
migration2015_2019 <-  read_rds("data/final/migration/migration2015_2019_hetero_refine.rds")
migration2020 <-  read_rds("data/final/migration/migration2020_hetero_refine.rds")


migration2001_2014 <- migration2001_2014 %>% select(year, iso_o, iso_d, city_label, reason, age_h, gen_h, household, migrate=`이동_총인구`) %>% mutate(across(5:8, as.numeric))

migration2015_2019 <- migration2015_2019 %>% select(year, iso_o, iso_d, city_label, reason, age_h, gen_h, household, migrate=`이동_총인구수`) %>% mutate(across(5:8, as.numeric))

migration2020 <- migration2020 %>% select(year, iso_o, iso_d, city_label, reason, age_h, gen_h, household, migrate=`이동_총인구수`) %>% mutate(across(5:8, as.numeric))


migration <- migration2001_2014 %>% bind_rows(migration2015_2019) %>%
  bind_rows(migration2020)


rm(migration2001_2014)
rm(migration2015_2019)
rm(migration2020)

migration %>% write_rds("data/final/migration/migration_all_hetero_refine.rds", "xz", compression=9L)

migration <- read_rds("data/final/migration/migration_all_hetero_refine.rds")





# now read in population to do weighting
# 
# pop <- readxl::read_excel("data/final/pop_age.xlsx") %>%
#   mutate(across(starts_with("age"), as.numeric), iso=str_sub(iso, 1, 5))
# 
# 
# # read in the concordance for region stat----
# region_stat <- readxl::read_excel("concordance/region/region_stat.xlsx")
# 
# region_stat <- region_stat %>% mutate(across(everything(), as.character))
# 
# region_kosis <- readxl::read_excel("concordance/region/region_kosis.xlsx")
# 
# region_kosis <- region_kosis %>% mutate(across(everything(), as.character))
# 
# region_kosis <- region_kosis %>% mutate(city_stat=str_sub(city_stat, 1, 5))
# 
# pop <- pop %>% mutate(iso=ifelse(iso=="44825", "70000", iso))
# 
# pop <- pop %>% mutate(iso=ifelse(iso=="43745", "80000", iso))
# 
# pop <- pop %>% left_join(region_kosis, by=c("iso"="city_kosis"))
# 
# pop <- pop %>% mutate(iso=city_stat) %>% select(-city_stat)
# 
# 
# pop <- pop %>% select(-city_label)
# 
# pop <- pop %>% filter(!is.na(iso))
# 
# 
# 
# pop <- pop %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
# 
# pop <- pop %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso))
# 
# pop <- pop %>% select(-city_stat)
# 
# pop <- pop %>% group_by(iso) %>%
#   summarise(across(starts_with("age"), ~ sum(.x, na.rm = TRUE)))
# pop %>% write_rds("data/final/pop_age.xlsx")


pop <- read_rds("data/final/pop_age.xlsx")




# pop <- readxl::read_excel("data/final/pop_age2.xlsx") %>%
#   mutate(across(starts_with("age"), as.numeric), iso=str_sub(iso, 1, 5))
# 
#
# read in the concordance for region stat----
# region_stat <- readxl::read_excel("concordance/region/region_stat.xlsx")
# 
# region_stat <- region_stat %>% mutate(across(everything(), as.character))
# 
# region_kosis <- readxl::read_excel("concordance/region/region_kosis.xlsx")
# 
# region_kosis <- region_kosis %>% mutate(across(everything(), as.character))
# 
# region_kosis <- region_kosis %>% mutate(city_stat=str_sub(city_stat, 1, 5))
# 
# pop <- pop %>% mutate(iso=ifelse(iso=="44825", "70000", iso))
# 
# pop <- pop %>% mutate(iso=ifelse(iso=="43745", "80000", iso))
# 
# pop <- pop %>% left_join(region_kosis, by=c("iso"="city_kosis"))
# 
# pop <- pop %>% mutate(iso=city_stat) %>% select(-city_stat)
# 
# 
# pop <- pop %>% select(-city_label)
# 
# pop <- pop %>% filter(!is.na(iso))
# 
# 
# 
# pop <- pop %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
# 
# pop <- pop %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso))
# 
# pop <- pop %>% select(-city_stat)
# 
# pop <- pop %>% group_by(iso) %>%
#   summarise(across(starts_with("age"), ~ sum(.x, na.rm = TRUE)))
# 
# pop %>% write_rds("data/final/pop_age2.xlsx")


pop2 <- read_rds("data/final/pop_age2.xlsx")



# for now we will add up 2030, 4050, 60+

pop <- pop %>% mutate(pop2030=age20_24+age25_29+age30_34+age35_39, pop4050=age40_44+age45_49+age50_54+age55_59, pop60=age60_64+age65_69+age70_74+age75_79+age80_84+age85_89+age90_94+age95_99+age100) %>% 
  select(iso, pop2030, pop4050, pop60)

pop2 <- pop2 %>% mutate(pop2030=age20_24+age25_29+age30_34+age35_39, pop4050=age40_44+age45_49+age50_54+age55_59, pop60=age60_64+age65_69+age70_74+age75_79+age80_84+age85_89+age90_94+age95_99+age100) %>% 
  select(iso, pop2030, pop4050, pop60)












# baseline column formats : for both one period and two period

# hetero by age : 2030, 4050, 60+

# japan IV, with control, cluster-sigungu

# 1st column: Together-both 1 year

# 2nd column: separate-Both 1 year

# 1st column: Together-both 2 year

# 2nd column: separate-Both 2 year

# 3rd column: Sep-both cz origin and dest fef 2 year


base_reg_age <- function(age) {
  
  tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")
  tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")
  
  if (age != 60) {
    migration_data <- migration %>%
      filter(year != 2020, age_h >= age & age_h < age + 20) %>%
      group_by(iso_o, iso_d) %>%
      summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
      ungroup()
    
    
    migration_data2 <- migration %>% 
      filter(age_h >= age & age_h < age + 20) %>% 
      mutate(year=ifelse(year<2011, 1, 2)) %>% 
      group_by(year, iso_o, iso_d) %>% 
      summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
      ungroup()
    
    
  } else {
    migration_data <- migration %>%
      filter(year != 2020, age_h >= 60) %>%
      group_by(iso_o, iso_d) %>%
      summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
      ungroup()
    
    migration_data2 <- migration %>% 
      filter(age_h >= 60) %>% 
      mutate(year=ifelse(year<2011, 1, 2)) %>% 
      group_by(year, iso_o, iso_d) %>% 
      summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
      ungroup()
  }
  
  tb_all <- tb_all %>%
    left_join(migration_data, by = c("iso_o", "iso_d"))
  
  tb_two <- tb_two %>% 
    left_join(migration_data2, by = c("year", "iso_o", "iso_d"))
  
  tb_all <- tb_all %>%
    filter(cz_o != cz_d)
  
  tb_two <- tb_two %>%
    filter(cz_o != cz_d)
  
  out_tog1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                      shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                    weights = tb_all$pop2001_o, cluster = tb_all$iso_o, data = tb_all %>% 
                      mutate(shock_o = (exp_shock_o - imp_shock_o) / 1000, shock_o_iv = (exp_shock1999_iv_o - imp_shock1999_iv_o) / 1000,
                             shock_d = (exp_shock_d - imp_shock_d) / 1000, shock_d_iv = (exp_shock1999_iv_d - imp_shock1999_iv_d) / 1000))
  
tb_all <- tb_all %>%
    mutate(across(starts_with("imp"), ~ .x / 1000)) %>%
    mutate(across(starts_with("exp"), ~ .x / 1000))
  
  out_sep1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                      imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                    weights = tb_all$pop2001_o, cluster = tb_all$iso_o, data = tb_all)
  
  rm(migration_data)
  gc()  # Explicitly request garbage collection to free memory
  
  
  out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                      shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                    weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                      mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                             shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

  tb_two <- tb_two %>%
    mutate(across(starts_with("imp"), ~ .x / 1000)) %>%
    mutate(across(starts_with("exp"), ~ .x / 1000))
  
  
  out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                      imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                    weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)
  
  out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                          imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                        weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)
  
  
  rm(migration_data2)
  gc()  # Explicitly request garbage collection to free memory
  
  
  
  texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
         include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file = paste0("results/tables/baseline_regression_hetero", as.character(age), ".tex"))
}


base_reg_age(20)
base_reg_age(40)
base_reg_age(60)







base_reg_reason <- function(x) {
  tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")
  tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")
  
    migration_data <- migration %>%
      filter(year != 2020, reason==x) %>%
      group_by(iso_o, iso_d) %>%
      summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
      ungroup()
    
    
    migration_data2 <- migration %>% 
      filter(reason==x) %>% 
      mutate(year=ifelse(year<2011, 1, 2)) %>% 
      group_by(year, iso_o, iso_d) %>% 
      summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
      ungroup()
    
    
  
  
  tb_all <- tb_all %>%
    left_join(migration_data, by = c("iso_o", "iso_d"))
  
  tb_two <- tb_two %>% 
    left_join(migration_data2, by = c("year", "iso_o", "iso_d"))
  
  tb_all <- tb_all %>%
    filter(cz_o != cz_d)
  
  tb_two <- tb_two %>%
    filter(cz_o != cz_d)
  
  out_tog1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                      shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                    weights = tb_all$pop2001_o, cluster = tb_all$iso_o, data = tb_all %>% 
                      mutate(shock_o = (exp_shock_o - imp_shock_o) / 1000, shock_o_iv = (exp_shock1999_iv_o - imp_shock1999_iv_o) / 1000,
                             shock_d = (exp_shock_d - imp_shock_d) / 1000, shock_d_iv = (exp_shock1999_iv_d - imp_shock1999_iv_d) / 1000))
  
  tb_all <- tb_all %>%
    mutate(across(starts_with("imp"), ~ .x / 1000)) %>%
    mutate(across(starts_with("exp"), ~ .x / 1000))
  
  out_sep1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                      imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                    weights = tb_all$pop2001_o, cluster = tb_all$iso_o, data = tb_all)
  
  rm(migration_data)
  gc()  # Explicitly request garbage collection to free memory
  
  
  out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                      shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                    weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                      mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                             shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))
  
  tb_two <- tb_two %>%
    mutate(across(starts_with("imp"), ~ .x / 1000)) %>%
    mutate(across(starts_with("exp"), ~ .x / 1000))
  
  
  out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                      imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                    weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)
  
  out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                          imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                        weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)
  
  
  rm(migration_data2)
  gc()  # Explicitly request garbage collection to free memory
  
  
  
  texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
         include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file = paste0("results/tables/baseline_regression_hetero", as.character(x), ".tex"))
}


base_reg_reason(1)
base_reg_reason(2)
base_reg_reason(3)
base_reg_reason(4)
base_reg_reason(5)
base_reg_reason(6)



# use log mig + 1



# tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds") %>% select(-c("migrate", "migrate_female", "migrate_male"))
# tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds") %>% select(-c("migrate", "migrate_female", "migrate_male"))
# 
# tb_all %>% write_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")
# 
# tb_two  %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

# read in the migration data

migration2001_2014 <-  read_rds("data/final/migration/migration2001_2014_hetero_refine.rds")
migration2015_2019 <-  read_rds("data/final/migration/migration2015_2019_hetero_refine.rds")
migration2020 <-  read_rds("data/final/migration/migration2020_hetero_refine.rds")


migration2001_2014 <- migration2001_2014 %>% select(year, iso_o, iso_d, city_label, reason, age_h, gen_h, household, migrate=`이동_총인구`) %>% mutate(across(5:8, as.numeric))

migration2015_2019 <- migration2015_2019 %>% select(year, iso_o, iso_d, city_label, reason, age_h, gen_h, household, migrate=`이동_총인구수`) %>% mutate(across(5:8, as.numeric))

migration2020 <- migration2020 %>% select(year, iso_o, iso_d, city_label, reason, age_h, gen_h, household, migrate=`이동_총인구수`) %>% mutate(across(5:8, as.numeric))


migration <- migration2001_2014 %>% bind_rows(migration2015_2019) %>%
  bind_rows(migration2020)


rm(migration2001_2014)
rm(migration2015_2019)
rm(migration2020)



# baseline column formats : for both one period and two period

# hetero by age : 2030, 4050, 60+

# japan IV, with control, cluster-sigungu

# 1st column: Together-both 1 year

# 2nd column: separate-Both 1 year

# 1st column: Together-both 2 year

# 2nd column: separate-Both 2 year

# 3rd column: Sep-both cz origin and dest fef 2 year


base_reg_age <- function(age) {
  
  tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")
  tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")
  
  if (age != 60) {
    migration_data <- migration %>%
      filter(year != 2020, age_h >= age & age_h < age + 20) %>%
      group_by(iso_o, iso_d) %>%
      summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
      ungroup()
    
    
    migration_data2 <- migration %>% 
      filter(age_h >= age & age_h < age + 20) %>% 
      mutate(year=ifelse(year<2011, 1, 2)) %>% 
      group_by(year, iso_o, iso_d) %>% 
      summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
      ungroup()
    
    
  } else {
    migration_data <- migration %>%
      filter(year != 2020, age_h >= 60) %>%
      group_by(iso_o, iso_d) %>%
      summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
      ungroup()
    
    migration_data2 <- migration %>% 
      filter(age_h >= 60) %>% 
      mutate(year=ifelse(year<2011, 1, 2)) %>% 
      group_by(year, iso_o, iso_d) %>% 
      summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
      ungroup()
  }
  
  tb_all <- tb_all %>%
    left_join(migration_data, by = c("iso_o", "iso_d"))
  
  tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))
  
  tb_all <- tb_all %>% mutate(migrate=migrate+1)
  
  tb_two <- tb_two %>% 
    left_join(migration_data2, by = c("year", "iso_o", "iso_d"))
  
  tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))
  
  tb_two <- tb_two %>% mutate(migrate=migrate+1)
  
  tb_all <- tb_all %>%
    filter(cz_o != cz_d)
  
  tb_two <- tb_two %>%
    filter(cz_o != cz_d)
  
  out_tog1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                      shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                    weights = tb_all$pop2001_o, cluster = tb_all$iso_o, data = tb_all %>% 
                      mutate(shock_o = (exp_shock_o - imp_shock_o) / 1000, shock_o_iv = (exp_shock1999_iv_o - imp_shock1999_iv_o) / 1000,
                             shock_d = (exp_shock_d - imp_shock_d) / 1000, shock_d_iv = (exp_shock1999_iv_d - imp_shock1999_iv_d) / 1000))
  
  tb_all <- tb_all %>%
    mutate(across(starts_with("imp"), ~ .x / 1000)) %>%
    mutate(across(starts_with("exp"), ~ .x / 1000))
  
  out_sep1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                      imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                    weights = tb_all$pop2001_o, cluster = tb_all$iso_o, data = tb_all)
  
  rm(migration_data)
  gc()  # Explicitly request garbage collection to free memory
  
  
  out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                      shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                    weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                      mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                             shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))
  
  tb_two <- tb_two %>%
    mutate(across(starts_with("imp"), ~ .x / 1000)) %>%
    mutate(across(starts_with("exp"), ~ .x / 1000))
  
  
  out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                      imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                    weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)
  
  out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                          imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                        weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)
  
  
  rm(migration_data2)
  gc()  # Explicitly request garbage collection to free memory
  
  
  
  texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
         include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file = paste0("results/tables/baseline_regression_hetero", as.character(age), "logadd.tex"))
}


base_reg_age(20)
base_reg_age(40)
base_reg_age(60)







base_reg_reason <- function(x) {
  tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")
  tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")
  
  migration_data <- migration %>%
    filter(year != 2020, reason==x) %>%
    group_by(iso_o, iso_d) %>%
    summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
    ungroup()
  
  
  migration_data2 <- migration %>% 
    filter(reason==x) %>% 
    mutate(year=ifelse(year<2011, 1, 2)) %>% 
    group_by(year, iso_o, iso_d) %>% 
    summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
    ungroup()
  
  
  
  
  tb_all <- tb_all %>%
    left_join(migration_data, by = c("iso_o", "iso_d"))
  
  tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))
  
  tb_all <- tb_all %>% mutate(migrate=migrate+1)
  
  tb_two <- tb_two %>% 
    left_join(migration_data2, by = c("year", "iso_o", "iso_d"))
  
  tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))
  
  tb_two <- tb_two %>% mutate(migrate=migrate+1)
  
  tb_all <- tb_all %>%
    filter(cz_o != cz_d)
  
  tb_two <- tb_two %>%
    filter(cz_o != cz_d)
  
  out_tog1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                      shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                    weights = tb_all$pop2001_o, cluster = tb_all$iso_o, data = tb_all %>% 
                      mutate(shock_o = (exp_shock_o - imp_shock_o) / 1000, shock_o_iv = (exp_shock1999_iv_o - imp_shock1999_iv_o) / 1000,
                             shock_d = (exp_shock_d - imp_shock_d) / 1000, shock_d_iv = (exp_shock1999_iv_d - imp_shock1999_iv_d) / 1000))
  
  tb_all <- tb_all %>%
    mutate(across(starts_with("imp"), ~ .x / 1000)) %>%
    mutate(across(starts_with("exp"), ~ .x / 1000))
  
  out_sep1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                      imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                    weights = tb_all$pop2001_o, cluster = tb_all$iso_o, data = tb_all)
  
  rm(migration_data)
  gc()  # Explicitly request garbage collection to free memory
  
  
  out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                      shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                    weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                      mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                             shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))
  
  tb_two <- tb_two %>%
    mutate(across(starts_with("imp"), ~ .x / 1000)) %>%
    mutate(across(starts_with("exp"), ~ .x / 1000))
  
  
  out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                      imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                    weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)
  
  out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                          imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                        weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)
  
  
  rm(migration_data2)
  gc()  # Explicitly request garbage collection to free memory
  
  
  
  texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
         include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file = paste0("results/tables/baseline_regression_hetero", as.character(x), "logadd.tex"))
}


base_reg_reason(1)
base_reg_reason(2)
base_reg_reason(3)
base_reg_reason(4)
base_reg_reason(5)
base_reg_reason(6)





# stack them


######


tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")

# one period


tb_all2030 <- tb_all %>% mutate(ind2030=1)
tb_all4050 <- tb_all %>% mutate(ind4050=1)
tb_all60 <- tb_all %>% mutate(ind60=1)



migration2030 <- migration %>% 
  filter(year<2020, age_h>=20, age_h<40) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration4050 <- migration %>% 
  filter(year<2020, age_h>=40, age_h<60) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration60 <- migration %>% 
  filter(year<2020, age_h>=60) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()


tb_all2030 <- tb_all2030 %>% left_join(migration2030, by=c("iso_o", "iso_d"))
tb_all4050 <- tb_all4050 %>% left_join(migration4050, by=c("iso_o", "iso_d"))
tb_all60 <- tb_all60 %>% left_join(migration60, by=c("iso_o", "iso_d"))


tb_all2030 <- tb_all2030 %>% left_join(pop %>% select(iso, pop2030), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop2030)
tb_all2030 <- tb_all2030 %>% left_join(pop %>% select(iso, pop2030), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop2030)

tb_all4050 <- tb_all4050 %>% left_join(pop %>% select(iso, pop4050), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop4050)
tb_all4050 <- tb_all4050 %>% left_join(pop %>% select(iso, pop4050), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop4050)

tb_all60 <- tb_all60 %>% left_join(pop %>% select(iso, pop60), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop60)
tb_all60 <- tb_all60 %>% left_join(pop %>% select(iso, pop60), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop60)




tb_all <- tb_all2030 %>% bind_rows(tb_all4050) %>% bind_rows(tb_all60)


tb_all <- tb_all %>% mutate(ind2030=ifelse(is.na(ind2030), 0, 1), ind4050=ifelse(is.na(ind4050), 0, 1), ind60=ifelse(is.na(ind60), 0, 1))


tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")


tb_two2030 <- tb_two %>% mutate(ind2030=1)
tb_two4050 <- tb_two %>% mutate(ind4050=1)
tb_two60 <- tb_two %>% mutate(ind60=1)



migration2030 <- migration %>% 
  filter(age_h>=20, age_h<40) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration4050 <- migration %>% 
  filter(age_h>=40, age_h<60) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration60 <- migration %>% 
  filter(age_h>=60) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()


tb_two2030 <- tb_two2030 %>% left_join(migration2030, by=c("year", "iso_o", "iso_d"))
tb_two4050 <- tb_two4050 %>% left_join(migration4050, by=c("year", "iso_o", "iso_d"))
tb_two60 <- tb_two60 %>% left_join(migration60, by=c("year", "iso_o", "iso_d"))


pop <- pop %>% mutate(year=1)
pop2 <- pop2 %>% mutate(year=2)

pop <- pop %>% bind_rows(pop2)


tb_two2030 <- tb_two2030 %>% left_join(pop %>% select(iso, pop2030, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop2030) %>% select(-pop2030)


tb_two2030 <- tb_two2030 %>% left_join(pop %>% select(iso, pop2030, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop2030) %>% select(-pop2030)

tb_two4050 <- tb_two4050 %>% left_join(pop %>% select(iso, pop4050, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop4050) %>% select(-pop4050)

tb_two4050 <- tb_two4050 %>% left_join(pop %>% select(iso, pop4050, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop4050) %>% select(-pop4050)

tb_two60 <- tb_two60 %>% left_join(pop %>% select(iso, pop60, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop60) %>% select(-pop60)

tb_two60 <- tb_two60 %>% left_join(pop %>% select(iso, pop60, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop60) %>% select(-pop60)




tb_two <- tb_two2030 %>% bind_rows(tb_two4050) %>% bind_rows(tb_two60)


tb_two <- tb_two %>% mutate(ind2030=ifelse(is.na(ind2030), 0, 1), ind4050=ifelse(is.na(ind4050), 0, 1), ind60=ifelse(is.na(ind60), 0, 1))


tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age.rds")







# stack regression start


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age.rds")

tb_all <- tb_all %>% mutate(age_group=ifelse(ind2030==1, 2030, 0)) %>% 
  mutate(age_group=ifelse(ind4050==1, 4050, age_group)) %>% 
  mutate(age_group=ifelse(ind60==1, 60, age_group))



tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | age_group |
                    shock_o + shock_o:ind4050 + shock_o:ind60 + shock_d + shock_d:ind4050 + shock_d:ind60  ~ shock_o_iv + shock_o_iv:ind4050 + shock_o_iv:ind60 + shock_d_iv + shock_d_iv:ind4050 + shock_d_iv:ind60, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age.rds")

tb_two <- tb_two %>% mutate(age_group=ifelse(ind2030==1, 2030, 0)) %>% 
  mutate(age_group=ifelse(ind4050==1, 4050, age_group)) %>% 
  mutate(age_group=ifelse(ind60==1, 60, age_group))

tb_two <- tb_two %>% mutate(year_age=paste0(as.character(year), as.character(age_group)))


tb_two <- tb_two %>% filter(cz_o!=cz_d)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year + year_age | 
                    shock_o + shock_o:ind4050 + shock_o:ind60 + shock_d + shock_d:ind4050 + shock_d:ind60  ~ shock_o_iv + shock_o_iv:ind4050 + shock_o_iv:ind60 + shock_d_iv + shock_d_iv:ind4050 + shock_d_iv:ind60, 
                  weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


texreg(list(out_tog1, out_tog2), 
       stars = c(0.1, 0.05, 0.01, 0.001), 
       digits = 5, 
       booktabs = TRUE,
       include.adjrs = FALSE, 
       include.proj.stats = FALSE, 
       use.packages = FALSE, 
       table = FALSE, 
       custom.model.names = c("log(migrate)", "log(migrate)"), 
       custom.coef.map = list("fit_shock_o" = "exp-imp origin",
                              "fit_shock_o:ind4050" = "exp-imp origin:ind4050",
                              "fit_shock_o:ind60" = "exp-imp origin:ind60",
                              "fit_shock_d" = "exp-imp dest",
                              "fit_ind4050:shock_d" = "exp-imp dest:ind4050",
                              "fit_ind60:shock_d" = "exp-imp dest:ind60"
                              ), 
       custom.gof.rows = list("Controls" = c("yes", "yes"),
                              "age fixed effects"= c("yes", "."),
                              "year:age fixed effects"= c(".", "yes"),
                              "year fixed effects" = c("no", "yes"), 
                              "region fixed effects" = c("no", "no")),
       "period" = c("1", "2"), 
       file = "results/tables/baseline_regression_age_all.tex")





# stack regression start


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age.rds")


tb_all <- tb_all %>% mutate(age_group=ifelse(ind2030==1, 2030, 0)) %>% 
  mutate(age_group=ifelse(ind4050==1, 4050, age_group)) %>% 
  mutate(age_group=ifelse(ind60==1, 60, age_group))


tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))
tb_all <- tb_all %>% mutate(migrate=migrate+1)
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | age_group |
                    shock_o + shock_o:ind4050 + shock_o:ind60 + shock_d + shock_d:ind4050 + shock_d:ind60  ~ shock_o_iv + shock_o_iv:ind4050 + shock_o_iv:ind60 + shock_d_iv + shock_d_iv:ind4050 + shock_d_iv:ind60, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age.rds")


tb_two <- tb_two %>% mutate(age_group=ifelse(ind2030==1, 2030, 0)) %>% 
  mutate(age_group=ifelse(ind4050==1, 4050, age_group)) %>% 
  mutate(age_group=ifelse(ind60==1, 60, age_group))

tb_two <- tb_two %>% mutate(year_age=paste0(as.character(year), as.character(age_group)))




tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))
tb_two <- tb_two %>% mutate(migrate=migrate+1)
tb_two <- tb_two %>% filter(cz_o!=cz_d)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year + year_age | 
                    shock_o + shock_o:ind4050 + shock_o:ind60 + shock_d + shock_d:ind4050 + shock_d:ind60  ~ shock_o_iv + shock_o_iv:ind4050 + shock_o_iv:ind60 + shock_d_iv + shock_d_iv:ind4050 + shock_d_iv:ind60, 
                  weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


texreg(list(out_tog1, out_tog2), 
       stars = c(0.1, 0.05, 0.01, 0.001), 
       digits = 5, 
       booktabs = TRUE,
       include.adjrs = FALSE, 
       include.proj.stats = FALSE, 
       use.packages = FALSE, 
       table = FALSE, 
       custom.model.names = c("log(migrate)", "log(migrate)"), 
       custom.coef.map = list("fit_shock_o" = "exp-imp origin",
                              "fit_shock_o:ind4050" = "exp-imp origin:ind4050",
                              "fit_shock_o:ind60" = "exp-imp origin:ind60",
                              "fit_shock_d" = "exp-imp dest",
                              "fit_ind4050:shock_d" = "exp-imp dest:ind4050",
                              "fit_ind60:shock_d" = "exp-imp dest:ind60"
       ), 
       custom.gof.rows = list("Controls" = c("yes", "yes"),
                              "age fixed effects"= c("yes", "."),
                              "year:age fixed effects"= c(".", "yes"),
                              "year fixed effects" = c("no", "yes"), 
                              "region fixed effects" = c("no", "no")),
       "period" = c("1", "2"), 
       file = "results/tables/baseline_regression_age_all_logadd.tex")


