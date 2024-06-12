if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(sf, fixest, texreg, tidyverse)




# we do baseline regression with just import competition


# baseline column formats : for both one period and two period

# japan IV, with control, cluster-sigungu

# 1st column: separate-Both 1 year

# 2nd column: separate-Both 2 year

# 3rd column: Sep-both cz origin and dest fef 2 year


#baseline with just import

tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + imp_shock_d ~ imp_shock1999_iv_o + imp_shock1999_iv_d, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                   imp_shock_o + imp_shock_d ~ imp_shock1999_iv_o + imp_shock1999_iv_d, 
                 weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o + cz_d | 
                    imp_shock_o + imp_shock_d ~ imp_shock1999_iv_o + imp_shock1999_iv_d, 
                  weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two)



screenreg(list(out_sep1, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes"), "year fixed effects"=c("no", "yes", "yes"), "region fixed effects"=c("no", "no", "dest and origin"), "period"=c("1", "2", "2")))

texreg(list(out_sep1, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes"), "year fixed effects"=c("no", "yes", "yes"), "region fixed effects"=c("no", "no", "dest and origin"), "period"=c("1", "2", "2")), file="results/tables/baseline_regression_import.tex")




# baseline with export control

tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ exp_shock_o + exp_shock_d + log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + imp_shock_d ~ imp_shock1999_iv_o + imp_shock1999_iv_d, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)

out_tog2 <- feols(log(migrate) ~ exp_shock_o + exp_shock_d + log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock  ~ shock_iv, 
                  weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock=(imp_shock_d - imp_shock_o)/1000, shock_iv=(imp_shock1999_iv_d - imp_shock1999_iv_o)/1000))


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ exp_shock_o + exp_shock_d + log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + imp_shock_d ~ imp_shock1999_iv_o + imp_shock1999_iv_d, 
                  weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2_fef <- feols(log(migrate) ~ exp_shock_o + exp_shock_d + log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o + cz_d | 
                        imp_shock_o + imp_shock_d ~ imp_shock1999_iv_o + imp_shock1999_iv_d, 
                      weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two)



screenreg(list(out_sep1, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "exp_shock_d"="exp_shock_d", "exp_shock_o"="exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes"), "year fixed effects"=c("no", "yes", "yes"), "region fixed effects"=c("no", "no", "dest and origin"), "period"=c("1", "2", "2")))

texreg(list(out_sep1, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "exp_shock_d"="exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "exp_shock_o"="exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes"), "year fixed effects"=c("no", "yes", "yes"), "region fixed effects"=c("no", "no", "dest and origin"), "period"=c("1", "2", "2")), file="results/tables/baseline_regression_import_exp_control.tex")























### for age 25-64

# more finer version in age regression


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





pop <- read_rds("data/final/pop_age.xlsx")


pop2 <- read_rds("data/final/pop_age2.xlsx")



# for now we will add up 20, 30, 40, 50, 60+

pop <- pop %>% mutate(pop=age20_24+age25_29+age30_34+age35_39+age40_44+age45_49+age50_54+age55_59+age60_64) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age20_24+age25_29+age30_34+age35_39+age40_44+age45_49+age50_54+age55_59+age60_64) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=25, age_h<65) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_64.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=25, age_h<65) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()


tb_two <- tb_two %>% left_join(migration25, by=c("year", "iso_o", "iso_d"))


pop <- pop %>% mutate(year=1)
pop2 <- pop2 %>% mutate(year=2)

pop <- pop %>% bind_rows(pop2)



tb_two <- tb_two %>% left_join(pop %>% select(iso, pop, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop) %>% select(-pop)


tb_two <- tb_two %>% left_join(pop %>% select(iso, pop, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop) %>% select(-pop)





tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_64.rds")





tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_64.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + imp_shock_d ~ imp_shock1999_iv_o + imp_shock1999_iv_d, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)



tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_64.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + imp_shock_d ~ imp_shock1999_iv_o + imp_shock1999_iv_d, 
                  weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_64.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o + cz_d | 
                        imp_shock_o + imp_shock_d ~ imp_shock1999_iv_o + imp_shock1999_iv_d, 
                      weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_sep1, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes"), "year fixed effects"=c("no", "yes", "yes"), "region fixed effects"=c("no", "no", "dest and origin"), "period"=c("1", "2", "2")))

texreg(list(out_sep1, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes"), "year fixed effects"=c("no", "yes", "yes"), "region fixed effects"=c("no", "no", "dest and origin"), "period"=c("1", "2", "2")), file="results/tables/baseline_regression_import2564.tex")










### male age 40-64.


# now read in population to do weighting

pop <- readxl::read_excel("data/final/pop_age_m.xlsx") %>%
  mutate(across(starts_with("age"), as.numeric), iso=str_sub(iso, 1, 5))


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
  summarise(across(starts_with("age"), ~ sum(.x, na.rm = TRUE)))
pop %>% write_rds("data/final/pop_age_m.rds")


pop <- read_rds("data/final/pop_age_m.rds")




pop <- readxl::read_excel("data/final/pop_age2_m.xlsx") %>%
  mutate(across(starts_with("age"), as.numeric), iso=str_sub(iso, 1, 5))


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
  summarise(across(starts_with("age"), ~ sum(.x, na.rm = TRUE)))

pop %>% write_rds("data/final/pop_age2_m.rds")


pop2 <- read_rds("data/final/pop_age2_m.rds")







#####

### for male age 40-64

# more finer version in age regression


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





pop <- read_rds("data/final/pop_age_m.rds")


pop2 <- read_rds("data/final/pop_age2_m.rds")



# for now we will add up 40-64

pop <- pop %>% mutate(pop=age40_44+age45_49+age50_54+age55_59+age60_64) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age40_44+age45_49+age50_54+age55_59+age60_64) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=40 & age_h<65, gen_h==1 | gen_h==3 | gen_h==9) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age40_64.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=40 & age_h<65, gen_h==1 | gen_h==3 | gen_h==9) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()


tb_two <- tb_two %>% left_join(migration25, by=c("year", "iso_o", "iso_d"))


pop <- pop %>% mutate(year=1)
pop2 <- pop2 %>% mutate(year=2)

pop <- pop %>% bind_rows(pop2)



tb_two <- tb_two %>% left_join(pop %>% select(iso, pop, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop) %>% select(-pop)


tb_two <- tb_two %>% left_join(pop %>% select(iso, pop, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop) %>% select(-pop)





tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age40_64.rds")



tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age40_64.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d |
                    imp_shock_o + imp_shock_d ~ imp_shock1999_iv_o + imp_shock1999_iv_d, 
                  weights = tb_all$pop2001_o, cluster="iso_o^iso_d", data=tb_all)



tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age40_64.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + imp_shock_d ~ imp_shock1999_iv_o + imp_shock1999_iv_d, 
                  weights = tb_two$pop_o, cluster="iso_o^iso_d", data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age40_64.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o^cz_d | 
                        imp_shock_o + imp_shock_d ~ imp_shock1999_iv_o + imp_shock1999_iv_d, 
                      weights = tb_two$pop_o, cluster="iso_o^iso_d", data=tb_two)






screenreg(list(out_sep1, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes"), "year fixed effects"=c("no", "yes", "yes"), "region fixed effects"=c("no", "no", "dest and origin"), "period"=c("1", "2", "2")))

texreg(list(out_sep1, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes"), "year fixed effects"=c("no", "yes", "yes"), "region fixed effects"=c("no", "no", "dest and origin"), "period"=c("1", "2", "2")), file="results/tables/baseline_regression_import4064.tex")








### female age 40-64.


# now read in population to do weighting

pop <- readxl::read_excel("data/final/pop_age_w.xlsx") %>%
  mutate(across(starts_with("age"), as.numeric), iso=str_sub(iso, 1, 5))


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
  summarise(across(starts_with("age"), ~ sum(.x, na.rm = TRUE)))
pop %>% write_rds("data/final/pop_age_w.rds")


pop <- read_rds("data/final/pop_age_w.rds")




pop <- readxl::read_excel("data/final/pop_age2_w.xlsx") %>%
  mutate(across(starts_with("age"), as.numeric), iso=str_sub(iso, 1, 5))


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
  summarise(across(starts_with("age"), ~ sum(.x, na.rm = TRUE)))

pop %>% write_rds("data/final/pop_age2_w.rds")


pop2 <- read_rds("data/final/pop_age2_w.rds")







#####

### for female age 40-64

# more finer version in age regression


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





pop <- read_rds("data/final/pop_age_w.rds")


pop2 <- read_rds("data/final/pop_age2_w.rds")



# for now we will add up 40-64

pop <- pop %>% mutate(pop=age40_44+age45_49+age50_54+age55_59+age60_64) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age40_44+age45_49+age50_54+age55_59+age60_64) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=40 & age_h<65, gen_h==0 | gen_h==2 | gen_h==4) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age40_64_w.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=40 & age_h<65, gen_h==0 | gen_h==2 | gen_h==4) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()


tb_two <- tb_two %>% left_join(migration25, by=c("year", "iso_o", "iso_d"))


pop <- pop %>% mutate(year=1)
pop2 <- pop2 %>% mutate(year=2)

pop <- pop %>% bind_rows(pop2)



tb_two <- tb_two %>% left_join(pop %>% select(iso, pop, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop) %>% select(-pop)


tb_two <- tb_two %>% left_join(pop %>% select(iso, pop, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop) %>% select(-pop)





tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age40_64_w.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age40_64_w.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age40_64_w.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age40_64_w.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age40_64_w.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age40_64_w.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_40_64_w.tex")

