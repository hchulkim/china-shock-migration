

if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(sf, fixest, texreg, tidyverse)




# we try to find out why there is reverse pattern for 25-34.




# 1. maybe differs by 1 household and others.


## age 25-34 1 household

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



# for now we will add up

pop <- pop %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=25, age_h<35, household==2) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_1household.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=25, age_h<35, household==2) %>% 
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

tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d"))



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_1household.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_1household.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_1household.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_1household.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)


out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_1household.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_1household.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_25_34_1household.tex")





## age 25-34 more than 1 household

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



# for now we will add up

pop <- pop %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=25, age_h<35, household==1) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_2household.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=25, age_h<35, household==1) %>% 
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

tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d"))



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_2household.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_2household.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_2household.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_2household.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_2household.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_2household.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_25_34_2household.tex")




# indicator for 25-34 1 house and 25-34 more than one household




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



# for now we will use pop for 25-34

pop <- pop %>% mutate(pop25=age25_29+age30_34) %>% 
  select(iso, pop25)



pop2 <- pop2 %>% mutate(pop25=age25_29+age30_34) %>% 
  select(iso, pop25)



# stack them


######


tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")

# one period


tb_all1 <- tb_all %>% mutate(ind1=1)
tb_all2 <- tb_all %>% mutate(ind2=1)




migration1 <- migration %>% 
  filter(year<2020, age_h>=25 & age_h<35, household==2) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration2 <- migration %>% 
  filter(year<2020, age_h>=25 & age_h<35, household==1) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()







tb_all1 <- tb_all1 %>% left_join(migration1, by=c("iso_o", "iso_d"))
tb_all2 <- tb_all2 %>% left_join(migration2, by=c("iso_o", "iso_d"))




tb_all1 <- tb_all1 %>% left_join(pop %>% select(iso, pop25), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop25)

tb_all1 <- tb_all1 %>% left_join(pop %>% select(iso, pop25), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop25)


tb_all2 <- tb_all2 %>% left_join(pop %>% select(iso, pop25), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop25)

tb_all2 <- tb_all2 %>% left_join(pop %>% select(iso, pop25), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop25)




tb_all <- tb_all1 %>% bind_rows(tb_all2) 


tb_all <- tb_all %>% mutate(ind1=ifelse(is.na(ind1), 0, 1), ind2=ifelse(is.na(ind2), 0, 1))



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))


tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_household_korea.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")


tb_two1 <- tb_two %>% mutate(ind1=1)
tb_two2 <- tb_two %>% mutate(ind2=1)




migration1 <- migration %>% 
  filter(age_h>=25 & age_h<35, household==2) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration2 <- migration %>% 
  filter(age_h>=25 & age_h<35, household==1) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_two1 <- tb_two1 %>% left_join(migration1, by=c("year", "iso_o", "iso_d"))
tb_two2 <- tb_two2 %>% left_join(migration2, by=c("year", "iso_o", "iso_d"))




pop <- pop %>% mutate(year=1)
pop2 <- pop2 %>% mutate(year=2)

pop <- pop %>% bind_rows(pop2)






tb_two1 <- tb_two1 %>% left_join(pop %>% select(iso, pop25, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop25) %>% select(-pop25)


tb_two1 <- tb_two1 %>% left_join(pop %>% select(iso, pop25, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop25) %>% select(-pop25)


tb_two2 <- tb_two2 %>% left_join(pop %>% select(iso, pop25, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop25) %>% select(-pop25)


tb_two2 <- tb_two2 %>% left_join(pop %>% select(iso, pop25, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop25) %>% select(-pop25)





tb_two <- tb_two1 %>% bind_rows(tb_two2)



tb_two <- tb_two %>% mutate(ind1=ifelse(is.na(ind1), 0, 1), ind2=ifelse(is.na(ind2), 0, 1))


tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))



tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, ind1, ind2, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d", "ind1", "ind2"))

tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_household_korea.rds")







# stack regression start


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_household_korea.rds")

tb_all <- tb_all %>% mutate(house_group=ifelse(ind1==1, 1, 0)) %>% 
  mutate(house_group=ifelse(ind2==1, 2, house_group))



tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(
  log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
    house_group |
    imp_shock_o + imp_shock_o:ind2 + exp_shock_o + exp_shock_o:ind2 + imp_shock_d + imp_shock_d:ind2 + exp_shock_d + exp_shock_d:ind2 ~
    imp_shock1999_iv_o + imp_shock1999_iv_o:ind2  + exp_shock1999_iv_o + exp_shock1999_iv_o:ind2 + imp_shock1999_iv_d + imp_shock1999_iv_d:ind2 + exp_shock1999_iv_d + exp_shock1999_iv_d:ind2,
  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all
)



tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_household_korea.rds")

tb_two <- tb_two %>% mutate(house_group=ifelse(ind1==1, 1, 0)) %>% 
  mutate(house_group=ifelse(ind2==1, 2, house_group))

tb_two <- tb_two %>% mutate(year_house=paste0(as.character(year), as.character(house_group)))


tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year + year_house | imp_shock_o + imp_shock_o:ind2 + exp_shock_o + exp_shock_o:ind2 + imp_shock_d + imp_shock_d:ind2 + exp_shock_d + exp_shock_d:ind2 ~
                    imp_shock1999_iv_o + imp_shock1999_iv_o:ind2  + exp_shock1999_iv_o + exp_shock1999_iv_o:ind2 + imp_shock1999_iv_d + imp_shock1999_iv_d:ind2 + exp_shock1999_iv_d + exp_shock1999_iv_d:ind2,
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)




tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_household_korea.rds")

tb_two <- tb_two %>% mutate(house_group=ifelse(ind1==1, 1, 0)) %>% 
  mutate(house_group=ifelse(ind2==1, 2, house_group))

tb_two <- tb_two %>% mutate(year_house=paste0(as.character(year), as.character(house_group)))


tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2_fef  <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year + year_house + cz_o + cz_d | imp_shock_o + imp_shock_o:ind2 + exp_shock_o + exp_shock_o:ind2 + imp_shock_d + imp_shock_d:ind2 + exp_shock_d + exp_shock_d:ind2 ~
                         imp_shock1999_iv_o + imp_shock1999_iv_o:ind2  + exp_shock1999_iv_o + exp_shock1999_iv_o:ind2 + imp_shock1999_iv_d + imp_shock1999_iv_d:ind2 + exp_shock1999_iv_d + exp_shock1999_iv_d:ind2,
                       weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


screenreg(list(out_sep1, out_sep2, out_sep2_fef), 
          stars = c(0.1, 0.05, 0.01, 0.001), 
          digits = 5, 
          custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), 
          custom.coef.map = list("fit_imp_shock_o" = "imp origin",
                                 "fit_imp_shock_o:ind2" = "imp origin:ind2",
                                 "fit_imp_shock_o:ind35" = "imp origin:ind35",
                                 "fit_imp_shock_o:ind45" = "imp origin:ind45",
                                 "fit_imp_shock_o:ind55" = "imp origin:ind55",
                                 "fit_imp_shock_d" = "imp dest",
                                 "fit_ind2:imp_shock_d" = "imp dest:ind2",
                                 "fit_ind35:imp_shock_d" = "imp dest:ind35",
                                 "fit_ind45:imp_shock_d" = "imp dest:ind45",
                                 "fit_ind55:imp_shock_d" = "imp dest:ind55",
                                 "fit_exp_shock_o" = "exp origin",
                                 "fit_ind2:exp_shock_o" = "exp origin:ind2",
                                 "fit_ind35:exp_shock_o" = "exp origin:ind35",
                                 "fit_ind45:exp_shock_o" = "exp origin:ind45",
                                 "fit_ind55:exp_shock_o" = "exp origin:ind55",
                                 "fit_exp_shock_d" = "exp dest",
                                 "fit_ind2:exp_shock_d" = "exp dest:ind2",
                                 "fit_ind35:exp_shock_d" = "exp dest:ind35",
                                 "fit_ind45:exp_shock_d" = "exp dest:ind45",
                                 "fit_ind55:exp_shock_d" = "exp dest:ind55"
          ), 
          custom.gof.rows = list("Controls" = c("yes", "yes", "yes"),
                                 "house fixed effects"= c("yes", ".", "."),
                                 "year:house fixed effects"= c(".", "yes", "yes"),
                                 "year fixed effects" = c("no", "yes", "yes"), 
                                 "region fixed effects" = c("no", "no", "yes")),
          "period" = c("1", "2", "2"))


texreg(list(out_sep1, out_sep2, out_sep2_fef), 
       stars = c(0.1, 0.05, 0.01, 0.001), 
       digits = 5, 
       booktabs = TRUE,
       include.adjrs = FALSE, 
       include.proj.stats = FALSE, 
       use.packages = FALSE, 
       table = FALSE, 
       custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), 
       custom.coef.map = list("fit_imp_shock_o" = "imp origin",
                              "fit_imp_shock_o:ind25" = "imp origin:ind25",
                              "fit_imp_shock_o:ind35" = "imp origin:ind35",
                              "fit_imp_shock_o:ind45" = "imp origin:ind45",
                              "fit_imp_shock_o:ind55" = "imp origin:ind55",
                              "fit_imp_shock_d" = "imp dest",
                              "fit_ind25:imp_shock_d" = "imp dest:ind25",
                              "fit_ind35:imp_shock_d" = "imp dest:ind35",
                              "fit_ind45:imp_shock_d" = "imp dest:ind45",
                              "fit_ind55:imp_shock_d" = "imp dest:ind55",
                              "fit_exp_shock_o" = "exp origin",
                              "fit_ind25:exp_shock_o" = "exp origin:ind25",
                              "fit_ind35:exp_shock_o" = "exp origin:ind35",
                              "fit_ind45:exp_shock_o" = "exp origin:ind45",
                              "fit_ind55:exp_shock_o" = "exp origin:ind55",
                              "fit_exp_shock_d" = "exp dest",
                              "fit_ind25:exp_shock_d" = "exp dest:ind25",
                              "fit_ind35:exp_shock_d" = "exp dest:ind35",
                              "fit_ind45:exp_shock_d" = "exp dest:ind45",
                              "fit_ind55:exp_shock_d" = "exp dest:ind55"
       ), 
       custom.gof.rows = list("Controls" = c("yes", "yes", "yes"),
                              "age fixed effects"= c("yes", ".", "."),
                              "year:age fixed effects"= c(".", "yes", "yes"),
                              "year fixed effects" = c("no", "yes", "yes"), 
                              "region fixed effects" = c("no", "no", "yes")),
       "period" = c("1", "2", "2"), 
       file = "results/tables/baseline_regression_age_sep.tex")





# now 2x2 figures





# Extracting relevant coefficients from out_sep1
plot_data <- out_sep1 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(house_group = case_when(
    str_detect(term, "ind2") ~ "2+ household",
    TRUE ~ "1 household"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(house_group == "1 household" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(house_group == "1 household" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(house_group == "1 household" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(house_group == "1 household" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & house_group != "1 household" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & house_group != "1 household" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & house_group != "1 household" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & house_group != "1 household" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=house_group, y=adjusted_estimate)) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 2 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_grid(type ~ color_group, scales="free_y", labeller = labeller(type = label_parsed, color_group = label_parsed)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "none") # hiding legend as it's redundant now


ggsave("results/figures/coef_plot1house.png", dpi=400, height=8, width=15, scale=0.5)



# Extracting relevant coefficients from out_sep2
plot_data <- out_sep2 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(house_group = case_when(
    str_detect(term, "ind2") ~ "2+ household",
    TRUE ~ "1 household"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(house_group == "1 household" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(house_group == "1 household" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(house_group == "1 household" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(house_group == "1 household" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & house_group != "1 household" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & house_group != "1 household" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & house_group != "1 household" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & house_group != "1 household" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=house_group, y=adjusted_estimate)) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 4 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_grid(type ~ color_group, scales="free_y", labeller = labeller(type = label_parsed, color_group = label_parsed)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "none") # hiding legend as it's redundant now


ggsave("results/figures/coef_plot2house.png", dpi=400, height=8, width=15, scale=0.5)




# Extracting relevant coefficients from out_sep2_fef
plot_data <- out_sep2_fef %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(house_group = case_when(
    str_detect(term, "ind2") ~ "2+ household",
    TRUE ~ "1 household"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(house_group == "1 household" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(house_group == "1 household" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(house_group == "1 household" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(house_group == "1 household" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & house_group != "1 household" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & house_group != "1 household" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & house_group != "1 household" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & house_group != "1 household" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=house_group, y=adjusted_estimate)) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 5 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_grid(type ~ color_group, scales="free_y", labeller = labeller(type = label_parsed, color_group = label_parsed)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "none") # hiding legend as it's redundant now


ggsave("results/figures/coef_plot3house.png", dpi=400, height=8, width=15, scale=0.5)

















# 2. differ by man and woman


## man

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



# for now we will add up

pop <- pop %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=25, age_h<35, gen_h==1 | gen_h==3 | gen_h==9) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_man.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=25, age_h<35, gen_h==1 | gen_h==3 | gen_h==9) %>% 
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

tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d"))



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_man.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_man.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_man.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_man.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_man.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_man.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))



out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_25_34_man.tex")






## woman

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



# for now we will add up

pop <- pop %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=25, age_h<35, gen_h==0 | gen_h==2 | gen_h==4) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_woman.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=25, age_h<35, gen_h==0 | gen_h==2 | gen_h==4) %>% 
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

tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d"))



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_woman.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_woman.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_woman.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_woman.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)


out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d   | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_woman.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d   | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_woman.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o ^cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_25_34_woman.tex")






# 4. does this change then if there are only 35 +?



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

pop <- pop %>% mutate(pop=age35_39+age40_44+age45_49+age50_54+age55_59+age60_64) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age35_39+age40_44+age45_49+age50_54+age55_59+age60_64) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=35 & age_h<65 ) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age35_64.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=35 & age_h<65 ) %>% 
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

tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d"))



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age35_64.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age35_64.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age35_64.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age35_64.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age35_64.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age35_64.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_35more.tex")








# 5. maybe the effect disappear if we take out metro areas?



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

pop <- pop %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=25 & age_h<35 ) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=25 & age_h<35 ) %>% 
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

tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d"))



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

tb_all <- tb_all %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_all <- tb_all %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_all <- tb_all %>% filter(cz_o!=1 & cz_d!=1)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_all <- tb_all %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_all <- tb_all %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_all <- tb_all %>% filter(cz_o!=1 & cz_d!=1)

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)

tb_two <- tb_two %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_two <- tb_two %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_two <- tb_two %>% filter(cz_o!=1 & cz_d!=1)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_two <- tb_two %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_two <- tb_two %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_two <- tb_two %>% filter(cz_o!=1 & cz_d!=1)


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_two <- tb_two %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_two <- tb_two %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_two <- tb_two %>% filter(cz_o!=1 & cz_d!=1)

out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_nometro.tex")










# 5. maybe the effect disappear if we take out metro areas? and also 1 household?



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

pop <- pop %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=25 & age_h<35, household==2) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_1household.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=25 & age_h<35, household==2 ) %>% 
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

tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d"))



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_1household.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_1household.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

tb_all <- tb_all %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_all <- tb_all %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_all <- tb_all %>% filter(cz_o!=1 & cz_d!=1)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_1household.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_all <- tb_all %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_all <- tb_all %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_all <- tb_all %>% filter(cz_o!=1 & cz_d!=1)

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_1household.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)

tb_two <- tb_two %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_two <- tb_two %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_two <- tb_two %>% filter(cz_o!=1 & cz_d!=1)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_1household.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_two <- tb_two %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_two <- tb_two %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_two <- tb_two %>% filter(cz_o!=1 & cz_d!=1)


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_1household.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_two <- tb_two %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_two <- tb_two %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_two <- tb_two %>% filter(cz_o!=1 & cz_d!=1)

out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_nometro_1household.tex")









# 5. maybe the effect disappear if we take out metro areas? and also 2+ household?



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

pop <- pop %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=25 & age_h<35, household==1) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_2household.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=25 & age_h<35, household==1 ) %>% 
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

tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d"))



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_2household.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_2household.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

tb_all <- tb_all %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_all <- tb_all %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_all <- tb_all %>% filter(cz_o!=1 & cz_d!=1)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_2household.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_all <- tb_all %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_all <- tb_all %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_all <- tb_all %>% filter(cz_o!=1 & cz_d!=1)

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_2household.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)

tb_two <- tb_two %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_two <- tb_two %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_two <- tb_two %>% filter(cz_o!=1 & cz_d!=1)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_2household.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_two <- tb_two %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_two <- tb_two %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_two <- tb_two %>% filter(cz_o!=1 & cz_d!=1)


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_2household.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_two <- tb_two %>% filter(str_sub(iso_o)>30 & str_sub(iso_o)<39)
tb_two <- tb_two %>% filter(str_sub(iso_d)>30 & str_sub(iso_d)<39)
tb_two <- tb_two %>% filter(cz_o!=1 & cz_d!=1)

out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_nometro_2household.tex")












#go for reasons just job

## age 25-34 1 household

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



# for now we will add up

pop <- pop %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=25, age_h<35, reason==1) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_job.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=25, age_h<35, reason==1) %>% 
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

tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d"))



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_job.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_job.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_job.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_job.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)


out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_job.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_job.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_25_34_job.tex")












#go for reasons just family

## age 25-34 1 household

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



# for now we will add up

pop <- pop %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=25, age_h<35, reason==2) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_family.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=25, age_h<35, reason==2) %>% 
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

tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d"))



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_family.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_family.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_family.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_family.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)


out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_family.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_family.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_25_34_family.tex")










#go for reasons just house

## age 25-34 1 household

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



# for now we will add up

pop <- pop %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=25, age_h<35, reason==3) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_house.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=25, age_h<35, reason==3) %>% 
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

tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d"))



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_house.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_house.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_house.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_house.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)


out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_house.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_house.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_25_34_house.tex")










#go for reasons just edu

## age 25-34 1 household

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



# for now we will add up

pop <- pop %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



pop2 <- pop2  %>% mutate(pop=age25_29+age30_34) %>% 
  select(iso, pop)



tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")


migration25 <- migration %>% 
  filter(year<2020, age_h>=25, age_h<35, reason==4) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()



tb_all <- tb_all %>% left_join(migration25, by=c("iso_o", "iso_d"))


tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop)

tb_all <- tb_all %>% left_join(pop %>% select(iso, pop), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop)



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_edu.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")

migration25 <- migration %>% 
  filter(age_h>=25, age_h<35, reason==4) %>% 
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

tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d"))



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_edu.rds")






tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_edu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_34_edu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_edu.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)


out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_edu.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_34_edu.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_25_34_edu.tex")










# hetero 25-34, 35-44, 45-54, 55-64, 65+





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



# for now we will add up 25-34, 35-44, 45-54, 55-64

pop <- pop %>% mutate(pop25=age25_29+age30_34,
                      pop35=age35_39+age40_44,
                      pop45=age45_49+age50_54,
                      pop55=age55_59+age60_64,
                      pop65=age65_69+age70_74+age75_79+age80_84+age85_89+age90_94+age95_99+age100) %>% 
  select(iso, pop25, pop35, pop45, pop55, pop65)



pop2 <- pop2 %>%  mutate(pop25=age25_29+age30_34,
                         pop35=age35_39+age40_44,
                         pop45=age45_49+age50_54,
                         pop55=age55_59+age60_64,
                         pop65=age65_69+age70_74+age75_79+age80_84+age85_89+age90_94+age95_99+age100) %>% 
  select(iso, pop25, pop35, pop45, pop55, pop65)






# stack them


######


tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")

# one period


tb_all25 <- tb_all %>% mutate(ind25=1)
tb_all35 <- tb_all %>% mutate(ind35=1)
tb_all45 <- tb_all %>% mutate(ind45=1)
tb_all55 <- tb_all %>% mutate(ind55=1)
tb_all65 <- tb_all %>% mutate(ind65=1)



migration25 <- migration %>% 
  filter(year<2020, age_h>=25 & age_h<35, reason!=3) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration35 <- migration %>% 
  filter(year<2020, age_h>=35 & age_h<45, reason!=3) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration45 <- migration %>% 
  filter(year<2020, age_h>=45 & age_h<55, reason!=3) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration55 <- migration %>% 
  filter(year<2020, age_h>=55 & age_h<65, reason!=3) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration65 <- migration %>% 
  filter(year<2020, age_h>=65, reason!=3) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()







tb_all25 <- tb_all25 %>% left_join(migration25, by=c("iso_o", "iso_d"))
tb_all35 <- tb_all35 %>% left_join(migration35, by=c("iso_o", "iso_d"))
tb_all45 <- tb_all45 %>% left_join(migration45, by=c("iso_o", "iso_d"))
tb_all55 <- tb_all55 %>% left_join(migration55, by=c("iso_o", "iso_d"))
tb_all65 <- tb_all65 %>% left_join(migration65, by=c("iso_o", "iso_d"))



tb_all25 <- tb_all25 %>% left_join(pop %>% select(iso, pop25), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop25)

tb_all25 <- tb_all25 %>% left_join(pop %>% select(iso, pop25), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop25)


tb_all35 <- tb_all35 %>% left_join(pop %>% select(iso, pop35), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop35)

tb_all35 <- tb_all35 %>% left_join(pop %>% select(iso, pop35), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop35)


tb_all45 <- tb_all45 %>% left_join(pop %>% select(iso, pop45), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop45)

tb_all45 <- tb_all45 %>% left_join(pop %>% select(iso, pop45), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop45)



tb_all55 <- tb_all55 %>% left_join(pop %>% select(iso, pop55), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop55)

tb_all55 <- tb_all55 %>% left_join(pop %>% select(iso, pop55), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop55)

tb_all65 <- tb_all65 %>% left_join(pop %>% select(iso, pop65), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop65)

tb_all65 <- tb_all65 %>% left_join(pop %>% select(iso, pop65), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop65)







tb_all <- tb_all25 %>% bind_rows(tb_all35) %>% bind_rows(tb_all45) %>% bind_rows(tb_all55) %>% bind_rows(tb_all65) 


tb_all <- tb_all %>% mutate(ind25=ifelse(is.na(ind25), 0, 1), ind35=ifelse(is.na(ind35), 0, 1), ind45=ifelse(is.na(ind45), 0, 1), ind55=ifelse(is.na(ind55), 0, 1), ind65=ifelse(is.na(ind65), 0, 1))



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age_korea_nohouse.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")


tb_two25 <- tb_two %>% mutate(ind25=1)
tb_two35 <- tb_two %>% mutate(ind35=1)
tb_two45 <- tb_two %>% mutate(ind45=1)
tb_two55 <- tb_two %>% mutate(ind55=1)
tb_two65 <- tb_two %>% mutate(ind65=1)



migration25 <- migration %>% 
  filter(age_h>=25 & age_h<35, reason!=3) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration35 <- migration %>% 
  filter(age_h>=35 & age_h<45, reason!=3) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration45 <- migration %>% 
  filter(age_h>=45 & age_h<55, reason!=3) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration55 <- migration %>% 
  filter(age_h>=55 & age_h<65, reason!=3) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration65 <- migration %>% 
  filter(age_h>=65, reason!=3) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()




tb_two25 <- tb_two25 %>% left_join(migration25, by=c("year", "iso_o", "iso_d"))
tb_two35 <- tb_two35 %>% left_join(migration35, by=c("year", "iso_o", "iso_d"))
tb_two45 <- tb_two45 %>% left_join(migration45, by=c("year", "iso_o", "iso_d"))
tb_two55 <- tb_two55 %>% left_join(migration55, by=c("year", "iso_o", "iso_d"))
tb_two65 <- tb_two65 %>% left_join(migration65, by=c("year", "iso_o", "iso_d"))



pop <- pop %>% mutate(year=1)
pop2 <- pop2 %>% mutate(year=2)

pop <- pop %>% bind_rows(pop2)






tb_two25 <- tb_two25 %>% left_join(pop %>% select(iso, pop25, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop25) %>% select(-pop25)


tb_two25 <- tb_two25 %>% left_join(pop %>% select(iso, pop25, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop25) %>% select(-pop25)


tb_two35 <- tb_two35 %>% left_join(pop %>% select(iso, pop35, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop35) %>% select(-pop35)


tb_two35 <- tb_two35 %>% left_join(pop %>% select(iso, pop35, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop35) %>% select(-pop35)




tb_two45 <- tb_two45 %>% left_join(pop %>% select(iso, pop45, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop45) %>% select(-pop45)

tb_two45 <- tb_two45 %>% left_join(pop %>% select(iso, pop45, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop45) %>% select(-pop45)


tb_two55 <- tb_two55 %>% left_join(pop %>% select(iso, pop55, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop55) %>% select(-pop55)

tb_two55 <- tb_two55 %>% left_join(pop %>% select(iso, pop55, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop55) %>% select(-pop55)


tb_two65 <- tb_two65 %>% left_join(pop %>% select(iso, pop65, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop65) %>% select(-pop65)

tb_two65 <- tb_two65 %>% left_join(pop %>% select(iso, pop65, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop65) %>% select(-pop65)




tb_two <- tb_two25 %>% bind_rows(tb_two35) %>% bind_rows(tb_two45) %>% bind_rows(tb_two55) %>% bind_rows(tb_two65)


tb_two <- tb_two %>% mutate(ind25=ifelse(is.na(ind25), 0, 1), ind35=ifelse(is.na(ind35), 0, 1), ind45=ifelse(is.na(ind45), 0, 1), ind55=ifelse(is.na(ind55), 0, 1), ind65=ifelse(is.na(ind65), 0, 1))


tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))



tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, ind25, ind35, ind45, ind55, ind65, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d", "ind25", "ind35", "ind45", "ind55", "ind65"))

tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age_korea_nohouse.rds")







# stack regression start


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age_korea_nohouse.rds")

tb_all <- tb_all %>% mutate(age_group=ifelse(ind25==1, 25, 0)) %>% 
  mutate(age_group=ifelse(ind35==1, 35, age_group)) %>% 
  mutate(age_group=ifelse(ind45==1, 45, age_group)) %>% 
  mutate(age_group=ifelse(ind55==1, 55, age_group)) %>% 
  mutate(age_group=ifelse(ind65==1, 65, age_group))



tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(
  log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
    age_group |
    imp_shock_o + imp_shock_o:ind25 + imp_shock_o:ind35 + imp_shock_o:ind45 + imp_shock_o:ind55 + exp_shock_o + exp_shock_o:ind25 + exp_shock_o:ind35 + exp_shock_o:ind45 + exp_shock_o:ind55 + imp_shock_d + imp_shock_d:ind25 + imp_shock_d:ind35 + imp_shock_d:ind45 + imp_shock_d:ind55 + exp_shock_d + exp_shock_d:ind25 + exp_shock_d:ind35 + exp_shock_d:ind45 + exp_shock_d:ind55 ~
    imp_shock1999_iv_o + imp_shock1999_iv_o:ind25 + imp_shock1999_iv_o:ind35 + imp_shock1999_iv_o:ind45 + imp_shock1999_iv_o:ind55 + exp_shock1999_iv_o + exp_shock1999_iv_o:ind25 + exp_shock1999_iv_o:ind35 + exp_shock1999_iv_o:ind45 + exp_shock1999_iv_o:ind55 + imp_shock1999_iv_d + imp_shock1999_iv_d:ind25 + imp_shock1999_iv_d:ind35 + imp_shock1999_iv_d:ind45 + imp_shock1999_iv_d:ind55 + exp_shock1999_iv_d + exp_shock1999_iv_d:ind25 + exp_shock1999_iv_d:ind35 + exp_shock1999_iv_d:ind45 + exp_shock1999_iv_d:ind55,
  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all
)



tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age_korea_nohouse.rds")

tb_two <- tb_two %>% mutate(age_group=ifelse(ind25==1, 25, 0)) %>% 
  mutate(age_group=ifelse(ind35==1, 35, age_group)) %>% 
  mutate(age_group=ifelse(ind45==1, 45, age_group)) %>% 
  mutate(age_group=ifelse(ind55==1, 55, age_group)) %>% 
  mutate(age_group=ifelse(ind65==1, 65, age_group))

tb_two <- tb_two %>% mutate(year_age=paste0(as.character(year), as.character(age_group)))


tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year + year_age |imp_shock_o + imp_shock_o:ind25 + imp_shock_o:ind35 + imp_shock_o:ind45 + imp_shock_o:ind55 + exp_shock_o + exp_shock_o:ind25 + exp_shock_o:ind35 + exp_shock_o:ind45 + exp_shock_o:ind55 + imp_shock_d + imp_shock_d:ind25 + imp_shock_d:ind35 + imp_shock_d:ind45 + imp_shock_d:ind55 + exp_shock_d + exp_shock_d:ind25 + exp_shock_d:ind35 + exp_shock_d:ind45 + exp_shock_d:ind55 ~
                    imp_shock1999_iv_o + imp_shock1999_iv_o:ind25 + imp_shock1999_iv_o:ind35 + imp_shock1999_iv_o:ind45 + imp_shock1999_iv_o:ind55 + exp_shock1999_iv_o + exp_shock1999_iv_o:ind25 + exp_shock1999_iv_o:ind35 + exp_shock1999_iv_o:ind45 + exp_shock1999_iv_o:ind55 + imp_shock1999_iv_d + imp_shock1999_iv_d:ind25 + imp_shock1999_iv_d:ind35 + imp_shock1999_iv_d:ind45 + imp_shock1999_iv_d:ind55 + exp_shock1999_iv_d + exp_shock1999_iv_d:ind25 + exp_shock1999_iv_d:ind35 + exp_shock1999_iv_d:ind45 + exp_shock1999_iv_d:ind55,
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)




tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age_korea_nohouse.rds")

tb_two <- tb_two %>% mutate(age_group=ifelse(ind25==1, 25, 0)) %>% 
  mutate(age_group=ifelse(ind35==1, 35, age_group)) %>% 
  mutate(age_group=ifelse(ind45==1, 45, age_group)) %>% 
  mutate(age_group=ifelse(ind55==1, 55, age_group)) %>% 
  mutate(age_group=ifelse(ind65==1, 65, age_group))

tb_two <- tb_two %>% mutate(year_age=paste0(as.character(year), as.character(age_group)))


tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2_fef  <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year + year_age + cz_o + cz_d |imp_shock_o + imp_shock_o:ind25 + imp_shock_o:ind35 + imp_shock_o:ind45 + imp_shock_o:ind55 + exp_shock_o + exp_shock_o:ind25 + exp_shock_o:ind35 + exp_shock_o:ind45 + exp_shock_o:ind55 + imp_shock_d + imp_shock_d:ind25 + imp_shock_d:ind35 + imp_shock_d:ind45 + imp_shock_d:ind55 + exp_shock_d + exp_shock_d:ind25 + exp_shock_d:ind35 + exp_shock_d:ind45 + exp_shock_d:ind55 ~
                         imp_shock1999_iv_o + imp_shock1999_iv_o:ind25 + imp_shock1999_iv_o:ind35 + imp_shock1999_iv_o:ind45 + imp_shock1999_iv_o:ind55 + exp_shock1999_iv_o + exp_shock1999_iv_o:ind25 + exp_shock1999_iv_o:ind35 + exp_shock1999_iv_o:ind45 + exp_shock1999_iv_o:ind55 + imp_shock1999_iv_d + imp_shock1999_iv_d:ind25 + imp_shock1999_iv_d:ind35 + imp_shock1999_iv_d:ind45 + imp_shock1999_iv_d:ind55 + exp_shock1999_iv_d + exp_shock1999_iv_d:ind25 + exp_shock1999_iv_d:ind35 + exp_shock1999_iv_d:ind45 + exp_shock1999_iv_d:ind55,
                       weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


screenreg(list(out_sep1, out_sep2, out_sep2_fef), 
          stars = c(0.1, 0.05, 0.01, 0.001), 
          digits = 5, 
          custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), 
          custom.coef.map = list("fit_imp_shock_o" = "imp origin",
                                 "fit_imp_shock_o:ind25" = "imp origin:ind25",
                                 "fit_imp_shock_o:ind35" = "imp origin:ind35",
                                 "fit_imp_shock_o:ind45" = "imp origin:ind45",
                                 "fit_imp_shock_o:ind55" = "imp origin:ind55",
                                 "fit_imp_shock_d" = "imp dest",
                                 "fit_ind25:imp_shock_d" = "imp dest:ind25",
                                 "fit_ind35:imp_shock_d" = "imp dest:ind35",
                                 "fit_ind45:imp_shock_d" = "imp dest:ind45",
                                 "fit_ind55:imp_shock_d" = "imp dest:ind55",
                                 "fit_exp_shock_o" = "exp origin",
                                 "fit_ind25:exp_shock_o" = "exp origin:ind25",
                                 "fit_ind35:exp_shock_o" = "exp origin:ind35",
                                 "fit_ind45:exp_shock_o" = "exp origin:ind45",
                                 "fit_ind55:exp_shock_o" = "exp origin:ind55",
                                 "fit_exp_shock_d" = "exp dest",
                                 "fit_ind25:exp_shock_d" = "exp dest:ind25",
                                 "fit_ind35:exp_shock_d" = "exp dest:ind35",
                                 "fit_ind45:exp_shock_d" = "exp dest:ind45",
                                 "fit_ind55:exp_shock_d" = "exp dest:ind55"
          ), 
          custom.gof.rows = list("Controls" = c("yes", "yes", "yes"),
                                 "age fixed effects"= c("yes", ".", "."),
                                 "year:age fixed effects"= c(".", "yes", "yes"),
                                 "year fixed effects" = c("no", "yes", "yes"), 
                                 "region fixed effects" = c("no", "no", "yes")),
          "period" = c("1", "2", "2"))


texreg(list(out_sep1, out_sep2, out_sep2_fef), 
       stars = c(0.1, 0.05, 0.01, 0.001), 
       digits = 5, 
       booktabs = TRUE,
       include.adjrs = FALSE, 
       include.proj.stats = FALSE, 
       use.packages = FALSE, 
       table = FALSE, 
       custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), 
       custom.coef.map = list("fit_imp_shock_o" = "imp origin",
                              "fit_imp_shock_o:ind25" = "imp origin:ind25",
                              "fit_imp_shock_o:ind35" = "imp origin:ind35",
                              "fit_imp_shock_o:ind45" = "imp origin:ind45",
                              "fit_imp_shock_o:ind55" = "imp origin:ind55",
                              "fit_imp_shock_d" = "imp dest",
                              "fit_ind25:imp_shock_d" = "imp dest:ind25",
                              "fit_ind35:imp_shock_d" = "imp dest:ind35",
                              "fit_ind45:imp_shock_d" = "imp dest:ind45",
                              "fit_ind55:imp_shock_d" = "imp dest:ind55",
                              "fit_exp_shock_o" = "exp origin",
                              "fit_ind25:exp_shock_o" = "exp origin:ind25",
                              "fit_ind35:exp_shock_o" = "exp origin:ind35",
                              "fit_ind45:exp_shock_o" = "exp origin:ind45",
                              "fit_ind55:exp_shock_o" = "exp origin:ind55",
                              "fit_exp_shock_d" = "exp dest",
                              "fit_ind25:exp_shock_d" = "exp dest:ind25",
                              "fit_ind35:exp_shock_d" = "exp dest:ind35",
                              "fit_ind45:exp_shock_d" = "exp dest:ind45",
                              "fit_ind55:exp_shock_d" = "exp dest:ind55"
       ), 
       custom.gof.rows = list("Controls" = c("yes", "yes", "yes"),
                              "age fixed effects"= c("yes", ".", "."),
                              "year:age fixed effects"= c(".", "yes", "yes"),
                              "year fixed effects" = c("no", "yes", "yes"), 
                              "region fixed effects" = c("no", "no", "yes")),
       "period" = c("1", "2", "2"), 
       file = "results/tables/baseline_regression_age_sep_nohouse.tex")







# Extracting relevant coefficients from out_sep1
plot_data <- out_sep1 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate, color=color_group)) + 
  geom_point(position=position_dodge(width=0.6), size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error), position=position_dodge(width=0.6)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 2 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_wrap(~type, ncol=2, scales="free_y") +
  #labs(title="{facet_var}") +
  theme_bw() + 
  theme(legend.title = element_blank())
ggsave("results/figures/coef_plot1_nohouse.png", dpi=400, height=8, width=15, scale=0.5)


# Extracting relevant coefficients from out_sep2
plot_data <- out_sep2 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate, color=color_group)) + 
  geom_point(position=position_dodge(width=0.6), size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error), position=position_dodge(width=0.6)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 4 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_wrap(~type, ncol=2, scales="free_y") +
  #labs(title="{facet_var}") +
  theme_bw() + 
  theme(legend.title = element_blank())
ggsave("results/figures/coef_plot2_nohouse.png", dpi=400, height=8, width=15, scale=0.5)



# Extracting relevant coefficients from out_sep2_fef
plot_data <- out_sep2_fef %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate, color=color_group)) + 
  geom_point(position=position_dodge(width=0.6), size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error), position=position_dodge(width=0.6)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 5 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_wrap(~type, ncol=2, scales="free_y") +
  #labs(title="{facet_var}") +
  theme_bw() + 
  theme(legend.title = element_blank())
ggsave("results/figures/coef_plot3_nohouse.png", dpi=400, height=8, width=15, scale=0.5)




# now 2x2 figures


# Extracting relevant coefficients from out_sep1
plot_data <- out_sep1 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate)) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 2 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_grid(type ~ color_group, scales="free_y", labeller = labeller(type = label_parsed, color_group = label_parsed)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "none") # hiding legend as it's redundant now


ggsave("results/figures/coef_plot11_nohouse.png", dpi=400, height=8, width=15, scale=0.5)



# Extracting relevant coefficients from out_sep2
plot_data <- out_sep2 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate)) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 4 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_grid(type ~ color_group, scales="free_y", labeller = labeller(type = label_parsed, color_group = label_parsed)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "none") # hiding legend as it's redundant now


ggsave("results/figures/coef_plot22_nohouse.png", dpi=400, height=8, width=15, scale=0.5)



# Extracting relevant coefficients from out_sep2_fef
plot_data <- out_sep2_fef %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate)) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 5 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_grid(type ~ color_group, scales="free_y", labeller = labeller(type = label_parsed, color_group = label_parsed)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "none") # hiding legend as it's redundant now


ggsave("results/figures/coef_plot33_nohouse.png", dpi=400, height=8, width=15, scale=0.5)



# hetero 25-34, 35-44, 45-54, 55-64, 65+





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



# for now we will add up 25-34, 35-44, 45-54, 55-64

pop <- pop %>% mutate(pop25=age25_29+age30_34,
                      pop35=age35_39+age40_44,
                      pop45=age45_49+age50_54,
                      pop55=age55_59+age60_64,
                      pop65=age65_69+age70_74+age75_79+age80_84+age85_89+age90_94+age95_99+age100) %>% 
  select(iso, pop25, pop35, pop45, pop55, pop65)



pop2 <- pop2 %>%  mutate(pop25=age25_29+age30_34,
                         pop35=age35_39+age40_44,
                         pop45=age45_49+age50_54,
                         pop55=age55_59+age60_64,
                         pop65=age65_69+age70_74+age75_79+age80_84+age85_89+age90_94+age95_99+age100) %>% 
  select(iso, pop25, pop35, pop45, pop55, pop65)






# stack them


######


tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")

# one period


tb_all25 <- tb_all %>% mutate(ind25=1)
tb_all35 <- tb_all %>% mutate(ind35=1)
tb_all45 <- tb_all %>% mutate(ind45=1)
tb_all55 <- tb_all %>% mutate(ind55=1)
tb_all65 <- tb_all %>% mutate(ind65=1)



migration25 <- migration %>% 
  filter(year<2020, age_h>=25 & age_h<35, reason==1) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration35 <- migration %>% 
  filter(year<2020, age_h>=35 & age_h<45, reason==1) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration45 <- migration %>% 
  filter(year<2020, age_h>=45 & age_h<55, reason==1) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration55 <- migration %>% 
  filter(year<2020, age_h>=55 & age_h<65, reason==1) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration65 <- migration %>% 
  filter(year<2020, age_h>=65, reason==1) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()







tb_all25 <- tb_all25 %>% left_join(migration25, by=c("iso_o", "iso_d"))
tb_all35 <- tb_all35 %>% left_join(migration35, by=c("iso_o", "iso_d"))
tb_all45 <- tb_all45 %>% left_join(migration45, by=c("iso_o", "iso_d"))
tb_all55 <- tb_all55 %>% left_join(migration55, by=c("iso_o", "iso_d"))
tb_all65 <- tb_all65 %>% left_join(migration65, by=c("iso_o", "iso_d"))



tb_all25 <- tb_all25 %>% left_join(pop %>% select(iso, pop25), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop25)

tb_all25 <- tb_all25 %>% left_join(pop %>% select(iso, pop25), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop25)


tb_all35 <- tb_all35 %>% left_join(pop %>% select(iso, pop35), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop35)

tb_all35 <- tb_all35 %>% left_join(pop %>% select(iso, pop35), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop35)


tb_all45 <- tb_all45 %>% left_join(pop %>% select(iso, pop45), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop45)

tb_all45 <- tb_all45 %>% left_join(pop %>% select(iso, pop45), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop45)



tb_all55 <- tb_all55 %>% left_join(pop %>% select(iso, pop55), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop55)

tb_all55 <- tb_all55 %>% left_join(pop %>% select(iso, pop55), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop55)

tb_all65 <- tb_all65 %>% left_join(pop %>% select(iso, pop65), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop65)

tb_all65 <- tb_all65 %>% left_join(pop %>% select(iso, pop65), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop65)







tb_all <- tb_all25 %>% bind_rows(tb_all35) %>% bind_rows(tb_all45) %>% bind_rows(tb_all55) %>% bind_rows(tb_all65) 


tb_all <- tb_all %>% mutate(ind25=ifelse(is.na(ind25), 0, 1), ind35=ifelse(is.na(ind35), 0, 1), ind45=ifelse(is.na(ind45), 0, 1), ind55=ifelse(is.na(ind55), 0, 1), ind65=ifelse(is.na(ind65), 0, 1))



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age_koreajob.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")


tb_two25 <- tb_two %>% mutate(ind25=1)
tb_two35 <- tb_two %>% mutate(ind35=1)
tb_two45 <- tb_two %>% mutate(ind45=1)
tb_two55 <- tb_two %>% mutate(ind55=1)
tb_two65 <- tb_two %>% mutate(ind65=1)



migration25 <- migration %>% 
  filter(age_h>=25 & age_h<35, reason==1) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration35 <- migration %>% 
  filter(age_h>=35 & age_h<45, reason==1) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration45 <- migration %>% 
  filter(age_h>=45 & age_h<55, reason==1) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration55 <- migration %>% 
  filter(age_h>=55 & age_h<65, reason==1) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration65 <- migration %>% 
  filter(age_h>=65, reason==1) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()




tb_two25 <- tb_two25 %>% left_join(migration25, by=c("year", "iso_o", "iso_d"))
tb_two35 <- tb_two35 %>% left_join(migration35, by=c("year", "iso_o", "iso_d"))
tb_two45 <- tb_two45 %>% left_join(migration45, by=c("year", "iso_o", "iso_d"))
tb_two55 <- tb_two55 %>% left_join(migration55, by=c("year", "iso_o", "iso_d"))
tb_two65 <- tb_two65 %>% left_join(migration65, by=c("year", "iso_o", "iso_d"))



pop <- pop %>% mutate(year=1)
pop2 <- pop2 %>% mutate(year=2)

pop <- pop %>% bind_rows(pop2)






tb_two25 <- tb_two25 %>% left_join(pop %>% select(iso, pop25, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop25) %>% select(-pop25)


tb_two25 <- tb_two25 %>% left_join(pop %>% select(iso, pop25, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop25) %>% select(-pop25)


tb_two35 <- tb_two35 %>% left_join(pop %>% select(iso, pop35, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop35) %>% select(-pop35)


tb_two35 <- tb_two35 %>% left_join(pop %>% select(iso, pop35, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop35) %>% select(-pop35)




tb_two45 <- tb_two45 %>% left_join(pop %>% select(iso, pop45, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop45) %>% select(-pop45)

tb_two45 <- tb_two45 %>% left_join(pop %>% select(iso, pop45, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop45) %>% select(-pop45)


tb_two55 <- tb_two55 %>% left_join(pop %>% select(iso, pop55, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop55) %>% select(-pop55)

tb_two55 <- tb_two55 %>% left_join(pop %>% select(iso, pop55, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop55) %>% select(-pop55)


tb_two65 <- tb_two65 %>% left_join(pop %>% select(iso, pop65, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop65) %>% select(-pop65)

tb_two65 <- tb_two65 %>% left_join(pop %>% select(iso, pop65, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop65) %>% select(-pop65)




tb_two <- tb_two25 %>% bind_rows(tb_two35) %>% bind_rows(tb_two45) %>% bind_rows(tb_two55) %>% bind_rows(tb_two65)


tb_two <- tb_two %>% mutate(ind25=ifelse(is.na(ind25), 0, 1), ind35=ifelse(is.na(ind35), 0, 1), ind45=ifelse(is.na(ind45), 0, 1), ind55=ifelse(is.na(ind55), 0, 1), ind65=ifelse(is.na(ind65), 0, 1))


tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))



tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, ind25, ind35, ind45, ind55, ind65, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d", "ind25", "ind35", "ind45", "ind55", "ind65"))

tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age_koreajob.rds")







# stack regression start


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age_koreajob.rds")

tb_all <- tb_all %>% mutate(age_group=ifelse(ind25==1, 25, 0)) %>% 
  mutate(age_group=ifelse(ind35==1, 35, age_group)) %>% 
  mutate(age_group=ifelse(ind45==1, 45, age_group)) %>% 
  mutate(age_group=ifelse(ind55==1, 55, age_group)) %>% 
  mutate(age_group=ifelse(ind65==1, 65, age_group))



tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(
  log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
    age_group |
    imp_shock_o + imp_shock_o:ind25 + imp_shock_o:ind35 + imp_shock_o:ind45 + imp_shock_o:ind55 + exp_shock_o + exp_shock_o:ind25 + exp_shock_o:ind35 + exp_shock_o:ind45 + exp_shock_o:ind55 + imp_shock_d + imp_shock_d:ind25 + imp_shock_d:ind35 + imp_shock_d:ind45 + imp_shock_d:ind55 + exp_shock_d + exp_shock_d:ind25 + exp_shock_d:ind35 + exp_shock_d:ind45 + exp_shock_d:ind55 ~
    imp_shock1999_iv_o + imp_shock1999_iv_o:ind25 + imp_shock1999_iv_o:ind35 + imp_shock1999_iv_o:ind45 + imp_shock1999_iv_o:ind55 + exp_shock1999_iv_o + exp_shock1999_iv_o:ind25 + exp_shock1999_iv_o:ind35 + exp_shock1999_iv_o:ind45 + exp_shock1999_iv_o:ind55 + imp_shock1999_iv_d + imp_shock1999_iv_d:ind25 + imp_shock1999_iv_d:ind35 + imp_shock1999_iv_d:ind45 + imp_shock1999_iv_d:ind55 + exp_shock1999_iv_d + exp_shock1999_iv_d:ind25 + exp_shock1999_iv_d:ind35 + exp_shock1999_iv_d:ind45 + exp_shock1999_iv_d:ind55,
  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all
)



tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age_koreajob.rds")

tb_two <- tb_two %>% mutate(age_group=ifelse(ind25==1, 25, 0)) %>% 
  mutate(age_group=ifelse(ind35==1, 35, age_group)) %>% 
  mutate(age_group=ifelse(ind45==1, 45, age_group)) %>% 
  mutate(age_group=ifelse(ind55==1, 55, age_group)) %>% 
  mutate(age_group=ifelse(ind65==1, 65, age_group))

tb_two <- tb_two %>% mutate(year_age=paste0(as.character(year), as.character(age_group)))


tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year + year_age |imp_shock_o + imp_shock_o:ind25 + imp_shock_o:ind35 + imp_shock_o:ind45 + imp_shock_o:ind55 + exp_shock_o + exp_shock_o:ind25 + exp_shock_o:ind35 + exp_shock_o:ind45 + exp_shock_o:ind55 + imp_shock_d + imp_shock_d:ind25 + imp_shock_d:ind35 + imp_shock_d:ind45 + imp_shock_d:ind55 + exp_shock_d + exp_shock_d:ind25 + exp_shock_d:ind35 + exp_shock_d:ind45 + exp_shock_d:ind55 ~
                    imp_shock1999_iv_o + imp_shock1999_iv_o:ind25 + imp_shock1999_iv_o:ind35 + imp_shock1999_iv_o:ind45 + imp_shock1999_iv_o:ind55 + exp_shock1999_iv_o + exp_shock1999_iv_o:ind25 + exp_shock1999_iv_o:ind35 + exp_shock1999_iv_o:ind45 + exp_shock1999_iv_o:ind55 + imp_shock1999_iv_d + imp_shock1999_iv_d:ind25 + imp_shock1999_iv_d:ind35 + imp_shock1999_iv_d:ind45 + imp_shock1999_iv_d:ind55 + exp_shock1999_iv_d + exp_shock1999_iv_d:ind25 + exp_shock1999_iv_d:ind35 + exp_shock1999_iv_d:ind45 + exp_shock1999_iv_d:ind55,
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)




tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age_koreajob.rds")

tb_two <- tb_two %>% mutate(age_group=ifelse(ind25==1, 25, 0)) %>% 
  mutate(age_group=ifelse(ind35==1, 35, age_group)) %>% 
  mutate(age_group=ifelse(ind45==1, 45, age_group)) %>% 
  mutate(age_group=ifelse(ind55==1, 55, age_group)) %>% 
  mutate(age_group=ifelse(ind65==1, 65, age_group))

tb_two <- tb_two %>% mutate(year_age=paste0(as.character(year), as.character(age_group)))


tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2_fef  <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year + year_age + cz_o + cz_d |imp_shock_o + imp_shock_o:ind25 + imp_shock_o:ind35 + imp_shock_o:ind45 + imp_shock_o:ind55 + exp_shock_o + exp_shock_o:ind25 + exp_shock_o:ind35 + exp_shock_o:ind45 + exp_shock_o:ind55 + imp_shock_d + imp_shock_d:ind25 + imp_shock_d:ind35 + imp_shock_d:ind45 + imp_shock_d:ind55 + exp_shock_d + exp_shock_d:ind25 + exp_shock_d:ind35 + exp_shock_d:ind45 + exp_shock_d:ind55 ~
                         imp_shock1999_iv_o + imp_shock1999_iv_o:ind25 + imp_shock1999_iv_o:ind35 + imp_shock1999_iv_o:ind45 + imp_shock1999_iv_o:ind55 + exp_shock1999_iv_o + exp_shock1999_iv_o:ind25 + exp_shock1999_iv_o:ind35 + exp_shock1999_iv_o:ind45 + exp_shock1999_iv_o:ind55 + imp_shock1999_iv_d + imp_shock1999_iv_d:ind25 + imp_shock1999_iv_d:ind35 + imp_shock1999_iv_d:ind45 + imp_shock1999_iv_d:ind55 + exp_shock1999_iv_d + exp_shock1999_iv_d:ind25 + exp_shock1999_iv_d:ind35 + exp_shock1999_iv_d:ind45 + exp_shock1999_iv_d:ind55,
                       weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


screenreg(list(out_sep1, out_sep2, out_sep2_fef), 
          stars = c(0.1, 0.05, 0.01, 0.001), 
          digits = 5, 
          custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), 
          custom.coef.map = list("fit_imp_shock_o" = "imp origin",
                                 "fit_imp_shock_o:ind25" = "imp origin:ind25",
                                 "fit_imp_shock_o:ind35" = "imp origin:ind35",
                                 "fit_imp_shock_o:ind45" = "imp origin:ind45",
                                 "fit_imp_shock_o:ind55" = "imp origin:ind55",
                                 "fit_imp_shock_d" = "imp dest",
                                 "fit_ind25:imp_shock_d" = "imp dest:ind25",
                                 "fit_ind35:imp_shock_d" = "imp dest:ind35",
                                 "fit_ind45:imp_shock_d" = "imp dest:ind45",
                                 "fit_ind55:imp_shock_d" = "imp dest:ind55",
                                 "fit_exp_shock_o" = "exp origin",
                                 "fit_ind25:exp_shock_o" = "exp origin:ind25",
                                 "fit_ind35:exp_shock_o" = "exp origin:ind35",
                                 "fit_ind45:exp_shock_o" = "exp origin:ind45",
                                 "fit_ind55:exp_shock_o" = "exp origin:ind55",
                                 "fit_exp_shock_d" = "exp dest",
                                 "fit_ind25:exp_shock_d" = "exp dest:ind25",
                                 "fit_ind35:exp_shock_d" = "exp dest:ind35",
                                 "fit_ind45:exp_shock_d" = "exp dest:ind45",
                                 "fit_ind55:exp_shock_d" = "exp dest:ind55"
          ), 
          custom.gof.rows = list("Controls" = c("yes", "yes", "yes"),
                                 "age fixed effects"= c("yes", ".", "."),
                                 "year:age fixed effects"= c(".", "yes", "yes"),
                                 "year fixed effects" = c("no", "yes", "yes"), 
                                 "region fixed effects" = c("no", "no", "yes")),
          "period" = c("1", "2", "2"))


texreg(list(out_sep1, out_sep2, out_sep2_fef), 
       stars = c(0.1, 0.05, 0.01, 0.001), 
       digits = 5, 
       booktabs = TRUE,
       include.adjrs = FALSE, 
       include.proj.stats = FALSE, 
       use.packages = FALSE, 
       table = FALSE, 
       custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), 
       custom.coef.map = list("fit_imp_shock_o" = "imp origin",
                              "fit_imp_shock_o:ind25" = "imp origin:ind25",
                              "fit_imp_shock_o:ind35" = "imp origin:ind35",
                              "fit_imp_shock_o:ind45" = "imp origin:ind45",
                              "fit_imp_shock_o:ind55" = "imp origin:ind55",
                              "fit_imp_shock_d" = "imp dest",
                              "fit_ind25:imp_shock_d" = "imp dest:ind25",
                              "fit_ind35:imp_shock_d" = "imp dest:ind35",
                              "fit_ind45:imp_shock_d" = "imp dest:ind45",
                              "fit_ind55:imp_shock_d" = "imp dest:ind55",
                              "fit_exp_shock_o" = "exp origin",
                              "fit_ind25:exp_shock_o" = "exp origin:ind25",
                              "fit_ind35:exp_shock_o" = "exp origin:ind35",
                              "fit_ind45:exp_shock_o" = "exp origin:ind45",
                              "fit_ind55:exp_shock_o" = "exp origin:ind55",
                              "fit_exp_shock_d" = "exp dest",
                              "fit_ind25:exp_shock_d" = "exp dest:ind25",
                              "fit_ind35:exp_shock_d" = "exp dest:ind35",
                              "fit_ind45:exp_shock_d" = "exp dest:ind45",
                              "fit_ind55:exp_shock_d" = "exp dest:ind55"
       ), 
       custom.gof.rows = list("Controls" = c("yes", "yes", "yes"),
                              "age fixed effects"= c("yes", ".", "."),
                              "year:age fixed effects"= c(".", "yes", "yes"),
                              "year fixed effects" = c("no", "yes", "yes"), 
                              "region fixed effects" = c("no", "no", "yes")),
       "period" = c("1", "2", "2"), 
       file = "results/tables/baseline_regression_age_sepjob.tex")







# Extracting relevant coefficients from out_sep1
plot_data <- out_sep1 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate, color=color_group)) + 
  geom_point(position=position_dodge(width=0.6), size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error), position=position_dodge(width=0.6)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 2 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_wrap(~type, ncol=2, scales="free_y") +
  #labs(title="{facet_var}") +
  theme_bw() + 
  theme(legend.title = element_blank())
ggsave("results/figures/coef_plot1job.png", dpi=400, height=8, width=15, scale=0.5)


# Extracting relevant coefficients from out_sep2
plot_data <- out_sep2 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate, color=color_group)) + 
  geom_point(position=position_dodge(width=0.6), size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error), position=position_dodge(width=0.6)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 4 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_wrap(~type, ncol=2, scales="free_y") +
  #labs(title="{facet_var}") +
  theme_bw() + 
  theme(legend.title = element_blank())
ggsave("results/figures/coef_plot2job.png", dpi=400, height=8, width=15, scale=0.5)



# Extracting relevant coefficients from out_sep2_fef
plot_data <- out_sep2_fef %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate, color=color_group)) + 
  geom_point(position=position_dodge(width=0.6), size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error), position=position_dodge(width=0.6)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 5 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_wrap(~type, ncol=2, scales="free_y") +
  #labs(title="{facet_var}") +
  theme_bw() + 
  theme(legend.title = element_blank())
ggsave("results/figures/coef_plot3job.png", dpi=400, height=8, width=15, scale=0.5)




# now 2x2 figures


# Extracting relevant coefficients from out_sep1
plot_data <- out_sep1 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate)) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 2 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_grid(type ~ color_group, scales="free_y", labeller = labeller(type = label_parsed, color_group = label_parsed)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "none") # hiding legend as it's redundant now


ggsave("results/figures/coef_plot11job.png", dpi=400, height=8, width=15, scale=0.5)



# Extracting relevant coefficients from out_sep2
plot_data <- out_sep2 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate)) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 4 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_grid(type ~ color_group, scales="free_y", labeller = labeller(type = label_parsed, color_group = label_parsed)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "none") # hiding legend as it's redundant now


ggsave("results/figures/coef_plot22job.png", dpi=400, height=8, width=15, scale=0.5)



# Extracting relevant coefficients from out_sep2_fef
plot_data <- out_sep2_fef %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate)) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 5 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_grid(type ~ color_group, scales="free_y", labeller = labeller(type = label_parsed, color_group = label_parsed)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "none") # hiding legend as it's redundant now


ggsave("results/figures/coef_plot33job.png", dpi=400, height=8, width=15, scale=0.5)








# hetero 25-34, 35-44, 45-54, 55-64, 65+





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



# for now we will add up 25-34, 35-44, 45-54, 55-64

pop <- pop %>% mutate(pop25=age25_29+age30_34,
                      pop35=age35_39+age40_44,
                      pop45=age45_49+age50_54,
                      pop55=age55_59+age60_64,
                      pop65=age65_69+age70_74+age75_79+age80_84+age85_89+age90_94+age95_99+age100) %>% 
  select(iso, pop25, pop35, pop45, pop55, pop65)



pop2 <- pop2 %>%  mutate(pop25=age25_29+age30_34,
                         pop35=age35_39+age40_44,
                         pop45=age45_49+age50_54,
                         pop55=age55_59+age60_64,
                         pop65=age65_69+age70_74+age75_79+age80_84+age85_89+age90_94+age95_99+age100) %>% 
  select(iso, pop25, pop35, pop45, pop55, pop65)






# stack them


######


tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")

# one period


tb_all25 <- tb_all %>% mutate(ind25=1)
tb_all35 <- tb_all %>% mutate(ind35=1)
tb_all45 <- tb_all %>% mutate(ind45=1)
tb_all55 <- tb_all %>% mutate(ind55=1)
tb_all65 <- tb_all %>% mutate(ind65=1)



migration25 <- migration %>% 
  filter(year<2020, age_h>=25 & age_h<35, reason==1, household==2) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration35 <- migration %>% 
  filter(year<2020, age_h>=35 & age_h<45, reason==1, household==2) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration45 <- migration %>% 
  filter(year<2020, age_h>=45 & age_h<55, reason==1, household==2) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration55 <- migration %>% 
  filter(year<2020, age_h>=55 & age_h<65, reason==1, household==2) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration65 <- migration %>% 
  filter(year<2020, age_h>=65, reason==1, household==2) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()







tb_all25 <- tb_all25 %>% left_join(migration25, by=c("iso_o", "iso_d"))
tb_all35 <- tb_all35 %>% left_join(migration35, by=c("iso_o", "iso_d"))
tb_all45 <- tb_all45 %>% left_join(migration45, by=c("iso_o", "iso_d"))
tb_all55 <- tb_all55 %>% left_join(migration55, by=c("iso_o", "iso_d"))
tb_all65 <- tb_all65 %>% left_join(migration65, by=c("iso_o", "iso_d"))



tb_all25 <- tb_all25 %>% left_join(pop %>% select(iso, pop25), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop25)

tb_all25 <- tb_all25 %>% left_join(pop %>% select(iso, pop25), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop25)


tb_all35 <- tb_all35 %>% left_join(pop %>% select(iso, pop35), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop35)

tb_all35 <- tb_all35 %>% left_join(pop %>% select(iso, pop35), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop35)


tb_all45 <- tb_all45 %>% left_join(pop %>% select(iso, pop45), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop45)

tb_all45 <- tb_all45 %>% left_join(pop %>% select(iso, pop45), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop45)



tb_all55 <- tb_all55 %>% left_join(pop %>% select(iso, pop55), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop55)

tb_all55 <- tb_all55 %>% left_join(pop %>% select(iso, pop55), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop55)

tb_all65 <- tb_all65 %>% left_join(pop %>% select(iso, pop65), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop65)

tb_all65 <- tb_all65 %>% left_join(pop %>% select(iso, pop65), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop65)







tb_all <- tb_all25 %>% bind_rows(tb_all35) %>% bind_rows(tb_all45) %>% bind_rows(tb_all55) %>% bind_rows(tb_all65) 


tb_all <- tb_all %>% mutate(ind25=ifelse(is.na(ind25), 0, 1), ind35=ifelse(is.na(ind35), 0, 1), ind45=ifelse(is.na(ind45), 0, 1), ind55=ifelse(is.na(ind55), 0, 1), ind65=ifelse(is.na(ind65), 0, 1))



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age_korea_job1house.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")


tb_two25 <- tb_two %>% mutate(ind25=1)
tb_two35 <- tb_two %>% mutate(ind35=1)
tb_two45 <- tb_two %>% mutate(ind45=1)
tb_two55 <- tb_two %>% mutate(ind55=1)
tb_two65 <- tb_two %>% mutate(ind65=1)



migration25 <- migration %>% 
  filter(age_h>=25 & age_h<35, reason==1, household==2) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration35 <- migration %>% 
  filter(age_h>=35 & age_h<45, reason==1, household==2) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration45 <- migration %>% 
  filter(age_h>=45 & age_h<55, reason==1, household==2) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration55 <- migration %>% 
  filter(age_h>=55 & age_h<65, reason==1, household==2) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration65 <- migration %>% 
  filter(age_h>=65, reason==1, household==2) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()




tb_two25 <- tb_two25 %>% left_join(migration25, by=c("year", "iso_o", "iso_d"))
tb_two35 <- tb_two35 %>% left_join(migration35, by=c("year", "iso_o", "iso_d"))
tb_two45 <- tb_two45 %>% left_join(migration45, by=c("year", "iso_o", "iso_d"))
tb_two55 <- tb_two55 %>% left_join(migration55, by=c("year", "iso_o", "iso_d"))
tb_two65 <- tb_two65 %>% left_join(migration65, by=c("year", "iso_o", "iso_d"))



pop <- pop %>% mutate(year=1)
pop2 <- pop2 %>% mutate(year=2)

pop <- pop %>% bind_rows(pop2)






tb_two25 <- tb_two25 %>% left_join(pop %>% select(iso, pop25, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop25) %>% select(-pop25)


tb_two25 <- tb_two25 %>% left_join(pop %>% select(iso, pop25, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop25) %>% select(-pop25)


tb_two35 <- tb_two35 %>% left_join(pop %>% select(iso, pop35, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop35) %>% select(-pop35)


tb_two35 <- tb_two35 %>% left_join(pop %>% select(iso, pop35, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop35) %>% select(-pop35)




tb_two45 <- tb_two45 %>% left_join(pop %>% select(iso, pop45, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop45) %>% select(-pop45)

tb_two45 <- tb_two45 %>% left_join(pop %>% select(iso, pop45, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop45) %>% select(-pop45)


tb_two55 <- tb_two55 %>% left_join(pop %>% select(iso, pop55, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop55) %>% select(-pop55)

tb_two55 <- tb_two55 %>% left_join(pop %>% select(iso, pop55, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop55) %>% select(-pop55)


tb_two65 <- tb_two65 %>% left_join(pop %>% select(iso, pop65, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop65) %>% select(-pop65)

tb_two65 <- tb_two65 %>% left_join(pop %>% select(iso, pop65, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop65) %>% select(-pop65)




tb_two <- tb_two25 %>% bind_rows(tb_two35) %>% bind_rows(tb_two45) %>% bind_rows(tb_two55) %>% bind_rows(tb_two65)


tb_two <- tb_two %>% mutate(ind25=ifelse(is.na(ind25), 0, 1), ind35=ifelse(is.na(ind35), 0, 1), ind45=ifelse(is.na(ind45), 0, 1), ind55=ifelse(is.na(ind55), 0, 1), ind65=ifelse(is.na(ind65), 0, 1))


tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))



tb_two <- tb_two %>% select(-c("popw_o", "popw_d"))


pop <- tb_two %>% filter(year==1) %>% 
  select(iso_o, iso_d, ind25, ind35, ind45, ind55, ind65, pop_o, pop_d) %>% 
  rename(popw_o=pop_o, popw_d=pop_d)

tb_two <- tb_two %>% left_join(pop, by=c("iso_o", "iso_d", "ind25", "ind35", "ind45", "ind55", "ind65"))

tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age_korea_job1house.rds")







# stack regression start


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age_korea_job1house.rds")

tb_all <- tb_all %>% mutate(age_group=ifelse(ind25==1, 25, 0)) %>% 
  mutate(age_group=ifelse(ind35==1, 35, age_group)) %>% 
  mutate(age_group=ifelse(ind45==1, 45, age_group)) %>% 
  mutate(age_group=ifelse(ind55==1, 55, age_group)) %>% 
  mutate(age_group=ifelse(ind65==1, 65, age_group))



tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(
  log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
    age_group |
    imp_shock_o + imp_shock_o:ind25 + imp_shock_o:ind35 + imp_shock_o:ind45 + imp_shock_o:ind55 + exp_shock_o + exp_shock_o:ind25 + exp_shock_o:ind35 + exp_shock_o:ind45 + exp_shock_o:ind55 + imp_shock_d + imp_shock_d:ind25 + imp_shock_d:ind35 + imp_shock_d:ind45 + imp_shock_d:ind55 + exp_shock_d + exp_shock_d:ind25 + exp_shock_d:ind35 + exp_shock_d:ind45 + exp_shock_d:ind55 ~
    imp_shock1999_iv_o + imp_shock1999_iv_o:ind25 + imp_shock1999_iv_o:ind35 + imp_shock1999_iv_o:ind45 + imp_shock1999_iv_o:ind55 + exp_shock1999_iv_o + exp_shock1999_iv_o:ind25 + exp_shock1999_iv_o:ind35 + exp_shock1999_iv_o:ind45 + exp_shock1999_iv_o:ind55 + imp_shock1999_iv_d + imp_shock1999_iv_d:ind25 + imp_shock1999_iv_d:ind35 + imp_shock1999_iv_d:ind45 + imp_shock1999_iv_d:ind55 + exp_shock1999_iv_d + exp_shock1999_iv_d:ind25 + exp_shock1999_iv_d:ind35 + exp_shock1999_iv_d:ind45 + exp_shock1999_iv_d:ind55,
  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all
)



tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age_korea_job1house.rds")

tb_two <- tb_two %>% mutate(age_group=ifelse(ind25==1, 25, 0)) %>% 
  mutate(age_group=ifelse(ind35==1, 35, age_group)) %>% 
  mutate(age_group=ifelse(ind45==1, 45, age_group)) %>% 
  mutate(age_group=ifelse(ind55==1, 55, age_group)) %>% 
  mutate(age_group=ifelse(ind65==1, 65, age_group))

tb_two <- tb_two %>% mutate(year_age=paste0(as.character(year), as.character(age_group)))


tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year + year_age |imp_shock_o + imp_shock_o:ind25 + imp_shock_o:ind35 + imp_shock_o:ind45 + imp_shock_o:ind55 + exp_shock_o + exp_shock_o:ind25 + exp_shock_o:ind35 + exp_shock_o:ind45 + exp_shock_o:ind55 + imp_shock_d + imp_shock_d:ind25 + imp_shock_d:ind35 + imp_shock_d:ind45 + imp_shock_d:ind55 + exp_shock_d + exp_shock_d:ind25 + exp_shock_d:ind35 + exp_shock_d:ind45 + exp_shock_d:ind55 ~
                    imp_shock1999_iv_o + imp_shock1999_iv_o:ind25 + imp_shock1999_iv_o:ind35 + imp_shock1999_iv_o:ind45 + imp_shock1999_iv_o:ind55 + exp_shock1999_iv_o + exp_shock1999_iv_o:ind25 + exp_shock1999_iv_o:ind35 + exp_shock1999_iv_o:ind45 + exp_shock1999_iv_o:ind55 + imp_shock1999_iv_d + imp_shock1999_iv_d:ind25 + imp_shock1999_iv_d:ind35 + imp_shock1999_iv_d:ind45 + imp_shock1999_iv_d:ind55 + exp_shock1999_iv_d + exp_shock1999_iv_d:ind25 + exp_shock1999_iv_d:ind35 + exp_shock1999_iv_d:ind45 + exp_shock1999_iv_d:ind55,
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)




tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age_korea_job1house.rds")

tb_two <- tb_two %>% mutate(age_group=ifelse(ind25==1, 25, 0)) %>% 
  mutate(age_group=ifelse(ind35==1, 35, age_group)) %>% 
  mutate(age_group=ifelse(ind45==1, 45, age_group)) %>% 
  mutate(age_group=ifelse(ind55==1, 55, age_group)) %>% 
  mutate(age_group=ifelse(ind65==1, 65, age_group))

tb_two <- tb_two %>% mutate(year_age=paste0(as.character(year), as.character(age_group)))


tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2_fef  <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year + year_age + cz_o + cz_d |imp_shock_o + imp_shock_o:ind25 + imp_shock_o:ind35 + imp_shock_o:ind45 + imp_shock_o:ind55 + exp_shock_o + exp_shock_o:ind25 + exp_shock_o:ind35 + exp_shock_o:ind45 + exp_shock_o:ind55 + imp_shock_d + imp_shock_d:ind25 + imp_shock_d:ind35 + imp_shock_d:ind45 + imp_shock_d:ind55 + exp_shock_d + exp_shock_d:ind25 + exp_shock_d:ind35 + exp_shock_d:ind45 + exp_shock_d:ind55 ~
                         imp_shock1999_iv_o + imp_shock1999_iv_o:ind25 + imp_shock1999_iv_o:ind35 + imp_shock1999_iv_o:ind45 + imp_shock1999_iv_o:ind55 + exp_shock1999_iv_o + exp_shock1999_iv_o:ind25 + exp_shock1999_iv_o:ind35 + exp_shock1999_iv_o:ind45 + exp_shock1999_iv_o:ind55 + imp_shock1999_iv_d + imp_shock1999_iv_d:ind25 + imp_shock1999_iv_d:ind35 + imp_shock1999_iv_d:ind45 + imp_shock1999_iv_d:ind55 + exp_shock1999_iv_d + exp_shock1999_iv_d:ind25 + exp_shock1999_iv_d:ind35 + exp_shock1999_iv_d:ind45 + exp_shock1999_iv_d:ind55,
                       weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


screenreg(list(out_sep1, out_sep2, out_sep2_fef), 
          stars = c(0.1, 0.05, 0.01, 0.001), 
          digits = 5, 
          custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), 
          custom.coef.map = list("fit_imp_shock_o" = "imp origin",
                                 "fit_imp_shock_o:ind25" = "imp origin:ind25",
                                 "fit_imp_shock_o:ind35" = "imp origin:ind35",
                                 "fit_imp_shock_o:ind45" = "imp origin:ind45",
                                 "fit_imp_shock_o:ind55" = "imp origin:ind55",
                                 "fit_imp_shock_d" = "imp dest",
                                 "fit_ind25:imp_shock_d" = "imp dest:ind25",
                                 "fit_ind35:imp_shock_d" = "imp dest:ind35",
                                 "fit_ind45:imp_shock_d" = "imp dest:ind45",
                                 "fit_ind55:imp_shock_d" = "imp dest:ind55",
                                 "fit_exp_shock_o" = "exp origin",
                                 "fit_ind25:exp_shock_o" = "exp origin:ind25",
                                 "fit_ind35:exp_shock_o" = "exp origin:ind35",
                                 "fit_ind45:exp_shock_o" = "exp origin:ind45",
                                 "fit_ind55:exp_shock_o" = "exp origin:ind55",
                                 "fit_exp_shock_d" = "exp dest",
                                 "fit_ind25:exp_shock_d" = "exp dest:ind25",
                                 "fit_ind35:exp_shock_d" = "exp dest:ind35",
                                 "fit_ind45:exp_shock_d" = "exp dest:ind45",
                                 "fit_ind55:exp_shock_d" = "exp dest:ind55"
          ), 
          custom.gof.rows = list("Controls" = c("yes", "yes", "yes"),
                                 "age fixed effects"= c("yes", ".", "."),
                                 "year:age fixed effects"= c(".", "yes", "yes"),
                                 "year fixed effects" = c("no", "yes", "yes"), 
                                 "region fixed effects" = c("no", "no", "yes")),
          "period" = c("1", "2", "2"))


texreg(list(out_sep1, out_sep2, out_sep2_fef), 
       stars = c(0.1, 0.05, 0.01, 0.001), 
       digits = 5, 
       booktabs = TRUE,
       include.adjrs = FALSE, 
       include.proj.stats = FALSE, 
       use.packages = FALSE, 
       table = FALSE, 
       custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)"), 
       custom.coef.map = list("fit_imp_shock_o" = "imp origin",
                              "fit_imp_shock_o:ind25" = "imp origin:ind25",
                              "fit_imp_shock_o:ind35" = "imp origin:ind35",
                              "fit_imp_shock_o:ind45" = "imp origin:ind45",
                              "fit_imp_shock_o:ind55" = "imp origin:ind55",
                              "fit_imp_shock_d" = "imp dest",
                              "fit_ind25:imp_shock_d" = "imp dest:ind25",
                              "fit_ind35:imp_shock_d" = "imp dest:ind35",
                              "fit_ind45:imp_shock_d" = "imp dest:ind45",
                              "fit_ind55:imp_shock_d" = "imp dest:ind55",
                              "fit_exp_shock_o" = "exp origin",
                              "fit_ind25:exp_shock_o" = "exp origin:ind25",
                              "fit_ind35:exp_shock_o" = "exp origin:ind35",
                              "fit_ind45:exp_shock_o" = "exp origin:ind45",
                              "fit_ind55:exp_shock_o" = "exp origin:ind55",
                              "fit_exp_shock_d" = "exp dest",
                              "fit_ind25:exp_shock_d" = "exp dest:ind25",
                              "fit_ind35:exp_shock_d" = "exp dest:ind35",
                              "fit_ind45:exp_shock_d" = "exp dest:ind45",
                              "fit_ind55:exp_shock_d" = "exp dest:ind55"
       ), 
       custom.gof.rows = list("Controls" = c("yes", "yes", "yes"),
                              "age fixed effects"= c("yes", ".", "."),
                              "year:age fixed effects"= c(".", "yes", "yes"),
                              "year fixed effects" = c("no", "yes", "yes"), 
                              "region fixed effects" = c("no", "no", "yes")),
       "period" = c("1", "2", "2"), 
       file = "results/tables/baseline_regression_age_sep_job1house.tex")







# Extracting relevant coefficients from out_sep1
plot_data <- out_sep1 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate, color=color_group)) + 
  geom_point(position=position_dodge(width=0.6), size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error), position=position_dodge(width=0.6)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 2 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_wrap(~type, ncol=2, scales="free_y") +
  #labs(title="{facet_var}") +
  theme_bw() + 
  theme(legend.title = element_blank())
ggsave("results/figures/coef_plot1_job1house.png", dpi=400, height=8, width=15, scale=0.5)


# Extracting relevant coefficients from out_sep2
plot_data <- out_sep2 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate, color=color_group)) + 
  geom_point(position=position_dodge(width=0.6), size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error), position=position_dodge(width=0.6)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 4 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_wrap(~type, ncol=2, scales="free_y") +
  #labs(title="{facet_var}") +
  theme_bw() + 
  theme(legend.title = element_blank())
ggsave("results/figures/coef_plot2_job1house.png", dpi=400, height=8, width=15, scale=0.5)



# Extracting relevant coefficients from out_sep2_fef
plot_data <- out_sep2_fef %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate, color=color_group)) + 
  geom_point(position=position_dodge(width=0.6), size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error), position=position_dodge(width=0.6)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 5 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_wrap(~type, ncol=2, scales="free_y") +
  #labs(title="{facet_var}") +
  theme_bw() + 
  theme(legend.title = element_blank())
ggsave("results/figures/coef_plot3_job1house.png", dpi=400, height=8, width=15, scale=0.5)




# now 2x2 figures


# Extracting relevant coefficients from out_sep1
plot_data <- out_sep1 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate)) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 2 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_grid(type ~ color_group, scales="free_y", labeller = labeller(type = label_parsed, color_group = label_parsed)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "none") # hiding legend as it's redundant now


ggsave("results/figures/coef_plot11_job1house.png", dpi=400, height=8, width=15, scale=0.5)



# Extracting relevant coefficients from out_sep2
plot_data <- out_sep2 %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate)) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 4 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_grid(type ~ color_group, scales="free_y", labeller = labeller(type = label_parsed, color_group = label_parsed)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "none") # hiding legend as it's redundant now


ggsave("results/figures/coef_plot22_job1house.png", dpi=400, height=8, width=15, scale=0.5)



# Extracting relevant coefficients from out_sep2_fef
plot_data <- out_sep2_fef %>%
  broom::tidy() %>%
  filter(str_detect(term, "imp_shock_") | str_detect(term, "exp_shock_")) %>%
  mutate(age_group = case_when(
    str_detect(term, "ind25") ~ "25-34",
    str_detect(term, "ind35") ~ "35-44",
    str_detect(term, "ind45") ~ "45-54",
    str_detect(term, "ind55") ~ "55-64",
    TRUE ~ "base(65+)"
  ),
  color_group = case_when(
    str_detect(term, "_o") ~ "Origin",
    str_detect(term, "_d") ~ "Destination",
    TRUE ~ NA_character_
  ),
  type = case_when(
    str_detect(term, "imp_shock_") ~ "Import",
    str_detect(term, "exp_shock_") ~ "Export",
    TRUE ~ NA_character_
  ))

# Getting the base values for each combination of Import/Export and Origin/Destination
import_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Origin") %>% pull(estimate)
import_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Import" & color_group == "Destination") %>% pull(estimate)
export_origin_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Origin") %>% pull(estimate)
export_dest_base <- plot_data %>% filter(age_group == "base(65+)" & type == "Export" & color_group == "Destination") %>% pull(estimate)

# Adjusting the estimates
plot_data <- plot_data %>%
  mutate(adjusted_estimate = case_when(
    type == "Import" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + import_origin_base,
    type == "Import" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + import_dest_base,
    type == "Export" & color_group == "Origin" & age_group != "base(65+)" ~ estimate + export_origin_base,
    type == "Export" & color_group == "Destination" & age_group != "base(65+)" ~ estimate + export_dest_base,
    TRUE ~ estimate
  ))

# Plotting the coefficients with the adjusted values
# Plotting the coefficients with the adjusted values
plot_data %>%
  ggplot(aes(x=age_group, y=adjusted_estimate)) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=adjusted_estimate-1.96*std.error, ymax=adjusted_estimate+1.96*std.error)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  labs(x="Age Group (col 5 of table 1)", y="Adjusted Coefficient") + 
  scale_color_manual(values=c("Origin"="blue", "Destination"="red")) +
  facet_grid(type ~ color_group, scales="free_y", labeller = labeller(type = label_parsed, color_group = label_parsed)) +
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = "none") # hiding legend as it's redundant now


ggsave("results/figures/coef_plot33_job1house.png", dpi=400, height=8, width=15, scale=0.5)


