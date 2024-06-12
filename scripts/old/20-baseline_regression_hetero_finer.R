if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(sf, fixest, texreg, tidyverse)



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

pop <- pop %>% mutate(pop20=age20_24+age25_29,
                      pop30=age30_34+age35_39,
                      pop40=age40_44+age45_49,
                      pop50=age50_54+age55_59,
                      pop60=age60_64+age65_69+age70_74+age75_79+age80_84+age85_89+age90_94+age95_99+age100) %>% 
  select(iso, pop20, pop30, pop40, pop50, pop60)
  


pop2 <- pop2 %>% mutate(pop20=age20_24+age25_29,
                        pop30=age30_34+age35_39,
                        pop40=age40_44+age45_49,
                        pop50=age50_54+age55_59,
                        pop60=age60_64+age65_69+age70_74+age75_79+age80_84+age85_89+age90_94+age95_99+age100) %>% 
  select(iso, pop20, pop30, pop40, pop50, pop60)





# baseline column formats : for both one period and two period

# hetero by age : 2030, 4050, 60+

# japan IV, with control, cluster-sigungu

# 1st column: Together-both 1 year

# 2nd column: separate-Both 1 year

# 1st column: Together-both 2 year

# 2nd column: separate-Both 2 year

# 3rd column: Sep-both cz origin and dest fef 2 year



# stack them


######


tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")

# one period


tb_all20 <- tb_all %>% mutate(ind20=1)
tb_all30 <- tb_all %>% mutate(ind30=1)
tb_all40 <- tb_all %>% mutate(ind40=1)
tb_all50 <- tb_all %>% mutate(ind50=1)
tb_all60 <- tb_all %>% mutate(ind60=1)



migration20 <- migration %>% 
  filter(year<2020, age_h>=20, age_h<30) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration30 <- migration %>% 
  filter(year<2020, age_h>=30, age_h<40) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration40 <- migration %>% 
  filter(year<2020, age_h>=40, age_h<50) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration50 <- migration %>% 
  filter(year<2020, age_h>=50, age_h<60) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()


migration60 <- migration %>% 
  filter(year<2020, age_h>=60) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()





tb_all20 <- tb_all20 %>% left_join(migration20, by=c("iso_o", "iso_d"))
tb_all30 <- tb_all30 %>% left_join(migration30, by=c("iso_o", "iso_d"))
tb_all40 <- tb_all40 %>% left_join(migration40, by=c("iso_o", "iso_d"))
tb_all50 <- tb_all50 %>% left_join(migration50, by=c("iso_o", "iso_d"))
tb_all60 <- tb_all60 %>% left_join(migration60, by=c("iso_o", "iso_d"))



tb_all20 <- tb_all20 %>% left_join(pop %>% select(iso, pop20), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop20)

tb_all20 <- tb_all20 %>% left_join(pop %>% select(iso, pop20), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop20)


tb_all30 <- tb_all30 %>% left_join(pop %>% select(iso, pop30), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop30)

tb_all30 <- tb_all30 %>% left_join(pop %>% select(iso, pop30), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop30)


tb_all40 <- tb_all40 %>% left_join(pop %>% select(iso, pop40), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop40)

tb_all40 <- tb_all40 %>% left_join(pop %>% select(iso, pop40), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop40)



tb_all50 <- tb_all50 %>% left_join(pop %>% select(iso, pop50), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop50)

tb_all50 <- tb_all50 %>% left_join(pop %>% select(iso, pop50), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop50)


tb_all60 <- tb_all60 %>% left_join(pop %>% select(iso, pop60), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop60)

tb_all60 <- tb_all60 %>% left_join(pop %>% select(iso, pop60), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop60)




tb_all <- tb_all20 %>% bind_rows(tb_all30) %>% bind_rows(tb_all40) %>% bind_rows(tb_all50) %>% bind_rows(tb_all60)


tb_all <- tb_all %>% mutate(ind20=ifelse(is.na(ind20), 0, 1), ind30=ifelse(is.na(ind30), 0, 1), ind40=ifelse(is.na(ind40), 0, 1), ind50=ifelse(is.na(ind50), 0, 1), ind60=ifelse(is.na(ind60), 0, 1))



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_age10.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")


tb_two20 <- tb_two %>% mutate(ind20=1)
tb_two30 <- tb_two %>% mutate(ind30=1)
tb_two40 <- tb_two %>% mutate(ind40=1)
tb_two50 <- tb_two %>% mutate(ind50=1)
tb_two60 <- tb_two %>% mutate(ind60=1)



migration20 <- migration %>% 
  filter(age_h>=20, age_h<30) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration30 <- migration %>% 
  filter(age_h>=30, age_h<40) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration40 <- migration %>% 
  filter(age_h>=40, age_h<50) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration50 <- migration %>% 
  filter(age_h>=50, age_h<60) %>% 
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


tb_two20 <- tb_two20 %>% left_join(migration20, by=c("year", "iso_o", "iso_d"))
tb_two30 <- tb_two30 %>% left_join(migration30, by=c("year", "iso_o", "iso_d"))
tb_two40 <- tb_two40 %>% left_join(migration40, by=c("year", "iso_o", "iso_d"))
tb_two50 <- tb_two50 %>% left_join(migration50, by=c("year", "iso_o", "iso_d"))
tb_two60 <- tb_two60 %>% left_join(migration60, by=c("year", "iso_o", "iso_d"))


pop <- pop %>% mutate(year=1)
pop2 <- pop2 %>% mutate(year=2)

pop <- pop %>% bind_rows(pop2)






tb_two20 <- tb_two20 %>% left_join(pop %>% select(iso, pop20, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop20) %>% select(-pop20)


tb_two20 <- tb_two20 %>% left_join(pop %>% select(iso, pop20, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop20) %>% select(-pop20)


tb_two30 <- tb_two30 %>% left_join(pop %>% select(iso, pop30, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop30) %>% select(-pop30)


tb_two30 <- tb_two30 %>% left_join(pop %>% select(iso, pop30, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop30) %>% select(-pop30)




tb_two40 <- tb_two40 %>% left_join(pop %>% select(iso, pop40, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop40) %>% select(-pop40)

tb_two40 <- tb_two40 %>% left_join(pop %>% select(iso, pop40, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop40) %>% select(-pop40)

tb_two50 <- tb_two50 %>% left_join(pop %>% select(iso, pop50, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop50) %>% select(-pop50)

tb_two50 <- tb_two50 %>% left_join(pop %>% select(iso, pop50, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop50) %>% select(-pop50)



tb_two60 <- tb_two60 %>% left_join(pop %>% select(iso, pop60, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop60) %>% select(-pop60)

tb_two60 <- tb_two60 %>% left_join(pop %>% select(iso, pop60, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop60) %>% select(-pop60)




tb_two <- tb_two20 %>% bind_rows(tb_two30) %>% bind_rows(tb_two40) %>% bind_rows(tb_two50) %>% bind_rows(tb_two60)


tb_two <- tb_two %>% mutate(ind20=ifelse(is.na(ind20), 0, 1), ind30=ifelse(is.na(ind30), 0, 1), ind40=ifelse(is.na(ind40), 0, 1), ind50=ifelse(is.na(ind50), 0, 1), ind60=ifelse(is.na(ind60), 0, 1))


tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_age10.rds")







# stack regression start


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age10.rds")

tb_all <- tb_all %>% mutate(age_group=ifelse(ind20==1, 20, 0)) %>% 
  mutate(age_group=ifelse(ind30==1, 30, age_group)) %>% 
  mutate(age_group=ifelse(ind40==1, 40, age_group)) %>% 
  mutate(age_group=ifelse(ind50==1, 50, age_group)) %>% 
  mutate(age_group=ifelse(ind60==1, 60, age_group))



tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | age_group |
                    shock_o + shock_o:ind30 + shock_o:ind40 + shock_o:ind50 + shock_o:ind60 + shock_d + shock_d:ind30 + shock_d:ind40 + shock_d:ind50 + shock_d:ind60  ~ shock_o_iv + shock_o_iv:ind30 + shock_o_iv:ind40 + shock_o_iv:ind50 + shock_o_iv:ind60 + shock_d_iv + shock_d_iv:ind30 + shock_d_iv:ind40 + shock_d_iv:ind50 + shock_d_iv:ind60, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age10.rds")

tb_two <- tb_two %>% mutate(age_group=ifelse(ind20==1, 20, 0)) %>% 
  mutate(age_group=ifelse(ind30==1, 30, age_group)) %>% 
  mutate(age_group=ifelse(ind40==1, 40, age_group)) %>% 
  mutate(age_group=ifelse(ind50==1, 50, age_group)) %>% 
  mutate(age_group=ifelse(ind60==1, 60, age_group))

tb_two <- tb_two %>% mutate(year_age=paste0(as.character(year), as.character(age_group)))


tb_two <- tb_two %>% filter(cz_o!=cz_d)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year + year_age | 
                    shock_o + shock_o:ind30 + shock_o:ind40 + shock_o:ind50 + shock_o:ind60 + shock_d + shock_d:ind30 + shock_d:ind40 + shock_d:ind50 + shock_d:ind60  ~ shock_o_iv + shock_o_iv:ind30 + shock_o_iv:ind40 + shock_o_iv:ind50 + shock_o_iv:ind60 + shock_d_iv + shock_d_iv:ind30 + shock_d_iv:ind40 + shock_d_iv:ind50 + shock_d_iv:ind60, 
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
                              "fit_shock_o:ind30" = "exp-imp origin:ind30",
                              "fit_shock_o:ind40" = "exp-imp origin:ind40",
                              "fit_shock_o:ind50" = "exp-imp origin:ind50",
                              "fit_shock_o:ind60" = "exp-imp origin:ind60",
                              "fit_shock_d" = "exp-imp dest",
                              "fit_ind30:shock_d" = "exp-imp dest:ind30",
                              "fit_ind40:shock_d" = "exp-imp dest:ind40",
                              "fit_ind50:shock_d" = "exp-imp dest:ind50",
                              "fit_ind60:shock_d" = "exp-imp dest:ind60"
                              ), 
       custom.gof.rows = list("Controls" = c("yes", "yes"),
                              "age fixed effects"= c("yes", "."),
                              "year:age fixed effects"= c(".", "yes"),
                              "year fixed effects" = c("no", "yes"), 
                              "region fixed effects" = c("no", "no")),
       "period" = c("1", "2"), 
       file = "results/tables/baseline_regression_age_all10.tex")





# stack regression start


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age10.rds")

tb_all <- tb_all %>% mutate(age_group=ifelse(ind20==1, 20, 0)) %>% 
  mutate(age_group=ifelse(ind30==1, 30, age_group)) %>% 
  mutate(age_group=ifelse(ind40==1, 40, age_group)) %>% 
  mutate(age_group=ifelse(ind50==1, 50, age_group)) %>% 
  mutate(age_group=ifelse(ind60==1, 60, age_group))

tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))
tb_all <- tb_all %>% mutate(migrate=migrate+1)
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | age_group |
                    shock_o + shock_o:ind30 + shock_o:ind40 + shock_o:ind50 + shock_o:ind60 + shock_d + shock_d:ind30 + shock_d:ind40 + shock_d:ind50 + shock_d:ind60  ~ shock_o_iv + shock_o_iv:ind30 + shock_o_iv:ind40 + shock_o_iv:ind50 + shock_o_iv:ind60 + shock_d_iv + shock_d_iv:ind30 + shock_d_iv:ind40 + shock_d_iv:ind50 + shock_d_iv:ind60, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age10.rds")


tb_two <- tb_two %>% mutate(age_group=ifelse(ind20==1, 20, 0)) %>% 
  mutate(age_group=ifelse(ind30==1, 30, age_group)) %>% 
  mutate(age_group=ifelse(ind40==1, 40, age_group)) %>% 
  mutate(age_group=ifelse(ind50==1, 50, age_group)) %>% 
  mutate(age_group=ifelse(ind60==1, 60, age_group))

tb_two <- tb_two %>% mutate(year_age=paste0(as.character(year), as.character(age_group)))


tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))
tb_two <- tb_two %>% mutate(migrate=migrate+1)
tb_two <- tb_two %>% filter(cz_o!=cz_d)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year + year_age | 
                    shock_o + shock_o:ind30 + shock_o:ind40 + shock_o:ind50 + shock_o:ind60 + shock_d + shock_d:ind30 + shock_d:ind40 + shock_d:ind50 + shock_d:ind60  ~ shock_o_iv + shock_o_iv:ind30 + shock_o_iv:ind40 + shock_o_iv:ind50 + shock_o_iv:ind60 + shock_d_iv + shock_d_iv:ind30 + shock_d_iv:ind40 + shock_d_iv:ind50 + shock_d_iv:ind60, 
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
                              "fit_shock_o:ind30" = "exp-imp origin:ind30",
                              "fit_shock_o:ind40" = "exp-imp origin:ind40",
                              "fit_shock_o:ind50" = "exp-imp origin:ind50",
                              "fit_shock_o:ind60" = "exp-imp origin:ind60",
                              "fit_shock_d" = "exp-imp dest",
                              "fit_ind30:shock_d" = "exp-imp dest:ind30",
                              "fit_ind40:shock_d" = "exp-imp dest:ind40",
                              "fit_ind50:shock_d" = "exp-imp dest:ind50",
                              "fit_ind60:shock_d" = "exp-imp dest:ind60"
       ), 
       custom.gof.rows = list("Controls" = c("yes", "yes"),
                              "age fixed effects"= c("yes", "."),
                              "year:age fixed effects"= c(".", "yes"),
                              "year fixed effects" = c("no", "yes"), 
                              "region fixed effects" = c("no", "no")),
       "period" = c("1", "2"), 
       file = "results/tables/baseline_regression_age_all10_logadd.tex")





#### hetero my number in house hold

# 1, 2, more than two

pop <- read_rds("data/final/pop_age.xlsx")


pop2 <- read_rds("data/final/pop_age2.xlsx")


# stack them

pop <- pop %>%
  mutate(pop2001 = rowSums(select(., starts_with("age")), na.rm = TRUE))

pop2 <- pop2 %>%
  mutate(pop2011 = rowSums(select(., starts_with("age")), na.rm = TRUE))


######


tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu_nomig.rds")

# one period


tb_all1 <- tb_all %>% mutate(ind1=1)
tb_all2 <- tb_all %>% mutate(ind2=1)
tb_all3 <- tb_all %>% mutate(ind3=1)




migration1 <- migration %>% 
  filter(year<2020, migrate==1) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration2 <- migration %>% 
  filter(year<2020, migrate==2) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration3 <- migration %>% 
  filter(year<2020, migrate>=3) %>% 
  group_by(iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()







tb_all1 <- tb_all1 %>% left_join(migration1, by=c("iso_o", "iso_d"))
tb_all2 <- tb_all2 %>% left_join(migration2, by=c("iso_o", "iso_d"))
tb_all3 <- tb_all3 %>% left_join(migration3, by=c("iso_o", "iso_d"))




tb_all1 <- tb_all1 %>% left_join(pop %>% select(iso, pop2001), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop2001)

tb_all1 <- tb_all1 %>% left_join(pop %>% select(iso, pop2001), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop2001)


tb_all2 <- tb_all2 %>% left_join(pop %>% select(iso, pop2001), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop2001)

tb_all2 <- tb_all2 %>% left_join(pop %>% select(iso, pop2001), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop2001)


tb_all3 <- tb_all3 %>% left_join(pop %>% select(iso, pop2001), by=c("iso_o"="iso")) %>% 
  rename(pop_o=pop2001)

tb_all3 <- tb_all3 %>% left_join(pop %>% select(iso, pop2001), by=c("iso_d"="iso")) %>% 
  rename(pop_d=pop2001)






tb_all <- tb_all1 %>% bind_rows(tb_all2) %>% bind_rows(tb_all3)


tb_all <- tb_all %>% mutate(ind1=ifelse(is.na(ind1), 0, 1), ind2=ifelse(is.na(ind2), 0, 1), ind3=ifelse(is.na(ind3), 0, 1))



tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_all %>% write_rds("data/final/shock/shock_main_data_sigungu_one_period_house.rds", "xz", compression=9L)




# two period

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_nomig.rds")


tb_two1 <- tb_two %>% mutate(ind1=1)
tb_two2 <- tb_two %>% mutate(ind2=1)
tb_two3 <- tb_two %>% mutate(ind3=1)



migration1 <- migration %>% 
  filter(migrate==1) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration2 <- migration %>% 
  filter(migrate==2) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()

migration3 <- migration %>% 
  filter(migrate>=3) %>% 
  mutate(year=ifelse(year<2011, 1, 2)) %>% 
  group_by(year, iso_o, iso_d) %>% 
  summarise(migrate = sum(migrate, na.rm = TRUE)) %>%
  ungroup()


tb_two1 <- tb_two1 %>% left_join(migration1, by=c("year", "iso_o", "iso_d"))
tb_two2 <- tb_two2 %>% left_join(migration2, by=c("year", "iso_o", "iso_d"))
tb_two3 <- tb_two3 %>% left_join(migration3, by=c("year", "iso_o", "iso_d"))



pop <- pop %>% mutate(year=1)
pop2 <- pop2 %>% mutate(year=2)

pop <- pop %>% bind_rows(pop2)

pop <- pop %>%
  mutate(pop = rowSums(select(., starts_with("pop")), na.rm = TRUE))


tb_two1 <- tb_two1 %>% left_join(pop %>% select(iso, pop, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop) %>% select(-pop)


tb_two1 <- tb_two1 %>% left_join(pop %>% select(iso, pop, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop) %>% select(-pop)


tb_two2 <- tb_two2 %>% left_join(pop %>% select(iso, pop, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop) %>% select(-pop)


tb_two2 <- tb_two2 %>% left_join(pop %>% select(iso, pop, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop) %>% select(-pop)



tb_two3 <- tb_two3 %>% left_join(pop %>% select(iso, pop, year), by=c("iso_o"="iso", "year")) %>% 
  mutate(pop_o=pop) %>% select(-pop)


tb_two3 <- tb_two3 %>% left_join(pop %>% select(iso, pop, year), by=c("iso_d"="iso", "year")) %>% 
  mutate(pop_d=pop) %>% select(-pop)




tb_two <- tb_two1 %>% bind_rows(tb_two2) %>% bind_rows(tb_two3)


tb_two <- tb_two %>% mutate(ind1=ifelse(is.na(ind1), 0, 1), ind2=ifelse(is.na(ind2), 0, 1), ind3=ifelse(is.na(ind3), 0, 1))


tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))

tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period_house.rds", "xz", compression=9L)







# stack regression start


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_house.rds")

tb_all <- tb_all %>% mutate(house_group=ifelse(ind1==1, 1, 0)) %>% 
  mutate(house_group=ifelse(ind2==1, 2, house_group)) %>% 
  mutate(house_group=ifelse(ind3==1, 3, house_group))



tb_all <- tb_all %>% filter(cz_o!=cz_d)


out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | house_group |
                    shock_o + shock_o:ind2 + shock_o:ind3 + shock_d + shock_d:ind2 + shock_d:ind3 ~ shock_o_iv + shock_o_iv:ind2 + shock_o_iv:ind3 + shock_d_iv + shock_d_iv:ind2 + shock_d_iv:ind3, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_house.rds")

tb_two <- tb_two %>% mutate(house_group=ifelse(ind1==1, 1, 0)) %>% 
  mutate(house_group=ifelse(ind2==1, 2, house_group)) %>% 
  mutate(house_group=ifelse(ind3==1, 3, house_group))

tb_two <- tb_two %>% mutate(year_house=paste0(as.character(year), as.character(house_group)))


tb_two <- tb_two %>% filter(cz_o!=cz_d)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year + year_house | 
                    shock_o + shock_o:ind2 + shock_o:ind3 + shock_d + shock_d:ind2 + shock_d:ind3 ~ shock_o_iv + shock_o_iv:ind2 + shock_o_iv:ind3 + shock_d_iv + shock_d_iv:ind2 + shock_d_iv:ind3, 
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
                              "fit_shock_o:ind2" = "exp-imp origin:ind2",
                              "fit_shock_o:ind3" = "exp-imp origin:ind3",
                              "fit_shock_d" = "exp-imp dest",
                              "fit_ind2:shock_d" = "exp-imp dest:ind2",
                              "fit_ind3:shock_d" = "exp-imp dest:ind3"
       ), 
       custom.gof.rows = list("Controls" = c("yes", "yes"),
                              "household fixed effects"= c("yes", "."),
                              "year:household fixed effects"= c(".", "yes"),
                              "year fixed effects" = c("no", "yes"), 
                              "region fixed effects" = c("no", "no")),
       "period" = c("1", "2"), 
       file = "results/tables/baseline_regression_house_all.tex")





# stack regression start



tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_house.rds")

tb_all <- tb_all %>% mutate(house_group=ifelse(ind1==1, 1, 0)) %>% 
  mutate(house_group=ifelse(ind2==1, 2, house_group)) %>% 
  mutate(house_group=ifelse(ind3==1, 3, house_group))

tb_all <- tb_all %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))
tb_all <- tb_all %>% mutate(migrate=migrate+1)

tb_all <- tb_all %>% filter(cz_o!=cz_d)


out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | house_group |
                    shock_o + shock_o:ind2 + shock_o:ind3 + shock_d + shock_d:ind2 + shock_d:ind3 ~ shock_o_iv + shock_o_iv:ind2 + shock_o_iv:ind3 + shock_d_iv + shock_d_iv:ind2 + shock_d_iv:ind3, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_house.rds")

tb_two <- tb_two %>% mutate(house_group=ifelse(ind1==1, 1, 0)) %>% 
  mutate(house_group=ifelse(ind2==1, 2, house_group)) %>% 
  mutate(house_group=ifelse(ind3==1, 3, house_group))

tb_two <- tb_two %>% mutate(year_house=paste0(as.character(year), as.character(house_group)))

tb_two <- tb_two %>% mutate(migrate=ifelse(is.na(migrate), 0, migrate))
tb_two <- tb_two %>% mutate(migrate=migrate+1)


tb_two <- tb_two %>% filter(cz_o!=cz_d)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year + year_house | 
                    shock_o + shock_o:ind2 + shock_o:ind3 + shock_d + shock_d:ind2 + shock_d:ind3 ~ shock_o_iv + shock_o_iv:ind2 + shock_o_iv:ind3 + shock_d_iv + shock_d_iv:ind2 + shock_d_iv:ind3, 
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
                              "fit_shock_o:ind2" = "exp-imp origin:ind2",
                              "fit_shock_o:ind3" = "exp-imp origin:ind3",
                              "fit_shock_d" = "exp-imp dest",
                              "fit_ind2:shock_d" = "exp-imp dest:ind2",
                              "fit_ind3:shock_d" = "exp-imp dest:ind3"
       ), 
       custom.gof.rows = list("Controls" = c("yes", "yes"),
                              "household fixed effects"= c("yes", "."),
                              "year:household fixed effects"= c(".", "yes"),
                              "year fixed effects" = c("no", "yes"), 
                              "region fixed effects" = c("no", "no")),
       "period" = c("1", "2"), 
       file = "results/tables/baseline_regression_house_all_logadd.tex")








