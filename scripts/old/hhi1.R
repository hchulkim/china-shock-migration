tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_64.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

tb_all <- tb_all %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_all <- tb_all %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)

out_tog1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_o:hhi_index_o + shock_d  ~ shock_o_iv + shock_o_iv:hhi_index_o + shock_d_iv, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_64.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_all <- tb_all %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_all <- tb_all %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)

out_sep1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + imp_shock_o:hhi_index_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + imp_shock1999_iv_o:hhi_index_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_64.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)

tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_o:hhi_index_o + shock_d  ~ shock_o_iv + shock_o_iv:hhi_index_o + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_64.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + imp_shock_o:hhi_index_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + imp_shock1999_iv_o:hhi_index_o  + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_64.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)


out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + imp_shock_o:hhi_index_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + imp_shock1999_iv_o:hhi_index_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_d:hhi_index_d"="exp_d-imp_d:hhi", "fit_shock_o"="exp_o-imp_o", "fit_shock_o:hhi_index_o"="exp_o-imp_o:hhi", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_exp_shock_d:hhi_index_d"="fit_exp_shock_d:hhi", "fit_imp_shock_o"="fit_imp_shock_o", "fit_imp_shock_o:hhi_index_o"="fit_imp_shock_o:hhi", "fit_exp_shock_o"="fit_exp_shock_o", "fit_exp_shock_o:hhi_index_o"="fit_exp_shock_o:hhi"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_d:hhi_index_d"="exp_d-imp_d:hhi", "fit_shock_o"="exp_o-imp_o", "fit_shock_o:hhi_index_o"="exp_o-imp_o:hhi", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_exp_shock_d:hhi_index_d"="fit_exp_shock_d:hhi", "fit_imp_shock_o"="fit_imp_shock_o", "fit_imp_shock_o:hhi_index_o"="fit_imp_shock_o:hhi", "fit_exp_shock_o"="fit_exp_shock_o", "fit_hhi_index_o:exp_shock_o"="fit_exp_shock_o:hhi"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_hhi_onlyimporto_raw2.tex")
