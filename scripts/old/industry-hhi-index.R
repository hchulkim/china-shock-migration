library(tidyverse)


est1999 <- read_rds("data/final/hs_cpc_ksic/est1999_refine.rds")
est2000 <- read_rds("data/final/hs_cpc_ksic/est2000_refine.rds")
est2001 <- read_rds("data/final/hs_cpc_ksic/est2001_refine.rds")


com <- read_rds("data/final/cz_code/commuting_zone.rds")


est1999 <- est1999 %>% left_join(com, by="iso") %>% rename(cz=cz_id)
est2000 <- est2000 %>% left_join(com, by="iso") %>% rename(cz=cz_id)
est2001 <- est2001 %>% left_join(com, by="iso") %>% rename(cz=cz_id)



est2000 <- est2000 %>% group_by(cz, ksic10) %>% 
  summarise(emp_all = sum(emp_all, na.rm=T)) %>% ungroup()

est2000 <- est2000 %>% mutate(ksic2=str_sub(ksic10, 1, 2))
est2000 <- est2000 %>% mutate(ksic2=as.numeric(ksic2))

# manufacturing code: 10~34


est2000 <- est2000 %>% mutate(manu_all=ifelse(ksic2>=10 & ksic2<=34, emp_all, 0))


est2000 <- est2000 %>% rename(emp=emp_all, manu=manu_all)


base <- est2000 %>% group_by(cz) %>% 
  summarise(emp_all=sum(emp, na.rm=T), manu_all=sum(manu, na.rm=T))


base <- base %>% mutate(manu_sh = manu_all / emp_all)


hhi <- est2000 %>% select(cz, ksic10, ksic2, manu)

hhi <- hhi %>% filter(ksic2>=10 & ksic2<=34)


hhi <- hhi %>% group_by(cz) %>% 
  mutate(manu_all=sum(manu, na.rm=T))

hhi <- hhi %>% mutate(hhi_sh = manu / manu_all)

hhi <- hhi %>% mutate(hhi_sh = hhi_sh^2)

hhi <- hhi %>% group_by(cz) %>% 
  summarise(hhi_index = sum(hhi_sh, na.rm=T))



hhi <- hhi %>% left_join(base, by="cz")

hhi <- hhi %>% arrange(manu_sh)

hhi <- hhi %>% filter(cz!=34)

com <- com %>% distinct(cz_id, cz_label)

hhi <- hhi %>% left_join(com, by=c("cz"="cz_id"))




hhi %>% ggplot(aes(x = factor(cz_label, levels = cz_label), y = hhi_index)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text=element_text(family="AppleGothic")) +
  labs(x = "CZ (ordered by manufacutring share)", y = "HHI index", title = "Plot of HHI by manufacturing share") + theme_bw()

ggsave("results/figures/hhi_index.png", width=15, dpi=400)



if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(sf, fixest, texreg, tidyverse)

# read in the main regression data


tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

tb_all <- tb_all %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_all <- tb_all %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)

out_tog1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_all <- tb_all %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_all <- tb_all %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)

out_sep1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)

tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))

tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)


out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression.tex")









### for age 25-64



tb_all <- read_rds("data/final/shock/shock_main_data_sigungu_one_period_age25_64.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

tb_all <- tb_all %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_all <- tb_all %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)

out_tog1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all %>% 
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

out_sep1 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_64.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)


tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two %>% 
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
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period_age25_64.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_o"="cz")) %>% 
  rename(hhi_index_o = hhi_index)
tb_two <- tb_two %>% left_join(hhi %>% select(cz, hhi_index), by=c("cz_d"="cz")) %>% 
  rename(hhi_index_d = hhi_index)


out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$pop_o, cluster=tb_two$iso_o, data=tb_two)






screenreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin"), "period"=c("1", "1", "2", "2", "2")))

texreg(list(out_tog1, out_sep1, out_tog2, out_sep2, out_sep2_fef), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)", "log(migrate)"), custom.coef.map = list("fit_shock_d"="exp_d-imp_d", "fit_shock_o"="exp_o-imp_o", "fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o"), custom.gof.rows=list("Controls" =c("yes", "yes", "yes", "yes", "yes"), "year fixed effects"=c("no", "no", "yes", "yes", "yes"), "region fixed effects"=c("no", "no", "no", "no", "dest and origin")), "period"=c("1", "1", "2", "2", "2"), file="results/tables/baseline_regression_25_64.tex")






  
  