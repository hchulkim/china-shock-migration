if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(sf, fixest, texreg, tidyverse)

# tb_all <- read_rds("data/final/shock/shock_main_data2001_2019.rds")

# tb_all <- tb_all %>% filter(cz_o != 1 & cz_o !=4, cz_d != 1 & cz_d !=4)

# Regression with no controls (Japan 1999 IV)

#together with no control

tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_dest <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) | 
                    shock  ~ shock_iv, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock=(exp_shock_d - imp_shock_d)/1000, shock_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

out_org <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) | 
                   shock  ~ shock_iv, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                   mutate(shock=(exp_shock_o - imp_shock_o)/1000, shock_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000))

out_all <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) | 
                   shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                   mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                          shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

screenreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_shock"="fit_shock", "fit_shock_o"="fit_shock_o", "fit_shock_d"="fit_shock_d", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "(Intercept)"="Intercept"))

texreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_shock"="fit_shock", "fit_shock_o"="fit_shock_o", "fit_shock_d"="fit_shock_d", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "(Intercept)"="Intercept"), file="results/tables/out_all_nocontrol_sigungu.tex")


#separate with no control

tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_dest <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) | 
                    imp_shock_d + exp_shock_d   ~ imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


out_org <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) | 
                   imp_shock_o + exp_shock_o   ~ imp_shock1999_iv_o + exp_shock1999_iv_o, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


out_all <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) | 
                   imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


screenreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "(Intercept)"="Intercept"))

texreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "(Intercept)"="Intercept"), file="results/tables/out_all_nocontrol_sep_sigungu.tex")


# Regression with no controls (World 1999 IV)

#together with no control

tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_dest <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) | 
                    shock  ~ shock_iv, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock=(exp_shock_d - imp_shock_d)/1000, shock_iv=(exp_shock1999world_iv_d - imp_shock1999world_iv_d)/1000))

out_org <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) | 
                   shock  ~ shock_iv, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                   mutate(shock=(exp_shock_o - imp_shock_o)/1000, shock_iv=(exp_shock1999world_iv_o - imp_shock1999world_iv_o)/1000))

out_all <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) | 
                   shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                   mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999world_iv_o - imp_shock1999world_iv_o)/1000,
                          shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999world_iv_d - imp_shock1999world_iv_d)/1000))

screenreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_shock"="fit_shock", "fit_shock_o"="fit_shock_o", "fit_shock_d"="fit_shock_d", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "(Intercept)"="Intercept"))

texreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_shock"="fit_shock", "fit_shock_o"="fit_shock_o", "fit_shock_d"="fit_shock_d", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "(Intercept)"="Intercept"), file="results/tables/out_all_nocontrol_world_sigungu.tex")


#separate with no control

tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_dest <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) | 
                    imp_shock_d + exp_shock_d   ~ imp_shock1999world_iv_d + exp_shock1999world_iv_d, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


out_org <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) | 
                   imp_shock_o + exp_shock_o   ~ imp_shock1999world_iv_o + exp_shock1999world_iv_o, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


out_all <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) | 
                   imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999world_iv_o + exp_shock1999world_iv_o + imp_shock1999world_iv_d + exp_shock1999world_iv_d, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


screenreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "(Intercept)"="Intercept"))

texreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "(Intercept)"="Intercept"), file="results/tables/out_all_nocontrol_world_sep_sigungu.tex")






# Regression with controls (Japan 1999 IV)

#together with control

tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

out_dest <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock  ~ shock_iv, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock=(exp_shock_d - imp_shock_d)/1000, shock_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

out_org <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                   shock  ~ shock_iv, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                   mutate(shock=(exp_shock_o - imp_shock_o)/1000, shock_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000))

out_all <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                   shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                   mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                          shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

screenreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_shock"="fit_shock", "fit_shock_o"="fit_shock_o", "fit_shock_d"="fit_shock_d", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "log(distance)"="log(distance)", "emp_share_female_o"="emp_share_female_o", "emp_share_female_d"="emp_share_female_d", "emp_share_manu_o"="emp_share_manu_o", "emp_share_manu_d"="emp_share_manu_d", "edu_o"="edu_o", "edu_d"="edu_d", "(Intercept)"="Intercept"))

texreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE,  custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_shock"="fit_shock", "fit_shock_o"="fit_shock_o", "fit_shock_d"="fit_shock_d", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "log(distance)"="log(distance)", "emp_share_female_o"="emp_share_female_o", "emp_share_female_d"="emp_share_female_d", "emp_share_manu_o"="emp_share_manu_o", "emp_share_manu_d"="emp_share_manu_d", "edu_o"="edu_o", "edu_d"="edu_d", "(Intercept)"="Intercept"), file="results/tables/out_all_control_sigungu.tex")

#separate with control

tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_dest <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_d + exp_shock_d   ~ imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


out_org <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o)  + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d| 
                   imp_shock_o + exp_shock_o   ~ imp_shock1999_iv_o + exp_shock1999_iv_o, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


out_all <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                   imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


screenreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "log(distance)"="log(distance)", "emp_share_female_o"="emp_share_female_o", "emp_share_female_d"="emp_share_female_d", "emp_share_manu_o"="emp_share_manu_o", "emp_share_manu_d"="emp_share_manu_d", "edu_o"="edu_o", "edu_d"="edu_d", "(Intercept)"="Intercept"))

texreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "log(distance)"="log(distance)", "emp_share_female_o"="emp_share_female_o", "emp_share_female_d"="emp_share_female_d", "emp_share_manu_o"="emp_share_manu_o", "emp_share_manu_d"="emp_share_manu_d", "edu_o"="edu_o", "edu_d"="edu_d", "(Intercept)"="Intercept"), file="results/tables/out_all_control_sep_sigungu.tex")






# Regression with controls (World 1999 IV)

#together with control

tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)


out_dest <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    shock  ~ shock_iv, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock=(exp_shock_d - imp_shock_d)/1000, shock_iv=(exp_shock1999world_iv_d - imp_shock1999world_iv_d)/1000))

out_org <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                   shock  ~ shock_iv, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                   mutate(shock=(exp_shock_o - imp_shock_o)/1000, shock_iv=(exp_shock1999world_iv_o - imp_shock1999world_iv_o)/1000))

out_all <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                   shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                   mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999world_iv_o - imp_shock1999world_iv_o)/1000,
                          shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999world_iv_d - imp_shock1999world_iv_d)/1000))

screenreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_shock"="fit_shock", "fit_shock_o"="fit_shock_o", "fit_shock_d"="fit_shock_d", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "log(distance)"="log(distance)", "emp_share_female_o"="emp_share_female_o", "emp_share_female_d"="emp_share_female_d", "emp_share_manu_o"="emp_share_manu_o", "emp_share_manu_d"="emp_share_manu_d", "edu_o"="edu_o", "edu_d"="edu_d", "(Intercept)"="Intercept"))

texreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_shock"="fit_shock", "fit_shock_o"="fit_shock_o", "fit_shock_d"="fit_shock_d", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "log(distance)"="log(distance)", "emp_share_female_o"="emp_share_female_o", "emp_share_female_d"="emp_share_female_d", "emp_share_manu_o"="emp_share_manu_o", "emp_share_manu_d"="emp_share_manu_d", "edu_o"="edu_o", "edu_d"="edu_d", "(Intercept)"="Intercept"), file="results/tables/out_all_control_world_sigungu.tex")

#separate with control

tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_dest <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_d + exp_shock_d   ~ imp_shock1999world_iv_d + exp_shock1999world_iv_d, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


out_org <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o)  + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d| 
                   imp_shock_o + exp_shock_o   ~ imp_shock1999world_iv_o + exp_shock1999world_iv_o, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


out_all <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                   imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999world_iv_o + exp_shock1999world_iv_o + imp_shock1999world_iv_d + exp_shock1999world_iv_d, 
                 weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


screenreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), include.adjrs = FALSE, include.proj.stats=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "log(distance)"="log(distance)", "emp_share_female_o"="emp_share_female_o", "emp_share_female_d"="emp_share_female_d", "emp_share_manu_o"="emp_share_manu_o", "emp_share_manu_d"="emp_share_manu_d", "edu_o"="edu_o", "edu_d"="edu_d", "(Intercept)"="Intercept"))

texreg(list(out_dest, out_org, out_all), stars=c(0.1, 0.05, 0.01, 0.001), digits=5, booktabs = TRUE,
       include.adjrs = FALSE, include.proj.stats=FALSE, use.packages = FALSE, table=FALSE, custom.model.names = c("dest", "origin", "both"), custom.coef.map = list("fit_imp_shock_d"="fit_imp_shock_d", "fit_exp_shock_d"="fit_exp_shock_d", "fit_imp_shock_o"="fit_imp_shock_o", "fit_exp_shock_o"="fit_exp_shock_o", "log(pop2001_o)"="log(pop2001_o)", "log(pop2001_d)"="log(pop2001_d)", "log(distance)"="log(distance)", "emp_share_female_o"="emp_share_female_o", "emp_share_female_d"="emp_share_female_d", "emp_share_manu_o"="emp_share_manu_o", "emp_share_manu_d"="emp_share_manu_d", "edu_o"="edu_o", "edu_d"="edu_d", "(Intercept)"="Intercept"), file="results/tables/out_all_control_world_sep_sigungu.tex")


