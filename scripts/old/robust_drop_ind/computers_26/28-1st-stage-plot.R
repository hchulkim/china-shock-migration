
if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(sf, fixest, texreg, tidyverse)
# 
# 
# tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
# 
# tb_all <- tb_all %>% distinct(iso_o, .keep_all = T)
# 
# 
# # Transform the columns first
# tb_all$imp_shock_o_transformed <- tb_all$imp_shock_o / 1000
# tb_all$imp_shock_iv_o_transformed <- tb_all$imp_shock_iv_o / 1000
# 
# 
# tb_all$exp_shock_o_transformed <- tb_all$exp_shock_o / 1000
# tb_all$exp_shock_iv_o_transformed <- tb_all$exp_shock_iv_o / 1000
# 
# # Now run the regression
# model <- feols(imp_shock_o_transformed ~ imp_shock_iv_o_transformed, data = tb_all, weights = tb_all$pop2001_o)
# 
# model2 <- feols(exp_shock_o_transformed ~ exp_shock_iv_o_transformed, data = tb_all, weights = tb_all$pop2001_o)
# 
# # Extract coefficient, standard error, and t-statistic
# coef_val <- coef(model)[2]  # Coefficient of imp_shock_iv_b
# se_val <- 0.139015  # Standard error of imp_shock_iv_b
# t_stat <- 45.80403
# 
# 
# # 2. Create the added-variable plot
# # Note: Creating an exact "avplot" equivalent in ggplot2 would require computing residuals and fitted values.
# 
# fitted_vals <- predict(model)
# 
# 
# ggplot(tb_all, aes(x=fitted_vals, y=imp_shock_o_transformed)) +
#   geom_point(shape=1) +  # circle points
#   geom_smooth(method="lm", aes(weight=pop2001_o), se=FALSE, color="gray") +
#   labs(title="First Stage Regression (Import), 2001-2019",
#        x="Change in Predicted Import Exposure per Worker (in kUSD)",
#        y="Change in Import Exposure per Worker (in kUSD)") +
#   scale_y_continuous(breaks=seq(-20, 20, by=5)) +
#   geom_label(aes(x=min(fitted_vals), y=max(imp_shock_o_transformed)),
#              label=sprintf("Coef: %.3f\nStd. Error: %.3f\nT-stat: %.2f", coef_val, se_val, t_stat), 
#              hjust=0, vjust=1, size=4, fill="lightgray", color="black", family="sans") +  # Adjust position and size as needed
#   theme_bw()
# 
# 
# ggsave("results/figures/1st-stage-import-2001_2019.png", dpi=400, height=8, width=15, scale=0.5)
# 
# 
# fitted_vals <- predict(model2)
# 
# # Extract coefficient, standard error, and t-statistic
# coef_val <- coef(model2)[2]  # Coefficient of imp_shock_iv_b
# se_val <- 0.059873  # Standard error of imp_shock_iv_b
# t_stat <- 20.21606
# 
# ggplot(tb_all, aes(x=fitted_vals, y=exp_shock_o_transformed)) +
#   geom_point(shape=1) +  # circle points
#   geom_smooth(method="lm", aes(weight=pop2001_o), se=FALSE, color="gray") +
#   labs(title="First Stage Regression (Export), 2001-2019",
#        x="Change in Predicted Export Exposure per Worker (in kUSD)",
#        y="Change in Export Exposure per Worker (in kUSD)") +
#   scale_y_continuous(breaks=seq(-20, 30, by=5)) +
#   geom_label(aes(x=min(fitted_vals), y=max(exp_shock_o_transformed)),
#              label=sprintf("Coef: %.2f\nStd. Error: %.2f\nT-stat: %.2f", coef_val, se_val, t_stat), 
#              hjust=0, vjust=1, size=4, fill="lightgray", color="black", family="sans") +  # Adjust position and size as needed
#   theme_bw()
# 
# 
# ggsave("results/figures/1st-stage-export-2001_2019.png", dpi=400, height=8, width=15, scale=0.5)
# 
# 









tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)

#shock_o
model <- feols(shock_o ~ shock_o_iv + shock_d_iv + log(pop2001_d) + log(pop2001_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d, weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


est <- model %>% broom::tidy()


# Extract coefficient, standard error, and t-statistic
coef_val <- coef(model)[2]  # Coefficient of imp_shock_iv_b
se_val <- 0.139015  # Standard error of imp_shock_iv_b
t_stat <- 45.80403


# 2. Create the added-variable plot
# Note: Creating an exact "avplot" equivalent in ggplot2 would require computing residuals and fitted values.

fitted_vals <- predict(model)


ggplot(tb_all, aes(x=fitted_vals, y=imp_shock_o_transformed)) +
  geom_point(shape=1) +  # circle points
  geom_smooth(method="lm", aes(weight=pop2001_o), se=FALSE, color="gray") +
  labs(title="First Stage Regression (Import), 2001-2019",
       x="Change in Predicted Import Exposure per Worker (in kUSD)",
       y="Change in Import Exposure per Worker (in kUSD)") +
  scale_y_continuous(breaks=seq(-20, 20, by=5)) +
  geom_label(aes(x=min(fitted_vals), y=max(imp_shock_o_transformed)),
             label=sprintf("Coef: %.3f\nStd. Error: %.3f\nT-stat: %.2f", coef_val, se_val, t_stat), 
             hjust=0, vjust=1, size=4, fill="lightgray", color="black", family="sans") +  # Adjust position and size as needed
  theme_bw()


ggsave("results/figures/1st-stage-import-2001_2019.png", dpi=400, height=8, width=15, scale=0.5)



#shock_d
out_tog1 <- feols(shock_d ~ shock_o_iv + shock_d_iv + log(pop2001_d) + log(pop2001_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))


tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
tb_all <- tb_all %>% filter(cz_o!=cz_d)
tb_all <- tb_all %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_all <- tb_all %>% mutate(across(starts_with("exp"), ~ .x/1000))

out_sep1 <- feols(log(migrate) ~ log(pop2001_d) + log(pop2001_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d + edu_o + edu_d | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_all$pop2001_o, cluster=tb_all$iso_o, data=tb_all)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)

out_tog2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) +log(distance)  + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year |
                    shock_o + shock_d  ~ shock_o_iv + shock_d_iv, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two %>% 
                    mutate(shock_o=(exp_shock_o - imp_shock_o)/1000, shock_o_iv=(exp_shock1999_iv_o - imp_shock1999_iv_o)/1000,
                           shock_d=(exp_shock_d - imp_shock_d)/1000, shock_d_iv=(exp_shock1999_iv_d - imp_shock1999_iv_d)/1000))

tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2 <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d  | year | 
                    imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                  weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)


tb_two <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")
tb_two <- tb_two %>% filter(cz_o!=cz_d)
tb_two <- tb_two %>% mutate(across(starts_with("imp"), ~ .x/1000))
tb_two <- tb_two %>% mutate(across(starts_with("exp"), ~ .x/1000))


out_sep2_fef <- feols(log(migrate) ~ log(pop_d) + log(pop_o) + log(distance) + emp_share_female_o + emp_share_female_d + emp_share_manu_o + emp_share_manu_d | year +cz_o +cz_d | 
                        imp_shock_o + exp_shock_o + imp_shock_d + exp_shock_d  ~ imp_shock1999_iv_o + exp_shock1999_iv_o + imp_shock1999_iv_d + exp_shock1999_iv_d, 
                      weights = tb_two$popw_o, cluster=tb_two$iso_o, data=tb_two)




# Extracting the first-stage results
first_stage <- etables(out_sep1, only = "first")

# Tidying up the results using broom
tidy_results <- tidy(first_stage)





