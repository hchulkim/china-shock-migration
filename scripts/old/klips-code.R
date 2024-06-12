

library(tidyverse)
library(fixest)
library(texreg)


data <- haven::read_dta("data/klips/klips_all_rough_230913.dta")


data <- data %>% select(hhid, pid, wave, year, p_age, p_ind2000, p_sex, p_weight09_c, p_weight18_c, p_weight98_c)


# change the ind code to manu, non-manu, and all

data <- data %>% mutate(p_ind=as.character(p_ind2000))
data <- data %>% mutate(p_ind=str_sub(p_ind, 1, 2))

# 1= manu, 0 = non-manu
data <- data %>% mutate(p_ind = ifelse(p_ind>=15 & p_ind<37, 1, 0))


# as NA are jobless time, for now, take it out
data <- data %>% filter(!is.na(p_ind2000))


# use from 1998 to 2019
data <- data %>% filter(year<2020)



final_data <- expand_grid(year=1998:2019, pind=c("all", "manu", "non-manu", "male-manu"))


data <- data %>% mutate(p_sex=ifelse(p_sex==1, 1, 0))

# divide by year: all workers
data_all98 <- data %>% filter(year<2009)

data_all09 <- data %>% filter(year>=2009 & year<2018) %>% filter(!is.na(p_weight09_c))

data_all18 <- data %>% filter(year>=2018) %>% filter(!is.na(p_weight18_c))


data_all98 <- data_all98 %>% group_by(year) %>% 
  summarise(age=weighted.mean(p_age, w=p_weight98_c), gender=weighted.mean(p_sex, w=p_weight98_c))

data_all09 <- data_all09 %>% group_by(year) %>% 
  summarise(age=weighted.mean(p_age, w=p_weight09_c), gender=weighted.mean(p_sex, w=p_weight09_c))

data_all18 <- data_all18 %>% group_by(year) %>% 
  summarise(age=weighted.mean(p_age, w=p_weight18_c), gender=weighted.mean(p_sex, w=p_weight18_c))

data_all <- data_all98 %>% bind_rows(data_all09) %>% 
  bind_rows(data_all18)

data_all <- data_all %>% mutate(pind="all")




# divide by year: manu
data_ind98 <- data %>% filter(year<2009, p_ind==1)

data_ind09 <- data %>% filter(year>=2009 & year<2018, p_ind==1) %>% filter(!is.na(p_weight09_c))

data_ind18 <- data %>% filter(year>=2018, p_ind==1) %>% filter(!is.na(p_weight18_c))


data_ind98 <- data_ind98 %>% group_by(year) %>% 
  summarise(age=weighted.mean(p_age, w=p_weight98_c), gender=weighted.mean(p_sex, w=p_weight98_c))

data_ind09 <- data_ind09 %>% group_by(year) %>% 
  summarise(age=weighted.mean(p_age, w=p_weight09_c), gender=weighted.mean(p_sex, w=p_weight09_c))

data_ind18 <- data_ind18 %>% group_by(year) %>% 
  summarise(age=weighted.mean(p_age, w=p_weight18_c), gender=weighted.mean(p_sex, w=p_weight18_c))

data_ind <- data_ind98 %>% bind_rows(data_ind09) %>% 
  bind_rows(data_ind18)

data_ind <- data_ind %>% mutate(pind="manu")



# divide by year: non-manu
data_ind98 <- data %>% filter(year<2009, p_ind==0)

data_ind09 <- data %>% filter(year>=2009 & year<2018, p_ind==0) %>% filter(!is.na(p_weight09_c))

data_ind18 <- data %>% filter(year>=2018, p_ind==0) %>% filter(!is.na(p_weight18_c))


data_ind98 <- data_ind98 %>% group_by(year) %>% 
  summarise(age=weighted.mean(p_age, w=p_weight98_c), gender=weighted.mean(p_sex, w=p_weight98_c))

data_ind09 <- data_ind09 %>% group_by(year) %>% 
  summarise(age=weighted.mean(p_age, w=p_weight09_c), gender=weighted.mean(p_sex, w=p_weight09_c))

data_ind18 <- data_ind18 %>% group_by(year) %>% 
  summarise(age=weighted.mean(p_age, w=p_weight18_c), gender=weighted.mean(p_sex, w=p_weight18_c))

data_noind <- data_ind98 %>% bind_rows(data_ind09) %>% 
  bind_rows(data_ind18)

data_noind <- data_noind %>% mutate(pind="non-manu")




# male manu
data_all98 <- data %>% filter(year<2009, p_ind==1, p_sex==1)

data_all09 <- data %>% filter(year>=2009 & year<2018, p_ind==1, p_sex==1) %>% filter(!is.na(p_weight09_c))

data_all18 <- data %>% filter(year>=2018, p_ind==1, p_sex==1) %>% filter(!is.na(p_weight18_c))


data_all98 <- data_all98 %>% group_by(year) %>% 
  summarise(age=weighted.mean(p_age, w=p_weight98_c), gender=weighted.mean(p_sex, w=p_weight98_c))

data_all09 <- data_all09 %>% group_by(year) %>% 
  summarise(age=weighted.mean(p_age, w=p_weight09_c), gender=weighted.mean(p_sex, w=p_weight09_c))

data_all18 <- data_all18 %>% group_by(year) %>% 
  summarise(age=weighted.mean(p_age, w=p_weight18_c), gender=weighted.mean(p_sex, w=p_weight18_c))

data_male_manu <- data_all98 %>% bind_rows(data_all09) %>% 
  bind_rows(data_all18)

data_male_manu <- data_male_manu %>% mutate(pind="male-manu")



data <- data_all %>% bind_rows(data_ind) %>% bind_rows(data_noind) %>% bind_rows(data_male_manu)


final_data <- final_data %>% left_join(data, by=c("year", "pind"))


data_ind <- data_ind %>% select(-pind) %>% 
  pivot_longer(cols=-year, values_to = "value", names_to = "type")


final_data %>%
  ggplot(aes(year, age, color=pind)) + 
  geom_point() + geom_line() +
  scale_x_continuous(name="Year", breaks = 1998:2019) +
  scale_y_continuous(name="Age", breaks = seq(35, 50, by=1)) +
  labs(title="Mean age by industry") +
  theme_bw()
ggsave("results/figures/manu-age.png")
  
final_data %>% filter(pind!="male-manu") %>% 
  ggplot(aes(year, gender, color=pind)) + 
  geom_point() + geom_line() +
  scale_x_continuous(name="Year", breaks = 1998:2019) +
  scale_y_continuous(name="Gender (male %)", labels=scales::label_comma(scale = 100, suffix="%")) +
  labs(title="Proportion of Male by industry") +
  theme_bw()
ggsave("results/figures/manu-gen.png")




# now let's check transition and movement 

# 2001-2010 and 2010-2020

# data <- haven::read_dta("data/klips/klips_all_rough_230913.dta")
data <- haven::read_dta("data/klips/klips_raw.dta")

data <- data %>% select(hhid, pid, wave, year, p_age, p_ind2000, p_sex, p_weight09_c, p_weight18_c, p_weight98_c, p_weight09_l, p_weight18_l, p_weight98_l, p_region=h0142)


data <- data %>% filter(year>=2000 & year<2021)



data <- data %>% mutate(p_ind = case_when(
  
  p_ind2000>=11 & p_ind2000<=52 ~ "agriculture-forestry-fishery",
  p_ind2000>=101 & p_ind2000<=122 ~ "mine",
  p_ind2000>=151 & p_ind2000<=369 ~ "manu",
  p_ind2000>=401 & p_ind2000<=410 ~ "electricity-water",
  p_ind2000>=451 & p_ind2000<=465 ~ "construction",
  p_ind2000>=501 & p_ind2000<=528 ~ "wholesale-resail",
  p_ind2000>=551 & p_ind2000<=552 ~ "food-hotel",
  p_ind2000>=601 & p_ind2000<=642 ~ "shipping",
  p_ind2000>=651 & p_ind2000<=672 ~ "finance-insurance",
  p_ind2000>=701 & p_ind2000<=702 ~ "housing",
  p_ind2000>=711 & p_ind2000<=713 ~ "rental",
  p_ind2000>=721 & p_ind2000<=749 ~ "information",
  p_ind2000>=751 & p_ind2000<=759 ~ "business-support",
  p_ind2000>=761 & p_ind2000<=765 ~ "public-service",
  p_ind2000>=801 & p_ind2000<=862 ~ "education-health",
  p_ind2000>=871 & p_ind2000<=889 ~ "entertainment",
  p_ind2000>=901 & p_ind2000<=903 ~ "cleaning",
  
  .default = as.character(p_ind2000)
))

data <- data %>% mutate(p_ind=ifelse(is.na(p_ind), "no-job", p_ind))


data <- data %>% filter(year==2000 | year==2005 | year==2010 | year==2015 | year==2020)


data1 <- data %>% filter(year==2000 | year==2005)
data2 <- data %>% filter(year==2005 | year==2010)

data1 <- data1 %>% mutate(p_age = case_when(
  p_age<25 ~ "25-",
  p_age>=25 & p_age<35 ~ "25-34",
  p_age>=35 & p_age<45 ~ "35-44",
  p_age>=45 & p_age<55 ~ "45-54",
  p_age>=55 & p_age<65 ~ "55-64",
  p_age>=65 ~ "65+",
  .default = as.character(p_age)
))

data2 <- data2 %>% mutate(p_age = case_when(
  p_age<25 ~ "25-",
  p_age>=25 & p_age<35 ~ "25-34",
  p_age>=35 & p_age<45 ~ "35-44",
  p_age>=45 & p_age<55 ~ "45-54",
  p_age>=55 & p_age<65 ~ "55-64",
  p_age>=65 ~ "65+",
  .default = as.character(p_age)
))



data1 <- data1 %>% group_by(pid) %>% 
  mutate(n=n())

data1 <- data1 %>% filter(n==2)

data2 <- data2 %>% group_by(pid) %>% 
  mutate(n=n())

data2 <- data2 %>% filter(n==2)




data1 <- data1 %>% group_by(pid) %>% 
  mutate(p_ind_lag=lag(p_ind))

data1 <- data1 %>% group_by(pid) %>% 
  mutate(p_age_lag=lag(p_age))

data1 <- data1 %>% group_by(pid) %>% 
  mutate(p_region_lag=lag(p_region))

# data1 <- data1 %>% mutate(p_ind_lag=ifelse(is.na(p_ind_lag), p_ind, p_ind_lag))
# 
# data1 <- data1 %>% mutate(p_age_lag=ifelse(is.na(p_age_lag), p_age, p_age_lag))
# 
# data1 <- data1 %>% mutate(p_region_lag=ifelse(is.na(p_region_lag), p_region, p_region_lag))


data1 <- data1 %>% filter(!is.na(p_ind_lag))

data1 <- data1 %>% filter(year>2000)

data1 <- data1 %>% mutate(p_age=p_age_lag)


data2 <- data2 %>% group_by(pid) %>% 
  mutate(p_ind_lag=lag(p_ind))

data2 <- data2 %>% group_by(pid) %>% 
  mutate(p_age_lag=lag(p_age))

data2 <- data2 %>% group_by(pid) %>% 
  mutate(p_region_lag=lag(p_region))


data2 <- data2 %>% filter(!is.na(p_ind_lag))



data2 <- data2 %>% filter(year>2005)

data2 <- data2 %>% mutate(p_age=p_age_lag)


# check if manu
data1 <- data1 %>% mutate(manu=ifelse(p_ind_lag=="manu", 1, 0))
data2 <- data2 %>% mutate(manu=ifelse(p_ind_lag=="manu", 1, 0))



# divide by year: all workers
data1 <- data1 %>% mutate(p_wt = p_weight18_l)

data1 <- data1 %>% mutate(p_wt = ifelse(is.na(p_wt), p_weight09_l, p_wt))

data1 <- data1 %>% mutate(p_wt = ifelse(is.na(p_wt), p_weight98_l, p_wt))

data2 <- data2 %>% mutate(p_wt = p_weight18_l)

data2 <- data2 %>% mutate(p_wt = ifelse(is.na(p_wt), p_weight09_l, p_wt))

data2 <- data2 %>% mutate(p_wt = ifelse(is.na(p_wt), p_weight98_l, p_wt))



data1 <- data1 %>% mutate(ind_change = ifelse(p_ind != p_ind_lag, 1, 0))

data1 <- data1 %>% mutate(region_change = ifelse(p_region != p_region_lag, 1, 0))

data2 <- data2 %>% mutate(ind_change = ifelse(p_ind != p_ind_lag, 1, 0))

data2 <- data2 %>% mutate(region_change = ifelse(p_region != p_region_lag, 1, 0))



data1 <- data1 %>% filter(manu==1)

data2 <- data2 %>% filter(manu==1)



# all_workers

data1 <- data1 %>% group_by(p_age) %>%
  summarise(ind_change_pt = sum(ind_change),
region_change_pt = sum(region_change))

data1 <- data1 %>% group_by(p_age) %>% 
  summarise(ind_change_pt = weighted.mean(ind_change, w=p_wt),
            region_change_pt = weighted.mean(region_change, w=p_wt))

data1 %>% pivot_longer(cols=-p_age, values_to = "change_pt", names_to = "type") %>%
  ggplot(aes(p_age, change_pt, color=type)) + 
  geom_point() +
  geom_line() +
  scale_x_discrete(name="Age") +
  scale_y_continuous(name="Change within age", labels=scales::label_comma(suffix="%", scale=100)) +
  theme_bw()
ggsave("results/figures/change-ind.png")


data2 <- data2 %>% group_by(p_age) %>% 
  summarise(ind_change_pt = weighted.mean(ind_change, w=p_wt),
            region_change_pt = weighted.mean(region_change, w=p_wt))



data2 %>% pivot_longer(cols=-p_age, values_to = "change_pt", names_to = "type") %>%
  ggplot(aes(p_age, change_pt, color=type)) + 
  geom_point() + 
  geom_line() +
  scale_x_discrete(name="Age") +
  scale_y_continuous(name="Change within age", labels=scales::label_comma(suffix="%", scale=100)) +
  theme_bw()




# also do c : similar


data <- haven::read_dta("data/klips/klips_all_rough_230913.dta")


data <- data %>% select(hhid, pid, wave, year, p_age, p_ind2000, p_sex, p_weight09_c, p_weight18_c, p_weight98_c, p_weight09_l, p_weight18_l, p_weight98_l)




data <- data %>% mutate(p_ind=as.character(p_ind2000))

data <- data %>% mutate(p_ind=ifelse(is.na(p_ind), "9999999", p_ind))

data <- data %>% group_by(pid) %>% 
  mutate(p_ind_lag=lag(p_ind))

data <- data %>% group_by(pid) %>% 
  mutate(p_age_lag=lag(p_age))


data <- data %>% filter(!is.na(p_ind_lag))







# divide by year: all workers
data <- data %>% mutate(p_wt = p_weight18_c)

data <- data %>% mutate(p_wt = ifelse(is.na(p_wt), p_weight09_c, p_wt))

data <- data %>% mutate(p_wt = ifelse(is.na(p_wt), p_weight98_c, p_wt))


data <- data %>% mutate(change = ifelse(p_ind != p_ind_lag, 1, 0))

data <- data %>% filter(!is.na(p_age))

data <- data %>% mutate(change = ifelse(p_age-p_age_lag!=1, 0, change))






data <- data %>% group_by(p_age) %>% 
  summarise(change_pt = weighted.mean(change, w=p_wt))



data <- data %>% filter(p_age>=25 & p_age<65)


data %>% ggplot(aes(p_age, change_pt)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name="Age", breaks=seq(25, 64, by=1)) +
  scale_y_continuous(name="Ind change within year", labels=scales::label_comma(suffix="%", scale=100)) +
  theme_bw()






# proportion by age

# manu
data <- haven::read_dta("data/klips/klips_all_rough_230913.dta")


data <- data %>% select(hhid, pid, wave, year, p_age, p_ind2000, p_sex, p_weight09_c, p_weight18_c, p_weight98_c)


# change the ind code to manu, non-manu, and all

data <- data %>% mutate(p_ind=as.character(p_ind2000))
data <- data %>% mutate(p_ind=str_sub(p_ind, 1, 2))

# 1= manu, 0 = non-manu
data <- data %>% mutate(p_ind = ifelse(p_ind>=15 & p_ind<37, 1, 0))


# as NA are jobless time, for now, take it out
data <- data %>% filter(!is.na(p_ind2000))


# use from 1998 to 2019
data <- data %>% filter(year<2020)



final_data <- expand_grid(year=1998:2019, pind=c("25-34", "35-44", "45-54", "55-64"))


data <- data %>% mutate(p_sex=ifelse(p_sex==1, 1, 0))

data <- data %>% filter(p_age<65, p_age>=25)

# divide by year: manu
data <- data %>% filter(p_ind==1, p_sex==1)


data <- data %>% mutate(age=case_when(
  p_age>=25 & p_age<35 ~ "25-34",
  p_age>=35 & p_age<45 ~ "35-44",
  p_age>=45 & p_age<55 ~ "45-54",
  p_age>=55 ~ "55-64",
  .default = NULL
))



data_all98 <- data %>% filter(year<2009)

data_all09 <- data %>% filter(year>=2009 & year<2018) %>% filter(!is.na(p_weight09_c))

data_all18 <- data %>% filter(year>=2018) %>% filter(!is.na(p_weight18_c))



data_all98 <- data_all98 %>% group_by(year, age) %>% 
  summarise(n=sum(p_weight98_c, na.rm=T)) %>% ungroup()

data_all09 <- data_all09 %>% group_by(year, age) %>% 
  summarise(n=sum(p_weight09_c, na.rm=T)) %>% ungroup()

data_all18 <- data_all18 %>% group_by(year, age) %>% 
  summarise(n=sum(p_weight18_c, na.rm=T)) %>% ungroup()


data_all98 <- data_all98 %>% group_by(year) %>% 
  mutate(pop=sum(n))

data_all09 <- data_all09 %>% group_by(year) %>% 
  mutate(pop=sum(n))

data_all18 <- data_all18 %>% group_by(year) %>% 
  mutate(pop=sum(n))


data <- data_all98 %>% bind_rows(data_all09) %>% 
  bind_rows(data_all18)


data <- data %>% mutate(value=n/pop)

data %>% ggplot(aes(year, value, color=age)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name="Year", breaks=1998:2019) +
  scale_y_continuous(name="Proportion by age group", labels=scales::label_comma(suffix="%", scale=100)) +
  theme_bw()
ggsave("results/figures/4age.png")


data2 <- data %>% mutate(age=ifelse(age=="45-54", "45-64", age))
data2 <- data2 %>% mutate(age=ifelse(age=="55-64", "45-64", age))

data2 <- data2 %>% group_by(year, age) %>% 
  summarise(value=sum(value, na.rm=T)) %>% ungroup()

data2 %>% ggplot(aes(year, value, color=age)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name="Year", breaks=1998:2019) +
  scale_y_continuous(name="Proportion by age group", labels=scales::label_comma(suffix="%", scale=100)) +
  theme_bw()
ggsave("results/figures/3age.png")


data3 <- data2 %>% mutate(age=ifelse(age=="25-34", "25-44", age))
data3 <- data3 %>% mutate(age=ifelse(age=="35-44", "25-44", age))

data3 <- data3 %>% group_by(year, age) %>% 
  summarise(value=sum(value, na.rm=T)) %>% ungroup()

data3 %>% ggplot(aes(year, value, color=age)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name="Year", breaks=1998:2019) +
  scale_y_continuous(name="Proportion by age group", labels=scales::label_comma(suffix="%", scale=100)) +
  theme_bw()
ggsave("results/figures/2age.png")







# no manu
data <- haven::read_dta("data/klips/klips_all_rough_230913.dta")


data <- data %>% select(hhid, pid, wave, year, p_age, p_ind2000, p_sex, p_weight09_c, p_weight18_c, p_weight98_c)


# change the ind code to manu, non-manu, and all

data <- data %>% mutate(p_ind=as.character(p_ind2000))
data <- data %>% mutate(p_ind=str_sub(p_ind, 1, 2))

# 1= manu, 0 = non-manu
data <- data %>% mutate(p_ind = ifelse(p_ind>=15 & p_ind<37, 1, 0))


# as NA are jobless time, for now, take it out
data <- data %>% filter(!is.na(p_ind2000))


# use from 1998 to 2019
data <- data %>% filter(year<2020)



final_data <- expand_grid(year=1998:2019, pind=c("25-34", "35-44", "45-54", "55-64"))


data <- data %>% mutate(p_sex=ifelse(p_sex==1, 1, 0))

data <- data %>% filter(p_age<65, p_age>=25)

# divide by year: non-manu
data <- data %>% filter(p_ind==0, p_sex==1)


data <- data %>% mutate(age=case_when(
  p_age>=25 & p_age<35 ~ "25-34",
  p_age>=35 & p_age<45 ~ "35-44",
  p_age>=45 & p_age<55 ~ "45-54",
  p_age>=55 ~ "55-64",
  .default = NULL
))



data_all98 <- data %>% filter(year<2009)

data_all09 <- data %>% filter(year>=2009 & year<2018) %>% filter(!is.na(p_weight09_c))

data_all18 <- data %>% filter(year>=2018) %>% filter(!is.na(p_weight18_c))



data_all98 <- data_all98 %>% group_by(year, age) %>% 
  summarise(n=sum(p_weight98_c, na.rm=T)) %>% ungroup()

data_all09 <- data_all09 %>% group_by(year, age) %>% 
  summarise(n=sum(p_weight09_c, na.rm=T)) %>% ungroup()

data_all18 <- data_all18 %>% group_by(year, age) %>% 
  summarise(n=sum(p_weight18_c, na.rm=T)) %>% ungroup()


data_all98 <- data_all98 %>% group_by(year) %>% 
  mutate(pop=sum(n))

data_all09 <- data_all09 %>% group_by(year) %>% 
  mutate(pop=sum(n))

data_all18 <- data_all18 %>% group_by(year) %>% 
  mutate(pop=sum(n))


data <- data_all98 %>% bind_rows(data_all09) %>% 
  bind_rows(data_all18)


data <- data %>% mutate(value=n/pop)

data %>% ggplot(aes(year, value, color=age)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name="Year", breaks=1998:2019) +
  scale_y_continuous(name="Proportion by age group", labels=scales::label_comma(suffix="%", scale=100)) +
  theme_bw()
ggsave("results/figures/4age-non-manu.png")


data2 <- data %>% mutate(age=ifelse(age=="45-54", "45-64", age))
data2 <- data2 %>% mutate(age=ifelse(age=="55-64", "45-64", age))

data2 <- data2 %>% group_by(year, age) %>% 
  summarise(value=sum(value, na.rm=T)) %>% ungroup()

data2 %>% ggplot(aes(year, value, color=age)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name="Year", breaks=1998:2019) +
  scale_y_continuous(name="Proportion by age group", labels=scales::label_comma(suffix="%", scale=100)) +
  theme_bw()
ggsave("results/figures/3age-non-manu.png")


data3 <- data2 %>% mutate(age=ifelse(age=="25-34", "25-44", age))
data3 <- data3 %>% mutate(age=ifelse(age=="35-44", "25-44", age))

data3 <- data3 %>% group_by(year, age) %>% 
  summarise(value=sum(value, na.rm=T)) %>% ungroup()

data3 %>% ggplot(aes(year, value, color=age)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name="Year", breaks=1998:2019) +
  scale_y_continuous(name="Proportion by age group", labels=scales::label_comma(suffix="%", scale=100)) +
  theme_bw()
ggsave("results/figures/2age-non-manu.png")









###


# proportion by age

# manu
data <- haven::read_dta("data/klips/klips_all_rough_230913.dta")


data <- data %>% select(hhid, pid, wave, year, p_age, p_ind2000, p_sex, p_weight09_c, p_weight18_c, p_weight98_c)


# change the ind code to manu, non-manu, and all

data <- data %>% mutate(p_ind=as.character(p_ind2000))
data <- data %>% mutate(p_ind2=str_sub(p_ind, 1, 2))

# by sectors
data <- data %>% mutate(p_ind = case_when(
  p_ind>=15 & p_ind<37 ~ "manu",
  
))

data <- data %>% mutate(p_ind = ifelse(p_ind>=15 & p_ind<37, 1, 0))


# as NA are jobless time, for now, take it out
data <- data %>% filter(!is.na(p_ind2000))


# use from 1998 to 2019
data <- data %>% filter(year<2020)



final_data <- expand_grid(year=1998:2019, pind=c("25-34", "35-44", "45-54", "55-64"))


data <- data %>% mutate(p_sex=ifelse(p_sex==1, 1, 0))

data <- data %>% filter(p_age<65, p_age>=25)

# divide by year: manu
data <- data %>% filter(p_ind==1, p_sex==1)


data <- data %>% mutate(age=case_when(
  p_age>=25 & p_age<35 ~ "25-34",
  p_age>=35 & p_age<45 ~ "35-44",
  p_age>=45 & p_age<55 ~ "45-54",
  p_age>=55 ~ "55-64",
  .default = NULL
))



data_all98 <- data %>% filter(year<2009)

data_all09 <- data %>% filter(year>=2009 & year<2018) %>% filter(!is.na(p_weight09_c))

data_all18 <- data %>% filter(year>=2018) %>% filter(!is.na(p_weight18_c))



data_all98 <- data_all98 %>% group_by(year, age) %>% 
  summarise(n=sum(p_weight98_c, na.rm=T)) %>% ungroup()

data_all09 <- data_all09 %>% group_by(year, age) %>% 
  summarise(n=sum(p_weight09_c, na.rm=T)) %>% ungroup()

data_all18 <- data_all18 %>% group_by(year, age) %>% 
  summarise(n=sum(p_weight18_c, na.rm=T)) %>% ungroup()


data_all98 <- data_all98 %>% group_by(year) %>% 
  mutate(pop=sum(n))

data_all09 <- data_all09 %>% group_by(year) %>% 
  mutate(pop=sum(n))

data_all18 <- data_all18 %>% group_by(year) %>% 
  mutate(pop=sum(n))


data <- data_all98 %>% bind_rows(data_all09) %>% 
  bind_rows(data_all18)


data <- data %>% mutate(value=n/pop)

data %>% ggplot(aes(year, value, color=age)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name="Year", breaks=1998:2019) +
  scale_y_continuous(name="Proportion by age group", labels=scales::label_comma(suffix="%", scale=100)) +
  theme_bw()
ggsave("results/figures/4age.png")


data2 <- data %>% mutate(age=ifelse(age=="45-54", "45-64", age))
data2 <- data2 %>% mutate(age=ifelse(age=="55-64", "45-64", age))

data2 <- data2 %>% group_by(year, age) %>% 
  summarise(value=sum(value, na.rm=T)) %>% ungroup()

data2 %>% ggplot(aes(year, value, color=age)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name="Year", breaks=1998:2019) +
  scale_y_continuous(name="Proportion by age group", labels=scales::label_comma(suffix="%", scale=100)) +
  theme_bw()
ggsave("results/figures/3age.png")


data3 <- data2 %>% mutate(age=ifelse(age=="25-34", "25-44", age))
data3 <- data3 %>% mutate(age=ifelse(age=="35-44", "25-44", age))

data3 <- data3 %>% group_by(year, age) %>% 
  summarise(value=sum(value, na.rm=T)) %>% ungroup()

data3 %>% ggplot(aes(year, value, color=age)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(name="Year", breaks=1998:2019) +
  scale_y_continuous(name="Proportion by age group", labels=scales::label_comma(suffix="%", scale=100)) +
  theme_bw()
ggsave("results/figures/2age.png")
