
##############################
# creator: Hyoungchul Kim
# revised date: 06/23/2024
# description: This script matches establishment data from ksic8 or 9 to ksic10 for analysis.
##############################

if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(here, tidyverse)

#--------------------
# read in the est data
#--------------------
est1994 <-read_rds(here("data", "temp", "est1994_region_matched.rds"))
est1996 <-read_rds(here("data", "temp", "est1996_region_matched.rds"))
est1999 <-read_rds(here("data", "temp", "est1999_region_matched.rds"))
est2000 <-read_rds(here("data", "temp", "est2000_region_matched.rds"))
est2001 <-read_rds(here("data", "temp", "est2001_region_matched.rds"))
est2010 <-read_rds(here("data", "temp", "est2010_region_matched.rds"))
est2019 <-read_rds(here("data", "temp", "est2019_region_matched.rds"))

ksic8_ksic9  <- read_rds(here("data", "concordance", "hs_cpc_ksic","ksic8_ksic9.rds"))
ksic9_ksic10  <- read_rds(here("data", "concordance", "hs_cpc_ksic", "ksic9_ksic10.rds"))

# paste industry code into 5 digit
est1994 <- est1994 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5))
est1996 <- est1996 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5))
est1999 <- est1999 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5))
est2000 <- est2000 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5))
  


est2001 <- est2001 %>% mutate(ksic8=paste0(`산업분류(중)`, `산업분류(소)`, `산업분류(세)`, `산업분류(세세)`), emp_female=`8-⑥종사자_합계_여(①+…+⑤)`, emp_male=`8-⑥종사자_합계_남(①+…+⑤)`, emp_all=`8-⑥종사자_합계_계(①+…+⑤)`) %>% select(iso, ksic8, emp_female, emp_male, emp_all)


est2010 <- est2010 %>% mutate(ksic9=paste0(`산업분류_중`, `산업분류_소`, `산업분류_세`, `산업분류_세세`), emp_female=`종사자수_합계_여`, emp_male=`종사자수_합계_남`, emp_all=`종사자수_합계_계`) %>% select(iso, ksic9, emp_female, emp_male, emp_all)



est2019 <- est2019 %>% mutate(ksic10=paste0(`주사업_산업중분류코드`, `주사업_산업소분류코드`, `주사업_산업세분류코드`, `주사업_산업세세분류코드`)) %>% 
  select(iso, ksic10, emp_female=`여자종사자수`, emp_male=`남자종사자수`, emp_all=`합계종사자수`)

# save 2019 as it is already ksic10

est2019 <- est2019 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% ungroup()

est2019 %>% haven::write_dta("data/final/hs_cpc_ksic/est2019_refine.dta")
est2019 %>% write_rds("data/final/hs_cpc_ksic/est2019_refine.rds")


#ksic 8 to 9 : 1994 only

est1994 <- est1994 %>% group_by(iso, ksic8) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% ungroup()


ksic8_ksic9_1 <- ksic8_ksic9 %>% filter(weight==1)
ksic8_ksic9_2 <- ksic8_ksic9 %>% filter(weight!=1)



check <- ksic8_ksic9_2 %>% distinct(ksic8) %>% pull(ksic8)


est1994_1 <- est1994 %>% filter(!(ksic8 %in% check))

est1994_2 <- est1994 %>% filter(ksic8 %in% check)

est1994_1 <- est1994_1 %>%
  left_join(ksic8_ksic9_1, by="ksic8") %>% select(-ksic8)



ksic8_ksic9_2 <- ksic8_ksic9_2 %>%
  left_join(est1994_2, by="ksic8") %>% select(-ksic8)


est1994 <- est1994_1 %>%
  bind_rows(ksic8_ksic9_2) %>% filter(!is.na(emp_all), !is.na(ksic9))

#ksic 8 to 9 : 1996 only

est1996 <- est1996 %>% group_by(iso, ksic8) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% ungroup()


ksic8_ksic9_1 <- ksic8_ksic9 %>% filter(weight==1)
ksic8_ksic9_2 <- ksic8_ksic9 %>% filter(weight!=1)



check <- ksic8_ksic9_2 %>% distinct(ksic8) %>% pull(ksic8)


est1996_1 <- est1996 %>% filter(!(ksic8 %in% check))

est1996_2 <- est1996 %>% filter(ksic8 %in% check)

est1996_1 <- est1996_1 %>%
  left_join(ksic8_ksic9_1, by="ksic8") %>% select(-ksic8)



ksic8_ksic9_2 <- ksic8_ksic9_2 %>%
  left_join(est1996_2, by="ksic8") %>% select(-ksic8)


est1996 <- est1996_1 %>%
  bind_rows(ksic8_ksic9_2) %>% filter(!is.na(emp_all), !is.na(ksic9))



#ksic 8 to 9 : 1999 only

est1999 <- est1999 %>% group_by(iso, ksic8) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% ungroup()


ksic8_ksic9_1 <- ksic8_ksic9 %>% filter(weight==1)
ksic8_ksic9_2 <- ksic8_ksic9 %>% filter(weight!=1)



check <- ksic8_ksic9_2 %>% distinct(ksic8) %>% pull(ksic8)


est1999_1 <- est1999 %>% filter(!(ksic8 %in% check))

est1999_2 <- est1999 %>% filter(ksic8 %in% check)

est1999_1 <- est1999_1 %>%
  left_join(ksic8_ksic9_1, by="ksic8") %>% select(-ksic8)



ksic8_ksic9_2 <- ksic8_ksic9_2 %>%
  left_join(est1999_2, by="ksic8") %>% select(-ksic8)


est1999 <- est1999_1 %>%
  bind_rows(ksic8_ksic9_2) %>% filter(!is.na(emp_all), !is.na(ksic9))



#ksic 8 to 9 : 2000 only

est2000 <- est2000 %>% group_by(iso, ksic8) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% ungroup()


ksic8_ksic9_1 <- ksic8_ksic9 %>% filter(weight==1)
ksic8_ksic9_2 <- ksic8_ksic9 %>% filter(weight!=1)



check <- ksic8_ksic9_2 %>% distinct(ksic8) %>% pull(ksic8)


est2000_1 <- est2000 %>% filter(!(ksic8 %in% check))

est2000_2 <- est2000 %>% filter(ksic8 %in% check)

est2000_1 <- est2000_1 %>%
  left_join(ksic8_ksic9_1, by="ksic8") %>% select(-ksic8)



ksic8_ksic9_2 <- ksic8_ksic9_2 %>%
  left_join(est2000_2, by="ksic8") %>% select(-ksic8)


est2000 <- est2000_1 %>%
  bind_rows(ksic8_ksic9_2) %>% filter(!is.na(emp_all), !is.na(ksic9))



#ksic 8 to 9 : 2001 only

est2001 <- est2001 %>% group_by(iso, ksic8) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% ungroup()


ksic8_ksic9_1 <- ksic8_ksic9 %>% filter(weight==1)
ksic8_ksic9_2 <- ksic8_ksic9 %>% filter(weight!=1)



check <- ksic8_ksic9_2 %>% distinct(ksic8) %>% pull(ksic8)


est2001_1 <- est2001 %>% filter(!(ksic8 %in% check))

est2001_2 <- est2001 %>% filter(ksic8 %in% check)

est2001_1 <- est2001_1 %>%
  left_join(ksic8_ksic9_1, by="ksic8") %>% select(-ksic8)



ksic8_ksic9_2 <- ksic8_ksic9_2 %>%
  left_join(est2001_2, by="ksic8") %>% select(-ksic8)


est2001 <- est2001_1 %>%
  bind_rows(ksic8_ksic9_2) %>% filter(!is.na(emp_all), !is.na(ksic9))



# #ksic 9 to 10 : 1994, 1996, 1999, 2000, 2001 and 2010


# est1994

est1994 <- est1994 %>% mutate(emp_all=emp_all*weight) %>% select(-weight)


est1994 <- est1994 %>% group_by(iso, ksic9) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% ungroup()








ksic9_ksic10_1 <- ksic9_ksic10 %>% filter(weight==1)
ksic9_ksic10_2 <- ksic9_ksic10 %>% filter(weight!=1)



check <- ksic9_ksic10_2 %>% distinct(ksic9) %>% pull(ksic9)


est1994_1 <- est1994 %>% filter(!(ksic9 %in% check))

est1994_2 <- est1994 %>% filter(ksic9 %in% check)

est1994_1 <- est1994_1 %>%
  left_join(ksic9_ksic10_1, by="ksic9") %>% select(-ksic9)



ksic9_ksic10_2 <- ksic9_ksic10_2 %>%
  left_join(est1994_2, by="ksic9") %>% select(-ksic9)


est1994 <- est1994_1 %>%
  bind_rows(ksic9_ksic10_2) %>% filter(!is.na(emp_all), !is.na(ksic10))


est1994 <- est1994 %>% mutate(emp_all=emp_all*weight) %>% select(-weight)


est1994 <- est1994 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% ungroup()


est1994 %>% haven::write_dta("data/final/hs_cpc_ksic/est1994_refine.dta")
est1994 %>% write_rds("data/final/hs_cpc_ksic/est1994_refine.rds")


# est1996

est1996 <- est1996 %>% mutate(emp_all=emp_all*weight) %>% select(-weight)


est1996 <- est1996 %>% group_by(iso, ksic9) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% ungroup()








ksic9_ksic10_1 <- ksic9_ksic10 %>% filter(weight==1)
ksic9_ksic10_2 <- ksic9_ksic10 %>% filter(weight!=1)



check <- ksic9_ksic10_2 %>% distinct(ksic9) %>% pull(ksic9)


est1996_1 <- est1996 %>% filter(!(ksic9 %in% check))

est1996_2 <- est1996 %>% filter(ksic9 %in% check)

est1996_1 <- est1996_1 %>%
  left_join(ksic9_ksic10_1, by="ksic9") %>% select(-ksic9)



ksic9_ksic10_2 <- ksic9_ksic10_2 %>%
  left_join(est1996_2, by="ksic9") %>% select(-ksic9)


est1996 <- est1996_1 %>%
  bind_rows(ksic9_ksic10_2) %>% filter(!is.na(emp_all), !is.na(ksic10))


est1996 <- est1996 %>% mutate(emp_all=emp_all*weight) %>% select(-weight)


est1996 <- est1996 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% ungroup()


est1996 %>% haven::write_dta("data/final/hs_cpc_ksic/est1996_refine.dta")
est1996 %>% write_rds("data/final/hs_cpc_ksic/est1996_refine.rds")

# est1999

est1999 <- est1999 %>% mutate(emp_all=emp_all*weight) %>% select(-weight)


est1999 <- est1999 %>% group_by(iso, ksic9) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% ungroup()








ksic9_ksic10_1 <- ksic9_ksic10 %>% filter(weight==1)
ksic9_ksic10_2 <- ksic9_ksic10 %>% filter(weight!=1)



check <- ksic9_ksic10_2 %>% distinct(ksic9) %>% pull(ksic9)


est1999_1 <- est1999 %>% filter(!(ksic9 %in% check))

est1999_2 <- est1999 %>% filter(ksic9 %in% check)

est1999_1 <- est1999_1 %>%
  left_join(ksic9_ksic10_1, by="ksic9") %>% select(-ksic9)



ksic9_ksic10_2 <- ksic9_ksic10_2 %>%
  left_join(est1999_2, by="ksic9") %>% select(-ksic9)


est1999 <- est1999_1 %>%
  bind_rows(ksic9_ksic10_2) %>% filter(!is.na(emp_all), !is.na(ksic10))


est1999 <- est1999 %>% mutate(emp_all=emp_all*weight) %>% select(-weight)


est1999 <- est1999 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% ungroup()


est1999 %>% haven::write_dta("data/final/hs_cpc_ksic/est1999_refine.dta")
est1999 %>% write_rds("data/final/hs_cpc_ksic/est1999_refine.rds")



# est2000


est2000 <- est2000 %>% mutate(emp_all=emp_all*weight) %>% select(-weight)


est2000 <- est2000 %>% group_by(iso, ksic9) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% ungroup()








ksic9_ksic10_1 <- ksic9_ksic10 %>% filter(weight==1)
ksic9_ksic10_2 <- ksic9_ksic10 %>% filter(weight!=1)



check <- ksic9_ksic10_2 %>% distinct(ksic9) %>% pull(ksic9)


est2000_1 <- est2000 %>% filter(!(ksic9 %in% check))

est2000_2 <- est2000 %>% filter(ksic9 %in% check)

est2000_1 <- est2000_1 %>%
  left_join(ksic9_ksic10_1, by="ksic9") %>% select(-ksic9)



ksic9_ksic10_2 <- ksic9_ksic10_2 %>%
  left_join(est2000_2, by="ksic9") %>% select(-ksic9)


est2000 <- est2000_1 %>%
  bind_rows(ksic9_ksic10_2) %>% filter(!is.na(emp_all), !is.na(ksic10))


est2000 <- est2000 %>% mutate(emp_all=emp_all*weight) %>% select(-weight)


est2000 <- est2000 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T)) %>% ungroup()


est2000 %>% haven::write_dta("data/final/hs_cpc_ksic/est2000_refine.dta")
est2000 %>% write_rds("data/final/hs_cpc_ksic/est2000_refine.rds")


# est2001

est2001 <- est2001 %>% mutate(emp_all=emp_all*weight, emp_male=emp_male*weight, emp_female=emp_female*weight) %>% select(-weight)


est2001 <- est2001 %>% group_by(iso, ksic9) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% ungroup()








ksic9_ksic10_1 <- ksic9_ksic10 %>% filter(weight==1)
ksic9_ksic10_2 <- ksic9_ksic10 %>% filter(weight!=1)



check <- ksic9_ksic10_2 %>% distinct(ksic9) %>% pull(ksic9)


est2001_1 <- est2001 %>% filter(!(ksic9 %in% check))

est2001_2 <- est2001 %>% filter(ksic9 %in% check)

est2001_1 <- est2001_1 %>%
  left_join(ksic9_ksic10_1, by="ksic9") %>% select(-ksic9)



ksic9_ksic10_2 <- ksic9_ksic10_2 %>%
  left_join(est2001_2, by="ksic9") %>% select(-ksic9)


est2001 <- est2001_1 %>%
  bind_rows(ksic9_ksic10_2) %>% filter(!is.na(emp_all), !is.na(ksic10))


est2001 <- est2001 %>% mutate(emp_all=emp_all*weight, emp_male=emp_male*weight, emp_female=emp_female*weight) %>% select(-weight)


est2001 <- est2001 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% ungroup()


est2001 %>% haven::write_dta("data/final/hs_cpc_ksic/est2001_refine.dta")
est2001 %>% write_rds("data/final/hs_cpc_ksic/est2001_refine.rds")






#est2010


est2010 <- est2010 %>% group_by(iso, ksic9) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% ungroup()


ksic9_ksic10_1 <- ksic9_ksic10 %>% filter(weight==1)
ksic9_ksic10_2 <- ksic9_ksic10 %>% filter(weight!=1)



check <- ksic9_ksic10_2 %>% distinct(ksic9) %>% pull(ksic9)


est2010_1 <- est2010 %>% filter(!(ksic9 %in% check))

est2010_2 <- est2010 %>% filter(ksic9 %in% check)

est2010_1 <- est2010_1 %>%
  left_join(ksic9_ksic10_1, by="ksic9") %>% select(-ksic9)



ksic9_ksic10_2 <- ksic9_ksic10_2 %>%
  left_join(est2010_2, by="ksic9") %>% select(-ksic9)


est2010 <- est2010_1 %>%
  bind_rows(ksic9_ksic10_2) %>% filter(!is.na(emp_all), !is.na(ksic10))


est2010 <- est2010 %>% mutate(emp_all=emp_all*weight, emp_male=emp_male*weight, emp_female=emp_female*weight) %>% select(-weight)


est2010 <- est2010 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% ungroup()


est2010 %>% haven::write_dta("data/final/hs_cpc_ksic/est2010_refine.dta")
est2010 %>% write_rds("data/final/hs_cpc_ksic/est2010_refine.rds")




