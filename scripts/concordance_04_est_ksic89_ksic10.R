
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
est1994 <- est1994 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5), year = 1994)
est1996 <- est1996 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5), year = 1996)
est1999 <- est1999 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5), year = 1999)
est2000 <- est2000 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5), year = 2000)
  


est2001 <- est2001 %>% mutate(ksic8=paste0(`산업분류(중)`, `산업분류(소)`, `산업분류(세)`, `산업분류(세세)`), emp_female=`8-⑥종사자_합계_여(①+…+⑤)`, emp_male=`8-⑥종사자_합계_남(①+…+⑤)`, emp_all=`8-⑥종사자_합계_계(①+…+⑤)`) %>% select(iso, ksic8, emp_female, emp_male, emp_all) %>% 
  mutate(year = 2001)


est2010 <- est2010 %>% mutate(ksic9=paste0(`산업분류_중`, `산업분류_소`, `산업분류_세`, `산업분류_세세`), emp_female=`종사자수_합계_여`, emp_male=`종사자수_합계_남`, emp_all=`종사자수_합계_계`) %>% select(iso, ksic9, emp_female, emp_male, emp_all) %>% 
  mutate(year = 2010)



est2019 <- est2019 %>% mutate(ksic10=paste0(`주사업_산업중분류코드`, `주사업_산업소분류코드`, `주사업_산업세분류코드`, `주사업_산업세세분류코드`)) %>% 
  select(iso, ksic10, emp_female=`여자종사자수`, emp_male=`남자종사자수`, emp_all=`합계종사자수`) %>% 
  mutate(year = 2019)

# save 2019 as it is already ksic10

est2019 <- est2019 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_female=sum(emp_female, na.rm=T), emp_male=sum(emp_male, na.rm=T)) %>% ungroup() %>% 
  mutate(year = 2019)

est2019 %>% write_rds(here("data", "temp", "est2019_region_industry_matched.rds"))



# ksic 8 to 9: 1994, 1996, 1999, 2000, 2001

ksic89_list <- list(est1994, est1996, est1999, est2000, est2001)

# Define the function to process each dataframe
process_ksic89 <- function(df) {
  # Perform desired operations on the dataframe
  
# divide the ksic8 to ksic9 to n:1 and 1:n case and process the transition.
# n:1 case
  ksic8_ksic9_n1 <- ksic8_ksic9 %>% 
   filter(weight >= 1)

  df <- df %>% 
    left_join(ksic8_ksic9_n1, by="ksic8")

  # leave out the one that has NA.
  df_1n <- df %>% 
    filter(is.na(ksic9)) %>% 
    select(-c("ksic9", "weight"))
  
  df <- df %>% 
    filter(!is.na(ksic9)) %>% 
    select(-weight) %>% 
    select(-ksic8)
  
  # take care of the 1:n case
  ksic8_ksic9_1n <- ksic8_ksic9 %>% 
    filter(weight < 1)
  
  df_1n <- df_1n %>% 
    left_join(ksic8_ksic9_1n, by="ksic8", relationship = "many-to-many")
  
  df_1n <- df_1n %>% 
    mutate(emp_all = emp_all * weight) %>% 
    select(-c("ksic8", "weight"))
  
  df <- df %>% 
    bind_rows(df_1n) %>% 
    group_by(iso, year, ksic9) %>% 
    summarise(emp_all = sum(emp_all, na.rm = T)) %>% 
    ungroup()

  return(df)
}

# Apply the function to each dataframe in the list
processed_ksic89 <- map(ksic89_list, process_ksic89) %>% 
  list_rbind()



# ksic 9 to 10: 1994, 1996, 1999, 2000, 2001, 2010

ksic910_list <- list(processed_ksic89, est2010)

# Define the function to process each dataframe
process_ksic910 <- function(df) {
  # Perform desired operations on the dataframe
  
# divide the ksic9 to ksic9 to n:1 and 1:n case and process the transition.
# n:1 case
  ksic9_ksic10_n1 <- ksic9_ksic10 %>% 
   filter(weight >= 1)

  df <- df %>% 
    left_join(ksic9_ksic10_n1, by="ksic9")

  # leave out the one that has NA.
  df_1n <- df %>% 
    filter(is.na(ksic10)) %>% 
    select(-c("ksic10", "weight"))
  
  df <- df %>% 
    filter(!is.na(ksic10)) %>% 
    select(-weight) %>% 
    select(-ksic9)
  
  # take care of the 1:n case
  ksic9_ksic10_1n <- ksic9_ksic10 %>% 
    filter(weight < 1)
  
  df_1n <- df_1n %>% 
    left_join(ksic9_ksic10_1n, by="ksic9", relationship = "many-to-many")
  
  df_1n <- df_1n %>% 
    mutate(emp_all = emp_all * weight) %>% 
    select(-c("ksic9", "weight"))
  
  df <- df %>% 
    bind_rows(df_1n) %>% 
    group_by(iso, year, ksic10) %>% 
    summarise(emp_all = sum(emp_all, na.rm = T)) %>% 
    ungroup()

  return(df)
}

# Apply the function to each dataframe in the list
processed_ksic910 <- map(ksic910_list, process_ksic910) %>% 
  list_rbind()


# rbind all the ksic10 est data.
processed_ksic10 <- processed_ksic910 %>% 
  bind_rows(est2019 %>% 
              group_by(iso, year, ksic10) %>% 
              summarise(emp_all = sum(emp_all, na.rm = T)) %>% 
              ungroup())

# download the finalized data
processed_ksic10 %>% write_rds(here("data", "temp", "est_region_industry_matched.rds"))
