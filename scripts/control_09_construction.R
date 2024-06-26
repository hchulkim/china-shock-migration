




if (!require(pacman)) install.packages("pacman")
pacman::p_load(fs, readxl, here, tidyverse)


# construct weight ( 2001 and 2010)
#read in population

# Get the list of file paths
population_files <- dir_ls(path = here("data", "population"), glob = "*.xlsx")

# Create a named list by reading each file into a data frame and naming each element with the filename (without the path and extension)
population <- population_files %>%
  set_names(nm = basename(tools::file_path_sans_ext(.))) %>%
  map(read_excel, skip = 2, na = "-", col_names = c("iso", "pop")) %>% 
  list_rbind(names_to = "year")


population <- population %>% 
  mutate(year = as.numeric(str_replace(year, "pop", "")), iso=str_replace_all(iso, " ", "")) %>% 
  select(iso, year, pop)

population <- population %>% 
  mutate(iso = str_extract(iso, "\\d{5}"))

population <- population %>% 
  filter(!is.na(iso))



# read in the concordance for region stat----
region_stat <- readxl::read_excel(here("data", "concordance", "region", "region_stat.xlsx"))

region_stat <- region_stat %>% mutate(across(everything(), as.character))

region_kosis <- readxl::read_excel(here("data", "concordance", "region", "region_kosis.xlsx"))

region_kosis <- region_kosis %>% mutate(across(everything(), as.character))

region_kosis <- region_kosis %>% mutate(city_stat=str_sub(city_stat, 1, 5))


## important: due to change in code of certain province, I manually change such case.
population <- population %>% 
  mutate(iso = ifelse(str_sub(iso, 1, 2) == "51", paste0("42", str_sub(iso, 3, 5)), iso))

population <- population %>% 
  mutate(iso = ifelse(str_sub(iso, 1, 2) == "52", paste0("45", str_sub(iso, 3, 5)), iso))

# now use region_stat to crosswalk change in regions (mostly to 2001 regions) -------
population <- population %>% 
  mutate(iso = ifelse(iso == "44825", "70000", iso))

population <- population %>% 
  mutate(iso = ifelse(iso == "43745", "80000", iso))

# population <- population %>% 
  # mutate(iso = ifelse(   as.numeric(str_sub(iso, 1, 2))>=40, paste0(str_sub(iso, 1, 4), "0"), iso))

population <- population %>% left_join(region_kosis, by=c("iso"="city_kosis"))

population <- population %>% select(-iso) %>% rename(iso = city_stat)

population <- population %>% select(-city_label)

# filter out na
population <- population %>% filter(!is.na(iso))



# now use stat region case

 population <- population %>% left_join(region_stat, by=c("iso"="city_stat_raw"))

 population <- population %>% 
   mutate(iso = ifelse(!is.na(city_stat), city_stat, iso))
 population <- population %>% select(-city_stat)

 
 
 population <- population %>% 
   group_by(year, iso) %>% 
   summarise(pop = sum(pop, na.rm = T)) %>% 
   ungroup()
 
# read in cz data
cz <- read_rds(here("proc", "commuting_zone.rds")) %>% 
  select(iso, cz_id, cz_label)

population <- population %>% 
  left_join(cz, by="iso")



# by sigungu
pop_sigungu <- population %>% 
  filter(year<2020) %>% 
  group_by(year, iso) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

pop_sigungu %>% write_rds("data/temp/weight_sigungu.rds")

# by cz
pop_cz <- population %>% 
  filter(year<2020) %>% 
  group_by(year, cz_id) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()

pop_cz %>% write_rds("data/temp/weight_cz.rds")





# employ share of female
est2000 <-read_rds(here("data", "temp", "est2000_region_matched.rds"))
est2010 <-read_rds(here("data", "temp", "est2010_region_matched.rds"))

## 2000
# sigungu
est2000 <- est2000 %>% 
  group_by(iso) %>% 
  summarise(emp_all = sum(emp_all, na.rm = T), emp_male = sum(emp_male, na.rm=T), emp_female = sum(emp_female, na.rm=T)) %>% 
  ungroup()

est2000 <- est2000 %>% 
  mutate(sh_female = emp_female / emp_all) 

est2000 %>% write_rds("data/temp/control_emp_female_sigungu.rds")

# cz
est2000 <- est2000 %>% 
  left_join(cz, by="iso")


est2000 <- est2000 %>% 
  group_by(cz_id) %>% 
  summarise(emp_all = sum(emp_all, na.rm = T), emp_male = sum(emp_male, na.rm=T), emp_female = sum(emp_female, na.rm=T)) %>% 
  ungroup()

est2000 <- est2000 %>% 
  mutate(sh_female = emp_female / emp_all) 

est2000 %>% write_rds("data/temp/control_emp_female_cz.rds")

## 2010

# sigungu
est2010 <- est2010 %>% 
  group_by(iso) %>% 
  summarise(emp_all = sum(`종사자수_합계_계`, na.rm = T), emp_male = sum(`종사자수_합계_남`, na.rm=T), emp_female = sum(`종사자수_합계_여`, na.rm=T)) %>% 
  ungroup()

est2010 <- est2010 %>% 
  mutate(sh_female = emp_female / emp_all) 

est2010 %>% write_rds("data/temp/control_emp_female2010_sigungu.rds")

# cz
est2010 <- est2010 %>% 
  left_join(cz, by="iso")


est2010 <- est2010 %>% 
  group_by(cz_id) %>% 
  summarise(emp_all = sum(emp_all, na.rm = T), emp_male = sum(emp_male, na.rm=T), emp_female = sum(emp_female, na.rm=T)) %>% 
  ungroup()

est2010 <- est2010 %>% 
  mutate(sh_female = emp_female / emp_all) 

est2010 %>% write_rds("data/temp/control_emp_female2010_cz.rds")


## share of manu
est_data <- read_rds((here("data", "temp", "est_region_industry_matched.rds")))

est_data <- est_data %>% 
  mutate(manu = ifelse(as.numeric(str_sub(ksic10, 1, 2))>=10 & as.numeric(str_sub(ksic10, 1, 2))<=34, 1, 0 ))

est_data <- est_data %>% 
  filter(year==2000 | year==2010)

est_data <- est_data %>% 
  left_join(cz, by="iso")

est_data <- est_data %>% 
  mutate(manu = manu * emp_all)

#sigungu
est_data_sigungu <- est_data %>% 
  group_by(iso, year) %>% 
  summarise(emp_all = sum(emp_all, na.rm=T), manu = sum(manu, na.rm = T)) %>% 
  ungroup()
est_data_sigungu <- est_data_sigungu %>% 
  mutate(sh_manu = manu / emp_all)

est_data_sigungu %>% write_rds(here("data", "temp", "control_manu_share_sigungu.rds"))

# cz
est_data_cz <- est_data %>% 
  group_by(cz_id, year) %>% 
  summarise(emp_all = sum(emp_all, na.rm=T), manu = sum(manu, na.rm = T)) %>% 
  ungroup()
est_data_cz <- est_data_cz %>% 
  mutate(sh_manu = manu / emp_all)

est_data_cz%>% write_rds(here("data", "temp", "control_manu_share_cz.rds"))


# foreign pop

# Get the list of file paths
foreign_files <- dir_ls(path = here("data", "controls"), regexp = "for")

# Create a named list by reading each file into a data frame and naming each element with the filename (without the path and extension)
foreign <- foreign_files %>%
  set_names(nm = basename(tools::file_path_sans_ext(.))) %>%
  map(read_excel, skip = 2, na = "-", col_names = c("iso", "pop")) %>% 
  list_rbind(names_to = "year")


foreign <- foreign %>% 
  mutate(year = as.numeric(str_replace(year, "for", "")), iso=str_replace_all(iso, " ", "")) %>% 
  select(iso, year, pop)

foreign <- foreign %>% 
  mutate(iso = str_extract(iso, "\\d{5}"))

 foreign <- foreign %>% 
  filter(!is.na(iso))

# read in the concordance for region stat----
region_stat <- readxl::read_excel(here("data", "concordance", "region", "region_stat.xlsx"))

region_stat <- region_stat %>% mutate(across(everything(), as.character))

region_kosis <- readxl::read_excel(here("data", "concordance", "region", "region_kosis.xlsx"))

region_kosis <- region_kosis %>% mutate(across(everything(), as.character))

region_kosis <- region_kosis %>% mutate(city_stat=str_sub(city_stat, 1, 5))



# now use stat region case

foreign <- foreign %>% left_join(region_stat, by=c("iso"="city_stat_raw"))

foreign <- foreign %>% 
  mutate(iso = ifelse(!is.na(city_stat), city_stat, iso))
foreign <- foreign %>% select(-city_stat)



foreign <- foreign %>% 
  group_by(year, iso) %>% 
  summarise(pop = sum(pop, na.rm = T)) %>% 
  ungroup()

foreign <- foreign %>% 
  filter(str_sub(iso, 5, 5)=="0")

# also get pop 2000, 2010-------

# Get the list of file paths
population_files <- dir_ls(path = here("data", "controls"), regexp = "pop")

# Create a named list by reading each file into a data frame and naming each element with the filename (without the path and extension)
population <- population_files %>%
  set_names(nm = basename(tools::file_path_sans_ext(.))) %>%
  map(read_excel, skip = 2, na = "-", col_names = c("iso", "pop")) %>% 
  list_rbind(names_to = "year")


population <- population %>% 
  mutate(year = as.numeric(str_replace(year, "pop", "")), iso=str_replace_all(iso, " ", "")) %>% 
  select(iso, year, pop)

population <- population %>% 
  mutate(iso = str_extract(iso, "\\d{5}"))

population <- population %>% 
  filter(!is.na(iso))



# read in the concordance for region stat----
region_stat <- readxl::read_excel(here("data", "concordance", "region", "region_stat.xlsx"))

region_stat <- region_stat %>% mutate(across(everything(), as.character))

region_kosis <- readxl::read_excel(here("data", "concordance", "region", "region_kosis.xlsx"))

region_kosis <- region_kosis %>% mutate(across(everything(), as.character))

region_kosis <- region_kosis %>% mutate(city_stat=str_sub(city_stat, 1, 5))


## important: due to change in code of certain province, I manually change such case.
population <- population %>% 
  mutate(iso = ifelse(str_sub(iso, 1, 2) == "51", paste0("42", str_sub(iso, 3, 5)), iso))

population <- population %>% 
  mutate(iso = ifelse(str_sub(iso, 1, 2) == "52", paste0("45", str_sub(iso, 3, 5)), iso))

# now use region_stat to crosswalk change in regions (mostly to 2001 regions) -------
population <- population %>% 
  mutate(iso = ifelse(iso == "44825", "70000", iso))

population <- population %>% 
  mutate(iso = ifelse(iso == "43745", "80000", iso))

# population <- population %>% 
# mutate(iso = ifelse(   as.numeric(str_sub(iso, 1, 2))>=40, paste0(str_sub(iso, 1, 4), "0"), iso))

population <- population %>% left_join(region_kosis, by=c("iso"="city_kosis"))

population <- population %>% select(-iso) %>% rename(iso = city_stat)

population <- population %>% select(-city_label)

# filter out na
population <- population %>% filter(!is.na(iso))



# now use stat region case

population <- population %>% left_join(region_stat, by=c("iso"="city_stat_raw"))

population <- population %>% 
  mutate(iso = ifelse(!is.na(city_stat), city_stat, iso))
population <- population %>% select(-city_stat)



population <- population %>% 
  group_by(year, iso) %>% 
  summarise(pop = sum(pop, na.rm = T)) %>% 
  ungroup()

# read in cz data
cz <- read_rds(here("proc", "commuting_zone.rds")) %>% 
  select(iso, cz_id, cz_label)

population <- population %>% 
  left_join(cz, by="iso")
  
foreign <- population %>% 
  left_join(foreign, by=c("year", "iso"))

#sigungu
foreign_sigungu <- foreign %>% 
  mutate(sh_for = pop.y / pop.x) %>% 
  select(year, iso, sh_for)
foreign_sigungu %>% write_rds(here("data", "temp", "control_for_share_sigungu.rds"))

# cz
foreign_cz <- foreign %>% 
  group_by(year, cz_id) %>% 
  summarise(pop.x = sum(pop.x), pop.y = sum(pop.y)) %>% 
  ungroup() %>% 
  mutate(sh_for = pop.y / pop.x) %>% 
  select(year, cz_id, sh_for)
foreign_cz %>% write_rds(here("data", "temp", "control_for_share_cz.rds"))


#college education
