

##############################
# creator: Hyoungchul Kim
# revised date: 06/25/2024
# description: This code matches and merges region in population data. 
##############################



if (!require(pacman)) install.packages("pacman")
pacman::p_load(fs, readxl, here, tidyverse)


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

# also get the net pop growth
 
# read in cz data
cz <- read_rds(here("proc", "commuting_zone.rds")) %>% 
  select(iso, cz_id, cz_label)

population <- population %>% 
  left_join(cz, by="iso")

# sigungu version
population_growth_sigungu <- population %>% 
  group_by(iso) %>% 
  mutate(pop_lag = lag(pop)) %>% 
  mutate(pop_gr = pop - pop_lag) %>% 
  select(year, iso, cz_id, cz_label, pop_gr) %>% 
  ungroup()

population_growth_sigungu %>% 
  write_rds(here("data", "temp", "pop_growth_sigungu.rds"))

# cz level

population_growth_cz <- population %>% 
  group_by(year, cz_id) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup()
  
  
population_growth_cz <- population_growth_cz %>%  
  group_by(cz_id) %>% 
  mutate(pop_lag = lag(pop)) %>% 
  mutate(pop_gr = pop - pop_lag) %>% 
  select(year, cz_id, pop_gr) %>% 
  ungroup()

population_growth_cz %>% 
  write_rds(here("data", "temp", "pop_growth_cz.rds"))



