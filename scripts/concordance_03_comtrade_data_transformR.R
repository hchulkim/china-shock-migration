


if (!require(pacman)) install.packages("pacman")
pacman::p_load(fs, here, gcamdata, tidyverse)

# read in the comtrade data
comtrade_data <- dir_ls(path = here("data/comtrade"), glob = "*.csv") %>% 
  map(read_csv) %>% 
  list_rbind()

# only select necessary columns
comtrade_data <- comtrade_data %>%
  select(year = RefYear, cntry = ReporterISO, flow_type = FlowDesc, hscode = CmdCode, hscode_type = ClassificationCode, fobvalue = Fobvalue, cifvalue = Cifvalue)

# This checks if there are some inconsistencies with the trade value for each countries.
if (FALSE) {
 # list countries used in the data
 country <- comtrade_data %>% 
   select(cntry) %>%
   distinct() %>% 
   pull()
 
 # check if import = cif value, export = fob value. also see if there is NA.
 
 check <- map_df(country, function(cty) {
   comtrade_data %>%
     filter(cntry == cty) %>%
     mutate(na_check = case_when(
       flow_type == "Import" & is.na(cifvalue) ~ 1,
       flow_type == "Export" & is.na(fobvalue) ~ 1,
       TRUE ~ 0 
     )) %>%
     group_by(cntry, flow_type) %>%
     summarise(na_check = sum(na_check), .groups = 'drop') %>%
     pivot_wider(names_from = flow_type, values_from = na_check)
 }) 

 # result: the only problem is the import part in AUS in year 2001, 2010.
 # solution: For 2001, 2010, use fobvalue for import.
}

# for 2001, 2010 in AUS import data, use fobvalue for import instead of cifvalue. this is due to reporting policy of AUS (probally)
comtrade_data <- comtrade_data %>% 
  mutate(cifvalue = if_else(cntry == "AUS" & year < 2019, fobvalue, cifvalue))

# use gdp deflator package gcamdata to set all values into 2019.
comtrade_data <- comtrade_data %>% 
  mutate(cifvalue = case_when(
    year == 2001 ~ cifvalue * gdp_deflator(2019, base_year = 2001),
    year == 2010 ~ cifvalue * gdp_deflator(2019, base_year = 2010),
    .default = cifvalue 
  ), 
  fobvalue = case_when(
    year == 2001 ~ fobvalue * gdp_deflator(2019, base_year = 2001),
    year == 2010 ~ fobvalue * gdp_deflator(2019, base_year = 2010),
    .default = fobvalue 
  ))

# separate data into two parts by export and import
import_data <- comtrade_data %>% 
  filter(flow_type == "Import") %>% 
  select(-c("flow_type", "fobvalue")) %>% 
  rename(value = cifvalue)

export_data <- comtrade_data %>% 
  filter(flow_type == "Export") %>% 
  select(-c("flow_type", "cifvalue")) %>% 
  rename(value = fobvalue)

# save the processed data into temp folder
import_data %>% write_rds("data/temp/import_data.rds")
export_data %>% write_rds("data/temp/export_data.rds")

#-------------------- 
# transform the hs code to cpc code.



