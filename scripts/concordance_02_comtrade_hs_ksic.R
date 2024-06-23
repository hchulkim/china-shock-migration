##############################
# creator: Hyoungchul Kim
# revised date: 06/23/2024
# description: This script uses crosswalk code to transition hs code level comtrade data into ksic10 industry level.
##############################


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

# for 2001, 2010 in AUS import data, use fobvalue for import instead of cifvalue. this is due to reporting policy of AUS (probably)
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
#-------------------- 

# read in the crosswalk data
hs96_hs07 <- read_rds(here("data", "concordance", "hs_cpc_ksic", "hs96_hs07.rds"))
hs07_cpc2 <- read_rds(here("data", "concordance", "hs_cpc_ksic", "hs07_cpc2.rds"))
cpc2_cpc21 <- read_rds(here("data", "concordance", "hs_cpc_ksic", "cpc2_cpc21.rds"))
hs17_cpc21 <- read_rds(here("data", "concordance", "hs_cpc_ksic", "hs17_cpc21.rds"))
cpc21_ksic10 <- read_rds(here("data", "concordance", "hs_cpc_ksic", "cpc21_ksic10.rds"))

#-------------------- 
# 1. 2001: hs96 to hs07 
# import 2001 data conversion

# 2001 data is in hs1, hs1996 version. we need to transition this into cpc2 code.
import_data2001 <- import_data %>% 
  filter(year == 2001)

# divide the hs96 to hs07 to n:1 and 1:n case and process the transition.
# n:1 case
hs96_hs07_n1 <- hs96_hs07 %>% 
  filter(weight >= 1)

import_data2001 <- import_data2001 %>% 
  left_join(hs96_hs07_n1, by=c("hscode"="HS_1996"))

# leave out the one that has NA.
import_data2001_1n <- import_data2001 %>% 
  filter(is.na(HS_2007)) %>% 
  select(-c("HS_2007", "weight"))

import_data2001 <- import_data2001 %>% 
  filter(!is.na(HS_2007)) %>% 
  select(-c("hscode", "weight")) %>% 
  rename(hscode = HS_2007) %>% 
  mutate(hscode_type = "H3")

# take care of the 1:n case
hs96_hs07_1n <- hs96_hs07 %>% 
  filter(weight < 1)

import_data2001_1n <- import_data2001_1n %>% 
  left_join(hs96_hs07_1n, by=c("hscode"="HS_1996"), relationship = "many-to-many")
  
import_data2001_1n <- import_data2001_1n %>% 
  mutate(value = value * weight, hscode_type = "H3") %>% 
  select(-c("hscode", "weight")) %>% 
  rename(hscode = HS_2007)

import_data2001 <- import_data2001 %>% 
  bind_rows(import_data2001_1n) %>% 
  group_by(year, cntry, hscode_type, hscode) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

# drop the NA as these are products that cannot be crosswalked. These are 999999 code which means the product type is not defined from the start.
import_data2001 <- import_data2001 %>% filter(!is.na(value))

import_data_crosswalk <- import_data %>% filter(year > 2001)

import_data_crosswalk <- import_data_crosswalk %>% 
  bind_rows(import_data2001)

# export 2001 data conversion

# 2001 data is in hs1, hs1996 version. we need to transition this into cpc2 code.
export_data2001 <- export_data %>% 
  filter(year == 2001)

# divide the hs96 to hs07 to n:1 and 1:n case and process the transition.
# n:1 case
hs96_hs07_n1 <- hs96_hs07 %>% 
  filter(weight >= 1)

export_data2001 <- export_data2001 %>% 
  left_join(hs96_hs07_n1, by=c("hscode"="HS_1996"))

# leave out the one that has NA.
export_data2001_1n <- export_data2001 %>% 
  filter(is.na(HS_2007)) %>% 
  select(-c("HS_2007", "weight"))

export_data2001 <- export_data2001 %>% 
  filter(!is.na(HS_2007)) %>% 
  select(-c("hscode", "weight")) %>% 
  rename(hscode = HS_2007) %>% 
  mutate(hscode_type = "H3")

# take care of the 1:n case
hs96_hs07_1n <- hs96_hs07 %>% 
  filter(weight < 1)

export_data2001_1n <- export_data2001_1n %>% 
  left_join(hs96_hs07_1n, by=c("hscode"="HS_1996"), relationship = "many-to-many")
  
export_data2001_1n <- export_data2001_1n %>% 
  mutate(value = value * weight, hscode_type = "H3") %>% 
  select(-c("hscode", "weight")) %>% 
  rename(hscode = HS_2007)

export_data2001 <- export_data2001 %>% 
  bind_rows(export_data2001_1n) %>% 
  group_by(year, cntry, hscode_type, hscode) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

# drop the NA as these are products that cannot be crosswalked. These are 999999 code which means the product type is not defined from the start.
export_data2001 <- export_data2001 %>% filter(!is.na(value))


export_data_crosswalk <- export_data %>% filter(year > 2001)

export_data_crosswalk <- export_data_crosswalk %>% 
  bind_rows(export_data2001)



#-------------------- 
# 2. 2001 and 2010: hs07 to cpc2 
# import data conversion

# 2001, 2010 data is in hs3, hs2007 version. we need to transition this into cpc2 code.
import_data_hs07 <- import_data_crosswalk %>% 
  filter(year < 2011)

# divide the hs07 to cpc2 to n:1 and 1:n case and process the transition.
# n:1 case
hs07_cpc2_n1 <- hs07_cpc2 %>% 
  filter(weight >= 1)

import_data_hs07 <- import_data_hs07 %>% 
  left_join(hs07_cpc2_n1, by=c("hscode"="HS07Code"))

# leave out the one that has NA.
import_data_hs07_1n <- import_data_hs07 %>% 
  filter(is.na(CPC2Code)) %>% 
  select(-c("CPC2Code", "weight"))

import_data_hs07 <- import_data_hs07 %>% 
  filter(!is.na(CPC2Code)) %>% 
  select(-c("hscode", "weight")) %>% 
  rename(cpccode = CPC2Code) %>% 
  mutate(hscode_type = "cpc2")

# take care of the 1:n case
hs07_cpc2_1n <- hs07_cpc2 %>% 
  filter(weight < 1)

import_data_hs07_1n <- import_data_hs07_1n %>% 
  left_join(hs07_cpc2_1n, by=c("hscode"="HS07Code"), relationship = "many-to-many")

import_data_hs07_1n <- import_data_hs07_1n %>% 
  mutate(value = value * weight, hscode_type = "cpc2") %>% 
  select(-c("hscode", "weight")) %>% 
  rename(cpccode = CPC2Code)

import_data_hs07 <- import_data_hs07 %>% 
  bind_rows(import_data_hs07_1n) %>% 
  group_by(year, cntry, hscode_type, cpccode) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

# drop the NA as these are products that cannot be crosswalked. 
import_data_hs07 <- import_data_hs07 %>% filter(!is.na(value))

import_data_crosswalk <- import_data_crosswalk %>% filter(year > 2010)

import_data_crosswalk <- import_data_crosswalk %>% 
  bind_rows(import_data_hs07)




# export data conversion

# 2001, 2010 data is in hs3, hs2007 version. we need to transition this into cpc2 code.
export_data_hs07 <- export_data_crosswalk %>% 
  filter(year < 2011)

# divide the hs07 to cpc2 to n:1 and 1:n case and process the transition.
# n:1 case
hs07_cpc2_n1 <- hs07_cpc2 %>% 
  filter(weight >= 1)

export_data_hs07 <- export_data_hs07 %>% 
  left_join(hs07_cpc2_n1, by=c("hscode"="HS07Code"))

# leave out the one that has NA.
export_data_hs07_1n <- export_data_hs07 %>% 
  filter(is.na(CPC2Code)) %>% 
  select(-c("CPC2Code", "weight"))

export_data_hs07 <- export_data_hs07 %>% 
  filter(!is.na(CPC2Code)) %>% 
  select(-c("hscode", "weight")) %>% 
  rename(cpccode = CPC2Code) %>% 
  mutate(hscode_type = "cpc2")

# take care of the 1:n case
hs07_cpc2_1n <- hs07_cpc2 %>% 
  filter(weight < 1)

export_data_hs07_1n <- export_data_hs07_1n %>% 
  left_join(hs07_cpc2_1n, by=c("hscode"="HS07Code"), relationship = "many-to-many")

export_data_hs07_1n <- export_data_hs07_1n %>% 
  mutate(value = value * weight, hscode_type = "cpc2") %>% 
  select(-c("hscode", "weight")) %>% 
  rename(cpccode = CPC2Code)

export_data_hs07 <- export_data_hs07 %>% 
  bind_rows(export_data_hs07_1n) %>% 
  group_by(year, cntry, hscode_type, cpccode) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

# drop the NA as these are products that cannot be crosswalked. 
export_data_hs07 <- export_data_hs07 %>% filter(!is.na(value))

export_data_crosswalk <- export_data_crosswalk %>% filter(year > 2010)

export_data_crosswalk <- export_data_crosswalk %>% 
  bind_rows(export_data_hs07)



#-------------------- 
# 3. 2001 and 2010: cpc2 to cpc21 
# import data conversion

# 2001, 2010 data is in cpc2 version. we need to transition this into cpc21 code.
import_data_cpc2 <- import_data_crosswalk %>% 
  filter(year < 2011) %>% 
  select(-hscode)

# divide the cpc2 to cpc21 to n:1 and 1:n case and process the transition.
# n:1 case
cpc2_cpc21_n1 <- cpc2_cpc21 %>% 
  filter(weight >= 1)

import_data_cpc2 <- import_data_cpc2 %>% 
  left_join(cpc2_cpc21_n1, by=c("cpccode"="CPC2code"))

# leave out the one that has NA.
import_data_cpc2_1n <- import_data_cpc2 %>% 
  filter(is.na(CPC21code)) %>% 
  select(-c("CPC21code", "weight"))

import_data_cpc2 <- import_data_cpc2 %>% 
  filter(!is.na(CPC21code)) %>% 
  select(-c("cpccode", "weight")) %>% 
  rename(cpccode = CPC21code) %>% 
  mutate(hscode_type = "cpc21")

# take care of the 1:n case
cpc2_cpc21_1n <- cpc2_cpc21 %>% 
  filter(weight < 1)

import_data_cpc2_1n <- import_data_cpc2_1n %>% 
  left_join(cpc2_cpc21_1n, by=c("cpccode"="CPC2code"), relationship = "many-to-many")

import_data_cpc2_1n <- import_data_cpc2_1n %>% 
  mutate(value = value * weight, hscode_type = "cpc21") %>% 
  select(-c("cpccode", "weight")) %>% 
  rename(cpccode = CPC21code)

import_data_cpc2 <- import_data_cpc2 %>% 
  bind_rows(import_data_cpc2_1n) %>% 
  group_by(year, cntry, hscode_type, cpccode) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

# drop the NA as these are products that cannot be crosswalked.
import_data_cpc2 <- import_data_cpc2 %>% filter(!is.na(value))

import_data_crosswalk <- import_data_crosswalk %>% filter(year > 2010)

import_data_crosswalk <- import_data_crosswalk %>% 
  bind_rows(import_data_cpc2)


# export data conversion

# 2001, 2010 data is in cpc2 version. we need to transition this into cpc21 code.
export_data_cpc2 <- export_data_crosswalk %>% 
  filter(year < 2011) %>% 
  select(-hscode)

# divide the cpc2 to cpc21 to n:1 and 1:n case and process the transition.
# n:1 case
cpc2_cpc21_n1 <- cpc2_cpc21 %>% 
  filter(weight >= 1)

export_data_cpc2 <- export_data_cpc2 %>% 
  left_join(cpc2_cpc21_n1, by=c("cpccode"="CPC2code"))

# leave out the one that has NA.
export_data_cpc2_1n <- export_data_cpc2 %>% 
  filter(is.na(CPC21code)) %>% 
  select(-c("CPC21code", "weight"))

export_data_cpc2 <- export_data_cpc2 %>% 
  filter(!is.na(CPC21code)) %>% 
  select(-c("cpccode", "weight")) %>% 
  rename(cpccode = CPC21code) %>% 
  mutate(hscode_type = "cpc21")

# take care of the 1:n case
cpc2_cpc21_1n <- cpc2_cpc21 %>% 
  filter(weight < 1)

export_data_cpc2_1n <- export_data_cpc2_1n %>% 
  left_join(cpc2_cpc21_1n, by=c("cpccode"="CPC2code"), relationship = "many-to-many")

export_data_cpc2_1n <- export_data_cpc2_1n %>% 
  mutate(value = value * weight, hscode_type = "cpc21") %>% 
  select(-c("cpccode", "weight")) %>% 
  rename(cpccode = CPC21code)

export_data_cpc2 <- export_data_cpc2 %>% 
  bind_rows(export_data_cpc2_1n) %>% 
  group_by(year, cntry, hscode_type, cpccode) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

# drop the NA as these are products that cannot be crosswalked.
export_data_cpc2 <- export_data_cpc2 %>% filter(!is.na(value))

export_data_crosswalk <- export_data_crosswalk %>% filter(year > 2010)

export_data_crosswalk <- export_data_crosswalk %>% 
  bind_rows(export_data_cpc2)




#-------------------- 
# 4. 2019 and 2020: hs17 to cpc21 
# import data conversion

# 2019, 2020 data is in hs17 version. we need to transition this into cpc21 code.
import_data_hs17 <- import_data_crosswalk %>% 
  filter(year > 2010) %>% 
  select(-cpccode)

# divide the hs17 to cpc21 to n:1 and 1:n case and process the transition.
# n:1 case
hs17_cpc21_n1 <- hs17_cpc21 %>% 
  filter(weight >= 1)

import_data_hs17 <- import_data_hs17 %>% 
  left_join(hs17_cpc21_n1, by=c("hscode"="HS2017"))

# leave out the one that has NA.
import_data_hs17_1n <- import_data_hs17 %>% 
  filter(is.na(CPC21)) %>% 
  select(-c("CPC21", "weight"))

import_data_hs17 <- import_data_hs17 %>% 
  filter(!is.na(CPC21)) %>% 
  select(-c("hscode", "weight")) %>% 
  rename(cpccode = CPC21) %>% 
  mutate(hscode_type = "cpc21")

# take care of the 1:n case
hs17_cpc21_1n <- hs17_cpc21 %>% 
  filter(weight < 1)

import_data_hs17_1n <- import_data_hs17_1n %>% 
  left_join(hs17_cpc21_1n, by=c("hscode"="HS2017"), relationship = "many-to-many")

import_data_hs17_1n <- import_data_hs17_1n %>% 
  mutate(value = value * weight, hscode_type = "cpc21") %>% 
  select(-c("hscode", "weight")) %>% 
  rename(cpccode = CPC21)

import_data_hs17 <- import_data_hs17 %>% 
  bind_rows(import_data_hs17_1n) %>% 
  group_by(year, cntry, hscode_type, cpccode) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

# drop the NA as these are products that cannot be crosswalked. case of 999999 code.
import_data_hs17 <- import_data_hs17 %>% filter(!is.na(value))

import_data_crosswalk <- import_data_crosswalk %>% filter(year < 2011) %>% 
  select(-hscode)

import_data_crosswalk <- import_data_crosswalk %>% 
  bind_rows(import_data_hs17)




# export data conversion

# 2019, 2020 data is in hs17 version. we need to transition this into cpc21 code.
export_data_hs17 <- export_data_crosswalk %>% 
  filter(year > 2010) %>% 
  select(-cpccode)

# divide the hs17 to cpc21 to n:1 and 1:n case and process the transition.
# n:1 case
hs17_cpc21_n1 <- hs17_cpc21 %>% 
  filter(weight >= 1)

export_data_hs17 <- export_data_hs17 %>% 
  left_join(hs17_cpc21_n1, by=c("hscode"="HS2017"))

# leave out the one that has NA.
export_data_hs17_1n <- export_data_hs17 %>% 
  filter(is.na(CPC21)) %>% 
  select(-c("CPC21", "weight"))

export_data_hs17 <- export_data_hs17 %>% 
  filter(!is.na(CPC21)) %>% 
  select(-c("hscode", "weight")) %>% 
  rename(cpccode = CPC21) %>% 
  mutate(hscode_type = "cpc21")

# take care of the 1:n case
hs17_cpc21_1n <- hs17_cpc21 %>% 
  filter(weight < 1)

export_data_hs17_1n <- export_data_hs17_1n %>% 
  left_join(hs17_cpc21_1n, by=c("hscode"="HS2017"), relationship = "many-to-many")

export_data_hs17_1n <- export_data_hs17_1n %>% 
  mutate(value = value * weight, hscode_type = "cpc21") %>% 
  select(-c("hscode", "weight")) %>% 
  rename(cpccode = CPC21)

export_data_hs17 <- export_data_hs17 %>% 
  bind_rows(export_data_hs17_1n) %>% 
  group_by(year, cntry, hscode_type, cpccode) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

# drop the NA as these are products that cannot be crosswalked. case of 999999 code.
export_data_hs17 <- export_data_hs17 %>% filter(!is.na(value))

export_data_crosswalk <- export_data_crosswalk %>% filter(year < 2011) %>% 
  select(-hscode)

export_data_crosswalk <- export_data_crosswalk %>% 
  bind_rows(export_data_hs17)





#-------------------- 
# 5. all data: cpc21 to ksic10 
# import data conversion

import_data_cpc21 <- import_data_crosswalk  

# divide the cpc21 to ksic10 to n:1 and 1:n case and process the transition.
# n:1 case
cpc21_ksic10_n1 <- cpc21_ksic10 %>% 
  filter(weight >= 1)

import_data_cpc21 <- import_data_cpc21 %>% 
  left_join(cpc21_ksic10_n1, by=c("cpccode"="cpc21"))

# leave out the one that has NA.
import_data_cpc21_1n <- import_data_cpc21 %>% 
  filter(is.na(ksic10)) %>% 
  select(-c("ksic10", "weight"))

import_data_cpc21 <- import_data_cpc21 %>% 
  filter(!is.na(ksic10)) %>% 
  select(-c("cpccode", "weight")) %>% 
  rename(ksiccode = ksic10) %>% 
  mutate(hscode_type = "ksic10")

# take care of the 1:n case
cpc21_ksic10_1n <- cpc21_ksic10 %>% 
  filter(weight < 1)

import_data_cpc21_1n <- import_data_cpc21_1n %>% 
  left_join(cpc21_ksic10_1n, by=c("cpccode"="cpc21"), relationship = "many-to-many")

import_data_cpc21_1n <- import_data_cpc21_1n %>% 
  mutate(value = value * weight, hscode_type = "ksic10") %>% 
  select(-c("cpccode", "weight")) %>% 
  rename(ksiccode = ksic10)

import_data_cpc21 <- import_data_cpc21 %>% 
  bind_rows(import_data_cpc21_1n) %>% 
  group_by(year, cntry, hscode_type, ksiccode) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

# drop the NA as these are products that cannot be crosswalked.
import_data_cpc21 <- import_data_cpc21 %>% filter(!is.na(value))

import_data_crosswalk <- import_data_cpc21 

import_data_crosswalk <- import_data_crosswalk %>% rename(code_type = hscode_type)


# export data conversion

export_data_cpc21 <- export_data_crosswalk  

# divide the cpc21 to ksic10 to n:1 and 1:n case and process the transition.
# n:1 case
cpc21_ksic10_n1 <- cpc21_ksic10 %>% 
  filter(weight >= 1)

export_data_cpc21 <- export_data_cpc21 %>% 
  left_join(cpc21_ksic10_n1, by=c("cpccode"="cpc21"))

# leave out the one that has NA.
export_data_cpc21_1n <- export_data_cpc21 %>% 
  filter(is.na(ksic10)) %>% 
  select(-c("ksic10", "weight"))

export_data_cpc21 <- export_data_cpc21 %>% 
  filter(!is.na(ksic10)) %>% 
  select(-c("cpccode", "weight")) %>% 
  rename(ksiccode = ksic10) %>% 
  mutate(hscode_type = "ksic10")

# take care of the 1:n case
cpc21_ksic10_1n <- cpc21_ksic10 %>% 
  filter(weight < 1)

export_data_cpc21_1n <- export_data_cpc21_1n %>% 
  left_join(cpc21_ksic10_1n, by=c("cpccode"="cpc21"), relationship = "many-to-many")

export_data_cpc21_1n <- export_data_cpc21_1n %>% 
  mutate(value = value * weight, hscode_type = "ksic10") %>% 
  select(-c("cpccode", "weight")) %>% 
  rename(ksiccode = ksic10)

export_data_cpc21 <- export_data_cpc21 %>% 
  bind_rows(export_data_cpc21_1n) %>% 
  group_by(year, cntry, hscode_type, ksiccode) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()

# drop the NA as these are products that cannot be crosswalked. 
export_data_cpc21 <- export_data_cpc21 %>% filter(!is.na(value))

export_data_crosswalk <- export_data_cpc21 

export_data_crosswalk <- export_data_crosswalk %>% rename(code_type = hscode_type)



#-------------------- 
# Compare the aggregate value between origin and transformed data
#-------------------- 

import <- import_data %>% 
  group_by(year, cntry) %>% 
  summarise(old_value = sum(value)) %>% 
  ungroup()

import_new <- import_data_crosswalk %>% 
  group_by(year, cntry) %>% 
  summarise(new_value = sum(value)) %>% 
  ungroup()

import <- import %>% 
  left_join(import_new, by=c("year", "cntry"))

import <- import %>% 
  mutate(difference = new_value / old_value)

import %>% write_rds(here("proc", "import_comtrade_data_transition_loss.rds"))
# result: retains at least 94%.

export <- export_data %>% 
  group_by(year, cntry) %>% 
  summarise(old_value = sum(value)) %>% 
  ungroup()

export_new <- export_data_crosswalk %>% 
  group_by(year, cntry) %>% 
  summarise(new_value = sum(value)) %>% 
  ungroup()

export <- export %>% 
  left_join(export_new, by=c("year", "cntry"))

export <- export %>% 
  mutate(difference = new_value / old_value)

export %>% write_rds(here("proc", "export_comtrade_data_transition_loss.rds"))

# result: retains at least 88~91%.

#--------------------
# Download the final comtrade data that we transfromed from hs code to ksic10.
#--------------------

import_data_crosswalk %>% write_rds(here("data", "temp", "import_data_ksic10.rds"))
export_data_crosswalk %>% write_rds(here("data", "temp", "export_data_ksic10.rds"))
