if (!require(remotes)) (install.packages("remotes"))
if (!require(gcamdata)) (remotes::install_github("JGCRI/gcamdata"))



if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(here, gt, data.table, sf, gcamdata, tidyverse)


# this code is to merge 2001-2010, 2010-2019 trade data and see how well they are linked. Also it will make the total import and export change by industry. 


#we are making iv


## for jpn -- chn


# read in import and export data

jpn_from_chn_import2001 <- haven::read_dta("data/final/hs_cpc_ksic/jpn_from_chn_import2001_ksic10.dta")
jpn_from_chn_import2010 <- haven::read_dta("data/final/hs_cpc_ksic/jpn_from_chn_import2010_ksic10.dta")
jpn_from_chn_import2019 <- haven::read_dta("data/final/hs_cpc_ksic/jpn_from_chn_import2019_ksic10.dta")

jpn_to_chn_export2001 <- haven::read_dta("data/final/hs_cpc_ksic/jpn_to_chn_export2001_ksic10.dta")
jpn_to_chn_export2010 <- haven::read_dta("data/final/hs_cpc_ksic/jpn_to_chn_export2010_ksic10.dta")
jpn_to_chn_export2019 <- haven::read_dta("data/final/hs_cpc_ksic/jpn_to_chn_export2019_ksic10.dta")



# first make total ksic10 table

ksic <- readxl::read_excel("data/final/hs_cpc_ksic/ksic10.xlsx")


# now make the change in import and export step by step

#import 2010 to 2001

jpn_from_chn_import_2010_2001 <- ksic %>% left_join(jpn_from_chn_import2010, by="ksic10") %>% 
  mutate(value2010=ifelse(is.na(Cifvalue), 0, Cifvalue)) %>% 
  select(-Cifvalue)

jpn_from_chn_import_2010_2001 <- jpn_from_chn_import_2010_2001 %>% left_join(jpn_from_chn_import2001, by="ksic10") %>% 
  mutate(value2001=ifelse(is.na(Cifvalue), 0, Cifvalue)) %>% 
  select(-Cifvalue)

#gdp deflator to 2019

jpn_from_chn_import_2010_2001 <- jpn_from_chn_import_2010_2001 %>% 
  mutate(value2010=value2010*gdp_deflator(2019, base_year = 2010), value2001=value2001*gdp_deflator(2019, base_year = 2001))

jpn_from_chn_import_2010_2001 <- jpn_from_chn_import_2010_2001 %>% 
  mutate(value=value2010-value2001)



#import 2019 to 2010

jpn_from_chn_import_2019_2010 <- ksic %>% left_join(jpn_from_chn_import2019, by="ksic10") %>% 
  mutate(value2019=ifelse(is.na(Cifvalue), 0, Cifvalue)) %>% 
  select(-Cifvalue)

jpn_from_chn_import_2019_2010 <- jpn_from_chn_import_2019_2010 %>% left_join(jpn_from_chn_import2010, by="ksic10") %>% 
  mutate(value2010=ifelse(is.na(Cifvalue), 0, Cifvalue)) %>% 
  select(-Cifvalue)

#gdp deflator to 2019

jpn_from_chn_import_2019_2010 <- jpn_from_chn_import_2019_2010 %>% 
  mutate(value2010=value2010*gdp_deflator(2019, base_year = 2010))

jpn_from_chn_import_2019_2010 <- jpn_from_chn_import_2019_2010 %>% 
  mutate(value=value2019-value2010)



#import 2019 to 2001

jpn_from_chn_import_2019_2001 <- ksic %>% left_join(jpn_from_chn_import2019, by="ksic10") %>% 
  mutate(value2019=ifelse(is.na(Cifvalue), 0, Cifvalue)) %>% 
  select(-Cifvalue)

jpn_from_chn_import_2019_2001 <- jpn_from_chn_import_2019_2001 %>% left_join(jpn_from_chn_import2001, by="ksic10") %>% 
  mutate(value2001=ifelse(is.na(Cifvalue), 0, Cifvalue)) %>% 
  select(-Cifvalue)

#gdp deflator to 2019

jpn_from_chn_import_2019_2001 <- jpn_from_chn_import_2019_2001 %>% 
  mutate(value2001=value2001*gdp_deflator(2019, base_year = 2001))

jpn_from_chn_import_2019_2001 <- jpn_from_chn_import_2019_2001 %>% 
  mutate(value=value2019-value2001)




#export 2010 to 2001

jpn_to_chn_export_2010_2001 <- ksic %>% left_join(jpn_to_chn_export2010, by="ksic10") %>% 
  mutate(value2010=ifelse(is.na(Fobvalue), 0, Fobvalue)) %>% 
  select(-Fobvalue)

jpn_to_chn_export_2010_2001 <- jpn_to_chn_export_2010_2001 %>% left_join(jpn_to_chn_export2001, by="ksic10") %>% 
  mutate(value2001=ifelse(is.na(Fobvalue), 0, Fobvalue)) %>% 
  select(-Fobvalue)

#gdp deflator to 2019

jpn_to_chn_export_2010_2001 <- jpn_to_chn_export_2010_2001 %>% 
  mutate(value2010=value2010*gdp_deflator(2019, base_year = 2010), value2001=value2001*gdp_deflator(2019, base_year = 2001))


jpn_to_chn_export_2010_2001 <- jpn_to_chn_export_2010_2001 %>% 
  mutate(value=value2010-value2001)



#export 2019 to 2010

jpn_to_chn_export_2019_2010 <- ksic %>% left_join(jpn_to_chn_export2019, by="ksic10") %>% 
  mutate(value2019=ifelse(is.na(Fobvalue), 0, Fobvalue)) %>% 
  select(-Fobvalue)

jpn_to_chn_export_2019_2010 <- jpn_to_chn_export_2019_2010 %>% left_join(jpn_to_chn_export2010, by="ksic10") %>% 
  mutate(value2010=ifelse(is.na(Fobvalue), 0, Fobvalue)) %>% 
  select(-Fobvalue)

#gdp deflator to 2019

jpn_to_chn_export_2019_2010 <- jpn_to_chn_export_2019_2010 %>% 
  mutate(value2010=value2010*gdp_deflator(2019, base_year = 2010))



jpn_to_chn_export_2019_2010 <- jpn_to_chn_export_2019_2010 %>% 
  mutate(value=value2019-value2010)


#export 2019 to 2001

jpn_to_chn_export_2019_2001 <- ksic %>% left_join(jpn_to_chn_export2019, by="ksic10") %>% 
  mutate(value2019=ifelse(is.na(Fobvalue), 0, Fobvalue)) %>% 
  select(-Fobvalue)

jpn_to_chn_export_2019_2001 <- jpn_to_chn_export_2019_2001 %>% left_join(jpn_to_chn_export2001, by="ksic10") %>% 
  mutate(value2001=ifelse(is.na(Fobvalue), 0, Fobvalue)) %>% 
  select(-Fobvalue)

#gdp deflator to 2019

jpn_to_chn_export_2019_2001 <- jpn_to_chn_export_2019_2001 %>% 
  mutate(value2001=value2001*gdp_deflator(2019, base_year = 2001))



jpn_to_chn_export_2019_2001 <- jpn_to_chn_export_2019_2001 %>% 
  mutate(value=value2019-value2001)





#save the import and export data

jpn_from_chn_import_2010_2001 %>%  haven::write_dta("data/final/hs_cpc_ksic/jpn_import2010_2001.dta")
jpn_from_chn_import_2019_2010 %>%  haven::write_dta("data/final/hs_cpc_ksic/jpn_import2019_2010.dta")
jpn_from_chn_import_2019_2001 %>%  haven::write_dta("data/final/hs_cpc_ksic/jpn_import2019_2001.dta")
jpn_to_chn_export_2010_2001 %>%  haven::write_dta("data/final/hs_cpc_ksic/jpn_export2010_2001.dta")
jpn_to_chn_export_2019_2010 %>%  haven::write_dta("data/final/hs_cpc_ksic/jpn_export2019_2010.dta")
jpn_to_chn_export_2019_2001 %>%  haven::write_dta("data/final/hs_cpc_ksic/jpn_export2019_2001.dta")





