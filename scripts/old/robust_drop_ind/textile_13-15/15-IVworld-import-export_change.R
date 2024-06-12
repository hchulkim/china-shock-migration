if (!require(remotes)) (install.packages("remotes"))
if (!require(gcamdata)) (remotes::install_github("JGCRI/gcamdata"))



if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(here, gt, data.table, sf, gcamdata, tidyverse)


# this code is to merge 2001-2010, 2010-2019 trade data and see how well they are linked. Also it will make the total import and export change by industry. 


#we are making iv


## for world -- chn


# read in import and export data

world_import2001 <- haven::read_dta("data/final/hs_cpc_ksic/chn_to_world_export2001_ksic10.dta") %>% rename(Cifvalue=Fobvalue)

world_import2019 <- haven::read_dta("data/final/hs_cpc_ksic/chn_to_world_export2019_ksic10.dta") %>% rename(Cifvalue=Fobvalue)

world_export2001 <- haven::read_dta("data/final/hs_cpc_ksic/chn_from_world_import2001_ksic10.dta") %>% rename(Fobvalue=Cifvalue)

world_export2019 <- haven::read_dta("data/final/hs_cpc_ksic/chn_from_world_import2019_ksic10.dta") %>% rename(Fobvalue=Cifvalue)




kor_import2001 <- haven::read_dta("data/final/hs_cpc_ksic/kor_from_chn_import2001_ksic10.dta")

kor_import2019 <- haven::read_dta("data/final/hs_cpc_ksic/kor_from_chn_import2019_ksic10.dta")

kor_export2001 <- haven::read_dta("data/final/hs_cpc_ksic/kor_to_chn_export2001_ksic10.dta")

kor_export2019 <- haven::read_dta("data/final/hs_cpc_ksic/kor_to_chn_export2019_ksic10.dta")



# take out kor from world

world_import2001 <- world_import2001 %>% left_join(kor_import2001, by="ksic10") %>% 
  mutate(Cifvalue.y=ifelse(is.na(Cifvalue.y), 0, Cifvalue.y)) %>% 
  mutate(value2001=Cifvalue.x-Cifvalue.y) %>% filter(value2001>=0) %>% 
  select(ksic10, value2001)

world_import2019  <- world_import2019 %>% left_join(kor_import2019, by="ksic10") %>% 
  mutate(Cifvalue.y=ifelse(is.na(Cifvalue.y), 0, Cifvalue.y)) %>% 
  mutate(value2019=Cifvalue.x-Cifvalue.y) %>% filter(value2019>=0) %>% 
  select(ksic10, value2019)

world_export2001 <- world_export2001 %>% left_join(kor_export2001, by="ksic10") %>% 
  mutate(Fobvalue.y=ifelse(is.na(Fobvalue.y), 0, Fobvalue.y)) %>% 
  mutate(value2001=Fobvalue.x-Fobvalue.y) %>% filter(value2001>=0) %>% 
  select(ksic10, value2001)

world_export2019  <- world_export2019 %>% left_join(kor_export2019, by="ksic10") %>% 
  mutate(Fobvalue.y=ifelse(is.na(Fobvalue.y), 0, Fobvalue.y)) %>% 
  mutate(value2019=Fobvalue.x-Fobvalue.y) %>% filter(value2019>=0) %>% 
  select(ksic10, value2019)




# first make total ksic10 table

ksic <- readxl::read_excel("data/final/hs_cpc_ksic/ksic10.xlsx")


# now make the change in import and export step by step


#import 2019 to 2001

world_import_2019_2001 <- ksic %>% left_join(world_import2019, by="ksic10") %>% 
  mutate(value2019=ifelse(is.na(value2019), 0, value2019))

world_import_2019_2001 <- world_import_2019_2001 %>% left_join(world_import2001, by="ksic10") %>%
  mutate(value2001=ifelse(is.na(value2001), 0, value2001))

#gdp deflator to 2019

world_import_2019_2001 <- world_import_2019_2001 %>% 
  mutate(value2001=value2001*gdp_deflator(2019, base_year = 2001))

world_import_2019_2001 <- world_import_2019_2001 %>% 
  mutate(value=value2019-value2001)




#export 2019 to 2001

world_export_2019_2001 <- ksic %>% left_join(world_export2019, by="ksic10") %>% 
  mutate(value2019=ifelse(is.na(value2019), 0, value2019))

world_export_2019_2001 <- world_export_2019_2001 %>% left_join(world_export2001, by="ksic10") %>%
  mutate(value2001=ifelse(is.na(value2001), 0, value2001))

#gdp deflator to 2019

world_export_2019_2001 <- world_export_2019_2001 %>% 
  mutate(value2001=value2001*gdp_deflator(2019, base_year = 2001))



world_export_2019_2001 <- world_export_2019_2001 %>% 
  mutate(value=value2019-value2001)





#save the import and export data

world_import_2019_2001 %>%  haven::write_dta("data/final/hs_cpc_ksic/world_import2019_2001.dta")


world_export_2019_2001 %>%  haven::write_dta("data/final/hs_cpc_ksic/world_export2019_2001.dta")









# this code is to merge 2001-2010, 2010-2019 trade data and see how well they are linked. Also it will make the total import and export change by industry.


# read in import and export data

world_import2001 <- haven::read_dta("data/final/hs_cpc_ksic/chn_to_world_export2001_ksic10.dta") %>% rename(Cifvalue=Fobvalue)

world_import2010 <- haven::read_dta("data/final/hs_cpc_ksic/chn_to_world_export2010_ksic10.dta") %>% rename(Cifvalue=Fobvalue)

world_import2019 <- haven::read_dta("data/final/hs_cpc_ksic/chn_to_world_export2019_ksic10.dta") %>% rename(Cifvalue=Fobvalue)

world_export2001 <- haven::read_dta("data/final/hs_cpc_ksic/chn_from_world_import2001_ksic10.dta") %>% rename(Fobvalue=Cifvalue)

world_export2010 <- haven::read_dta("data/final/hs_cpc_ksic/chn_from_world_import2001_ksic10.dta") %>% rename(Fobvalue=Cifvalue)

world_export2019 <- haven::read_dta("data/final/hs_cpc_ksic/chn_from_world_import2019_ksic10.dta") %>% rename(Fobvalue=Cifvalue)



kor_import2001 <- haven::read_dta("data/final/hs_cpc_ksic/kor_from_chn_import2001_ksic10.dta")

kor_import2010 <- haven::read_dta("data/final/hs_cpc_ksic/kor_from_chn_import2010_ksic10.dta")

kor_import2019 <- haven::read_dta("data/final/hs_cpc_ksic/kor_from_chn_import2019_ksic10.dta")

kor_export2001 <- haven::read_dta("data/final/hs_cpc_ksic/kor_to_chn_export2001_ksic10.dta")

kor_export2010 <- haven::read_dta("data/final/hs_cpc_ksic/kor_to_chn_export2010_ksic10.dta")

kor_export2019 <- haven::read_dta("data/final/hs_cpc_ksic/kor_to_chn_export2019_ksic10.dta")



# take out kor from world

world_import2001 <- world_import2001 %>% left_join(kor_import2001, by="ksic10") %>% 
  mutate(Cifvalue.y=ifelse(is.na(Cifvalue.y), 0, Cifvalue.y)) %>% 
  mutate(value2001=Cifvalue.x-Cifvalue.y) %>% filter(value2001>=0) %>% 
  select(ksic10, value2001)

world_import2010 <- world_import2010 %>% left_join(kor_import2010, by="ksic10") %>% 
  mutate(Cifvalue.y=ifelse(is.na(Cifvalue.y), 0, Cifvalue.y)) %>% 
  mutate(value2010=Cifvalue.x-Cifvalue.y) %>% filter(value2010>=0) %>% 
  select(ksic10, value2010)

world_import2019  <- world_import2019 %>% left_join(kor_import2019, by="ksic10") %>% 
  mutate(Cifvalue.y=ifelse(is.na(Cifvalue.y), 0, Cifvalue.y)) %>% 
  mutate(value2019=Cifvalue.x-Cifvalue.y) %>% filter(value2019>=0) %>% 
  select(ksic10, value2019)

world_export2001 <- world_export2001 %>% left_join(kor_export2001, by="ksic10") %>% 
  mutate(Fobvalue.y=ifelse(is.na(Fobvalue.y), 0, Fobvalue.y)) %>% 
  mutate(value2001=Fobvalue.x-Fobvalue.y) %>% filter(value2001>=0) %>% 
  select(ksic10, value2001)

world_export2010 <- world_export2010 %>% left_join(kor_export2010, by="ksic10") %>% 
  mutate(Fobvalue.y=ifelse(is.na(Fobvalue.y), 0, Fobvalue.y)) %>% 
  mutate(value2010=Fobvalue.x-Fobvalue.y) %>% filter(value2010>=0) %>% 
  select(ksic10, value2010)

world_export2019  <- world_export2019 %>% left_join(kor_export2019, by="ksic10") %>% 
  mutate(Fobvalue.y=ifelse(is.na(Fobvalue.y), 0, Fobvalue.y)) %>% 
  mutate(value2019=Fobvalue.x-Fobvalue.y) %>% filter(value2019>=0) %>% 
  select(ksic10, value2019)


# first make total ksic10 table

ksic <- readxl::read_excel("data/final/hs_cpc_ksic/ksic10.xlsx")





# now make the change in import and export step by step

#import 2010 to 2001

world_import_2010_2001 <- ksic %>% left_join(world_import2010, by="ksic10") %>% 
  mutate(value2010=ifelse(is.na(value2010), 0, value2010))

world_import_2010_2001 <- world_import_2010_2001 %>% left_join(world_import2001, by="ksic10") %>%
  mutate(value2001=ifelse(is.na(value2001), 0, value2001))


#gdp deflator to 2019

world_import_2010_2001 <- world_import_2010_2001 %>% 
  mutate(value2010=value2010*gdp_deflator(2019, base_year = 2010))

world_import_2010_2001 <- world_import_2010_2001 %>% 
  mutate(value2001=value2001*gdp_deflator(2019, base_year = 2001))

world_import_2010_2001 <- world_import_2010_2001 %>% 
  mutate(value=value2010-value2001)



#import 2019 to 2010

world_import_2019_2010 <- ksic %>% left_join(world_import2019, by="ksic10") %>% 
  mutate(value2019=ifelse(is.na(value2019), 0, value2019))

world_import_2019_2010 <- world_import_2019_2010 %>% left_join(world_import2010, by="ksic10") %>% 
  mutate(value2010=ifelse(is.na(value2010), 0, value2010))

#gdp deflator to 2019

world_import_2019_2010  <- world_import_2019_2010  %>% 
  mutate(value2010=value2010*gdp_deflator(2019, base_year = 2010))

world_import_2019_2010  <- world_import_2019_2010  %>% 
  mutate(value=value2019-value2010)



#export 2010 to 2001

world_export_2010_2001 <- ksic %>% left_join(world_export2010, by="ksic10") %>% 
  mutate(value2010=ifelse(is.na(value2010), 0, value2010))

world_export_2010_2001 <- world_export_2010_2001 %>% left_join(world_export2001, by="ksic10") %>% 
  mutate(value2001=ifelse(is.na(value2001), 0, value2001))

#gdp deflator to 2019

world_export_2010_2001 <- world_export_2010_2001 %>% 
  mutate(value2010=value2010*gdp_deflator(2019, base_year = 2010), value2001=value2001*gdp_deflator(2019, base_year = 2001))

world_export_2010_2001 <- world_export_2010_2001 %>% 
  mutate(value=value2010-value2001)



#export 2019 to 2010

world_export_2019_2010 <- ksic %>% left_join(world_export2019, by="ksic10") %>% 
  mutate(value2019=ifelse(is.na(value2019), 0, value2019))

world_export_2019_2010 <- world_export_2019_2010 %>% left_join(world_export2010, by="ksic10") %>% 
  mutate(value2010=ifelse(is.na(value2010), 0, value2010))

#gdp deflator to 2019

world_export_2019_2010 <- world_export_2019_2010 %>% 
  mutate(value2010=value2010*gdp_deflator(2019, base_year = 2010))


world_export_2019_2010 <- world_export_2019_2010 %>% 
  mutate(value=value2019-value2010)




#save the import and export data

world_import_2019_2010 %>%  haven::write_dta("data/final/hs_cpc_ksic/world_import2019_2010.dta")

world_export_2019_2010 %>%  haven::write_dta("data/final/hs_cpc_ksic/world_export2019_2010.dta")


world_import_2010_2001 %>%  haven::write_dta("data/final/hs_cpc_ksic/world_import2010_2001.dta")

world_export_2010_2001 %>%  haven::write_dta("data/final/hs_cpc_ksic/world_export2010_2001.dta")










