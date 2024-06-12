if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(here, gt, data.table, sf, tidyverse)






# we are transforming un comtrade data from hs to ksic. The process goes from hs to cpc to ksic.

##### overall framework:

# for 1:1 or n:1, just simply use left_join
# for 1:n we need to do right_join (or just do left join but otherway around)

# we can check this using weight. if weight is smaller than 1, that that's the 1:n case



# read in all the conversion tables -------





hs96_hs07 <- read_rds(here("data", "final", "hs_cpc_ksic","hs96_hs07.rds"))

hs07_cpc2  <- read_rds(here("data", "final", "hs_cpc_ksic","hs07_cpc2.rds"))
hs17_cpc21  <- read_rds(here("data", "final", "hs_cpc_ksic","hs17_cpc21.rds"))

cpc2_cpc21  <- read_rds(here("data", "final", "hs_cpc_ksic","cpc2_cpc21.rds"))

cpc21_ksic10  <- read_rds(here("data", "final", "hs_cpc_ksic","cpc21_ksic10.rds"))


ksic8_ksic9  <- read_rds(here("data", "final", "hs_cpc_ksic","ksic8_ksic9.rds"))
ksic9_ksic10  <- read_rds(here("data", "final", "hs_cpc_ksic","ksic9_ksic10.rds"))


# read in all the data ------

chn_from_world_import2001 <- read_csv("data/comtrade/CHN_from_WORLD_IMPORT_2001_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))

chn_from_world_import2010 <- read_csv("data/comtrade/CHN_from_WORLD_IMPORT_2010_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))

chn_from_world_import2019 <- read_csv("data/comtrade/CHN_from_WORLD_IMPORT_2019_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))



chn_to_world_export2001 <- read_csv("data/comtrade/CHN_to_WORLD_EXPORT_2001_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))

chn_to_world_export2010 <- read_csv("data/comtrade/CHN_to_WORLD_EXPORT_2010_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))

chn_to_world_export2019 <- read_csv("data/comtrade/CHN_to_WORLD_EXPORT_2019_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))



jpn_from_chn_import2001 <- read_csv("data/comtrade/JPN_from_CHN_IMPORT_2001_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))
jpn_from_chn_import2010 <- read_csv("data/comtrade/JPN_from_CHN_IMPORT_2010_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))
jpn_from_chn_import2019 <- read_csv("data/comtrade/JPN_from_CHN_IMPORT_2019_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))



jpn_to_chn_export2001 <- read_csv("data/comtrade/JPN_to_CHN_EXPORT_2001_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))
jpn_to_chn_export2010 <- read_csv("data/comtrade/JPN_to_CHN_EXPORT_2010_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))
jpn_to_chn_export2019 <- read_csv("data/comtrade/JPN_to_CHN_EXPORT_2019_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))



kor_from_chn_import2001 <- read_csv("data/comtrade/KOR_from_CHN_IMPORT_2001_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))
kor_from_chn_import2010 <- read_csv("data/comtrade/KOR_from_CHN_IMPORT_2010_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))
kor_from_chn_import2019 <- read_csv("data/comtrade/KOR_from_CHN_IMPORT_2019_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))



kor_to_chn_export2001 <- read_csv("data/comtrade/KOR_to_CHN_EXPORT_2001_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))
kor_to_chn_export2010 <- read_csv("data/comtrade/KOR_to_CHN_EXPORT_2010_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))
kor_to_chn_export2019 <- read_csv("data/comtrade/KOR_to_CHN_EXPORT_2019_HS6.csv") %>% mutate(CmdCode=as.character(CmdCode)) %>% mutate(CmdCode=ifelse(str_length(CmdCode)==5, paste0("0", CmdCode), CmdCode))






# hs96 to hs07 for year 2001


chn_from_world_import2001 <- chn_from_world_import2001 %>% group_by(CmdCode) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

hs96_hs07_1 <- hs96_hs07 %>% filter(weight==1)
hs96_hs07_2 <- hs96_hs07 %>% filter(weight!=1)

check <- hs96_hs07_2 %>% distinct(HS_1996) %>% pull(HS_1996)


chn_from_world_import2001_1 <- chn_from_world_import2001 %>% filter(!(CmdCode %in% check))  

chn_from_world_import2001_2 <- chn_from_world_import2001 %>% filter(CmdCode %in% check)  

chn_from_world_import2001_1 <- chn_from_world_import2001_1 %>% 
  left_join(hs96_hs07_1, by=c("CmdCode"="HS_1996")) %>% select(-CmdCode)

hs96_hs07_2 <- hs96_hs07_2 %>% 
  left_join(chn_from_world_import2001_2, by=c("HS_1996"="CmdCode")) %>% select(-HS_1996) 


chn_from_world_import2001 <- chn_from_world_import2001_1 %>% 
  bind_rows(hs96_hs07_2) %>% filter(!is.na(Cifvalue))


chn_from_world_import2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_from_world_import2001_hs07.dta"))










jpn_from_chn_import2001 <- jpn_from_chn_import2001 %>% group_by(CmdCode) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

hs96_hs07_1 <- hs96_hs07 %>% filter(weight==1)
hs96_hs07_2 <- hs96_hs07 %>% filter(weight!=1)

check <- hs96_hs07_2 %>% distinct(HS_1996) %>% pull(HS_1996)


jpn_from_chn_import2001_1 <- jpn_from_chn_import2001 %>% filter(!(CmdCode %in% check))  

jpn_from_chn_import2001_2 <- jpn_from_chn_import2001 %>% filter(CmdCode %in% check)  

jpn_from_chn_import2001_1 <- jpn_from_chn_import2001_1 %>% 
  left_join(hs96_hs07_1, by=c("CmdCode"="HS_1996")) %>% select(-CmdCode)

hs96_hs07_2 <- hs96_hs07_2 %>% 
  left_join(jpn_from_chn_import2001_2, by=c("HS_1996"="CmdCode")) %>% select(-HS_1996) 


jpn_from_chn_import2001 <- jpn_from_chn_import2001_1 %>% 
  bind_rows(hs96_hs07_2) %>%  filter(!is.na(Cifvalue))

jpn_from_chn_import2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_from_chn_import2001_hs07.dta"))






kor_from_chn_import2001 <- kor_from_chn_import2001 %>% group_by(CmdCode) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

hs96_hs07_1 <- hs96_hs07 %>% filter(weight==1)
hs96_hs07_2 <- hs96_hs07 %>% filter(weight!=1)

check <- hs96_hs07_2 %>% distinct(HS_1996) %>% pull(HS_1996)


kor_from_chn_import2001_1 <-kor_from_chn_import2001  %>% filter(!(CmdCode %in% check))  

kor_from_chn_import2001_2 <- kor_from_chn_import2001 %>% filter(CmdCode %in% check)  

kor_from_chn_import2001_1 <- kor_from_chn_import2001_1 %>% 
  left_join(hs96_hs07_1, by=c("CmdCode"="HS_1996")) %>% select(-CmdCode)

hs96_hs07_2 <- hs96_hs07_2 %>% 
  left_join(kor_from_chn_import2001_2, by=c("HS_1996"="CmdCode")) %>% select(-HS_1996) 


kor_from_chn_import2001 <- kor_from_chn_import2001_1 %>% 
  bind_rows(hs96_hs07_2) %>%  filter(!is.na(Cifvalue))

kor_from_chn_import2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_from_chn_import2001_hs07.dta"))











chn_to_world_export2001 <-chn_to_world_export2001  %>% group_by(CmdCode) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

hs96_hs07_1 <- hs96_hs07 %>% filter(weight==1)
hs96_hs07_2 <- hs96_hs07 %>% filter(weight!=1)

check <- hs96_hs07_2 %>% distinct(HS_1996) %>% pull(HS_1996)


chn_to_world_export2001_1 <- chn_to_world_export2001 %>% filter(!(CmdCode %in% check))  

chn_to_world_export2001_2 <-chn_to_world_export2001  %>% filter(CmdCode %in% check)  

chn_to_world_export2001_1 <- chn_to_world_export2001_1 %>% 
  left_join(hs96_hs07_1, by=c("CmdCode"="HS_1996")) %>% select(-CmdCode)

hs96_hs07_2 <- hs96_hs07_2 %>% 
  left_join(chn_to_world_export2001_2, by=c("HS_1996"="CmdCode")) %>% select(-HS_1996) 


chn_to_world_export2001 <- chn_to_world_export2001_1 %>% 
  bind_rows(hs96_hs07_2) %>%  filter(!is.na(Fobvalue))

chn_to_world_export2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_to_world_export2001_hs07.dta"))




jpn_to_chn_export2001 <-jpn_to_chn_export2001  %>% group_by(CmdCode) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

hs96_hs07_1 <- hs96_hs07 %>% filter(weight==1)
hs96_hs07_2 <- hs96_hs07 %>% filter(weight!=1)

check <- hs96_hs07_2 %>% distinct(HS_1996) %>% pull(HS_1996)


jpn_to_chn_export2001_1 <- jpn_to_chn_export2001 %>% filter(!(CmdCode %in% check))  

jpn_to_chn_export2001_2 <-jpn_to_chn_export2001  %>% filter(CmdCode %in% check)  

jpn_to_chn_export2001_1 <- jpn_to_chn_export2001_1 %>% 
  left_join(hs96_hs07_1, by=c("CmdCode"="HS_1996")) %>% select(-CmdCode)

hs96_hs07_2 <- hs96_hs07_2 %>% 
  left_join(jpn_to_chn_export2001_2, by=c("HS_1996"="CmdCode")) %>% select(-HS_1996) 


jpn_to_chn_export2001 <- jpn_to_chn_export2001_1 %>% 
  bind_rows(hs96_hs07_2) %>%  filter(!is.na(Fobvalue))

jpn_to_chn_export2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_to_chn_export2001_hs07.dta"))



kor_to_chn_export2001 <-kor_to_chn_export2001  %>% group_by(CmdCode) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

hs96_hs07_1 <- hs96_hs07 %>% filter(weight==1)
hs96_hs07_2 <- hs96_hs07 %>% filter(weight!=1)

check <- hs96_hs07_2 %>% distinct(HS_1996) %>% pull(HS_1996)


kor_to_chn_export2001_1 <- kor_to_chn_export2001 %>% filter(!(CmdCode %in% check))  

kor_to_chn_export2001_2 <-kor_to_chn_export2001  %>% filter(CmdCode %in% check)  

kor_to_chn_export2001_1 <- kor_to_chn_export2001_1 %>% 
  left_join(hs96_hs07_1, by=c("CmdCode"="HS_1996")) %>% select(-CmdCode)

hs96_hs07_2 <- hs96_hs07_2 %>% 
  left_join(kor_to_chn_export2001_2, by=c("HS_1996"="CmdCode")) %>% select(-HS_1996) 


kor_to_chn_export2001 <- kor_to_chn_export2001_1 %>% 
  bind_rows(hs96_hs07_2) %>%  filter(!is.na(Fobvalue))

kor_to_chn_export2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_to_chn_export2001_hs07.dta"))




# hs07 to cpc2

chn_from_world_import2001 <- chn_from_world_import2001 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)

chn_from_world_import2001 <- chn_from_world_import2001 %>% group_by(HS_2007) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

hs07_cpc2_1 <- hs07_cpc2 %>% filter(weight==1)
hs07_cpc2_2 <- hs07_cpc2 %>% filter(weight!=1)

check <- hs07_cpc2_2 %>% distinct(HS07Code) %>% pull(HS07Code)


chn_from_world_import2001_1 <- chn_from_world_import2001 %>% filter(!(HS_2007 %in% check))  

chn_from_world_import2001_2 <- chn_from_world_import2001 %>% filter(HS_2007 %in% check)  

chn_from_world_import2001_1 <- chn_from_world_import2001_1 %>% 
  left_join(hs07_cpc2_1, by=c("HS_2007"="HS07Code")) %>% select(-HS_2007)

hs07_cpc2_2 <- hs07_cpc2_2 %>% 
  left_join(chn_from_world_import2001_2, by=c("HS07Code"="HS_2007")) %>% select(-HS07Code) 


chn_from_world_import2001 <- chn_from_world_import2001_1 %>% 
  bind_rows(hs07_cpc2_2) %>% filter(!is.na(Cifvalue), !is.na(CPC2Code)) 

chn_from_world_import2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_from_world_import2001_cpc2.dta"))







jpn_from_chn_import2001 <- jpn_from_chn_import2001 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)


jpn_from_chn_import2001 <- jpn_from_chn_import2001 %>% group_by(HS_2007) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

hs07_cpc2_1 <- hs07_cpc2 %>% filter(weight==1)
hs07_cpc2_2 <- hs07_cpc2 %>% filter(weight!=1)

check <- hs07_cpc2_2 %>% distinct(HS07Code) %>% pull(HS07Code)


jpn_from_chn_import2001_1 <- jpn_from_chn_import2001 %>% filter(!(HS_2007 %in% check))  

jpn_from_chn_import2001_2 <- jpn_from_chn_import2001 %>% filter(HS_2007 %in% check)  

jpn_from_chn_import2001_1 <- jpn_from_chn_import2001_1 %>% 
  left_join(hs07_cpc2_1, by=c("HS_2007"="HS07Code")) %>% select(-HS_2007)

hs07_cpc2_2 <- hs07_cpc2_2 %>% 
  left_join(jpn_from_chn_import2001_2, by=c("HS07Code"="HS_2007")) %>% select(-HS07Code) 


jpn_from_chn_import2001 <- jpn_from_chn_import2001_1 %>% 
  bind_rows(hs07_cpc2_2) %>%  filter(!is.na(Cifvalue), !is.na(CPC2Code))

jpn_from_chn_import2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_from_chn_import2001_cpc2.dta"))




kor_from_chn_import2001 <- kor_from_chn_import2001 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)

kor_from_chn_import2001 <- kor_from_chn_import2001 %>% group_by(HS_2007) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

hs07_cpc2_1 <- hs07_cpc2 %>% filter(weight==1)
hs07_cpc2_2 <- hs07_cpc2 %>% filter(weight!=1)

check <- hs07_cpc2_2 %>% distinct(HS07Code) %>% pull(HS07Code)


kor_from_chn_import2001_1 <-kor_from_chn_import2001  %>% filter(!(HS_2007 %in% check))  

kor_from_chn_import2001_2 <- kor_from_chn_import2001 %>% filter(HS_2007 %in% check)  

kor_from_chn_import2001_1 <- kor_from_chn_import2001_1 %>% 
  left_join(hs07_cpc2_1, by=c("HS_2007"="HS07Code")) %>% select(-HS_2007)

hs07_cpc2_2 <- hs07_cpc2_2 %>% 
  left_join(kor_from_chn_import2001_2, by=c("HS07Code"="HS_2007")) %>% select(-HS07Code) 


kor_from_chn_import2001 <- kor_from_chn_import2001_1 %>% 
  bind_rows(hs07_cpc2_2) %>%  filter(!is.na(Cifvalue), !is.na(CPC2Code))

kor_from_chn_import2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_from_chn_import2001_cpc2.dta"))








chn_to_world_export2001 <- chn_to_world_export2001 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


chn_to_world_export2001 <-chn_to_world_export2001  %>% group_by(HS_2007) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

hs07_cpc2_1 <- hs07_cpc2 %>% filter(weight==1)
hs07_cpc2_2 <- hs07_cpc2 %>% filter(weight!=1)

check <- hs07_cpc2_2 %>% distinct(HS07Code) %>% pull(HS07Code)


chn_to_world_export2001_1 <- chn_to_world_export2001 %>% filter(!(HS_2007 %in% check))  

chn_to_world_export2001_2 <-chn_to_world_export2001  %>% filter(HS_2007 %in% check)  

chn_to_world_export2001_1 <- chn_to_world_export2001_1 %>% 
  left_join(hs07_cpc2_1, by=c("HS_2007"="HS07Code")) %>% select(-HS_2007)

hs07_cpc2_2 <- hs07_cpc2_2 %>% 
  left_join(chn_to_world_export2001_2, by=c("HS07Code"="HS_2007")) %>% select(-HS07Code) 


chn_to_world_export2001 <- chn_to_world_export2001_1 %>% 
  bind_rows(hs07_cpc2_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC2Code))

chn_to_world_export2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_to_world_export2001_cpc2.dta"))




jpn_to_chn_export2001 <- jpn_to_chn_export2001 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


jpn_to_chn_export2001 <-jpn_to_chn_export2001  %>% group_by(HS_2007) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

hs07_cpc2_1 <- hs07_cpc2 %>% filter(weight==1)
hs07_cpc2_2 <- hs07_cpc2 %>% filter(weight!=1)

check <- hs07_cpc2_2 %>% distinct(HS07Code) %>% pull(HS07Code)


jpn_to_chn_export2001_1 <- jpn_to_chn_export2001 %>% filter(!(HS_2007 %in% check))  

jpn_to_chn_export2001_2 <-jpn_to_chn_export2001  %>% filter(HS_2007 %in% check)  

jpn_to_chn_export2001_1 <- jpn_to_chn_export2001_1 %>% 
  left_join(hs07_cpc2_1, by=c("HS_2007"="HS07Code")) %>% select(-HS_2007)

hs07_cpc2_2 <- hs07_cpc2_2 %>% 
  left_join(jpn_to_chn_export2001_2, by=c("HS07Code"="HS_2007")) %>% select(-HS07Code) 


jpn_to_chn_export2001 <- jpn_to_chn_export2001_1 %>% 
  bind_rows(hs07_cpc2_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC2Code))

jpn_to_chn_export2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_to_chn_export2001_cpc2.dta"))





kor_to_chn_export2001 <- kor_to_chn_export2001 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)

kor_to_chn_export2001 <-kor_to_chn_export2001  %>% group_by(HS_2007) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

hs07_cpc2_1 <- hs07_cpc2 %>% filter(weight==1)
hs07_cpc2_2 <- hs07_cpc2 %>% filter(weight!=1)

check <- hs07_cpc2_2 %>% distinct(HS07Code) %>% pull(HS07Code)


kor_to_chn_export2001_1 <- kor_to_chn_export2001 %>% filter(!(HS_2007 %in% check))  

kor_to_chn_export2001_2 <-kor_to_chn_export2001  %>% filter(HS_2007 %in% check)  

kor_to_chn_export2001_1 <- kor_to_chn_export2001_1 %>% 
  left_join(hs07_cpc2_1, by=c("HS_2007"="HS07Code")) %>% select(-HS_2007)

hs07_cpc2_2 <- hs07_cpc2_2 %>% 
  left_join(kor_to_chn_export2001_2, by=c("HS07Code"="HS_2007")) %>% select(-HS07Code) 


kor_to_chn_export2001 <- kor_to_chn_export2001_1 %>% 
  bind_rows(hs07_cpc2_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC2Code))

kor_to_chn_export2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_to_chn_export2001_cpc2.dta"))








# cpc2 to cpc21

chn_from_world_import2001 <- chn_from_world_import2001 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)

chn_from_world_import2001 <- chn_from_world_import2001 %>% group_by(CPC2Code) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc2_cpc21_1 <- cpc2_cpc21 %>% filter(weight==1)
cpc2_cpc21_2 <- cpc2_cpc21 %>% filter(weight!=1)

check <- cpc2_cpc21_2 %>% distinct(CPC2code) %>% pull(CPC2code)


chn_from_world_import2001_1 <- chn_from_world_import2001 %>% filter(!(CPC2Code %in% check))  

chn_from_world_import2001_2 <- chn_from_world_import2001 %>% filter(CPC2Code %in% check)  

chn_from_world_import2001_1 <- chn_from_world_import2001_1 %>% 
  left_join(cpc2_cpc21_1, by=c("CPC2Code"="CPC2code")) %>% select(-CPC2Code)

cpc2_cpc21_2 <- cpc2_cpc21_2 %>% 
  left_join(chn_from_world_import2001_2, by=c("CPC2code"="CPC2Code")) %>% select(-CPC2code) 


chn_from_world_import2001 <- chn_from_world_import2001_1 %>% 
  bind_rows(cpc2_cpc21_2) %>% filter(!is.na(Cifvalue), !is.na(CPC21code))


chn_from_world_import2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_from_world_import2001_cpc21.dta"))







jpn_from_chn_import2001 <- jpn_from_chn_import2001 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)


jpn_from_chn_import2001 <- jpn_from_chn_import2001 %>% group_by(CPC2Code) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc2_cpc21_1 <- cpc2_cpc21 %>% filter(weight==1)
cpc2_cpc21_2 <- cpc2_cpc21 %>% filter(weight!=1)

check <- cpc2_cpc21_2 %>% distinct(CPC2code) %>% pull(CPC2code)


jpn_from_chn_import2001_1 <- jpn_from_chn_import2001 %>% filter(!(CPC2Code %in% check))  

jpn_from_chn_import2001_2 <- jpn_from_chn_import2001 %>% filter(CPC2Code %in% check)  

jpn_from_chn_import2001_1 <- jpn_from_chn_import2001_1 %>% 
  left_join(cpc2_cpc21_1, by=c("CPC2Code"="CPC2code")) %>% select(-CPC2Code)

cpc2_cpc21_2 <- cpc2_cpc21_2 %>% 
  left_join(jpn_from_chn_import2001_2, by=c("CPC2code"="CPC2Code")) %>% select(-CPC2code) 


jpn_from_chn_import2001 <- jpn_from_chn_import2001_1 %>% 
  bind_rows(cpc2_cpc21_2) %>%  filter(!is.na(Cifvalue), !is.na(CPC21code))

jpn_from_chn_import2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_from_chn_import2001_cpc21.dta"))




kor_from_chn_import2001 <- kor_from_chn_import2001 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)

kor_from_chn_import2001 <- kor_from_chn_import2001 %>% group_by(CPC2Code) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc2_cpc21_1 <- cpc2_cpc21 %>% filter(weight==1)
cpc2_cpc21_2 <- cpc2_cpc21 %>% filter(weight!=1)

check <- cpc2_cpc21_2 %>% distinct(CPC2code) %>% pull(CPC2code)


kor_from_chn_import2001_1 <-kor_from_chn_import2001  %>% filter(!(CPC2Code %in% check))  

kor_from_chn_import2001_2 <- kor_from_chn_import2001 %>% filter(CPC2Code %in% check)  

kor_from_chn_import2001_1 <- kor_from_chn_import2001_1 %>% 
  left_join(cpc2_cpc21_1, by=c("CPC2Code"="CPC2code")) %>% select(-CPC2Code)

cpc2_cpc21_2 <- cpc2_cpc21_2 %>% 
  left_join(kor_from_chn_import2001_2, by=c("CPC2code"="CPC2Code")) %>% select(-CPC2code) 


kor_from_chn_import2001 <- kor_from_chn_import2001_1 %>% 
  bind_rows(cpc2_cpc21_2) %>%  filter(!is.na(Cifvalue), !is.na(CPC21code))

kor_from_chn_import2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_from_chn_import2001_cpc21.dta"))








chn_to_world_export2001 <- chn_to_world_export2001 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


chn_to_world_export2001 <-chn_to_world_export2001  %>% group_by(CPC2Code) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc2_cpc21_1 <- cpc2_cpc21 %>% filter(weight==1)
cpc2_cpc21_2 <- cpc2_cpc21 %>% filter(weight!=1)

check <- cpc2_cpc21_2 %>% distinct(CPC2code) %>% pull(CPC2code)


chn_to_world_export2001_1 <- chn_to_world_export2001 %>% filter(!(CPC2Code %in% check))  

chn_to_world_export2001_2 <-chn_to_world_export2001  %>% filter(CPC2Code %in% check)  

chn_to_world_export2001_1 <- chn_to_world_export2001_1 %>% 
  left_join(cpc2_cpc21_1, by=c("CPC2Code"="CPC2code")) %>% select(-CPC2Code)

cpc2_cpc21_2 <- cpc2_cpc21_2 %>% 
  left_join(chn_to_world_export2001_2, by=c("CPC2code"="CPC2Code")) %>% select(-CPC2code) 

chn_to_world_export2001 <- chn_to_world_export2001_1 %>% 
  bind_rows(cpc2_cpc21_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC21code))

chn_to_world_export2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_to_world_export2001_cpc21.dta"))




jpn_to_chn_export2001 <- jpn_to_chn_export2001 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


jpn_to_chn_export2001 <-jpn_to_chn_export2001  %>% group_by(CPC2Code) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc2_cpc21_1 <- cpc2_cpc21 %>% filter(weight==1)
cpc2_cpc21_2 <- cpc2_cpc21 %>% filter(weight!=1)

check <- cpc2_cpc21_2 %>% distinct(CPC2code) %>% pull(CPC2code)


jpn_to_chn_export2001_1 <- jpn_to_chn_export2001 %>% filter(!(CPC2Code %in% check))  

jpn_to_chn_export2001_2 <-jpn_to_chn_export2001  %>% filter(CPC2Code %in% check)  

jpn_to_chn_export2001_1 <- jpn_to_chn_export2001_1 %>% 
  left_join(cpc2_cpc21_1, by=c("CPC2Code"="CPC2code")) %>% select(-CPC2Code)

cpc2_cpc21_2 <- cpc2_cpc21_2 %>% 
  left_join(jpn_to_chn_export2001_2, by=c("CPC2code"="CPC2Code")) %>% select(-CPC2code) 


jpn_to_chn_export2001 <- jpn_to_chn_export2001_1 %>% 
  bind_rows(cpc2_cpc21_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC21code))

jpn_to_chn_export2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_to_chn_export2001_cpc21.dta"))





kor_to_chn_export2001 <- kor_to_chn_export2001 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)

kor_to_chn_export2001 <-kor_to_chn_export2001  %>% group_by(CPC2Code) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc2_cpc21_1 <- cpc2_cpc21 %>% filter(weight==1)
cpc2_cpc21_2 <- cpc2_cpc21 %>% filter(weight!=1)

check <- cpc2_cpc21_2 %>% distinct(CPC2code) %>% pull(CPC2code)


kor_to_chn_export2001_1 <- kor_to_chn_export2001 %>% filter(!(CPC2Code %in% check))  

kor_to_chn_export2001_2 <-kor_to_chn_export2001  %>% filter(CPC2Code %in% check)  

kor_to_chn_export2001_1 <- kor_to_chn_export2001_1 %>% 
  left_join(cpc2_cpc21_1, by=c("CPC2Code"="CPC2code")) %>% select(-CPC2Code)

cpc2_cpc21_2 <- cpc2_cpc21_2 %>% 
  left_join(kor_to_chn_export2001_2, by=c("CPC2code"="CPC2Code")) %>% select(-CPC2code) 


kor_to_chn_export2001 <- kor_to_chn_export2001_1 %>% 
  bind_rows(cpc2_cpc21_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC21code))

kor_to_chn_export2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_to_chn_export2001_cpc21.dta"))









# cpc21 to ksic10

chn_from_world_import2001 <- chn_from_world_import2001 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)

chn_from_world_import2001 <- chn_from_world_import2001 %>% group_by(CPC21code) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


chn_from_world_import2001_1 <- chn_from_world_import2001 %>% filter(!(CPC21code %in% check))  

chn_from_world_import2001_2 <- chn_from_world_import2001 %>% filter(CPC21code %in% check)  

chn_from_world_import2001_1 <- chn_from_world_import2001_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21code"="cpc21")) %>% select(-CPC21code)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(chn_from_world_import2001_2, by=c("cpc21"="CPC21code")) %>% select(-cpc21) 


chn_from_world_import2001 <- chn_from_world_import2001_1 %>% 
  bind_rows(cpc21_ksic10_2) %>% filter(!is.na(Cifvalue), !is.na(ksic10))


chn_from_world_import2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_from_world_import2001_ksic10.dta"))







jpn_from_chn_import2001 <- jpn_from_chn_import2001 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)


jpn_from_chn_import2001 <- jpn_from_chn_import2001 %>% group_by(CPC21code) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


jpn_from_chn_import2001_1 <- jpn_from_chn_import2001 %>% filter(!(CPC21code %in% check))  

jpn_from_chn_import2001_2 <- jpn_from_chn_import2001 %>% filter(CPC21code %in% check)  

jpn_from_chn_import2001_1 <- jpn_from_chn_import2001_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21code"="cpc21")) %>% select(-CPC21code)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(jpn_from_chn_import2001_2, by=c("cpc21"="CPC21code")) %>% select(-cpc21) 


jpn_from_chn_import2001 <- jpn_from_chn_import2001_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Cifvalue), !is.na(ksic10))

jpn_from_chn_import2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_from_chn_import2001_ksic10.dta"))




kor_from_chn_import2001 <- kor_from_chn_import2001 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)

kor_from_chn_import2001 <- kor_from_chn_import2001 %>% group_by(CPC21code) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


kor_from_chn_import2001_1 <-kor_from_chn_import2001  %>% filter(!(CPC21code %in% check))  

kor_from_chn_import2001_2 <- kor_from_chn_import2001 %>% filter(CPC21code %in% check)  

kor_from_chn_import2001_1 <- kor_from_chn_import2001_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21code"="cpc21")) %>% select(-CPC21code)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(kor_from_chn_import2001_2, by=c("cpc21"="CPC21code")) %>% select(-cpc21) 


kor_from_chn_import2001 <- kor_from_chn_import2001_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Cifvalue), !is.na(ksic10))

kor_from_chn_import2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_from_chn_import2001_ksic10.dta"))








chn_to_world_export2001 <- chn_to_world_export2001 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


chn_to_world_export2001 <-chn_to_world_export2001  %>% group_by(CPC21code) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


chn_to_world_export2001_1 <- chn_to_world_export2001 %>% filter(!(CPC21code %in% check))  

chn_to_world_export2001_2 <-chn_to_world_export2001  %>% filter(CPC21code %in% check)  

chn_to_world_export2001_1 <- chn_to_world_export2001_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21code"="cpc21")) %>% select(-CPC21code)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(chn_to_world_export2001_2, by=c("cpc21"="CPC21code")) %>% select(-cpc21) 


chn_to_world_export2001 <- chn_to_world_export2001_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Fobvalue), !is.na(ksic10))

chn_to_world_export2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_to_world_export2001_ksic10.dta"))




jpn_to_chn_export2001 <- jpn_to_chn_export2001 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


jpn_to_chn_export2001 <-jpn_to_chn_export2001  %>% group_by(CPC21code) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


jpn_to_chn_export2001_1 <- jpn_to_chn_export2001 %>% filter(!(CPC21code %in% check))  

jpn_to_chn_export2001_2 <-jpn_to_chn_export2001  %>% filter(CPC21code %in% check)  

jpn_to_chn_export2001_1 <- jpn_to_chn_export2001_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21code"="cpc21")) %>% select(-CPC21code)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(jpn_to_chn_export2001_2, by=c("cpc21"="CPC21code")) %>% select(-cpc21) 


jpn_to_chn_export2001 <- jpn_to_chn_export2001_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Fobvalue), !is.na(ksic10))

jpn_to_chn_export2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_to_chn_export2001_ksic10.dta"))





kor_to_chn_export2001 <- kor_to_chn_export2001 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)

kor_to_chn_export2001 <-kor_to_chn_export2001  %>% group_by(CPC21code) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


kor_to_chn_export2001_1 <- kor_to_chn_export2001 %>% filter(!(CPC21code %in% check))  

kor_to_chn_export2001_2 <-kor_to_chn_export2001  %>% filter(CPC21code %in% check)  

kor_to_chn_export2001_1 <- kor_to_chn_export2001_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21code"="cpc21")) %>% select(-CPC21code)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(kor_to_chn_export2001_2, by=c("cpc21"="CPC21code")) %>% select(-cpc21) 


kor_to_chn_export2001 <- kor_to_chn_export2001_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Fobvalue), !is.na(ksic10))

kor_to_chn_export2001 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_to_chn_export2001_ksic10.dta"))





# cpc21 to ksic10 save

chn_from_world_import2001 <- chn_from_world_import2001 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)



chn_from_world_import2001 %>% group_by(ksic10) %>% summarise(Cifvalue=sum(Cifvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_from_world_import2001_ksic10.dta"))







jpn_from_chn_import2001 <- jpn_from_chn_import2001 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)



jpn_from_chn_import2001 %>%  group_by(ksic10) %>% summarise(Cifvalue=sum(Cifvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_from_chn_import2001_ksic10.dta"))




kor_from_chn_import2001 <- kor_from_chn_import2001 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)


kor_from_chn_import2001 %>%  group_by(ksic10) %>% summarise(Cifvalue=sum(Cifvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_from_chn_import2001_ksic10.dta"))








chn_to_world_export2001 <- chn_to_world_export2001 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


chn_to_world_export2001 %>%  group_by(ksic10) %>% summarise(Fobvalue=sum(Fobvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_to_world_export2001_ksic10.dta"))




jpn_to_chn_export2001 <- jpn_to_chn_export2001 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


jpn_to_chn_export2001 %>%  group_by(ksic10) %>% summarise(Fobvalue=sum(Fobvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_to_chn_export2001_ksic10.dta"))





kor_to_chn_export2001 <- kor_to_chn_export2001 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


kor_to_chn_export2001 %>%  group_by(ksic10) %>% summarise(Fobvalue=sum(Fobvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_to_chn_export2001_ksic10.dta"))



# year 2010

# hs07 to cpc2



chn_from_world_import2010 <- chn_from_world_import2010 %>% group_by(CmdCode) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

hs07_cpc2_1 <- hs07_cpc2 %>% filter(weight==1)
hs07_cpc2_2 <- hs07_cpc2 %>% filter(weight!=1)

check <- hs07_cpc2_2 %>% distinct(HS07Code) %>% pull(HS07Code)


chn_from_world_import2010_1 <- chn_from_world_import2010 %>% filter(!(CmdCode %in% check))  

chn_from_world_import2010_2 <- chn_from_world_import2010 %>% filter(CmdCode %in% check)  

chn_from_world_import2010_1 <- chn_from_world_import2010_1 %>% 
  left_join(hs07_cpc2_1, by=c("CmdCode"="HS07Code")) %>% select(-CmdCode)

hs07_cpc2_2 <- hs07_cpc2_2 %>% 
  left_join(chn_from_world_import2010_2, by=c("HS07Code"="CmdCode")) %>% select(-HS07Code) 


chn_from_world_import2010 <- chn_from_world_import2010_1 %>% 
  bind_rows(hs07_cpc2_2) %>% filter(!is.na(Cifvalue), !is.na(CPC2Code)) 

chn_from_world_import2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_from_world_import2010_cpc2.dta"))








jpn_from_chn_import2010 <- jpn_from_chn_import2010 %>% group_by(CmdCode) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

hs07_cpc2_1 <- hs07_cpc2 %>% filter(weight==1)
hs07_cpc2_2 <- hs07_cpc2 %>% filter(weight!=1)

check <- hs07_cpc2_2 %>% distinct(HS07Code) %>% pull(HS07Code)


jpn_from_chn_import2010_1 <- jpn_from_chn_import2010 %>% filter(!(CmdCode %in% check))  

jpn_from_chn_import2010_2 <- jpn_from_chn_import2010 %>% filter(CmdCode %in% check)  

jpn_from_chn_import2010_1 <- jpn_from_chn_import2010_1 %>% 
  left_join(hs07_cpc2_1, by=c("CmdCode"="HS07Code")) %>% select(-CmdCode)

hs07_cpc2_2 <- hs07_cpc2_2 %>% 
  left_join(jpn_from_chn_import2010_2, by=c("HS07Code"="CmdCode")) %>% select(-HS07Code) 


jpn_from_chn_import2010 <- jpn_from_chn_import2010_1 %>% 
  bind_rows(hs07_cpc2_2) %>%  filter(!is.na(Cifvalue), !is.na(CPC2Code))

jpn_from_chn_import2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_from_chn_import2010_cpc2.dta"))





kor_from_chn_import2010 <- kor_from_chn_import2010 %>% group_by(CmdCode) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

hs07_cpc2_1 <- hs07_cpc2 %>% filter(weight==1)
hs07_cpc2_2 <- hs07_cpc2 %>% filter(weight!=1)

check <- hs07_cpc2_2 %>% distinct(HS07Code) %>% pull(HS07Code)


kor_from_chn_import2010_1 <-kor_from_chn_import2010  %>% filter(!(CmdCode %in% check))  

kor_from_chn_import2010_2 <- kor_from_chn_import2010 %>% filter(CmdCode %in% check)  

kor_from_chn_import2010_1 <- kor_from_chn_import2010_1 %>% 
  left_join(hs07_cpc2_1, by=c("CmdCode"="HS07Code")) %>% select(-CmdCode)

hs07_cpc2_2 <- hs07_cpc2_2 %>% 
  left_join(kor_from_chn_import2010_2, by=c("HS07Code"="CmdCode")) %>% select(-HS07Code) 


kor_from_chn_import2010 <- kor_from_chn_import2010_1 %>% 
  bind_rows(hs07_cpc2_2) %>%  filter(!is.na(Cifvalue), !is.na(CPC2Code))

kor_from_chn_import2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_from_chn_import2010_cpc2.dta"))









chn_to_world_export2010 <-chn_to_world_export2010  %>% group_by(CmdCode) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

hs07_cpc2_1 <- hs07_cpc2 %>% filter(weight==1)
hs07_cpc2_2 <- hs07_cpc2 %>% filter(weight!=1)

check <- hs07_cpc2_2 %>% distinct(HS07Code) %>% pull(HS07Code)


chn_to_world_export2010_1 <- chn_to_world_export2010 %>% filter(!(CmdCode %in% check))  

chn_to_world_export2010_2 <-chn_to_world_export2010  %>% filter(CmdCode %in% check)  

chn_to_world_export2010_1 <- chn_to_world_export2010_1 %>% 
  left_join(hs07_cpc2_1, by=c("CmdCode"="HS07Code")) %>% select(-CmdCode)

hs07_cpc2_2 <- hs07_cpc2_2 %>% 
  left_join(chn_to_world_export2010_2, by=c("HS07Code"="CmdCode")) %>% select(-HS07Code) 


chn_to_world_export2010 <- chn_to_world_export2010_1 %>% 
  bind_rows(hs07_cpc2_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC2Code))

chn_to_world_export2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_to_world_export2010_cpc2.dta"))





jpn_to_chn_export2010 <-jpn_to_chn_export2010  %>% group_by(CmdCode) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

hs07_cpc2_1 <- hs07_cpc2 %>% filter(weight==1)
hs07_cpc2_2 <- hs07_cpc2 %>% filter(weight!=1)

check <- hs07_cpc2_2 %>% distinct(HS07Code) %>% pull(HS07Code)


jpn_to_chn_export2010_1 <- jpn_to_chn_export2010 %>% filter(!(CmdCode %in% check))  

jpn_to_chn_export2010_2 <-jpn_to_chn_export2010  %>% filter(CmdCode %in% check)  

jpn_to_chn_export2010_1 <- jpn_to_chn_export2010_1 %>% 
  left_join(hs07_cpc2_1, by=c("CmdCode"="HS07Code")) %>% select(-CmdCode)

hs07_cpc2_2 <- hs07_cpc2_2 %>% 
  left_join(jpn_to_chn_export2010_2, by=c("HS07Code"="CmdCode")) %>% select(-HS07Code) 


jpn_to_chn_export2010 <- jpn_to_chn_export2010_1 %>% 
  bind_rows(hs07_cpc2_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC2Code))

jpn_to_chn_export2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_to_chn_export2010_cpc2.dta"))





kor_to_chn_export2010 <-kor_to_chn_export2010  %>% group_by(CmdCode) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

hs07_cpc2_1 <- hs07_cpc2 %>% filter(weight==1)
hs07_cpc2_2 <- hs07_cpc2 %>% filter(weight!=1)

check <- hs07_cpc2_2 %>% distinct(HS07Code) %>% pull(HS07Code)


kor_to_chn_export2010_1 <- kor_to_chn_export2010 %>% filter(!(CmdCode %in% check))  

kor_to_chn_export2010_2 <-kor_to_chn_export2010  %>% filter(CmdCode %in% check)  

kor_to_chn_export2010_1 <- kor_to_chn_export2010_1 %>% 
  left_join(hs07_cpc2_1, by=c("CmdCode"="HS07Code")) %>% select(-CmdCode)

hs07_cpc2_2 <- hs07_cpc2_2 %>% 
  left_join(kor_to_chn_export2010_2, by=c("HS07Code"="CmdCode")) %>% select(-HS07Code) 


kor_to_chn_export2010 <- kor_to_chn_export2010_1 %>% 
  bind_rows(hs07_cpc2_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC2Code))

kor_to_chn_export2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_to_chn_export2010_cpc2.dta"))







# cpc2 to cpc21

chn_from_world_import2010 <- chn_from_world_import2010 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)

chn_from_world_import2010 <- chn_from_world_import2010 %>% group_by(CPC2Code) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc2_cpc21_1 <- cpc2_cpc21 %>% filter(weight==1)
cpc2_cpc21_2 <- cpc2_cpc21 %>% filter(weight!=1)

check <- cpc2_cpc21_2 %>% distinct(CPC2code) %>% pull(CPC2code)


chn_from_world_import2010_1 <- chn_from_world_import2010 %>% filter(!(CPC2Code %in% check))  

chn_from_world_import2010_2 <- chn_from_world_import2010 %>% filter(CPC2Code %in% check)  

chn_from_world_import2010_1 <- chn_from_world_import2010_1 %>% 
  left_join(cpc2_cpc21_1, by=c("CPC2Code"="CPC2code")) %>% select(-CPC2Code)

cpc2_cpc21_2 <- cpc2_cpc21_2 %>% 
  left_join(chn_from_world_import2010_2, by=c("CPC2code"="CPC2Code")) %>% select(-CPC2code) 


chn_from_world_import2010 <- chn_from_world_import2010_1 %>% 
  bind_rows(cpc2_cpc21_2) %>% filter(!is.na(Cifvalue), !is.na(CPC21code))


chn_from_world_import2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_from_world_import2010_cpc21.dta"))







jpn_from_chn_import2010 <- jpn_from_chn_import2010 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)


jpn_from_chn_import2010 <- jpn_from_chn_import2010 %>% group_by(CPC2Code) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc2_cpc21_1 <- cpc2_cpc21 %>% filter(weight==1)
cpc2_cpc21_2 <- cpc2_cpc21 %>% filter(weight!=1)

check <- cpc2_cpc21_2 %>% distinct(CPC2code) %>% pull(CPC2code)


jpn_from_chn_import2010_1 <- jpn_from_chn_import2010 %>% filter(!(CPC2Code %in% check))  

jpn_from_chn_import2010_2 <- jpn_from_chn_import2010 %>% filter(CPC2Code %in% check)  

jpn_from_chn_import2010_1 <- jpn_from_chn_import2010_1 %>% 
  left_join(cpc2_cpc21_1, by=c("CPC2Code"="CPC2code")) %>% select(-CPC2Code)

cpc2_cpc21_2 <- cpc2_cpc21_2 %>% 
  left_join(jpn_from_chn_import2010_2, by=c("CPC2code"="CPC2Code")) %>% select(-CPC2code) 


jpn_from_chn_import2010 <- jpn_from_chn_import2010_1 %>% 
  bind_rows(cpc2_cpc21_2) %>%  filter(!is.na(Cifvalue), !is.na(CPC21code))

jpn_from_chn_import2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_from_chn_import2010_cpc21.dta"))




kor_from_chn_import2010 <- kor_from_chn_import2010 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)

kor_from_chn_import2010 <- kor_from_chn_import2010 %>% group_by(CPC2Code) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc2_cpc21_1 <- cpc2_cpc21 %>% filter(weight==1)
cpc2_cpc21_2 <- cpc2_cpc21 %>% filter(weight!=1)

check <- cpc2_cpc21_2 %>% distinct(CPC2code) %>% pull(CPC2code)


kor_from_chn_import2010_1 <-kor_from_chn_import2010  %>% filter(!(CPC2Code %in% check))  

kor_from_chn_import2010_2 <- kor_from_chn_import2010 %>% filter(CPC2Code %in% check)  

kor_from_chn_import2010_1 <- kor_from_chn_import2010_1 %>% 
  left_join(cpc2_cpc21_1, by=c("CPC2Code"="CPC2code")) %>% select(-CPC2Code)

cpc2_cpc21_2 <- cpc2_cpc21_2 %>% 
  left_join(kor_from_chn_import2010_2, by=c("CPC2code"="CPC2Code")) %>% select(-CPC2code) 


kor_from_chn_import2010 <- kor_from_chn_import2010_1 %>% 
  bind_rows(cpc2_cpc21_2) %>%  filter(!is.na(Cifvalue), !is.na(CPC21code))

kor_from_chn_import2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_from_chn_import2010_cpc21.dta"))








chn_to_world_export2010 <- chn_to_world_export2010 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


chn_to_world_export2010 <-chn_to_world_export2010  %>% group_by(CPC2Code) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc2_cpc21_1 <- cpc2_cpc21 %>% filter(weight==1)
cpc2_cpc21_2 <- cpc2_cpc21 %>% filter(weight!=1)

check <- cpc2_cpc21_2 %>% distinct(CPC2code) %>% pull(CPC2code)


chn_to_world_export2010_1 <- chn_to_world_export2010 %>% filter(!(CPC2Code %in% check))  

chn_to_world_export2010_2 <-chn_to_world_export2010  %>% filter(CPC2Code %in% check)  

chn_to_world_export2010_1 <- chn_to_world_export2010_1 %>% 
  left_join(cpc2_cpc21_1, by=c("CPC2Code"="CPC2code")) %>% select(-CPC2Code)

cpc2_cpc21_2 <- cpc2_cpc21_2 %>% 
  left_join(chn_to_world_export2010_2, by=c("CPC2code"="CPC2Code")) %>% select(-CPC2code) 

chn_to_world_export2010 <- chn_to_world_export2010_1 %>% 
  bind_rows(cpc2_cpc21_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC21code))

chn_to_world_export2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_to_world_export2010_cpc21.dta"))




jpn_to_chn_export2010 <- jpn_to_chn_export2010 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


jpn_to_chn_export2010 <-jpn_to_chn_export2010  %>% group_by(CPC2Code) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc2_cpc21_1 <- cpc2_cpc21 %>% filter(weight==1)
cpc2_cpc21_2 <- cpc2_cpc21 %>% filter(weight!=1)

check <- cpc2_cpc21_2 %>% distinct(CPC2code) %>% pull(CPC2code)


jpn_to_chn_export2010_1 <- jpn_to_chn_export2010 %>% filter(!(CPC2Code %in% check))  

jpn_to_chn_export2010_2 <-jpn_to_chn_export2010  %>% filter(CPC2Code %in% check)  

jpn_to_chn_export2010_1 <- jpn_to_chn_export2010_1 %>% 
  left_join(cpc2_cpc21_1, by=c("CPC2Code"="CPC2code")) %>% select(-CPC2Code)

cpc2_cpc21_2 <- cpc2_cpc21_2 %>% 
  left_join(jpn_to_chn_export2010_2, by=c("CPC2code"="CPC2Code")) %>% select(-CPC2code) 


jpn_to_chn_export2010 <- jpn_to_chn_export2010_1 %>% 
  bind_rows(cpc2_cpc21_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC21code))

jpn_to_chn_export2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_to_chn_export2010_cpc21.dta"))





kor_to_chn_export2010 <- kor_to_chn_export2010 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)

kor_to_chn_export2010 <-kor_to_chn_export2010  %>% group_by(CPC2Code) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc2_cpc21_1 <- cpc2_cpc21 %>% filter(weight==1)
cpc2_cpc21_2 <- cpc2_cpc21 %>% filter(weight!=1)

check <- cpc2_cpc21_2 %>% distinct(CPC2code) %>% pull(CPC2code)


kor_to_chn_export2010_1 <- kor_to_chn_export2010 %>% filter(!(CPC2Code %in% check))  

kor_to_chn_export2010_2 <-kor_to_chn_export2010  %>% filter(CPC2Code %in% check)  

kor_to_chn_export2010_1 <- kor_to_chn_export2010_1 %>% 
  left_join(cpc2_cpc21_1, by=c("CPC2Code"="CPC2code")) %>% select(-CPC2Code)

cpc2_cpc21_2 <- cpc2_cpc21_2 %>% 
  left_join(kor_to_chn_export2010_2, by=c("CPC2code"="CPC2Code")) %>% select(-CPC2code) 


kor_to_chn_export2010 <- kor_to_chn_export2010_1 %>% 
  bind_rows(cpc2_cpc21_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC21code))

kor_to_chn_export2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_to_chn_export2010_cpc21.dta"))









# cpc21 to ksic10

chn_from_world_import2010 <- chn_from_world_import2010 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)

chn_from_world_import2010 <- chn_from_world_import2010 %>% group_by(CPC21code) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


chn_from_world_import2010_1 <- chn_from_world_import2010 %>% filter(!(CPC21code %in% check))  

chn_from_world_import2010_2 <- chn_from_world_import2010 %>% filter(CPC21code %in% check)  

chn_from_world_import2010_1 <- chn_from_world_import2010_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21code"="cpc21")) %>% select(-CPC21code)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(chn_from_world_import2010_2, by=c("cpc21"="CPC21code")) %>% select(-cpc21) 


chn_from_world_import2010 <- chn_from_world_import2010_1 %>% 
  bind_rows(cpc21_ksic10_2) %>% filter(!is.na(Cifvalue), !is.na(ksic10))


chn_from_world_import2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_from_world_import2010_ksic10.dta"))







jpn_from_chn_import2010 <- jpn_from_chn_import2010 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)


jpn_from_chn_import2010 <- jpn_from_chn_import2010 %>% group_by(CPC21code) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


jpn_from_chn_import2010_1 <- jpn_from_chn_import2010 %>% filter(!(CPC21code %in% check))  

jpn_from_chn_import2010_2 <- jpn_from_chn_import2010 %>% filter(CPC21code %in% check)  

jpn_from_chn_import2010_1 <- jpn_from_chn_import2010_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21code"="cpc21")) %>% select(-CPC21code)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(jpn_from_chn_import2010_2, by=c("cpc21"="CPC21code")) %>% select(-cpc21) 


jpn_from_chn_import2010 <- jpn_from_chn_import2010_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Cifvalue), !is.na(ksic10))

jpn_from_chn_import2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_from_chn_import2010_ksic10.dta"))




kor_from_chn_import2010 <- kor_from_chn_import2010 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)

kor_from_chn_import2010 <- kor_from_chn_import2010 %>% group_by(CPC21code) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


kor_from_chn_import2010_1 <-kor_from_chn_import2010  %>% filter(!(CPC21code %in% check))  

kor_from_chn_import2010_2 <- kor_from_chn_import2010 %>% filter(CPC21code %in% check)  

kor_from_chn_import2010_1 <- kor_from_chn_import2010_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21code"="cpc21")) %>% select(-CPC21code)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(kor_from_chn_import2010_2, by=c("cpc21"="CPC21code")) %>% select(-cpc21) 


kor_from_chn_import2010 <- kor_from_chn_import2010_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Cifvalue), !is.na(ksic10))

kor_from_chn_import2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_from_chn_import2010_ksic10.dta"))








chn_to_world_export2010 <- chn_to_world_export2010 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


chn_to_world_export2010 <-chn_to_world_export2010  %>% group_by(CPC21code) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


chn_to_world_export2010_1 <- chn_to_world_export2010 %>% filter(!(CPC21code %in% check))  

chn_to_world_export2010_2 <-chn_to_world_export2010  %>% filter(CPC21code %in% check)  

chn_to_world_export2010_1 <- chn_to_world_export2010_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21code"="cpc21")) %>% select(-CPC21code)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(chn_to_world_export2010_2, by=c("cpc21"="CPC21code")) %>% select(-cpc21) 


chn_to_world_export2010 <- chn_to_world_export2010_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Fobvalue), !is.na(ksic10))

chn_to_world_export2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_to_world_export2010_ksic10.dta"))




jpn_to_chn_export2010 <- jpn_to_chn_export2010 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


jpn_to_chn_export2010 <-jpn_to_chn_export2010  %>% group_by(CPC21code) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


jpn_to_chn_export2010_1 <- jpn_to_chn_export2010 %>% filter(!(CPC21code %in% check))  

jpn_to_chn_export2010_2 <-jpn_to_chn_export2010  %>% filter(CPC21code %in% check)  

jpn_to_chn_export2010_1 <- jpn_to_chn_export2010_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21code"="cpc21")) %>% select(-CPC21code)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(jpn_to_chn_export2010_2, by=c("cpc21"="CPC21code")) %>% select(-cpc21) 


jpn_to_chn_export2010 <- jpn_to_chn_export2010_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Fobvalue), !is.na(ksic10))

jpn_to_chn_export2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_to_chn_export2010_ksic10.dta"))





kor_to_chn_export2010 <- kor_to_chn_export2010 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)

kor_to_chn_export2010 <-kor_to_chn_export2010  %>% group_by(CPC21code) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


kor_to_chn_export2010_1 <- kor_to_chn_export2010 %>% filter(!(CPC21code %in% check))  

kor_to_chn_export2010_2 <-kor_to_chn_export2010  %>% filter(CPC21code %in% check)  

kor_to_chn_export2010_1 <- kor_to_chn_export2010_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21code"="cpc21")) %>% select(-CPC21code)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(kor_to_chn_export2010_2, by=c("cpc21"="CPC21code")) %>% select(-cpc21) 


kor_to_chn_export2010 <- kor_to_chn_export2010_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Fobvalue), !is.na(ksic10))

kor_to_chn_export2010 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_to_chn_export2010_ksic10.dta"))





# cpc21 to ksic10 save

chn_from_world_import2010 <- chn_from_world_import2010 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)



chn_from_world_import2010 %>% group_by(ksic10) %>% summarise(Cifvalue=sum(Cifvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_from_world_import2010_ksic10.dta"))







jpn_from_chn_import2010 <- jpn_from_chn_import2010 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)



jpn_from_chn_import2010 %>%  group_by(ksic10) %>% summarise(Cifvalue=sum(Cifvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_from_chn_import2010_ksic10.dta"))




kor_from_chn_import2010 <- kor_from_chn_import2010 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)


kor_from_chn_import2010 %>%  group_by(ksic10) %>% summarise(Cifvalue=sum(Cifvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_from_chn_import2010_ksic10.dta"))








chn_to_world_export2010 <- chn_to_world_export2010 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


chn_to_world_export2010 %>%  group_by(ksic10) %>% summarise(Fobvalue=sum(Fobvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_to_world_export2010_ksic10.dta"))




jpn_to_chn_export2010 <- jpn_to_chn_export2010 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


jpn_to_chn_export2010 %>%  group_by(ksic10) %>% summarise(Fobvalue=sum(Fobvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_to_chn_export2010_ksic10.dta"))





kor_to_chn_export2010 <- kor_to_chn_export2010 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


kor_to_chn_export2010 %>%  group_by(ksic10) %>% summarise(Fobvalue=sum(Fobvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_to_chn_export2010_ksic10.dta"))









# year 2019-----


# hs17 to cpc21


chn_from_world_import2019 <- chn_from_world_import2019 %>% group_by(CmdCode) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

hs17_cpc21_1 <- hs17_cpc21 %>% filter(weight==1)
hs17_cpc21_2 <- hs17_cpc21 %>% filter(weight!=1)

check <- hs17_cpc21_2 %>% distinct(HS2017) %>% pull(HS2017)


chn_from_world_import2019_1 <- chn_from_world_import2019 %>% filter(!(CmdCode %in% check))  

chn_from_world_import2019_2 <- chn_from_world_import2019 %>% filter(CmdCode %in% check)  

chn_from_world_import2019_1 <- chn_from_world_import2019_1 %>% 
  left_join(hs17_cpc21_1, by=c("CmdCode"="HS2017")) %>% select(-CmdCode)

hs17_cpc21_2 <- hs17_cpc21_2 %>% 
  left_join(chn_from_world_import2019_2, by=c("HS2017"="CmdCode")) %>% select(-HS2017) 


chn_from_world_import2019 <- chn_from_world_import2019_1 %>% 
  bind_rows(hs17_cpc21_2) %>% filter(!is.na(Cifvalue), !is.na(CPC21)) 

chn_from_world_import2019 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_from_world_import2019_cpc21.dta"))









jpn_from_chn_import2019 <- jpn_from_chn_import2019 %>% group_by(CmdCode) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

hs17_cpc21_1 <- hs17_cpc21 %>% filter(weight==1)
hs17_cpc21_2 <- hs17_cpc21 %>% filter(weight!=1)

check <- hs17_cpc21_2 %>% distinct(HS2017) %>% pull(HS2017)


jpn_from_chn_import2019_1 <- jpn_from_chn_import2019 %>% filter(!(CmdCode %in% check))  

jpn_from_chn_import2019_2 <- jpn_from_chn_import2019 %>% filter(CmdCode %in% check)  

jpn_from_chn_import2019_1 <- jpn_from_chn_import2019_1 %>% 
  left_join(hs17_cpc21_1, by=c("CmdCode"="HS2017")) %>% select(-CmdCode)

hs17_cpc21_2 <- hs17_cpc21_2 %>% 
  left_join(jpn_from_chn_import2019_2, by=c("HS2017"="CmdCode")) %>% select(-HS2017) 


jpn_from_chn_import2019 <- jpn_from_chn_import2019_1 %>% 
  bind_rows(hs17_cpc21_2) %>%  filter(!is.na(Cifvalue), !is.na(CPC21))

jpn_from_chn_import2019 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_from_chn_import2019_cpc21.dta"))




kor_from_chn_import2019 <- kor_from_chn_import2019 %>% group_by(CmdCode) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

hs17_cpc21_1 <- hs17_cpc21 %>% filter(weight==1)
hs17_cpc21_2 <- hs17_cpc21 %>% filter(weight!=1)

check <- hs17_cpc21_2 %>% distinct(HS2017) %>% pull(HS2017)


kor_from_chn_import2019_1 <-kor_from_chn_import2019  %>% filter(!(CmdCode %in% check))  

kor_from_chn_import2019_2 <- kor_from_chn_import2019 %>% filter(CmdCode %in% check)  

kor_from_chn_import2019_1 <- kor_from_chn_import2019_1 %>% 
  left_join(hs17_cpc21_1, by=c("CmdCode"="HS2017")) %>% select(-CmdCode)

hs17_cpc21_2 <- hs17_cpc21_2 %>% 
  left_join(kor_from_chn_import2019_2, by=c("HS2017"="CmdCode")) %>% select(-HS2017) 


kor_from_chn_import2019 <- kor_from_chn_import2019_1 %>% 
  bind_rows(hs17_cpc21_2) %>%  filter(!is.na(Cifvalue), !is.na(CPC21))

kor_from_chn_import2019 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_from_chn_import2019_cpc21.dta"))










chn_to_world_export2019 <-chn_to_world_export2019  %>% group_by(CmdCode) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

hs17_cpc21_1 <- hs17_cpc21 %>% filter(weight==1)
hs17_cpc21_2 <- hs17_cpc21 %>% filter(weight!=1)

check <- hs17_cpc21_2 %>% distinct(HS2017) %>% pull(HS2017)


chn_to_world_export2019_1 <- chn_to_world_export2019 %>% filter(!(CmdCode %in% check))  

chn_to_world_export2019_2 <-chn_to_world_export2019  %>% filter(CmdCode %in% check)  

chn_to_world_export2019_1 <- chn_to_world_export2019_1 %>% 
  left_join(hs17_cpc21_1, by=c("CmdCode"="HS2017")) %>% select(-CmdCode)

hs17_cpc21_2 <- hs17_cpc21_2 %>% 
  left_join(chn_to_world_export2019_2, by=c("HS2017"="CmdCode")) %>% select(-HS2017) 


chn_to_world_export2019 <- chn_to_world_export2019_1 %>% 
  bind_rows(hs17_cpc21_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC21))

chn_to_world_export2019 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_to_world_export2019_cpc21.dta"))





jpn_to_chn_export2019 <-jpn_to_chn_export2019  %>% group_by(CmdCode) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

hs17_cpc21_1 <- hs17_cpc21 %>% filter(weight==1)
hs17_cpc21_2 <- hs17_cpc21 %>% filter(weight!=1)

check <- hs17_cpc21_2 %>% distinct(HS2017) %>% pull(HS2017)


jpn_to_chn_export2019_1 <- jpn_to_chn_export2019 %>% filter(!(CmdCode %in% check))  

jpn_to_chn_export2019_2 <-jpn_to_chn_export2019  %>% filter(CmdCode %in% check)  

jpn_to_chn_export2019_1 <- jpn_to_chn_export2019_1 %>% 
  left_join(hs17_cpc21_1, by=c("CmdCode"="HS2017")) %>% select(-CmdCode)

hs17_cpc21_2 <- hs17_cpc21_2 %>% 
  left_join(jpn_to_chn_export2019_2, by=c("HS2017"="CmdCode")) %>% select(-HS2017) 


jpn_to_chn_export2019 <- jpn_to_chn_export2019_1 %>% 
  bind_rows(hs17_cpc21_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC21))

jpn_to_chn_export2019 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_to_chn_export2019_cpc21.dta"))






kor_to_chn_export2019 <-kor_to_chn_export2019  %>% group_by(CmdCode) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

hs17_cpc21_1 <- hs17_cpc21 %>% filter(weight==1)
hs17_cpc21_2 <- hs17_cpc21 %>% filter(weight!=1)

check <- hs17_cpc21_2 %>% distinct(HS2017) %>% pull(HS2017)


kor_to_chn_export2019_1 <- kor_to_chn_export2019 %>% filter(!(CmdCode %in% check))  

kor_to_chn_export2019_2 <-kor_to_chn_export2019  %>% filter(CmdCode %in% check)  

kor_to_chn_export2019_1 <- kor_to_chn_export2019_1 %>% 
  left_join(hs17_cpc21_1, by=c("CmdCode"="HS2017")) %>% select(-CmdCode)

hs17_cpc21_2 <- hs17_cpc21_2 %>% 
  left_join(kor_to_chn_export2019_2, by=c("HS2017"="CmdCode")) %>% select(-HS2017) 


kor_to_chn_export2019 <- kor_to_chn_export2019_1 %>% 
  bind_rows(hs17_cpc21_2) %>%  filter(!is.na(Fobvalue), !is.na(CPC21))

kor_to_chn_export2019 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_to_chn_export2019_cpc21.dta"))





# cpc21 to ksic10

chn_from_world_import2019 <- chn_from_world_import2019 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)

chn_from_world_import2019 <- chn_from_world_import2019 %>% group_by(CPC21) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


chn_from_world_import2019_1 <- chn_from_world_import2019 %>% filter(!(CPC21 %in% check))  

chn_from_world_import2019_2 <- chn_from_world_import2019 %>% filter(CPC21 %in% check)  

chn_from_world_import2019_1 <- chn_from_world_import2019_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21"="cpc21")) %>% select(-CPC21)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(chn_from_world_import2019_2, by=c("cpc21"="CPC21")) %>% select(-cpc21) 


chn_from_world_import2019 <- chn_from_world_import2019_1 %>% 
  bind_rows(cpc21_ksic10_2) %>% filter(!is.na(Cifvalue), !is.na(ksic10))


chn_from_world_import2019 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_from_world_import2019_ksic10.dta"))







jpn_from_chn_import2019 <- jpn_from_chn_import2019 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)


jpn_from_chn_import2019 <- jpn_from_chn_import2019 %>% group_by(CPC21) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


jpn_from_chn_import2019_1 <- jpn_from_chn_import2019 %>% filter(!(CPC21 %in% check))  

jpn_from_chn_import2019_2 <- jpn_from_chn_import2019 %>% filter(CPC21 %in% check)  

jpn_from_chn_import2019_1 <- jpn_from_chn_import2019_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21"="cpc21")) %>% select(-CPC21)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(jpn_from_chn_import2019_2, by=c("cpc21"="CPC21")) %>% select(-cpc21) 


jpn_from_chn_import2019 <- jpn_from_chn_import2019_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Cifvalue), !is.na(ksic10))

jpn_from_chn_import2019 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_from_chn_import2019_ksic10.dta"))




kor_from_chn_import2019 <- kor_from_chn_import2019 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)

kor_from_chn_import2019 <- kor_from_chn_import2019 %>% group_by(CPC21) %>% 
  summarise(Cifvalue=sum(Cifvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


kor_from_chn_import2019_1 <-kor_from_chn_import2019  %>% filter(!(CPC21 %in% check))  

kor_from_chn_import2019_2 <- kor_from_chn_import2019 %>% filter(CPC21 %in% check)  

kor_from_chn_import2019_1 <- kor_from_chn_import2019_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21"="cpc21")) %>% select(-CPC21)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(kor_from_chn_import2019_2, by=c("cpc21"="CPC21")) %>% select(-cpc21) 


kor_from_chn_import2019 <- kor_from_chn_import2019_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Cifvalue), !is.na(ksic10))

kor_from_chn_import2019 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_from_chn_import2019_ksic10.dta"))








chn_to_world_export2019 <- chn_to_world_export2019 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


chn_to_world_export2019 <-chn_to_world_export2019  %>% group_by(CPC21) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


chn_to_world_export2019_1 <- chn_to_world_export2019 %>% filter(!(CPC21 %in% check))  

chn_to_world_export2019_2 <-chn_to_world_export2019  %>% filter(CPC21 %in% check)  

chn_to_world_export2019_1 <- chn_to_world_export2019_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21"="cpc21")) %>% select(-CPC21)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(chn_to_world_export2019_2, by=c("cpc21"="CPC21")) %>% select(-cpc21) 


chn_to_world_export2019 <- chn_to_world_export2019_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Fobvalue), !is.na(ksic10))

chn_to_world_export2019 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_to_world_export2019_ksic10.dta"))




jpn_to_chn_export2019 <- jpn_to_chn_export2019 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


jpn_to_chn_export2019 <-jpn_to_chn_export2019  %>% group_by(CPC21) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


jpn_to_chn_export2019_1 <- jpn_to_chn_export2019 %>% filter(!(CPC21 %in% check))  

jpn_to_chn_export2019_2 <-jpn_to_chn_export2019  %>% filter(CPC21 %in% check)  

jpn_to_chn_export2019_1 <- jpn_to_chn_export2019_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21"="cpc21")) %>% select(-CPC21)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(jpn_to_chn_export2019_2, by=c("cpc21"="CPC21")) %>% select(-cpc21) 


jpn_to_chn_export2019 <- jpn_to_chn_export2019_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Fobvalue), !is.na(ksic10))

jpn_to_chn_export2019 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_to_chn_export2019_ksic10.dta"))





kor_to_chn_export2019 <- kor_to_chn_export2019 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)

kor_to_chn_export2019 <-kor_to_chn_export2019  %>% group_by(CPC21) %>% 
  summarise(Fobvalue=sum(Fobvalue, na.rm=T))

cpc21_ksic10_1 <- cpc21_ksic10 %>% filter(weight==1)
cpc21_ksic10_2 <- cpc21_ksic10 %>% filter(weight!=1)

check <- cpc21_ksic10_2 %>% distinct(cpc21) %>% pull(cpc21)


kor_to_chn_export2019_1 <- kor_to_chn_export2019 %>% filter(!(CPC21 %in% check))  

kor_to_chn_export2019_2 <-kor_to_chn_export2019  %>% filter(CPC21 %in% check)  

kor_to_chn_export2019_1 <- kor_to_chn_export2019_1 %>% 
  left_join(cpc21_ksic10_1, by=c("CPC21"="cpc21")) %>% select(-CPC21)

cpc21_ksic10_2 <- cpc21_ksic10_2 %>% 
  left_join(kor_to_chn_export2019_2, by=c("cpc21"="CPC21")) %>% select(-cpc21) 


kor_to_chn_export2019 <- kor_to_chn_export2019_1 %>% 
  bind_rows(cpc21_ksic10_2) %>%  filter(!is.na(Fobvalue), !is.na(ksic10))

kor_to_chn_export2019 %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_to_chn_export2019_ksic10.dta"))





# cpc21 to ksic10 save

chn_from_world_import2019 <- chn_from_world_import2019 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)



chn_from_world_import2019 %>% group_by(ksic10) %>% summarise(Cifvalue=sum(Cifvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_from_world_import2019_ksic10.dta"))







jpn_from_chn_import2019 <- jpn_from_chn_import2019 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)



jpn_from_chn_import2019 %>%  group_by(ksic10) %>% summarise(Cifvalue=sum(Cifvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_from_chn_import2019_ksic10.dta"))




kor_from_chn_import2019 <- kor_from_chn_import2019 %>% mutate(Cifvalue=Cifvalue*weight) %>% 
  select(-weight)


kor_from_chn_import2019 %>%  group_by(ksic10) %>% summarise(Cifvalue=sum(Cifvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_from_chn_import2019_ksic10.dta"))








chn_to_world_export2019 <- chn_to_world_export2019 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


chn_to_world_export2019 %>%  group_by(ksic10) %>% summarise(Fobvalue=sum(Fobvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "chn_to_world_export2019_ksic10.dta"))




jpn_to_chn_export2019 <- jpn_to_chn_export2019 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


jpn_to_chn_export2019 %>%  group_by(ksic10) %>% summarise(Fobvalue=sum(Fobvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "jpn_to_chn_export2019_ksic10.dta"))





kor_to_chn_export2019 <- kor_to_chn_export2019 %>% mutate(Fobvalue=Fobvalue*weight) %>% 
  select(-weight)


kor_to_chn_export2019 %>%  group_by(ksic10) %>% summarise(Fobvalue=sum(Fobvalue, na.rm=T)) %>% 
  haven::write_dta(here("data", "final", "hs_cpc_ksic", "kor_to_chn_export2019_ksic10.dta"))











