if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(concordance, here, sf, gt, texreg, fixest, viridis, patchwork, hrbrthemes, circlize, chorddiag, tidyverse, igraph, tidygraph, extrafont, magick, pdftools, broom, tidyverse)


# This code creates all the necessary summary stats tables used in the paper.




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




kor_to_chn_export2001 <- kor_to_chn_export2001 %>% 
  summarise(Cifvalue=sum(Fobvalue, na.rm=T))

