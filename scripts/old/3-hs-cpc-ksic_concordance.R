
##### overall framework:

# This file makes the concordance table for hs-hs, hs-cpc, cpc-cpc, cpc-ksic, ksic-ksic

# for 1:1 or n:1, just simply use left_join
# for 1:n we need to do right_join (or just do left join but otherway around)

if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(concordance, tidyverse)





# first refine cpc 2 to cpc 2.1 table by taking care of cases where cpc2:cpc2.1 = 1:n

cpc2_cpc21 <- read_csv("concordance/hscode/CPCv2_CPCv21.csv")


# filter out where cpc2:cpc2.1 = 1:n

cpc2_cpc21_n <- cpc2_cpc21 %>% group_by(CPC2code) %>% 
  summarise(n=n()) %>% arrange(desc(n)) %>% 
  filter(n>1) %>% select(-n)

cpc2_cpc21_n %>% view()

cpc2_cpc21_n <- cpc2_cpc21 %>% filter(CPC2code %in% pull(cpc2_cpc21_n %>% select(CPC2code))) %>% select(CPC2code, CPC21code)
  
cpc2_cpc21_n <- cpc2_cpc21_n %>% group_by(CPC2code) %>% mutate(n=n()) %>% ungroup()
cpc2_cpc21_n <- cpc2_cpc21_n %>% mutate(weight=1/n) %>% select(-n)
cpc2_cpc21_n <- cpc2_cpc21_n %>% select(CPC2code, CPC21code, weight) 

cpc2_cpc21 <- cpc2_cpc21 %>% filter( !(CPC2code %in% pull(cpc2_cpc21_n %>% select(CPC2code)))  )

cpc2_cpc21 <- cpc2_cpc21 %>% select(CPC2code, CPC21code) %>% mutate(weight=1)


cpc2_cpc21 <- cpc2_cpc21 %>% 
  bind_rows(cpc2_cpc21_n)

cpc2_cpc21 %>% write_rds("concordance/hs_cpc_ksic/cpc2_cpc21.rds")





##### now refine cpc 2.1 to ksic 10 table by taking care of cases where  1:n

#read isic-ksic corcordance
cpc21_ksic10 <- readxl::read_xlsx("concordance/industry/att5_concordance2_for_eng_name.xlsx", skip=1, col_names = TRUE) %>% 
  select(ksic10=`KSIC10\r\n한국표준산업분류`, cpc21=`CPC2.1\r\n중앙생산물분류(UN)`) %>% filter(!is.na(cpc21)) %>% unique()




# filter out where cpc21:ksic = 1:n

cpc21_ksic10_n <- cpc21_ksic10 %>% group_by(cpc21) %>% 
  summarise(n=n()) %>% arrange(desc(n)) %>% 
  filter(n>1) %>% select(-n)

cpc21_ksic10_n %>% view()

cpc21_ksic10_n <- cpc21_ksic10 %>% filter(cpc21 %in% pull(cpc21_ksic10_n %>% select(cpc21))) %>% select(cpc21, ksic10)

cpc21_ksic10_n <- cpc21_ksic10_n %>% group_by(cpc21) %>% mutate(n=n()) %>% ungroup()
cpc21_ksic10_n <- cpc21_ksic10_n %>% mutate(weight=1/n) %>% select(-n)
cpc21_ksic10_n <- cpc21_ksic10_n %>% select(cpc21, ksic10, weight) 

cpc21_ksic10 <- cpc21_ksic10 %>% filter( !(cpc21 %in% pull(cpc21_ksic10_n %>% select(cpc21)))  )

cpc21_ksic10 <- cpc21_ksic10 %>% select(cpc21, ksic10) %>% mutate(weight=1)


cpc21_ksic10 <- cpc21_ksic10 %>% 
  bind_rows(cpc21_ksic10_n)

cpc21_ksic10 %>% write_rds("concordance/hs_cpc_ksic/cpc21_ksic10.rds")
cpc21_ksic10 %>% write_rds("data/final/hs_cpc_ksic/cpc21_ksic10.rds")






# -----

# refine ksic8 to ksic9 table by taking care of cases where ksic8:ksic9 = 1:n

ksic8_ksic9 <- readxl::read_excel("concordance/industry/ksic_8to9.xls")

ksic8_ksic9 <- ksic8_ksic9 %>% mutate(ksic8=ifelse(!str_detect(ksic8, "\\d"), NA, ksic8), ksic9=ifelse(!str_detect(ksic9, "\\d"), NA, ksic9))

ksic8_ksic9 <- ksic8_ksic9 %>% fill(ksic8)


# filter out where ksic8:ksic9 = 1:n

ksic8_ksic9_n <- ksic8_ksic9 %>% group_by(ksic8) %>% 
  summarise(n=n()) %>% arrange(desc(n)) %>% 
  filter(n>1) %>% select(-n)

ksic8_ksic9_n %>% view()

ksic8_ksic9_n <- ksic8_ksic9 %>% filter(ksic8 %in% pull(ksic8_ksic9_n %>% select(ksic8))) %>% select(ksic8, ksic9)



ksic8_ksic9_n <- ksic8_ksic9_n %>% group_by(ksic8) %>% mutate(n=n()) %>% ungroup()
ksic8_ksic9_n <- ksic8_ksic9_n %>% mutate(weight=1/n) %>% select(-n)
ksic8_ksic9_n <- ksic8_ksic9_n %>% select(ksic8, ksic9, weight) 


ksic8_ksic9 <- ksic8_ksic9 %>% filter( !(ksic8 %in% pull(ksic8_ksic9_n %>% select(ksic8)))  )

ksic8_ksic9 <- ksic8_ksic9 %>% select(ksic8, ksic9) %>% mutate(weight=1)


ksic8_ksic9 <- ksic8_ksic9 %>% 
  bind_rows(ksic8_ksic9_n)

ksic8_ksic9 %>% write_rds("concordance/hs_cpc_ksic/ksic8_ksic9.rds")
ksic8_ksic9 %>% write_rds("data/final/hs_cpc_ksic/ksic8_ksic9.rds")






##

# refine ksic9 to ksic10 table by taking care of cases where ksic9:ksic10 = 1:n

ksic9_ksic10 <- readxl::read_excel("concordance/industry/ksic_9to10.xlsx")


ksic9_ksic10 <- ksic9_ksic10 %>% mutate(ksic9=ifelse(!str_detect(ksic9, "\\d"), NA, ksic9), ksic10=ifelse(!str_detect(ksic10, "\\d"), NA, ksic10))


# fill the na in ksic9 by upper value

ksic9_ksic10 <- ksic9_ksic10 %>% fill(ksic9)




# filter out where ksic8:ksic9 = 1:n

ksic9_ksic10_n <- ksic9_ksic10 %>% group_by(ksic9) %>% 
  summarise(n=n()) %>% arrange(desc(n)) %>% 
  filter(n>1) %>% select(-n)

ksic9_ksic10_n %>% view()

ksic9_ksic10_n <- ksic9_ksic10 %>% filter(ksic9 %in% pull(ksic9_ksic10_n %>% select(ksic9))) %>% select(ksic9, ksic10)



ksic9_ksic10_n <- ksic9_ksic10_n %>% group_by(ksic9) %>% mutate(n=n()) %>% ungroup()
ksic9_ksic10_n <- ksic9_ksic10_n %>% mutate(weight=1/n) %>% select(-n)
ksic9_ksic10_n <- ksic9_ksic10_n %>% select(ksic9, ksic10, weight) 


ksic9_ksic10 <- ksic9_ksic10 %>% filter( !(ksic9 %in% pull(ksic9_ksic10_n %>% select(ksic9)))  )

ksic9_ksic10 <- ksic9_ksic10 %>% select(ksic9, ksic10) %>% mutate(weight=1)


ksic9_ksic10<- ksic9_ksic10 %>% 
  bind_rows(ksic9_ksic10_n)

ksic9_ksic10 %>% write_rds("concordance/hs_cpc_ksic/ksic9_ksic10.rds")
ksic9_ksic10 %>% write_rds("data/final/hs_cpc_ksic/ksic9_ksic10.rds")





##


# hs1996 to hs2007

# first refine 1:n
hs96_hs07 <- readxl::read_excel("concordance/hscode/HS2007toHS1996Conversion.xlsx", sheet=2) %>% 
  filter(!is.na(HS_2007))


# filter out where  1:n

hs96_hs07_n <- hs96_hs07 %>% group_by(HS_1996) %>% 
  summarise(n=n()) %>% arrange(desc(n)) %>% 
  filter(n>1) %>% select(-n)

hs96_hs07_n %>% view()

hs96_hs07_n <- hs96_hs07 %>% filter(HS_1996 %in% pull(hs96_hs07_n %>% select(HS_1996))) %>% select(HS_1996, HS_2007)

hs96_hs07_n <- hs96_hs07_n %>% group_by(HS_1996) %>% mutate(n=n()) %>% ungroup()
hs96_hs07_n <- hs96_hs07_n %>% mutate(weight=1/n) %>% select(-n)
hs96_hs07_n <- hs96_hs07_n %>% select(HS_1996, HS_2007, weight) 

hs96_hs07 <- hs96_hs07 %>% filter( !(HS_1996 %in% pull(hs96_hs07_n %>% select(HS_1996)))  )

hs96_hs07  <- hs96_hs07  %>% select(HS_1996, HS_2007) %>% mutate(weight=1)


hs96_hs07  <- hs96_hs07  %>% 
  bind_rows(hs96_hs07_n)

hs96_hs07  %>% write_rds("concordance/hs_cpc_ksic/hs96_hs07.rds")
hs96_hs07  %>% write_rds("data/final/hs_cpc_ksic/hs96_hs07.rds")

##




# hs2007 to cpc2

# first refine cpc 2 to cpc 2.1 table by taking care of cases where cpc2:cpc2.1 = 1:n

hs07_cpc2 <- read_csv("concordance/hscode/CPCv2_HS2007.csv")

hs07_cpc2 <- hs07_cpc2 %>% mutate(HS07Code=str_remove(HS07Code, "\\."))


# filter out where cpc2:cpc2.1 = 1:n

hs07_cpc2_n <- hs07_cpc2 %>% group_by(HS07Code) %>% 
  summarise(n=n()) %>% arrange(desc(n)) %>% 
  filter(n>1) %>% select(-n)

hs07_cpc2_n %>% view()

hs07_cpc2_n <- hs07_cpc2 %>% filter(HS07Code %in% pull(hs07_cpc2_n %>% select(HS07Code))) %>% select(HS07Code, CPC2Code)

hs07_cpc2_n <- hs07_cpc2_n %>% group_by(HS07Code) %>% mutate(n=n()) %>% ungroup()
hs07_cpc2_n <- hs07_cpc2_n %>% mutate(weight=1/n) %>% select(-n)
hs07_cpc2_n <- hs07_cpc2_n %>% select(HS07Code, CPC2Code, weight) 

hs07_cpc2 <- hs07_cpc2 %>% filter( !(HS07Code %in% pull(hs07_cpc2_n %>% select(HS07Code)))  )

hs07_cpc2 <- hs07_cpc2 %>% select(HS07Code, CPC2Code) %>% mutate(weight=1)


hs07_cpc2 <- hs07_cpc2 %>% 
  bind_rows(hs07_cpc2_n)

hs07_cpc2 %>% write_rds("concordance/hs_cpc_ksic/hs07_cpc2.rds")
hs07_cpc2 %>% write_rds("data/final/hs_cpc_ksic/hs07_cpc2.rds")





# hs2017 to cpc 2.1


# first refine 1:n

hs17_cpc21 <- read_csv("concordance/hscode/CPCv21_HS2017.csv")

hs17_cpc21 <- hs17_cpc21 %>% rename(HS2017=`HS 2017`, CPC21=`CPC Ver. 2.1`)


hs17_cpc21 <- hs17_cpc21 %>% mutate(HS2017=str_remove(HS2017, "\\."))


# filter out where 1:n

hs17_cpc21_n <- hs17_cpc21 %>% group_by(HS2017) %>% 
  summarise(n=n()) %>% arrange(desc(n)) %>% 
  filter(n>1) %>% select(-n)

hs17_cpc21_n %>% view()

hs17_cpc21_n <- hs17_cpc21 %>% filter(HS2017 %in% pull(hs17_cpc21_n %>% select(HS2017))) %>% select(HS2017, CPC21)

hs17_cpc21_n <- hs17_cpc21_n %>% group_by(HS2017) %>% mutate(n=n()) %>% ungroup()
hs17_cpc21_n <- hs17_cpc21_n %>% mutate(weight=1/n) %>% select(-n)
hs17_cpc21_n <- hs17_cpc21_n %>% select(HS2017, CPC21, weight) 


hs17_cpc21 <- hs17_cpc21 %>% filter( !(HS2017 %in% pull(hs17_cpc21_n %>% select(HS2017)))  )

hs17_cpc21 <- hs17_cpc21 %>% select(HS2017, CPC21) %>% mutate(weight=1)


hs17_cpc21 <- hs17_cpc21 %>% 
  bind_rows(hs17_cpc21_n)

hs17_cpc21 %>% write_rds("concordance/hs_cpc_ksic/hs17_cpc21.rds")
hs17_cpc21 %>% write_rds("data/final/hs_cpc_ksic/hs17_cpc21.rds")
