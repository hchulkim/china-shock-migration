if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(concordance, here, sf, gt, texreg, fixest, viridis, patchwork, hrbrthemes, circlize, chorddiag, tidyverse, igraph, tidygraph, extrafont, magick, pdftools, broom, rmapshaper, sanghoon, tidyverse)


# This code adds control variables X into our main regression data.


tb_all <- read_rds("data/final/shock/shock_main_data2001_2019.rds")


## basic setup before making figures.

# read in the concordance for region stat----

com <- read_rds("data/final/cz_code/commuting_zone.rds")

region_stat <- readxl::read_excel("concordance/region/region_stat.xlsx")

region_stat <- region_stat %>% mutate(across(everything(), as.character))

region_kosis <- readxl::read_excel("concordance/region/region_kosis.xlsx")

region_kosis <- region_kosis %>% mutate(across(everything(), as.character))

region_kosis <- region_kosis %>% mutate(city_stat=str_sub(city_stat, 1, 5))



#read in the est data in 2000.

est2000 <- read.table("data/est/est2000_control.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                            , "character", "character", "character", "character", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(est2000) = c("행정구역(시도)", "행정구역(구시군)", "ind1", "ind2", "ind3", "ind4", "ind5", "emp_male", "emp_female", "emp_all")

# again concord the regions----

est2000 <- est2000 %>% 
  mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`), 
         ind=paste0(ind2, ind3, ind4, ind5))

est2000 <- est2000 %>% mutate(across(starts_with("emp"), as.numeric))




est2000 <- est2000 %>% group_by(iso, ind) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_male=sum(emp_male, na.rm=T), emp_female=sum(emp_female, na.rm=T)) %>% ungroup()



# change variable to have 0 at end
est2000 <- est2000 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))



est2000 <- est2000 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))



est2000 <- est2000 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)


est2000 <- est2000 %>% group_by(iso, ind) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_male=sum(emp_male, na.rm=T), emp_female=sum(emp_female, na.rm=T)) %>% ungroup()



# group by iso

est2000 <- est2000 %>% left_join(com, by="iso") %>% rename(cz=cz_id)


est2000 <- est2000 %>% group_by(cz, ind) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_male=sum(emp_male, na.rm=T), emp_female=sum(emp_female, na.rm=T)) %>% ungroup()


# 1. add control for percentage of woman emp


# first get percentage of woman employment

est2000_share_female <- est2000 %>% group_by(cz) %>% 
  summarise(emp_female=sum(emp_female, na.rm=T), emp_all=sum(emp_all, na.rm=T))

est2000_share_female <- est2000_share_female %>% 
  mutate(emp_share_female=emp_female/emp_all)


# add it onto the data

tb_all <- tb_all %>% left_join(est2000_share_female %>% select(cz, emp_share_female), by=c("cz_o"="cz")) %>% rename(emp_share_female_o=emp_share_female)
tb_all <- tb_all %>% left_join(est2000_share_female %>% select(cz, emp_share_female), by=c("cz_d"="cz")) %>% rename(emp_share_female_d=emp_share_female)





# 2. add control for percentage of manu worker
#create manu worker

est2000 <- est2000 %>% mutate(ind=ifelse(as.numeric(str_sub(ind, 1, 2))>=15 & as.numeric(str_sub(ind, 1, 2))<=37, 1, 0))

est2000 <- est2000 %>% group_by(cz) %>% 
  mutate(emp_tot=sum(emp_all))

est2000 <- est2000 %>% group_by(cz, ind) %>% 
  mutate(emp_manu=sum(emp_all)) %>% ungroup()


est2000 <- est2000 %>% filter(ind==1) %>% select(cz, emp_tot, emp_manu) %>% 
  distinct(cz, .keep_all = T) %>% 
  mutate(emp_share_manu=emp_manu/emp_tot)

# add it onto the data

tb_all <- tb_all %>% left_join(est2000 %>% select(cz, emp_share_manu), by=c("cz_o"="cz")) %>% rename(emp_share_manu_o=emp_share_manu)
tb_all <- tb_all %>% left_join(est2000 %>% select(cz, emp_share_manu), by=c("cz_d"="cz")) %>% rename(emp_share_manu_d=emp_share_manu)






# 3. add control for percentage of college educated pop



data <- read_delim("data/census/edu2000.txt", delim="^", col_names=F) %>% 
  select(iso=X2, type=X3, value=X4) %>% 
  mutate(iso=as.character(iso), value=as.numeric(value)) %>% 
  mutate(iso=str_sub(iso, 1, 5)) 


data <- data %>% group_by(iso, type) %>% 
  summarise(value=sum(value, na.rm=T)) %>% ungroup()



# use region match codes to fix the region.

data <- data %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))



data <- data %>% left_join(region_stat, by=c("iso"="city_stat_raw"))



data <- data %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)


# group by cz

data <- data %>% left_join(com, by="iso") %>% rename(cz=cz_id)


data <- data %>% group_by(cz, type) %>% 
  summarise(value=sum(value, na.rm=T)) %>% ungroup()


# now construct college edu percent

data <- data %>% group_by(cz) %>% 
  mutate(value_all=sum(value, na.rm=T))

data <- data %>% mutate(value=value/value_all)

data <- data %>% mutate(type=ifelse(type=="in_edu_005" | type=="in_edu_006" | type=="in_edu_007", "1", "0"))

data <- data %>% filter(type==1) %>% group_by(cz) %>% 
  summarise(value=sum(value, na.rm=T))



# add it onto the data

tb_all <- tb_all %>% left_join(data %>% select(cz, value), by=c("cz_o"="cz")) %>% rename(edu_o=value)
tb_all <- tb_all %>% left_join(data %>% select(cz, value), by=c("cz_d"="cz")) %>% rename(edu_d=value)



# =======================================================================================

# save the data


tb_all %>%  write_rds("data/final/shock/shock_main_data2001_2019.rds")







# =======================================================================================




# two year period data


tb_two <-  read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")


