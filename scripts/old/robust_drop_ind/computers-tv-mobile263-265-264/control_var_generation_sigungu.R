if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(concordance, here, sf, gt, texreg, fixest, viridis, patchwork, hrbrthemes, circlize, chorddiag, tidyverse, igraph, tidygraph, extrafont, magick, pdftools, broom, rmapshaper, sanghoon, tidyverse)


# This code adds control variables X into our main regression data.


tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")


## basic setup before making figures.

# read in the concordance for region stat----

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



# 1. add control for percentage of woman emp


# first get percentage of woman employment

est2000_share_female <- est2000 %>% group_by(iso) %>% 
  summarise(emp_female=sum(emp_female, na.rm=T), emp_all=sum(emp_all, na.rm=T))

est2000_share_female <- est2000_share_female %>% 
  mutate(emp_share_female=emp_female/emp_all)


# add it onto the data

tb_all <- tb_all %>% left_join(est2000_share_female %>% select(iso, emp_share_female), by=c("iso_o"="iso")) %>% rename(emp_share_female_o=emp_share_female)
tb_all <- tb_all %>% left_join(est2000_share_female %>% select(iso, emp_share_female), by=c("iso_d"="iso")) %>% rename(emp_share_female_d=emp_share_female)





# 2. add control for percentage of manu worker
#create manu worker

est2000 <- est2000 %>% mutate(ind=ifelse(as.numeric(str_sub(ind, 1, 2))>=15 & as.numeric(str_sub(ind, 1, 2))<=37, 1, 0))

est2000 <- est2000 %>% group_by(iso) %>% 
  mutate(emp_tot=sum(emp_all))

est2000 <- est2000 %>% group_by(iso, ind) %>% 
  mutate(emp_manu=sum(emp_all)) %>% ungroup()


est2000 <- est2000 %>% filter(ind==1) %>% select(iso, emp_tot, emp_manu) %>% 
  distinct(iso, .keep_all = T) %>% 
  mutate(emp_share_manu=emp_manu/emp_tot)

# add it onto the data

tb_all <- tb_all %>% left_join(est2000 %>% select(iso, emp_share_manu), by=c("iso_o"="iso")) %>% rename(emp_share_manu_o=emp_share_manu)
tb_all <- tb_all %>% left_join(est2000 %>% select(iso, emp_share_manu), by=c("iso_d"="iso")) %>% rename(emp_share_manu_d=emp_share_manu)






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

data <- data %>% group_by(iso, type) %>% 
  summarise(value=sum(value, na.rm=T)) %>% ungroup()


# now construct college edu percent

data <- data %>% group_by(iso) %>% 
  mutate(value_all=sum(value, na.rm=T))

data <- data %>% mutate(value=value/value_all)

data <- data %>% mutate(type=ifelse(type=="in_edu_005" | type=="in_edu_006" | type=="in_edu_007", "1", "0"))

data <- data %>% filter(type==1) %>% group_by(iso) %>% 
  summarise(value=sum(value, na.rm=T))



# add it onto the data

tb_all <- tb_all %>% left_join(data %>% select(iso, value), by=c("iso_o"="iso")) %>% rename(edu_o=value)
tb_all <- tb_all %>% left_join(data %>% select(iso, value), by=c("iso_d"="iso")) %>% rename(edu_d=value)










# use cz version to estimate distance

# read in shp and match region

region_stat <- readxl::read_excel("concordance/region/region_stat.xlsx")

region_stat <- region_stat %>% mutate(across(everything(), as.character))


shp <- st_read("data/final/region/bnd_sigungu_00_2021_2021_4Q.shp") %>% st_simplify(dTolerance = 1000)

shp <- shp %>% mutate(SIGUNGU_CD=ifelse(str_sub(SIGUNGU_CD, 5, 5)!="0", paste0(str_sub(SIGUNGU_CD, 1, 4), "0"), SIGUNGU_CD))  %>% 
  group_by(SIGUNGU_CD) %>% summarise(geometry = st_union(geometry), .groups = "keep") %>% 
  group_by(SIGUNGU_CD) %>% summarise(geometry = st_combine(geometry))


shp <- shp %>% rename(iso=SIGUNGU_CD)

shp <- shp %>% left_join(region_stat, by=c("iso"="city_stat_raw"))

shp <- shp %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)


shp <- shp %>% group_by(iso) %>% summarise(geometry = st_union(geometry), .groups="keep") %>% 
  summarise(geometry = st_combine(geometry))




region <- read_rds("data/final/final_region_code.rds")

region <- expand.grid(region %>% select(iso) %>% pull(), region %>% select(iso) %>% pull()) %>% rename(iso_o=Var1, iso_d=Var2)



region <- region %>% left_join(shp, by=c("iso_o"="iso")) %>% rename(geometry_o=geometry)
region <- region %>% left_join(shp, by=c("iso_d"="iso")) %>% rename(geometry_d=geometry)


region <- region %>% mutate(centroid_o=st_centroid(geometry_o),
                            centroid_d=st_centroid(geometry_d))


region <- region %>% mutate(distance=st_distance(centroid_o, centroid_d, by_element = T))

check <- region %>% 
  rowwise() %>%
  do(adjacent = ifelse(st_intersects(.$geometry_o, .$geometry_d), 1, 0)) %>%
  ungroup()


region <- region %>% bind_cols(check)

region <- region %>% mutate(adjacent=ifelse(is.na(adjacent), 0, 1))


region <- region %>% select(iso_o, iso_d, distance, adjacent)


# shp <- shp %>% ms_simplify(keep=0.35)


tb_all <- tb_all %>% left_join(region, by=c("iso_o", "iso_d"))



# =======================================================================================

# save the data


tb_all %>%  write_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")











# =======================================================================================




# two year period data


tb_two <-  read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")




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





#read in the est data in 2010.

est2010 <- read.table("data/est/est2010.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                    , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                    , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(est2010) = c("행정구역_시도"
                      , "행정구역_구시군", "산업분류_대", "ind2", "ind3", "ind4", "ind5", "조직형태", "사업체구분", "창업년도", "창업월", "대표자성별", "종사자수_상용종사자_남", "종사자수_상용종사자_여", "종사자수_상용종사자_계", "종사자수_임시 및 일일종사자_남", "종사자수_임시 및 일일종사자_여", "종사자수_임시 및 일일종사자_계", "종사자수_자영업주_남", "종사자수_자영업주_여", "종사자수_자영업주_계"
                      , "종사자수_무급가족종사자_남", "종사자수_무급가족종사자_여", "종사자수_무급가족종사자_계", "종사자수_무급종사자_남", "종사자수_무급종사자_여", "종사자수_무급종사자_계", "emp_male", "emp_female", "emp_all")




# again concord the regions----

est2010 <- est2010 %>% 
  mutate(iso=paste0(`행정구역_시도`, `행정구역_구시군`), 
         ind=paste0(ind2, ind3, ind4, ind5))

est2010 <- est2010 %>% mutate(across(starts_with("emp"), as.numeric))




est2010 <- est2010 %>% group_by(iso, ind) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_male=sum(emp_male, na.rm=T), emp_female=sum(emp_female, na.rm=T)) %>% ungroup()



# change variable to have 0 at end
est2010 <- est2010 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))



est2010 <- est2010 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))



est2010 <- est2010 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)


est2010 <- est2010 %>% group_by(iso, ind) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_male=sum(emp_male, na.rm=T), emp_female=sum(emp_female, na.rm=T)) %>% ungroup()





# 1. add control for percentage of woman emp


# first get percentage of woman employment

est2000_share_female <- est2000 %>% group_by(iso) %>% 
  summarise(emp_female=sum(emp_female, na.rm=T), emp_all=sum(emp_all, na.rm=T))

est2000_share_female <- est2000_share_female %>% 
  mutate(emp_share_female=emp_female/emp_all)


est2010_share_female <- est2010 %>% group_by(iso) %>% 
  summarise(emp_female=sum(emp_female, na.rm=T), emp_all=sum(emp_all, na.rm=T))

est2010_share_female <- est2010_share_female %>% 
  mutate(emp_share_female=emp_female/emp_all)


est2000_share_female <- est2000_share_female %>% mutate(year=1)
est2010_share_female <- est2010_share_female %>% mutate(year=2)

est_share_female <- est2000_share_female %>% bind_rows(est2010_share_female)




# 2. add control for percentage of manu worker
#create manu worker

est2000 <- est2000 %>% mutate(ind=ifelse(as.numeric(str_sub(ind, 1, 2))>=15 & as.numeric(str_sub(ind, 1, 2))<=37, 1, 0))

est2000 <- est2000 %>% group_by(iso) %>% 
  mutate(emp_tot=sum(emp_all))

est2000 <- est2000 %>% group_by(iso, ind) %>% 
  mutate(emp_manu=sum(emp_all)) %>% ungroup()


est2000 <- est2000 %>% filter(ind==1) %>% select(iso, emp_tot, emp_manu) %>% 
  distinct(iso, .keep_all = T) %>% 
  mutate(emp_share_manu=emp_manu/emp_tot)


est2010 <- est2010 %>% mutate(ind=ifelse(as.numeric(str_sub(ind, 1, 2))>=15 & as.numeric(str_sub(ind, 1, 2))<=37, 1, 0))

est2010 <- est2010 %>% group_by(iso) %>% 
  mutate(emp_tot=sum(emp_all))

est2010 <- est2010 %>% group_by(iso, ind) %>% 
  mutate(emp_manu=sum(emp_all)) %>% ungroup()


est2010 <- est2010 %>% filter(ind==1) %>% select(iso, emp_tot, emp_manu) %>% 
  distinct(iso, .keep_all = T) %>% 
  mutate(emp_share_manu=emp_manu/emp_tot)


est2000 <- est2000 %>% mutate(year=1)
est2010 <- est2010 %>% mutate(year=2)


est_manu <- est2000 %>% bind_rows(est2010)


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

data <- data %>% group_by(iso, type) %>% 
  summarise(value=sum(value, na.rm=T)) %>% ungroup()


# now construct college edu percent

data <- data %>% group_by(iso) %>% 
  mutate(value_all=sum(value, na.rm=T))

data <- data %>% mutate(value=value/value_all)

data <- data %>% mutate(type=ifelse(type=="in_edu_005" | type=="in_edu_006" | type=="in_edu_007", "1", "0"))

data <- data %>% filter(type==1) %>% group_by(iso) %>% 
  summarise(value=sum(value, na.rm=T))


edu2000 <- data

edu2000 <- edu2000 %>% mutate(year=1)





data <- read_delim("data/census/edu2010.txt", delim="^", col_names=F) %>% 
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

data <- data %>% group_by(iso, type) %>% 
  summarise(value=sum(value, na.rm=T)) %>% ungroup()


# now construct college edu percent

data <- data %>% group_by(iso) %>% 
  mutate(value_all=sum(value, na.rm=T))

data <- data %>% mutate(value=value/value_all)

data <- data %>% mutate(type=ifelse(type=="in_edu_005" | type=="in_edu_006" | type=="in_edu_007", "1", "0"))

data <- data %>% filter(type==1) %>% group_by(iso) %>% 
  summarise(value=sum(value, na.rm=T))


edu2010 <- data

edu2010 <- edu2010 %>% mutate(year=2)


edu <- edu2000 %>% select(iso)

edu <- edu %>% left_join(edu2010, by="iso")



edu <- edu2000 %>% bind_rows(edu2010)



# add it onto the data

# add it onto the data

tb_two <- tb_two %>% left_join(est_share_female %>% select(iso, emp_share_female, year), by=c("iso_o"="iso", "year")) %>% rename(emp_share_female_o=emp_share_female)
tb_two <- tb_two %>% left_join(est_share_female %>% select(iso, emp_share_female, year), by=c("iso_d"="iso", "year")) %>% rename(emp_share_female_d=emp_share_female)






# add it onto the data

tb_two <- tb_two %>% left_join(est_manu %>% select(iso, emp_share_manu, year), by=c("iso_o"="iso", "year")) %>% rename(emp_share_manu_o=emp_share_manu)
tb_two <- tb_two %>% left_join(est_manu %>% select(iso, emp_share_manu, year), by=c("iso_d"="iso", "year")) %>% rename(emp_share_manu_d=emp_share_manu)








tb_two <- tb_two %>% left_join(edu %>% select(iso, value, year), by=c("iso_o"="iso", "year")) %>% rename(edu_o=value)
tb_two <- tb_two %>% left_join(edu %>% select(iso, value, year), by=c("iso_d"="iso", "year")) %>% rename(edu_d=value)



tb_two %>% write_rds("data/final/shock/shock_main_data_sigungu_two_period.rds")
