
##############################
# creator: Hyoungchul Kim
# revised date: 06/23/2024
# description: This script matches establishment data into regions for analysis. Due to change in geographic unit, we need to make sure the unit code is consistent throught the whole year span. 
##############################

if (!require(pacman)) install.packages("pacman")
pacman::p_load(here, tidyverse)

#-------------------- 
# read in the necessary industry data and concordance table.
#-------------------- 

# read in the concordance for region stat----
region_stat <- readxl::read_excel(here("data", "region", "region_stat.xlsx"))

region_stat <- region_stat %>% mutate(across(everything(), as.character))

region_kosis <- readxl::read_excel(here("data", "region", "region_kosis.xlsx"))

region_kosis <- region_kosis %>% mutate(across(everything(), as.character))

region_kosis <- region_kosis %>% mutate(city_stat=str_sub(city_stat, 1, 5))



# read in est data 1994, 1996, 1998, 1999  2000,  2001, 2010, 2019-----

est1994 <- read.table(here("data", "est", "est1994.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                         , "character", "character", "character", "character", "character", "character", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(est1994) = c("행정구역(시도)"
                   , "행정구역(구시군)", "ind1", "ind2", "ind3", "ind4", "ind5", "emp_all")


est1996 <- read.table(here("data", "est", "est1996.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                    , "character", "character", "character", "character", "character", "character", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(est1996) = c("행정구역(시도)"
                      , "행정구역(구시군)", "ind1", "ind2", "ind3", "ind4", "ind5", "emp_all")




est1999 <- read.table(here("data", "est", "est1999.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                    , "character", "character", "character", "character", "character", "character", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(est1999) = c("행정구역(시도)"
                      , "행정구역(구시군)", "ind1", "ind2", "ind3", "ind4", "ind5", "emp_all")








est2000 <- read.table(here("data", "est", "est2000.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                            , "character", "character", "character", "character", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(est2000) = c("행정구역(시도)", "행정구역(구시군)", "ind1", "ind2", "ind3", "ind4", "ind5", "emp_male", "emp_female", "emp_all")









est2001 <- read.table(here("data", "est", "est2001.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                        , "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "numeric", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                        , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(est2001) = c("행정구역(시도)"
                   , "행정구역(구시군)", "산업분류(대)", "산업분류(중)", "산업분류(소)", "산업분류(세)", "산업분류(세세)", "4.조직형태", "5.사업체구분", "6-1.창설년", "6-2.창설월", "2.대표자성별", "8-①종사자_자영업주_남", "8-①종사자_자영업주_여", "8-①종사자_자영업주_계", "8-②종사자_무급가족_남", "8-②종사자_무급가족_여", "8-②종사자_무급가족_계", "8-③종사자_상용_남", "8-③종사자_상용_여", "8-③종사자_상용_계"
                   , "8-④종사자_임시및일일_남", "8-④종사자_임시및일일_여", "8-④종사자_임시및일일_계", "8-⑤종사자_무급_남", "8-⑤종사자_무급_여", "8-⑤종사자_무급_계", "8-⑥종사자_합계_남(①+…+⑤)", "8-⑥종사자_합계_여(①+…+⑤)", "8-⑥종사자_합계_계(①+…+⑤)")


est2010 <- read.table(here("data", "est", "est2010.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                    , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                    , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(est2010) = c("행정구역_시도"
                   , "행정구역_구시군", "산업분류_대", "산업분류_중", "산업분류_소", "산업분류_세", "산업분류_세세", "조직형태", "사업체구분", "창업년도", "창업월", "대표자성별", "종사자수_상용종사자_남", "종사자수_상용종사자_여", "종사자수_상용종사자_계", "종사자수_임시 및 일일종사자_남", "종사자수_임시 및 일일종사자_여", "종사자수_임시 및 일일종사자_계", "종사자수_자영업주_남", "종사자수_자영업주_여", "종사자수_자영업주_계"
                   , "종사자수_무급가족종사자_남", "종사자수_무급가족종사자_여", "종사자수_무급가족종사자_계", "종사자수_무급종사자_남", "종사자수_무급종사자_여", "종사자수_무급종사자_계", "종사자수_합계_남", "종사자수_합계_여", "종사자수_합계_계")




est2019 <- read.table(here("data", "est", "est2019.csv"), header=FALSE, sep=",", colClasses = c("character"
                                                                                         , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                         , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(est2019) = c("조사기준연도"
                      , "행정구역시도코드", "행정구역시군구코드", "대표자성별코드", "창업연도", "창업월", "조직형태코드", "사업체구분코드", "산업대분류코드", "주사업_산업중분류코드", "주사업_산업소분류코드", "상용근로_합계종사자수", "합계종사자수", "상용근로_남자종사자수", "남자종사자수", "상용근로_여자종사자수", "여자종사자수", "임시일용근로_합계종사자수", "임시일용근로_남자종사자수", "임시일용근로_여자종사자수", "자영업_합계종사자수"
                      , "자영업_남자종사자수", "자영업_여자종사자수", "무급가족_합계종사자수", "무급가족_남자종사자수", "무급가족_여자종사자수", "기타_합계종사자수", "기타_남자종사자수", "기타_여자종사자수", "주사업_산업세분류코드", "주사업_산업세세분류코드")



# again concord the regions----

est1994 <- est1994 %>% mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`))
est1996 <- est1996 %>% mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`))
# est1998 <- est1998 %>% mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`))
est1999 <- est1999 %>% mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`))
est2000 <- est2000 %>% mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`))
est2001 <- est2001 %>% mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`))
est2010 <- est2010 %>% mutate(iso=paste0(`행정구역_시도`, `행정구역_구시군`))
est2019 <- est2019 %>% mutate(iso=paste0(`행정구역시도코드`, `행정구역시군구코드`))


# change variable to have 0 at end
est1994 <- est1994 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
est1996 <- est1996 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
# est1998 <- est1998 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
est1999 <- est1999 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
est2000 <- est2000 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
est2001 <- est2001 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
est2010 <- est2010 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))
est2019 <- est2019 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))


est1994 <- est1994 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
est1996 <- est1996 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
# est1998 <- est1998 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
est1999 <- est1999 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
est2000 <- est2000 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
est2001 <- est2001 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
est2010 <- est2010 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))
est2019 <- est2019 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))


est1994 <- est1994 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
est1996 <- est1996 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
# est1998 <- est1998 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
est1999 <- est1999 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
est2000 <- est2000 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
est2001 <- est2001 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
est2010 <- est2010 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)
est2019 <- est2019 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)






# save them -----

est1994 %>% write_rds(here("data", "temp", "est1994_region_matched.rds"))
est1996 %>% write_rds(here("data", "temp", "est1996_region_matched.rds"))
est1999 %>% write_rds(here("data", "temp", "est1999_region_matched.rds"))
est2000 %>% write_rds(here("data", "temp", "est2000_region_matched.rds"))
est2001 %>% write_rds(here("data", "temp", "est2001_region_matched.rds"))
est2010 %>% write_rds(here("data", "temp", "est2010_region_matched.rds"))
est2019 %>% write_rds(here("data", "temp", "est2019_region_matched.rds"))
