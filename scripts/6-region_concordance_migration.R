library(tidyverse)


# read in the concordance for region stat----
region_stat <- readxl::read_excel("concordance/region/region_stat.xlsx")

region_stat <- region_stat %>% mutate(across(everything(), as.character))

region_kosis <- readxl::read_excel("concordance/region/region_kosis.xlsx")

region_kosis <- region_kosis %>% mutate(across(everything(), as.character))

region_kosis <- region_kosis %>% mutate(city_stat=str_sub(city_stat, 1, 5))


# read all the migration data----------- ----------------------------

migration1996 <- read.table("data/migration/migration1996.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                             , "character", "numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                             , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                             , "numeric", "numeric"), skip=1, na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(migration1996) = c("행정구역（시도）"
                   , "행정구역（구시군）", "신고년월", "전출지코드(시도)", "전출지코드(구시군)", "연령별남자(0-4)", "연령별남자(5-9)", "연령별남자(10-14)", "연령별남자(15-19)", "연령별남자(20-24)", "연령별남자(25-29)", "연령별남자(30-34)", "연령별남자(35-39)", "연령별남자(40-44)", "연령별남자(45-49)", "연령별남자(50-54)", "연령별남자(55-59)", "연령별남자(60-64)", "연령별남자(65-69)", "연령별남자(70-74)", "연령별남자(75-79)"
                   , "연령별남자(80+)", "연령별남자(OTHER)", "연령별남자(합계)", "연령별여자(0-4)", "연령별여자(5-9)", "연령별여자(10-14)", "연령별여자(15-19)", "연령별여자(20-24)", "연령별여자(25-29)", "연령별여자(30-34)", "연령별여자(35-39)", "연령별여자(40-44)", "연령별여자(45-49)", "연령별여자(50-54)", "연령별여자(55-59)", "연령별여자(60-64)", "연령별여자(65-69)", "연령별여자(70-74)", "연령별여자(75-79)", "연령별여자(80+)"
                   , "연령별여자(OTHER)", "연령별여자(합계)")



migration1997 <- read.table("data/migration/migration1997.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                             , "character", "numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                             , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                             , "numeric", "numeric"), skip=1, na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(migration1997) = c("행정구역（시도）"
                   , "행정구역（구시군）", "신고년월", "전출지코드(시도)", "전출지코드(구시군)", "연령별남자(0-4)", "연령별남자(5-9)", "연령별남자(10-14)", "연령별남자(15-19)", "연령별남자(20-24)", "연령별남자(25-29)", "연령별남자(30-34)", "연령별남자(35-39)", "연령별남자(40-44)", "연령별남자(45-49)", "연령별남자(50-54)", "연령별남자(55-59)", "연령별남자(60-64)", "연령별남자(65-69)", "연령별남자(70-74)", "연령별남자(75-79)"
                   , "연령별남자(80+)", "연령별남자(OTHER)", "연령별남자(합계)", "연령별여자(0-4)", "연령별여자(5-9)", "연령별여자(10-14)", "연령별여자(15-19)", "연령별여자(20-24)", "연령별여자(25-29)", "연령별여자(30-34)", "연령별여자(35-39)", "연령별여자(40-44)", "연령별여자(45-49)", "연령별여자(50-54)", "연령별여자(55-59)", "연령별여자(60-64)", "연령별여자(65-69)", "연령별여자(70-74)", "연령별여자(75-79)", "연령별여자(80+)"
                   , "연령별여자(OTHER)", "연령별여자(합계)")




migration1998 <- read.table("data/migration/migration1998.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                                      , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                                      , "numeric", "numeric"), skip=1, na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration1998) = c("행정구역（시도）"
                            , "행정구역（구시군）", "신고년월", "전출지코드(시도)", "전출지코드(구시군)", "연령별남자(0-4)", "연령별남자(5-9)", "연령별남자(10-14)", "연령별남자(15-19)", "연령별남자(20-24)", "연령별남자(25-29)", "연령별남자(30-34)", "연령별남자(35-39)", "연령별남자(40-44)", "연령별남자(45-49)", "연령별남자(50-54)", "연령별남자(55-59)", "연령별남자(60-64)", "연령별남자(65-69)", "연령별남자(70-74)", "연령별남자(75-79)"
                            , "연령별남자(80+)", "연령별남자(OTHER)", "연령별남자(합계)", "연령별여자(0-4)", "연령별여자(5-9)", "연령별여자(10-14)", "연령별여자(15-19)", "연령별여자(20-24)", "연령별여자(25-29)", "연령별여자(30-34)", "연령별여자(35-39)", "연령별여자(40-44)", "연령별여자(45-49)", "연령별여자(50-54)", "연령별여자(55-59)", "연령별여자(60-64)", "연령별여자(65-69)", "연령별여자(70-74)", "연령별여자(75-79)", "연령별여자(80+)"
                            , "연령별여자(OTHER)", "연령별여자(합계)")





migration1999 <- read.table("data/migration/migration1999.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                                      , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                                      , "numeric", "numeric"), skip=1, na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))



colnames(migration1999) = c("행정구역（시도）"
                            , "행정구역（구시군）", "신고년월", "전출지코드(시도)", "전출지코드(구시군)", "연령별남자(0-4)", "연령별남자(5-9)", "연령별남자(10-14)", "연령별남자(15-19)", "연령별남자(20-24)", "연령별남자(25-29)", "연령별남자(30-34)", "연령별남자(35-39)", "연령별남자(40-44)", "연령별남자(45-49)", "연령별남자(50-54)", "연령별남자(55-59)", "연령별남자(60-64)", "연령별남자(65-69)", "연령별남자(70-74)", "연령별남자(75-79)"
                            , "연령별남자(80+)", "연령별남자(OTHER)", "연령별남자(합계)", "연령별여자(0-4)", "연령별여자(5-9)", "연령별여자(10-14)", "연령별여자(15-19)", "연령별여자(20-24)", "연령별여자(25-29)", "연령별여자(30-34)", "연령별여자(35-39)", "연령별여자(40-44)", "연령별여자(45-49)", "연령별여자(50-54)", "연령별여자(55-59)", "연령별여자(60-64)", "연령별여자(65-69)", "연령별여자(70-74)", "연령별여자(75-79)", "연령별여자(80+)"
                            , "연령별여자(OTHER)", "연령별여자(합계)")





migration2000 <- read.table("data/migration/migration2000.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "numeric", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                                      , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                                      , "numeric", "numeric"), skip=1, na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2000) = c("행정구역（시도）"
                            , "행정구역（구시군）", "신고년월", "전출지코드(시도)", "전출지코드(구시군)", "연령별남자(0-4)", "연령별남자(5-9)", "연령별남자(10-14)", "연령별남자(15-19)", "연령별남자(20-24)", "연령별남자(25-29)", "연령별남자(30-34)", "연령별남자(35-39)", "연령별남자(40-44)", "연령별남자(45-49)", "연령별남자(50-54)", "연령별남자(55-59)", "연령별남자(60-64)", "연령별남자(65-69)", "연령별남자(70-74)", "연령별남자(75-79)"
                            , "연령별남자(80+)", "연령별남자(OTHER)", "연령별남자(합계)", "연령별여자(0-4)", "연령별여자(5-9)", "연령별여자(10-14)", "연령별여자(15-19)", "연령별여자(20-24)", "연령별여자(25-29)", "연령별여자(30-34)", "연령별여자(35-39)", "연령별여자(40-44)", "연령별여자(45-49)", "연령별여자(50-54)", "연령별여자(55-59)", "연령별여자(60-64)", "연령별여자(65-69)", "연령별여자(70-74)", "연령별여자(75-79)", "연령별여자(80+)"
                            , "연령별여자(OTHER)", "연령별여자(합계)")



migration2001 <- read.table("data/migration/migration2001.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(migration2001) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")





migration2002 <- read.table("data/migration/migration2002.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2002) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")



migration2003 <- read.table("data/migration/migration2003.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2003) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")



migration2004 <- read.table("data/migration/migration2004.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2004) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")




migration2005 <- read.table("data/migration/migration2005.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(migration2005) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")



migration2006 <- read.table("data/migration/migration2006.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(migration2006) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")



migration2007 <- read.table("data/migration/migration2007.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2007) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")





migration2008 <- read.table("data/migration/migration2008.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2008) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")



migration2009 <- read.table("data/migration/migration2009.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2009) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")




migration2010 <- read.table("data/migration/migration2010.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "numeric", "numeric", "numeric", "character", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2010) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")




migration2011 <- read.table("data/migration/migration2011.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                     , "character", "character", "numeric", "numeric", "numeric", "character", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2011) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")



migration2012 <- read.table("data/migration/migration2012.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "numeric", "numeric", "numeric", "character", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

colnames(migration2012) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")


migration2013 <- read.table("data/migration/migration2013.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2013) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")




migration2014 <- read.table("data/migration/migration2014.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2014) = c("전입행정_시도"
                            , "전입행정_시군구", "전입행정_동읍면", "전입년", "전입월", "전입일", "전출행정_시도", "전출행정_시군구", "전출행정_동읍면", "전입사유", "세대주_관계", "세대주_만나이", "세대주_성별", "세대관련", "이동_총인구", "이동_남인구", "이동_여인구")


migration2015 <- read.table("data/migration/migration2015.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2015) = c("전입행정구역_시도코드"
                            , "전입행정구역_시군구코드", "전입행정구역_읍면동코드", "전입연도", "전입월", "전입일", "전출행정구역_시도코드", "전출행정구역_시군구코드", "전출행정구역_읍면동코드", "전입사유코드", "세대주관계코드", "세대주만연령", "세대주성별코드", "세대관련코드", "이동_총인구수", "이동_남자인구수", "이동_여자인구수")






migration2016 <- read.table("data/migration/migration2016.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2016) = c("전입행정구역_시도코드"
                            , "전입행정구역_시군구코드", "전입행정구역_읍면동코드", "전입연도", "전입월", "전입일", "전출행정구역_시도코드", "전출행정구역_시군구코드", "전출행정구역_읍면동코드", "전입사유코드", "세대주관계코드", "세대주만연령", "세대주성별코드", "세대관련코드", "이동_총인구수", "이동_남자인구수", "이동_여자인구수")




migration2017 <- read.table("data/migration/migration2017.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))



colnames(migration2017) = c("전입행정구역_시도코드"
                            , "전입행정구역_시군구코드", "전입행정구역_읍면동코드", "전입연도", "전입월", "전입일", "전출행정구역_시도코드", "전출행정구역_시군구코드", "전출행정구역_읍면동코드", "전입사유코드", "세대주관계코드", "세대주만연령", "세대주성별코드", "세대관련코드", "이동_총인구수", "이동_남자인구수", "이동_여자인구수")








migration2018 <- read.table("data/migration/migration2018.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))



colnames(migration2018) = c("전입행정구역_시도코드"
                            , "전입행정구역_시군구코드", "전입행정구역_읍면동코드", "전입연도", "전입월", "전입일", "전출행정구역_시도코드", "전출행정구역_시군구코드", "전출행정구역_읍면동코드", "전입사유코드", "세대주관계코드", "세대주만연령", "세대주성별코드", "세대관련코드", "이동_총인구수", "이동_남자인구수", "이동_여자인구수")



migration2019 <- read.table("data/migration/migration2019.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2019) = c("전입행정구역_시도코드"
                            , "전입행정구역_시군구코드", "전입행정구역_읍면동코드", "전입연도", "전입월", "전입일", "전출행정구역_시도코드", "전출행정구역_시군구코드", "전출행정구역_읍면동코드", "전입사유코드", "세대주관계코드", "세대주만연령", "세대주성별코드", "세대관련코드", "이동_총인구수", "이동_남자인구수", "이동_여자인구수")











# rename some into english for convenience ----------- --------------------


# from 1996 - 2000

migration1996 <- migration1996 %>% rename(iso_d1=`행정구역（시도）`, iso_d2=`행정구역（구시군）`, iso_o1=`전출지코드(시도)`, iso_o2=`전출지코드(구시군)`)
migration1997 <- migration1997 %>% rename(iso_d1=`행정구역（시도）`, iso_d2=`행정구역（구시군）`, iso_o1=`전출지코드(시도)`, iso_o2=`전출지코드(구시군)`)
migration1998 <- migration1998 %>% rename(iso_d1=`행정구역（시도）`, iso_d2=`행정구역（구시군）`, iso_o1=`전출지코드(시도)`, iso_o2=`전출지코드(구시군)`)
migration1999 <- migration1999 %>% rename(iso_d1=`행정구역（시도）`, iso_d2=`행정구역（구시군）`, iso_o1=`전출지코드(시도)`, iso_o2=`전출지코드(구시군)`)
migration2000 <- migration2000 %>% rename(iso_d1=`행정구역（시도）`, iso_d2=`행정구역（구시군）`, iso_o1=`전출지코드(시도)`, iso_o2=`전출지코드(구시군)`)


# from 2001-2014

migration2001 <- migration2001 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2002 <- migration2002 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2003 <- migration2003 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2004 <- migration2004 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2005 <- migration2005 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2006 <- migration2006 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2007 <- migration2007 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2008 <- migration2008 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2009 <- migration2009 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2010 <- migration2010 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2011 <- migration2011 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2012 <- migration2012 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2013 <- migration2013 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)
migration2014 <- migration2014 %>% rename(iso_d1=`전입행정_시도`, iso_d2=`전입행정_시군구`, iso_o1=`전출행정_시도`, iso_o2=`전출행정_시군구`)



# from 2015 to 2020


migration2015 <- migration2015 %>% rename(iso_d1=`전입행정구역_시도코드`, iso_d2=`전입행정구역_시군구코드`, iso_o1=`전출행정구역_시도코드`, iso_o2=`전출행정구역_시군구코드`)
migration2016 <- migration2016 %>% rename(iso_d1=`전입행정구역_시도코드`, iso_d2=`전입행정구역_시군구코드`, iso_o1=`전출행정구역_시도코드`, iso_o2=`전출행정구역_시군구코드`)
migration2017 <- migration2017 %>% rename(iso_d1=`전입행정구역_시도코드`, iso_d2=`전입행정구역_시군구코드`, iso_o1=`전출행정구역_시도코드`, iso_o2=`전출행정구역_시군구코드`)
migration2018 <- migration2018 %>% rename(iso_d1=`전입행정구역_시도코드`, iso_d2=`전입행정구역_시군구코드`, iso_o1=`전출행정구역_시도코드`, iso_o2=`전출행정구역_시군구코드`)
migration2019 <- migration2019 %>% rename(iso_d1=`전입행정구역_시도코드`, iso_d2=`전입행정구역_시군구코드`, iso_o1=`전출행정구역_시도코드`, iso_o2=`전출행정구역_시군구코드`)






# -














# check that the iso variables are character -------

migration1996 <- migration1996 %>% mutate(across(starts_with("iso"), as.character))
migration1997 <- migration1997 %>% mutate(across(starts_with("iso"), as.character))
migration1998 <- migration1998 %>% mutate(across(starts_with("iso"), as.character))
migration1999 <- migration1999 %>% mutate(across(starts_with("iso"), as.character))
migration2000 <- migration2000 %>% mutate(across(starts_with("iso"), as.character))
migration2001 <- migration2001 %>% mutate(across(starts_with("iso"), as.character))
migration2002 <- migration2002 %>% mutate(across(starts_with("iso"), as.character))
migration2003 <- migration2003 %>% mutate(across(starts_with("iso"), as.character))
migration2004 <- migration2004 %>% mutate(across(starts_with("iso"), as.character))
migration2005 <- migration2005 %>% mutate(across(starts_with("iso"), as.character))
migration2006 <- migration2006 %>% mutate(across(starts_with("iso"), as.character))
migration2007 <- migration2007 %>% mutate(across(starts_with("iso"), as.character))
migration2008 <- migration2008 %>% mutate(across(starts_with("iso"), as.character))
migration2009 <- migration2009 %>% mutate(across(starts_with("iso"), as.character))
migration2010 <- migration2010 %>% mutate(across(starts_with("iso"), as.character))
migration2011 <- migration2011 %>% mutate(across(starts_with("iso"), as.character))
migration2012 <- migration2012 %>% mutate(across(starts_with("iso"), as.character))
migration2013 <- migration2013 %>% mutate(across(starts_with("iso"), as.character))
migration2014 <- migration2014 %>% mutate(across(starts_with("iso"), as.character))
migration2015 <- migration2015 %>% mutate(across(starts_with("iso"), as.character))
migration2016 <- migration2016 %>% mutate(across(starts_with("iso"), as.character))
migration2017 <- migration2017 %>% mutate(across(starts_with("iso"), as.character))
migration2018 <- migration2018 %>% mutate(across(starts_with("iso"), as.character))
migration2019 <- migration2019 %>% mutate(across(starts_with("iso"), as.character))












# combine data into three for convenience: 1996-2000, 2001-2014, 2015-2019 ------


# first, put year variable

migration1996 <- migration1996 %>% mutate(year=1996)
migration1997 <- migration1997 %>% mutate(year=1997)
migration1998 <- migration1998 %>% mutate(year=1998)
migration1999 <- migration1999 %>% mutate(year=1999)
migration2000 <- migration2000 %>% mutate(year=2000)
migration2001 <- migration2001 %>% mutate(year=2001)
migration2002 <- migration2002 %>% mutate(year=2002)
migration2003 <- migration2003 %>% mutate(year=2003)
migration2004 <- migration2004 %>% mutate(year=2004)
migration2005 <- migration2005 %>% mutate(year=2005)
migration2006 <- migration2006 %>% mutate(year=2006)
migration2007 <- migration2007 %>% mutate(year=2007)
migration2008 <- migration2008 %>% mutate(year=2008)
migration2009 <- migration2009 %>% mutate(year=2009)
migration2010 <- migration2010 %>% mutate(year=2010)
migration2011 <- migration2011 %>% mutate(year=2011)
migration2012 <- migration2012 %>% mutate(year=2012)
migration2013 <- migration2013 %>% mutate(year=2013)
migration2014 <- migration2014 %>% mutate(year=2014)
migration2015 <- migration2015 %>% mutate(year=2015)
migration2016 <- migration2016 %>% mutate(year=2016)
migration2017 <- migration2017 %>% mutate(year=2017)
migration2018 <- migration2018 %>% mutate(year=2018)
migration2019 <- migration2019 %>% mutate(year=2019)



# combine them

migration1996_2000 <- migration1996 %>%
  bind_rows(migration1997) %>%
  bind_rows(migration1998) %>%
  bind_rows(migration1999) %>%
  bind_rows(migration2000)


rm(migration1996)
rm(migration1997)
rm(migration1998)
rm(migration1999)
rm(migration2000)


migration1996_2000 %>% write_rds("data/migration1996_2000.rds")

rm(migration1996_2000)



migration2001 <- migration2001 %>% select(-`전입년`)
migration2002 <- migration2002 %>% select(-`전입년`)
migration2003 <- migration2003 %>% select(-`전입년`)
migration2004 <- migration2004 %>% select(-`전입년`)
migration2005 <- migration2005 %>% select(-`전입년`)
migration2006 <- migration2006 %>% select(-`전입년`)
migration2007 <- migration2007 %>% select(-`전입년`)
migration2008 <- migration2008 %>% select(-`전입년`)
migration2009 <- migration2009 %>% select(-`전입년`)
migration2010 <- migration2010 %>% select(-`전입년`)
migration2011 <- migration2011 %>% select(-`전입년`)
migration2012 <- migration2012 %>% select(-`전입년`)
migration2013 <- migration2013 %>% select(-`전입년`)
migration2014 <- migration2014 %>% select(-`전입년`)

migration2001 <- migration2001 %>% select(-`전입월`)
migration2002 <- migration2002 %>% select(-`전입월`)
migration2003 <- migration2003 %>% select(-`전입월`)
migration2004 <- migration2004 %>% select(-`전입월`)
migration2005 <- migration2005 %>% select(-`전입월`)
migration2006 <- migration2006 %>% select(-`전입월`)
migration2007 <- migration2007 %>% select(-`전입월`)
migration2008 <- migration2008 %>% select(-`전입월`)
migration2009 <- migration2009 %>% select(-`전입월`)
migration2010 <- migration2010 %>% select(-`전입월`)
migration2011 <- migration2011 %>% select(-`전입월`)
migration2012 <- migration2012 %>% select(-`전입월`)
migration2013 <- migration2013 %>% select(-`전입월`)
migration2014 <- migration2014 %>% select(-`전입월`)



migration2001_2014 <- migration2001 %>%
  bind_rows(migration2002)

rm(migration2001)
rm(migration2002)

migration2001_2014 <- migration2001_2014 %>%
  bind_rows(migration2003)

rm(migration2003)

migration2001_2014 <- migration2001_2014 %>%
  bind_rows(migration2004)

rm(migration2004)

migration2001_2014 <- migration2001_2014 %>%
  bind_rows(migration2005)

rm(migration2005)

migration2001_2014 <- migration2001_2014 %>%
  bind_rows(migration2006)

rm(migration2006)

migration2001_2014 <- migration2001_2014 %>%
  bind_rows(migration2007)

rm(migration2007)

migration2001_2014 <- migration2001_2014 %>%
  bind_rows(migration2008)

rm(migration2008)

migration2001_2014 <- migration2001_2014 %>%
  bind_rows(migration2009)

rm(migration2009)



migration2010 <- migration2010 %>% select(-`전입일`)
migration2011 <- migration2011 %>% select(-`전입일`)
migration2012 <- migration2012 %>% select(-`전입일`)
migration2013 <- migration2013 %>% select(-`전입일`)
migration2014 <- migration2014 %>% select(-`전입일`)



migration2001_2014 <- migration2001_2014 %>% mutate(`세대주_만나이`=as.numeric(`세대주_만나이`), `이동_총인구`=as.numeric(`이동_총인구`), `이동_여인구`=as.numeric(`이동_여인구`), `이동_남인구`=as.numeric(`이동_남인구`))


migration2001_2014 <- migration2001_2014 %>%
  bind_rows(migration2010)

rm(migration2010)

migration2001_2014 <- migration2001_2014 %>%
  bind_rows(migration2011)

rm(migration2011)

migration2001_2014 <- migration2001_2014 %>%
  bind_rows(migration2012)

rm(migration2012)

migration2001_2014 <- migration2001_2014 %>%
  bind_rows(migration2013)

rm(migration2013)

migration2001_2014 <- migration2001_2014 %>%
  bind_rows(migration2014)

rm(migration2014)


migration2001_2014 %>% write_rds("data/migration2001_2014.rds")


migration2015_2019 <- migration2015 %>%
  bind_rows(migration2016)

rm(migration2015)
rm(migration2016)

migration2015_2019 <- migration2015_2019 %>%
  bind_rows(migration2017)

rm(migration2017)

migration2015_2019 <- migration2015_2019 %>%
  bind_rows(migration2018)

rm(migration2018)

migration2015_2019 <- migration2015_2019 %>%
  bind_rows(migration2019)

rm(migration2019)

migration2015_2019 %>% write_rds("data/migration2015_2019.rds")


migration1996_2000 <- read_rds("data/migration1996_2000.rds")

# now combine iso_d, iso_o

migration1996_2000 <-migration1996_2000 %>% mutate(iso_d=paste0(iso_d1, iso_d2), iso_o=paste0(iso_o1, iso_o2)) %>% select(-c("iso_d1", "iso_d2", "iso_o1", "iso_o2"))

migration2001_2014 <-migration2001_2014 %>% mutate(iso_d=paste0(iso_d1, iso_d2), iso_o=paste0(iso_o1, iso_o2)) %>% select(-c("iso_d1", "iso_d2", "iso_o1", "iso_o2"))

migration2015_2019 <-migration2015_2019 %>% mutate(iso_d=paste0(iso_d1, iso_d2), iso_o=paste0(iso_o1, iso_o2)) %>% select(-c("iso_d1", "iso_d2", "iso_o1", "iso_o2"))




migration1996_2000 %>% write_rds("data/migration1996_2000.rds")
migration2001_2014 %>% write_rds("data/migration2001_2014.rds")
migration2015_2019 %>% write_rds("data/migration2015_2019.rds")





migration1996_2000 <- read_rds("data/migration/migration1996_2000.rds")
migration2001_2014 <- read_rds("data/migration/migration2001_2014.rds")
migration2015_2019 <- read_rds("data/migration/migration2015_2019.rds")

# now use region_stat to crosswalk change in regions (mostly to 2001 regions) -------


migration1996_2000 <- migration1996_2000 %>% mutate(iso_d=ifelse(iso_d=="44825", "70000", iso_d), iso_o=ifelse(iso_o=="44825", "70000", iso_o))

migration1996_2000 <- migration1996_2000 %>% mutate(iso_d=ifelse(iso_d=="43745", "80000", iso_d), iso_o=ifelse(iso_o=="43745", "80000", iso_o))


migration2001_2014 <- migration2001_2014 %>% mutate(iso_d=ifelse(iso_d=="44825", "70000", iso_d), iso_o=ifelse(iso_o=="44825", "70000", iso_o))

migration2001_2014 <- migration2001_2014 %>% mutate(iso_d=ifelse(iso_d=="43745", "80000", iso_d), iso_o=ifelse(iso_o=="43745", "80000", iso_o))


migration2015_2019 <- migration2015_2019 %>% mutate(iso_d=ifelse(iso_d=="44825", "70000", iso_d), iso_o=ifelse(iso_o=="44825", "70000", iso_o))

migration2015_2019 <- migration2015_2019 %>% mutate(iso_d=ifelse(iso_d=="43745", "80000", iso_d), iso_o=ifelse(iso_o=="43745", "80000", iso_o))


migration1996_2000 <- migration1996_2000 %>% mutate(iso_d=ifelse(   as.numeric(str_sub(iso_d, 1, 2))>=40, paste0(str_sub(iso_d, 1, 4), "0"), iso_d), iso_o=ifelse(   as.numeric(str_sub(iso_o, 1, 2))>=40, paste0(str_sub(iso_o, 1, 4), "0"), iso_o))
migration2001_2014 <- migration2001_2014 %>% mutate(iso_d=ifelse(   as.numeric(str_sub(iso_d, 1, 2))>=40, paste0(str_sub(iso_d, 1, 4), "0"), iso_d), iso_o=ifelse(   as.numeric(str_sub(iso_o, 1, 2))>=40, paste0(str_sub(iso_o, 1, 4), "0"), iso_o))
migration2015_2019 <- migration2015_2019 %>% mutate(iso_d=ifelse(   as.numeric(str_sub(iso_d, 1, 2))>=40, paste0(str_sub(iso_d, 1, 4), "0"), iso_d), iso_o=ifelse(   as.numeric(str_sub(iso_o, 1, 2))>=40, paste0(str_sub(iso_o, 1, 4), "0"), iso_o))







migration1996_2000 <- migration1996_2000 %>% left_join(region_kosis, by=c("iso_d"="city_kosis"))
migration2001_2014 <- migration2001_2014 %>% left_join(region_kosis, by=c("iso_d"="city_kosis"))
migration2015_2019 <- migration2015_2019 %>% left_join(region_kosis, by=c("iso_d"="city_kosis"))

migration1996_2000 <- migration1996_2000 %>% mutate(iso_d=city_stat) %>% select(-city_stat)
migration2001_2014 <- migration2001_2014 %>% mutate(iso_d=city_stat) %>% select(-city_stat)
migration2015_2019 <- migration2015_2019 %>% mutate(iso_d=city_stat) %>% select(-city_stat)

migration1996_2000 <- migration1996_2000 %>% select(-city_label)
migration2001_2014 <- migration2001_2014 %>% select(-city_label)
migration2015_2019 <- migration2015_2019 %>% select(-city_label)




migration1996_2000 <- migration1996_2000 %>% left_join(region_kosis, by=c("iso_o"="city_kosis"))
migration2001_2014 <- migration2001_2014 %>% left_join(region_kosis, by=c("iso_o"="city_kosis"))
migration2015_2019 <- migration2015_2019 %>% left_join(region_kosis, by=c("iso_o"="city_kosis"))

migration1996_2000 <- migration1996_2000 %>% mutate(iso_o=city_stat) %>% select(-city_stat)
migration2001_2014 <- migration2001_2014 %>% mutate(iso_o=city_stat) %>% select(-city_stat)
migration2015_2019 <- migration2015_2019 %>% mutate(iso_o=city_stat) %>% select(-city_stat)


# filter out NA
migration1996_2000 <- migration1996_2000 %>% filter(!is.na(iso_d), !is.na(iso_o))
migration2001_2014 <- migration2001_2014 %>% filter(!is.na(iso_d), !is.na(iso_o))
migration2015_2019 <- migration2015_2019 %>% filter(!is.na(iso_d), !is.na(iso_o))



# now use stat region case

migration1996_2000 <- migration1996_2000 %>% left_join(region_stat, by=c("iso_d"="city_stat_raw"))
migration2001_2014 <- migration2001_2014 %>% left_join(region_stat, by=c("iso_d"="city_stat_raw"))
migration2015_2019 <- migration2015_2019 %>% left_join(region_stat, by=c("iso_d"="city_stat_raw"))

migration1996_2000 <- migration1996_2000 %>% mutate(iso_d=ifelse(!is.na(city_stat), city_stat, iso_d))
migration2001_2014 <- migration2001_2014 %>% mutate(iso_d=ifelse(!is.na(city_stat), city_stat, iso_d))
migration2015_2019 <- migration2015_2019 %>% mutate(iso_d=ifelse(!is.na(city_stat), city_stat, iso_d))

migration1996_2000 <- migration1996_2000 %>% select(-city_stat)
migration2001_2014 <- migration2001_2014 %>% select(-city_stat)
migration2015_2019 <- migration2015_2019 %>% select(-city_stat)



migration1996_2000 <- migration1996_2000 %>% left_join(region_stat, by=c("iso_o"="city_stat_raw"))
migration2001_2014 <- migration2001_2014 %>% left_join(region_stat, by=c("iso_o"="city_stat_raw"))
migration2015_2019 <- migration2015_2019 %>% left_join(region_stat, by=c("iso_o"="city_stat_raw"))

migration1996_2000 <- migration1996_2000 %>% mutate(iso_o=ifelse(!is.na(city_stat), city_stat, iso_o))
migration2001_2014 <- migration2001_2014 %>% mutate(iso_o=ifelse(!is.na(city_stat), city_stat, iso_o))
migration2015_2019 <- migration2015_2019 %>% mutate(iso_o=ifelse(!is.na(city_stat), city_stat, iso_o))

migration1996_2000 <- migration1996_2000 %>% select(-city_stat)
migration2001_2014 <- migration2001_2014 %>% select(-city_stat)
migration2015_2019 <- migration2015_2019 %>% select(-city_stat)



# filter out case where the person does not move
migration1996_2000 <- migration1996_2000 %>% filter(iso_d!=iso_o)
migration2001_2014 <- migration2001_2014 %>% filter(iso_d!=iso_o)
migration2015_2019 <- migration2015_2019 %>% filter(iso_d!=iso_o)



migration1996_2000 %>% write_rds("data/migration/migration1996_2000_refine.rds")
migration2001_2014 %>% write_rds("data/migration/migration2001_2014_refine.rds")
migration2015_2019 %>% write_rds("data/migration/migration2015_2019_refine.rds")





migration1996_2000 %>% write_rds("data/final/migration/migration1996_2000_refine.rds")
migration2001_2014 %>% write_rds("data/final/migration/migration2001_2014_refine.rds")
migration2015_2019 %>% write_rds("data/final/migration/migration2015_2019_refine.rds")







# year 2020


# read in the concordance for region stat----
region_stat <- readxl::read_excel("concordance/region/region_stat.xlsx")

region_stat <- region_stat %>% mutate(across(everything(), as.character))

region_kosis <- readxl::read_excel("concordance/region/region_kosis.xlsx")

region_kosis <- region_kosis %>% mutate(across(everything(), as.character))

region_kosis <- region_kosis %>% mutate(city_stat=str_sub(city_stat, 1, 5))




migration2020 <- read.table("data/migration/migration2020.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                      , "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(migration2020) = c("전입행정구역_시도코드"
                            , "전입행정구역_시군구코드", "전입행정구역_읍면동코드", "전입연도", "전입월", "전입일", "전출행정구역_시도코드", "전출행정구역_시군구코드", "전출행정구역_읍면동코드", "전입사유코드", "세대주관계코드", "세대주만연령", "세대주성별코드", "세대관련코드", "이동_총인구수", "이동_남자인구수", "이동_여자인구수")


migration2020 <- migration2020 %>% rename(iso_d1=`전입행정구역_시도코드`, iso_d2=`전입행정구역_시군구코드`, iso_o1=`전출행정구역_시도코드`, iso_o2=`전출행정구역_시군구코드`)


migration2020 <- migration2020 %>% mutate(across(starts_with("iso"), as.character))

migration2020 <- migration2020 %>% mutate(year=2020)



migration2020<-migration2020 %>% mutate(iso_d=paste0(iso_d1, iso_d2), iso_o=paste0(iso_o1, iso_o2)) %>% select(-c("iso_d1", "iso_d2", "iso_o1", "iso_o2"))



migration2020 <- migration2020 %>% mutate(iso_d=ifelse(iso_d=="44825", "70000", iso_d), iso_o=ifelse(iso_o=="44825", "70000", iso_o))

migration2020 <- migration2020 %>% mutate(iso_d=ifelse(iso_d=="43745", "80000", iso_d), iso_o=ifelse(iso_o=="43745", "80000", iso_o))


migration2020 <- migration2020 %>% mutate(iso_d=ifelse(   as.numeric(str_sub(iso_d, 1, 2))>=40, paste0(str_sub(iso_d, 1, 4), "0"), iso_d), iso_o=ifelse(   as.numeric(str_sub(iso_o, 1, 2))>=40, paste0(str_sub(iso_o, 1, 4), "0"), iso_o))

migration2020 <- migration2020 %>% left_join(region_kosis, by=c("iso_d"="city_kosis"))

migration2020 <- migration2020 %>% mutate(iso_d=city_stat) %>% select(-city_stat)

migration2020 <- migration2020 %>% select(-city_label)


migration2020 <- migration2020 %>% left_join(region_kosis, by=c("iso_o"="city_kosis"))

migration2020 <- migration2020 %>% mutate(iso_o=city_stat) %>% select(-city_stat)


migration2020 <- migration2020 %>% filter(!is.na(iso_d), !is.na(iso_o))


migration2020 <- migration2020 %>% left_join(region_stat, by=c("iso_d"="city_stat_raw"))


migration2020 <- migration2020 %>% mutate(iso_d=ifelse(!is.na(city_stat), city_stat, iso_d))


migration2020 <- migration2020 %>% select(-city_stat)

migration2020 <- migration2020 %>% left_join(region_stat, by=c("iso_o"="city_stat_raw"))


migration2020 <- migration2020 %>% mutate(iso_o=ifelse(!is.na(city_stat), city_stat, iso_o))

migration2020 <- migration2020 %>% select(-city_stat)

migration2020 <- migration2020 %>% filter(iso_d!=iso_o)

migration2020 %>% write_rds("data/final/migration/migration2020_refine.rds")

migration2020 %>% haven::write_dta("data/final/migration/migration2020_refine.dta")



