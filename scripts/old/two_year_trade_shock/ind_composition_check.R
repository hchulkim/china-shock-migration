if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(concordance, here, sf, gt, texreg, fixest, viridis, patchwork, hrbrthemes, circlize, chorddiag, tidyverse, igraph, tidygraph, extrafont, magick, pdftools, broom, tidyverse)



# check industry for est2000 and est2001


com <- read_rds("data/final/cz_code/commuting_zone.rds")

est1999 <- read_rds("data/final/hs_cpc_ksic/est1999_refine.rds") %>% rename(emp_all1999=emp_all)

est2000 <- read_rds("data/final/hs_cpc_ksic/est2000_refine.rds") %>% rename(emp_all2000=emp_all)
est2001 <- read_rds("data/final/hs_cpc_ksic/est2001_refine.rds") %>% rename(emp_all2001=emp_all)





check2000 <- est2000 %>% anti_join(est2001, by=c("iso", "ksic10"))

check2001 <- est2001 %>% anti_join(est2000, by=c("iso", "ksic10"))



data2001 <- est2001 %>% left_join(est2000, by=c("iso", "ksic10")) %>% 
  mutate(emp_all2000=ifelse(is.na(emp_all2000), 0, emp_all2000))


data2001 %>% ggplot() +
  geom_point(aes(emp_all2000, emp_all2001))




est2000 <- est2000 %>% left_join(com, by="iso")
est2001 <- est2001 %>% left_join(com, by="iso")


est2000 <- est2000 %>% group_by(cz_id, ksic10) %>% 
  summarise(emp_all2000=sum(emp_all2000, na.rm=T)) %>% ungroup()

est2001 <- est2001 %>% group_by(cz_id, ksic10) %>% 
  summarise(emp_all2001=sum(emp_all2001, na.rm=T)) %>% ungroup()

data2001 <- est2001 %>% left_join(est2000, by=c("cz_id", "ksic10")) %>% 
  mutate(emp_all2000=ifelse(is.na(emp_all2000), 0, emp_all2000))

# %>% filter(emp_all2000<50000, emp_all2001<50000)


data2001  %>% ggplot() +
  geom_point(aes(emp_all2000, emp_all2001)) + geom_abline(intercept = 0, slope = 1)






est2000 <- read_rds("data/est/est2000.rds") %>% rename(emp_all2000=emp_all)

est2000 <- est2000 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5))


est2001 <- read_rds("data/est/est2001.rds") 

est2001 <- est2001 %>% mutate(ksic8=paste0(`산업분류(중)`, `산업분류(소)`, `산업분류(세)`, `산업분류(세세)`), emp_female=`8-⑥종사자_합계_여(①+…+⑤)`, emp_male=`8-⑥종사자_합계_남(①+…+⑤)`, emp_all=`8-⑥종사자_합계_계(①+…+⑤)`) %>% select(iso, ksic8, emp_female, emp_male, emp_all)

est2001 <- est2001 %>% rename(emp_all2001=emp_all)

est2000 <- est2000 %>% group_by(iso, ksic8) %>% 
  summarise(emp_all2000=sum(emp_all2000, na.rm=T))

est2001 <- est2001 %>% group_by(iso, ksic8) %>% 
  summarise(emp_all2001=sum(emp_all2001, na.rm=T))


check2000 <- est2000 %>% anti_join(est2001, by=c("iso", "ksic8"))

check2001 <- est2001 %>% anti_join(est2000, by=c("iso", "ksic8"))





est2000 <- est2000 %>% left_join(com, by="iso")
est2001 <- est2001 %>% left_join(com, by="iso")


est2000 <- est2000 %>% group_by(cz_id, ksic8) %>% 
  summarise(emp_all2000=sum(emp_all2000, na.rm=T)) %>% ungroup()

est2001 <- est2001 %>% group_by(cz_id, ksic8) %>% 
  summarise(emp_all2001=sum(emp_all2001, na.rm=T)) %>% ungroup()


est2000 <- est2000 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all2000=sum(emp_all2000, na.rm=T))

est2001 <- est2001 %>% group_by(iso, ksic10) %>% 
  summarise(emp_all2001=sum(emp_all2001, na.rm=T))


est2000 <- est2000 %>% group_by(ksic10) %>% 
  summarise(emp_all2000=sum(emp_all2000, na.rm=T))

est2001 <- est2001 %>% group_by(ksic10) %>% 
  summarise(emp_all2001=sum(emp_all2001, na.rm=T))


data2001 <- est2001 %>% full_join(est2000, by=c("ksic10")) %>% 
  mutate(emp_all2000=ifelse(is.na(emp_all2000), 0, emp_all2000), emp_all2001=ifelse(is.na(emp_all2001), 0, emp_all2001))





data2001  %>% ggplot() +
  geom_point(aes(emp_all2000, emp_all2001)) + geom_abline(intercept = 0, slope = 1)




data2001  %>% mutate(emp_all2000=log(emp_all2000), emp_all2001=log(emp_all2001)) %>% 
  ggplot() +
  geom_point(aes(emp_all2000, emp_all2001)) + geom_abline(intercept = 0, slope = 1)







ksic8 <- read_rds(here("data", "final", "hs_cpc_ksic","ksic8_ksic9.rds")) %>% select(ksic8)




est1999 <-read_rds("data/est/est1999.rds")
est2000 <-read_rds("data/est/est2000.rds")
est2001 <-read_rds("data/est/est2001.rds")


est1999 <- est1999 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5))
est2000 <- est2000 %>% mutate(ksic8=paste0(ind2, ind3, ind4, ind5))

est1999 <- est1999 %>% distinct(ksic8)
est2000 <- est2000 %>% distinct(ksic8)


est1999 <- est1999 %>% anti_join(ksic8, by="ksic8")


check <- est1999 %>% anti_join(est2000, by="ksic8")
check <- est2000 %>% anti_join(est1999, by="ksic8")



est2010 <-read_rds("data/est/est2010.rds")


