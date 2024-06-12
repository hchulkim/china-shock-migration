if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(concordance, here, sf, gt, texreg, fixest, viridis, patchwork, hrbrthemes, circlize, chorddiag, tidyverse, igraph, tidygraph, extrafont, magick, pdftools, broom, rmapshaper, sanghoon, tidyverse)


# This code plots all the maps we used as figures in our paper.

## basic setup before making figures.

# read in the concordance for region stat----

region_stat <- readxl::read_excel("concordance/region/region_stat.xlsx")

region_stat <- region_stat %>% mutate(across(everything(), as.character))

region_kosis <- readxl::read_excel("concordance/region/region_kosis.xlsx")

region_kosis <- region_kosis %>% mutate(across(everything(), as.character))

region_kosis <- region_kosis %>% mutate(city_stat=str_sub(city_stat, 1, 5))



# make shp map file for cz version

# read in shp and match region

com <- read_rds("data/final/cz_code/commuting_zone.rds")

shp <- st_read("data/final/region/bnd_sigungu_00_2021_2021_4Q.shp") %>% st_simplify(dTolerance = 1000)

shp <- shp %>% mutate(SIGUNGU_CD=ifelse(str_sub(SIGUNGU_CD, 5, 5)!="0", paste0(str_sub(SIGUNGU_CD, 1, 4), "0"), SIGUNGU_CD))  %>% 
  group_by(SIGUNGU_CD) %>% summarise(geometry = st_union(geometry), .groups = "keep") %>% 
  group_by(SIGUNGU_CD) %>% summarise(geometry = st_combine(geometry))


shp <- shp %>% rename(iso=SIGUNGU_CD)

shp <- shp %>% left_join(region_stat, by=c("iso"="city_stat_raw"))

shp <- shp %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)


shp <- shp %>% group_by(iso) %>% summarise(geometry = st_union(geometry), .groups="keep") %>% 
  summarise(geometry = st_combine(geometry))

shp <- shp %>% left_join(com, by="iso") %>% rename(cz=cz_id)


shp_cz <- shp %>% group_by(cz) %>% summarise(geometry = st_union(geometry), .groups="keep") %>% 
  summarise(geometry = st_combine(geometry))


shp_cz <- shp_cz %>% filter(cz!=34)

shp_cz <- shp_cz %>% ms_simplify(keep=0.33)


shp %>% filter(cz!=34) %>% 
  ggplot() +
  geom_sf() + 
  theme_void()
ggsave("results/figures/map_sigungu.png", width=10, height=7, dpi=400)


shp_cz %>% ggplot() +
  geom_sf() + 
  theme_void()
ggsave("results/figures/map_cz.png", width=10, height=7, dpi=400)


shp %>% filter(cz != 34) %>% 
  ggplot() +
  geom_sf() +
  coord_sf(expand = FALSE) +  # Reduces white space
  theme_void() +
  theme(plot.margin = margin(1, 0, 0, 1)) 

ggsave("results/figures/midwest/map_sigungu.png", width=10, height=7, dpi=400)

shp_cz %>% 
  ggplot() +
  geom_sf() +
  coord_sf(expand = FALSE) +  # Reduces white space
  theme_void() +
  theme(plot.margin = margin(1, 0, 0, 1))  # Sets margins to 0

ggsave("results/figures/midwest/map_cz.png", width=10, height=7, dpi=400)


# 1. figure drawing map with population in 2000 in south korea.



data <- read_delim("data/census/pop2000.txt", delim="^", col_names=F) %>% 
  select(iso=X2, value=X4) %>% 
  mutate(iso=as.character(iso)) %>% 
  mutate(iso=str_sub(iso, 1, 5)) %>% 
  group_by(iso) %>% 
  summarise(value=sum(value, na.rm=T))



# use region match codes to fix the region.

data <- data %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))



data <- data %>% left_join(region_stat, by=c("iso"="city_stat_raw"))



data <- data %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)


# group by cz

data <- data %>% left_join(com, by="iso") %>% rename(cz=cz_id)


data <- data %>% group_by(cz) %>% 
  summarise(value=sum(value, na.rm=T))




# merge pop data to shp_cz


shp_cz <- shp_cz %>% left_join(data, by="cz")




# map them


shp_cz %>% st_as_sf() %>% mutate(value_q=value %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=value_q))+
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_pop2000.png", width=10, height=7, dpi=400)







# 2. figure drawing map with employers in 2000 in south korea.


est2000 <- read.table("data/est/est2000.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                    , "character", "numeric", "numeric", "numeric"), na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))


colnames(est2000) = c("행정구역(시도)", "행정구역(구시군)", "emp_male", "emp_female", "emp_all")

# again concord the regions----

est2000 <- est2000 %>% mutate(iso=paste0(`행정구역(시도)`, `행정구역(구시군)`))

est2000 <- est2000 %>% mutate(across(starts_with("emp"), as.numeric))

est2000 <- est2000 %>% group_by(iso) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_male=sum(emp_male, na.rm=T), emp_female=sum(emp_female, na.rm=T))



# change variable to have 0 at end
est2000 <- est2000 %>% mutate(iso=paste0(str_sub(iso, 1, 4), "0"))



est2000 <- est2000 %>% left_join(region_stat, by=c("iso"="city_stat_raw"))



est2000 <- est2000 %>% mutate(iso=ifelse(!is.na(city_stat), city_stat, iso)) %>% select(-city_stat)


est2000 <- est2000 %>% group_by(iso) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_male=sum(emp_male, na.rm=T), emp_female=sum(emp_female, na.rm=T))



# group by cz

est2000 <- est2000 %>% left_join(com, by="iso") %>% rename(cz=cz_id)


est2000 <- est2000 %>% group_by(cz) %>% 
  summarise(emp_all=sum(emp_all, na.rm=T), emp_male=sum(emp_male, na.rm=T), emp_female=sum(emp_female, na.rm=T))





# merge emp data to shp_cz


shp_cz <- shp_cz %>% left_join(est2000, by="cz")




# map them


shp_cz %>% st_as_sf() %>% mutate(emp_q=emp_all %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf()+geom_sf(aes(fill=emp_q))+
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(0.9, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_emp2000.png", width=10, height=7, dpi=400)













