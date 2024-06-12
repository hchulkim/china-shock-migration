if (!require(pacman)) (install.packages("pacman"))
pacman::p_load(concordance, here, sf, gt, texreg, fixest, viridis, patchwork, hrbrthemes, circlize, chorddiag, tidyverse, igraph, tidygraph, extrafont, magick, pdftools, broom, rmapshaper, sanghoon, tidyverse, xtable)



# I do sum stat for migration

# a. 연령별 across cz migration / 전체 across cz migration 수 (share) sums stat 정리해보고,
# 
# b. 연령별 이동인구수 (across cz) / 전체 인구수(아마 initial period.)



# we first need to get data frame that has all migration and population for 2001


# layout of the data frame will be:

# age20s, age30, age40s, age50s, age60, total across cz migrate, pop2001


# more finer version in age regression


# migration2001_2014 <-  read_rds("data/final/migration/migration2001_2014_hetero_refine.rds")
# migration2015_2019 <-  read_rds("data/final/migration/migration2015_2019_hetero_refine.rds")
# migration2020 <-  read_rds("data/final/migration/migration2020_hetero_refine.rds")
# 
# 
# migration2001_2014 <- migration2001_2014 %>% select(year, iso_o, iso_d, city_label, reason, age_h, gen_h, household, migrate=`이동_총인구`) %>% mutate(across(5:8, as.numeric))
# 
# migration2015_2019 <- migration2015_2019 %>% select(year, iso_o, iso_d, city_label, reason, age_h, gen_h, household, migrate=`이동_총인구수`) %>% mutate(across(5:8, as.numeric))
# 
# migration2020 <- migration2020 %>% select(year, iso_o, iso_d, city_label, reason, age_h, gen_h, household, migrate=`이동_총인구수`) %>% mutate(across(5:8, as.numeric))
# 
# 
# migration <- migration2001_2014 %>% bind_rows(migration2015_2019) %>%
#   bind_rows(migration2020)
# 
# 
# rm(migration2001_2014)
# rm(migration2015_2019)
# rm(migration2020)
# 
# 
#
# 
# # now also make migration base table for sum stat
# 
# #take out NA
# migration <- migration %>% filter(!is.na(age_h))
# migration <- migration %>% filter(age_h>=20)
# 
# 
# migration <- migration %>% mutate(
#   age= case_when(age_h >=20 & age_h <30 ~ 20,
#                    age_h >=30 & age_h <40 ~ 30,
#                    age_h >=40 & age_h <50 ~ 40,
#                    age_h >=50 & age_h <60 ~ 50,
#                    TRUE ~ 60
# 
#   )
# )
# 
# 
# # read in the cz zone
# 
# cz <- read_rds("data/final/cz_code/commuting_zone.rds") %>%
#   select(iso, cz=cz_id)
# 
# 
# migration <- migration %>% left_join(cz, by=c("iso_o"="iso")) %>%
#   rename(cz_o=cz)
# migration <- migration %>% left_join(cz, by=c("iso_d"="iso"))%>%
#   rename(cz_d=cz)
# 
# migration <- migration %>% filter(cz_o!=34 & cz_d!=34)
# 
# 
# migration <- migration %>% mutate(cz_within=ifelse(cz_o==cz_d, 1, 0))
# 
# 
# # group them
# 
# migration <- migration %>% group_by(year, iso_o, age, household, reason, gen_h, cz_within) %>%
#   summarise(migrate=sum(migrate, na.rm=T)) %>%
#   ungroup()
# 
# migration <- migration %>% left_join(cz, by=c("iso_o"="iso")) %>%
#   rename(cz_o=cz)




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

shp_cz <- shp_cz %>% ms_simplify(keep=0.35)




  
  # save the main data: note that there are NAs in reason col.
  
  # migration %>% write_rds("data/final/migration_sum_stat.rds", "xz", compression=9L)


migration <- read_rds("data/final/migration_sum_stat.rds")

# now add in the pop

# read in population to get initial total 2001 population

pop <- read_rds("data/final.pop_age.xlsx")



pop <- pop %>%
  mutate(pop2001 = rowSums(select(., starts_with("age")), na.rm = TRUE))


pop <- pop %>% mutate(pop25=age25_29+age30_34, pop35=age35_39+age40_44, pop45=age45_49+age50_54, pop55=age55_59+age60_64, pop65=age65_69+age70_74+age75_79+age80_84+age85_89+age90_94+age95_99+age100) %>%
  select(iso, pop2001, pop25, pop35, pop45, pop55, pop65)



# make main data set


shp <- shp %>% select(iso, cz, region_label, geometry)

migration <- migration %>% left_join(pop, by=c("iso_o"="iso"))





# 1. sum stat: all out-migration in each reason out of 2001 pop

migration1 <- migration %>% group_by(iso_o) %>% 
  summarise(migrate=sum(migrate, na.rm=T), pop2001=mean(pop2001, na.rm=T)) %>% ungroup()


migration1 <- migration1 %>% left_join(shp, by=c("iso_o"="iso"))

migration1 <- migration1 %>% mutate(migrate=migrate/pop2001)


# breaks <- quantile(migration1$migrate, probs = seq(0, 1, 1/7))
# migration1$q7 <- cut(migration1$migrate, breaks = breaks, include.lowest = TRUE, labels = FALSE)


migration1 %>% st_as_sf()  %>% 
  mutate(migrate_q=migrate %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf(aes(fill=migrate_q))+
  #scale_fill_brewer(name = "7-Interval Quantile", palette = "YlOrRd", guide = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(1, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_outmigration_all.png", width=10, height=7, dpi=400)







# 2. sum stat: all within out-migration in each reason out of 2001 pop

migration2 <- migration %>% filter(cz_within==1) %>% 
  group_by(iso_o) %>% 
  summarise(migrate=sum(migrate, na.rm=T), pop2001=mean(pop2001, na.rm=T)) %>% ungroup()


migration2 <- migration2 %>% left_join(shp, by=c("iso_o"="iso"))


migration2<- migration2 %>% mutate(migrate=migrate/pop2001)


# breaks <- quantile(migration2$migrate, probs = seq(0, 1, 1/7))
# migration2$q7 <- cut(migration2$migrate, breaks = breaks, include.lowest = TRUE, labels = FALSE)


migration2 %>% st_as_sf()  %>% 
  mutate(migrate_q=migrate %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf(aes(fill=migrate_q))+
  #scale_fill_brewer(name = "7-Interval Quantile", palette = "YlOrRd", guide = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(1, 0.2), legend.key.size = unit(0.4, 'cm'))


ggsave("results/figures/map_outmigration_within.png", width=10, height=7, dpi=400)



# 3. sum stat: all across out-migration in each reason out of 2001 pop

migration3 <- migration %>% filter(cz_within==0) %>% 
  group_by(iso_o) %>% 
  summarise(migrate=sum(migrate, na.rm=T), pop2001=mean(pop2001, na.rm=T)) %>% ungroup()


migration3 <- migration3 %>% left_join(shp, by=c("iso_o"="iso"))


migration3 <- migration3 %>% mutate(migrate=migrate/pop2001)


# breaks <- quantile(migration3$migrate, probs = seq(0, 1, 1/7))
# migration3$q7 <- cut(migration3$migrate, breaks = breaks, include.lowest = TRUE, labels = FALSE)


migration3 %>% st_as_sf()  %>% 
  mutate(migrate_q=migrate %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf(aes(fill=migrate_q))+
  # scale_fill_brewer(name = "7-Interval Quantile", palette = "YlOrRd", guide = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(1, 0.2), legend.key.size = unit(0.4, 'cm'))



ggsave("results/figures/map_outmigration_across.png", width=10, height=7, dpi=400)






# 4. sum stat: do it for year average out migration

migration4 <- migration %>% group_by(year, iso_o) %>% 
  summarise(migrate=sum(migrate, na.rm=T), pop2001=mean(pop2001, na.rm=T)) %>% ungroup()


migration4 <- migration4 %>% group_by(iso_o) %>% 
  summarise(migrate=mean(migrate, na.rm=T), pop2001=mean(pop2001, na.rm=T)) %>% ungroup()


migration4 <- migration4 %>% left_join(shp, by=c("iso_o"="iso"))

migration4 <- migration4 %>% mutate(migrate=migrate/pop2001)


# breaks <- quantile(migration1$migrate, probs = seq(0, 1, 1/7))
# migration1$q7 <- cut(migration1$migrate, breaks = breaks, include.lowest = TRUE, labels = FALSE)


migration4 %>% st_as_sf()  %>% 
  mutate(migrate_q=migrate %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf(aes(fill=migrate_q))+
  # scale_fill_brewer(name = "7-Interval Quantile", palette = "YlOrRd", guide = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(1, 0.2), legend.key.size = unit(0.4, 'cm'))
ggsave("results/figures/map_outmigration_all_year.png", width=10, height=7, dpi=400)






# 5. imp and exp shock mapping

tb_all <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds") %>% 
  select(cz=cz_o, imp_shock=imp_shock_o, exp_shock=exp_shock_o) %>% 
  distinct(cz, .keep_all = T)




tb_all <- tb_all %>% left_join(shp_cz, by="cz")

tb_all %>% st_as_sf()  %>% 
  mutate(imp_shock_q=imp_shock %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf(aes(fill=imp_shock_q))+
  # scale_fill_brewer(name = "7-Interval Quantile", palette = "YlOrRd", guide = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(1, 0.2), legend.key.size = unit(0.4, 'cm'))


ggsave("results/figures/map_imp.png", width=10, height=7, dpi=400)



tb_all %>% st_as_sf()  %>% 
  mutate(exp_shock_q=exp_shock %>% sanghoon::classify(5, style = "quantile")) %>%
  ggplot()+
  geom_sf(aes(fill=exp_shock_q))+
  # scale_fill_brewer(name = "7-Interval Quantile", palette = "YlOrRd", guide = guide_legend(reverse = TRUE)) +
  scale_fill_brewer(name=NULL, label=c("0-20%", "20-40", "40-60", "60-80", "80-100"), palette="Yl0rRd")+
  theme_void() + theme(legend.position=c(1, 0.2), legend.key.size = unit(0.4, 'cm'))


ggsave("results/figures/map_exp.png", width=10, height=7, dpi=400)












# 
# 
# 
# # 1. sum stat: aggregated all cz across movers, within divied by pop2001
# 
# stat1 <- migration %>% group_by(cz_within) %>% 
#   summarise(migrate=sum(migrate, na.rm=T))
# 
# pop1 <- pop %>% summarise(pop2001=sum(pop2001))
# 
# 
# stat1 <- stat1 %>% bind_cols(pop1)
# 
# stat1 <- stat1 %>% mutate(migrate_sh=migrate/pop2001)
# 
# 
# 
# latex_output_stat1 <- xtable(stat1)
# 
# print(latex_output_stat1, type = "latex", table.envir = NULL, file = "results/tables/sumstat1.tex")
# 
# 
# 
# stat1 <- migration %>% group_by(year, cz_within) %>% 
#   summarise(migrate=sum(migrate, na.rm=T)) %>% ungroup()
# 
# pop1 <- pop %>% summarise(pop2001=sum(pop2001))
# 
# 
# stat1 <- stat1 %>% bind_cols(pop1)
# 
# stat1 <- stat1 %>% mutate(migrate_sh=migrate/pop2001)
# 
# 
# stat1 %>%
#   mutate(cz_within = ifelse(cz_within == 1, "Within", "Across")) %>%
#   ggplot(aes(year, migrate_sh, color = as.factor(cz_within))) +
#   geom_point(aes(size = migrate)) + 
#   theme_bw() +
#   scale_x_continuous(name = "Year", breaks = seq(from = 2001, to = 2020, by = 1)) +
#   labs(color = NULL)
# 
# 
# latex_output_stat1 <- xtable(stat1)
# 
# print(latex_output_stat1, type = "latex", table.envir = NULL, file = "results/tables/sumstat1_5.tex")
# 
# 
# 
# # 2. sum stat: age x cz
# 
# stat2 <- migration %>% group_by(age, cz_within) %>% 
#   summarise(migrate=sum(migrate, na.rm=T)) %>% 
#   ungroup()
# 
# pop2 <- pop %>% summarise(pop2001=sum(pop2001))
# 
# 
# stat2 <- stat2 %>% bind_cols(pop2)
# 
# stat2 <- stat2 %>% mutate(migrate_sh=migrate/pop2001)
# 
# 
# 
# latex_output_stat2 <- xtable(stat2)
# 
# print(latex_output_stat2, type = "latex", table.envir = NULL, file = "results/tables/sumstat2.tex")
# 
# 
# 
# 
# stat2 <- migration %>% group_by(age, cz_within) %>% 
#   summarise(migrate=sum(migrate, na.rm=T)) %>% 
#   ungroup()
# 
# pop2 <- pop %>% summarise(pop20=sum(pop20), pop30=sum(pop30), pop40=sum(pop40), pop50=sum(pop50), pop60=sum(pop60))
# 
# pop2 <- pop2 %>% pivot_longer(cols=starts_with("pop"), values_to = "pop") %>% select(pop)
# 
# pop2 <- pop2 %>% add_column(age=seq(from=20, to=60, by=10))
# 
# 
# stat2 <- stat2 %>% left_join(pop2, by="age")
# 
# stat2 <- stat2 %>% mutate(migrate_sh=migrate/pop)
# 
# 
# 
# latex_output_stat2 <- xtable(stat2)
# 
# print(latex_output_stat2, type = "latex", table.envir = NULL, file = "results/tables/sumstat2_5.tex")
# 
# 
# 
# 
# # 3. sum stat: age
# 
# stat3 <- migration %>% group_by(age) %>% 
#   summarise(migrate=sum(migrate, na.rm=T))
# 
# pop3 <- pop %>% summarise(pop2001=sum(pop2001))
# 
# 
# stat3 <- stat3 %>% bind_cols(pop3)
# 
# stat3 <- stat3 %>% mutate(migrate_sh=migrate/pop2001)
# 
# 
# 
# latex_output_stat3 <- xtable(stat3)
# 
# print(latex_output_stat3, type = "latex", table.envir = NULL, file = "results/tables/sumstat3.tex")
# 
# 
# 
# 
# stat3 <- migration %>% group_by(age) %>% 
#   summarise(migrate=sum(migrate, na.rm=T))
# 
# 
# 
# pop3 <- pop %>% summarise(pop20=sum(pop20), pop30=sum(pop30), pop40=sum(pop40), pop50=sum(pop50), pop60=sum(pop60))
# 
# pop3 <- pop3 %>% pivot_longer(cols=starts_with("pop"), values_to = "pop") %>% select(pop)
# 
# stat3 <- stat3 %>% bind_cols(pop3)
# 
# stat3 <- stat3 %>% mutate(migrate_sh=migrate/pop)
# 
# 
# latex_output_stat3 <- xtable(stat3)
# 
# print(latex_output_stat3, type = "latex", table.envir = NULL, file = "results/tables/sumstat3_5.tex")
# 
# 
# 
# # 4. sum stat: age across cz / all across cz
# 
# stat4 <- migration %>% filter(cz_within==0) %>% 
#   group_by(age) %>% 
#   summarise(migrate=sum(migrate, na.rm=T)) %>% 
#   mutate(migrate_all=sum(migrate)) %>% 
#   mutate(migrate_sh=migrate/migrate_all)
# 
# 
# latex_output_stat4 <- xtable(stat4)
# 
# print(latex_output_stat4, type = "latex", table.envir = NULL, file = "results/tables/sumstat4.tex")
# 
# 
# 
# 
# # 5. sum stat: age within cz / all within cz
# 
# 
# stat5 <- migration %>% filter(cz_within==1) %>% 
#   group_by(age) %>% 
#   summarise(migrate=sum(migrate, na.rm=T)) %>% 
#   mutate(migrate_all=sum(migrate)) %>% 
#   mutate(migrate_sh=migrate/migrate_all)
# 
# 
# latex_output_stat5 <- xtable(stat5)
# 
# print(latex_output_stat5, type = "latex", table.envir = NULL, file = "results/tables/sumstat5.tex")
# 
# 
# 
# 
# 
# # 6. sum stat: by iso origin within and across cz
# 
# 
# stat6 <- migration %>% group_by(iso_o, cz_within) %>% 
#   summarise(migrate=sum(migrate, na.rm=T)) %>% 
#   ungroup()
# 
# 
# stat6 <- stat6 %>% left_join(pop %>% select(iso, pop2001), by=c("iso_o"="iso"))
#   
# 
# stat6 <- stat6 %>% mutate(migration_sh=migrate/pop2001)
# 
# 
# stat6 <- stat6 %>% group_by(cz_within) %>% 
#   summarise(migrate=mean(migrate), pop2001=mean(pop2001), migration_sh=mean(migration_sh))
# 
# 
# latex_output_stat6 <- xtable(stat6)
# 
# print(latex_output_stat6, type = "latex", table.envir = NULL, file = "results/tables/sumstat6.tex")
# 
# 
# 
# 
# # 7. sum stat: average for every year. 
# 
# stat7 <- migration %>% group_by(year, iso_o) %>% 
#   summarise(migrate=sum(migrate, na.rm=T)) %>% 
#   ungroup()
# 
# 
# stat7 <- stat7 %>% left_join(pop %>% select(iso_o=iso, pop2001), by="iso_o")
# 
# stat7 <- stat7 %>% mutate(migration_sh=migrate/pop2001)
# 
# ## draw a plot
# 
# stat7 %>% ggplot(aes(iso_o, migration_sh, color=year)) +
#   geom_point()
# 
# stat7 %>% ggplot(aes(year, migration_sh, color=iso_o)) +
#   geom_point()
# 
# 
# stat7_sido <- stat7 %>% mutate(sido=str_sub(iso_o, 1, 2)) %>% 
#   group_by(year, sido) %>% 
#   summarise(migrate=sum(migrate, na.rm=T), pop2001=sum(pop2001, na.rm=T)) %>% 
#   mutate(migration_sh=migrate/pop2001)
# 
# 
# stat7_sido %>% ggplot(aes(sido, migration_sh, color=year)) +
#   geom_point()
# 
# stat7_sido %>% ggplot(aes(year, migration_sh, color=sido)) +
#   geom_point()
# 
# 
# 
# ggplot(stat7, aes(x=year, y=migration_sh, fill=iso_o)) +
#   geom_area(alpha=0.4, position='stack') +
#   theme_minimal() +
#   labs(title="Cumulative Migration Share by Year", x="Year", y="Migration Share")
# 
# 
# 
# 
