
# install devtools and chorddiag if you haven't.
# install.packages("devtools")
# devtools::install_github("mattflor/chorddiag")

library(tidyverse)
library(circlize)
library(chorddiag)
library(viridis)

# first, read in our baseline regression data as it has information for local exposure
data <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
# data <- data %>% filter(cz_o!=cz_d)
data <- data %>% mutate(across(starts_with("imp"), ~ .x/1000))
data <- data %>% mutate(across(starts_with("exp"), ~ .x/1000))

data <- data %>% select(iso_o, iso_d, cz_o, cz_d, imp_shock_o, imp_shock_d, exp_shock_o, exp_shock_d, pop2001_o, pop2001_d, migrate)

data <- data %>% group_by(cz_o, cz_d) %>% 
  summarise(imp_shock_o=mean(imp_shock_o, na.rm=T), imp_shock_d=mean(imp_shock_d, na.rm=T), exp_shock_o=mean(exp_shock_o, na.rm=T), exp_shock_d=mean(exp_shock_d, na.rm=T), migrate=sum(migrate, na.rm=T)) %>% ungroup()



check <- expand.grid(cz_o=1:33, cz_d=1:33)

data <- check %>% left_join(data, by=c("cz_o", "cz_d"))

data <- data %>% mutate(imp_shock_o = imp_shock_o %>% sanghoon::classify(6, style = "quantile"), 
                        imp_shock_d = imp_shock_d %>% sanghoon::classify(6, style = "quantile"),
                        exp_shock_o = exp_shock_o %>% sanghoon::classify(6, style = "quantile"),
                        exp_shock_d = exp_shock_d %>% sanghoon::classify(6, style = "quantile"))

data <- data %>% group_by(exp_shock_o, exp_shock_d ) %>% 
  summarise(migrate=sum(migrate, na.rm=T)) %>% ungroup()

data <- data %>% mutate(exp_shock_o=c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6))

data <- data %>% mutate(exp_shock_d=c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6))

data <- data %>% mutate(migrate=ifelse(exp_shock_o==exp_shock_d, 0, migrate))
# 
# data <- data %>% mutate(exp_shock_o=as.character(exp_shock_o), exp_shock_d=as.character(exp_shock_d))
# 
# data <- data %>% mutate(exp_shock_o=str_replace(exp_shock_o, " - ", ""),
#                         exp_shock_d=str_replace(exp_shock_d, " - ", ""))
# 
# data <- data %>% mutate(exp_shock_o=as.numeric(exp_shock_o), exp_shock_d=as.numeric(exp_shock_d))

data <- as.data.frame(data)

# parameters
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# color palette
mycolor <- viridis(6, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:6)]

# Base plot
chordDiagram(
  x = data, 
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)

# Add text and axis
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector. 
    circos.text(
      x = mean(xlim), 
      y = 3.2, 
      labels = sector.index, 
      facing = "bending", 
      cex = 0.8
    )
    
    # Add graduation on axis
    circos.axis(
      h = "top", 
      major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)), 
      minor.ticks = 1, 
      major.tick.percentage = 0.5,
      labels.niceFacing = FALSE)
  }
)

# data <- data %>% group_by(exp_shock_o, exp_shock_d) %>% 
#   summarise(migrate=sum(migrate, na.rm=T)) %>% ungroup()


# Reshape the data to a wider format
# wide_data <- data %>%
#   pivot_wider(names_from = cz_d, values_from = migrate, values_fill = list(migrate = NA))
# 
# 
# wide_data <- wide_data %>% group_by(cz_o) %>% 
#   summarise(imp_shock_o=mean(imp_shock_o, na.rm=T), imp_shock_d=mean(imp_shock_d, na.rm=T), exp_shock_o=mean(exp_shock_o, na.rm=T), exp_shock_d=mean(exp_shock_d, na.rm=T), across(matches("\\d"), ~ sum(.x, na.rm=T))) %>% ungroup()
# 
# data <- wide_data %>% select(cz_o, imp_shock=imp_shock_o, exp_shock=exp_shock_o, matches("\\d"))
# 
# # Set 'cz_o' as row names
# row.names(data) <- data$cz_o
# 
# # Remove the 'cz_o' column
# wide_data <- wide_data[, -which(names(wide_data) == "cz_o")]


# parameters
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# color palette
mycolor <- viridis(6, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:6)]

# Base plot
chordDiagram(
  x = data %>% select(exp_shock_o, exp_shock_d, migrate), 
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)

# Add text and axis
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector. 
    circos.text(
      x = mean(xlim),
      y = 3.2,
      labels = sector.index, 
      facing = "bending", 
      cex = 0.8
    )
    
    # Add graduation on axis
    circos.axis(
      h = "top",
      major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)),
      minor.ticks = 1,
      major.tick.length = 0.5,
      labels.niceFacing = FALSE)
   }
)









######################################################
library(tidyverse)


# first, read in our baseline regression data as it has information for local exposure
data <- read_rds("data/final/shock/shock_main_data2001_2019_sigungu.rds")
# data <- data %>% filter(cz_o!=cz_d)
data <- data %>% mutate(across(starts_with("imp"), ~ .x/1000))
data <- data %>% mutate(across(starts_with("exp"), ~ .x/1000))

data <- data %>% select(iso_o, iso_d, cz_o, cz_d, imp_shock_d, exp_shock_d, pop2001_o, pop2001_d, migrate)

data <- data %>% group_by(cz_d) %>% 
  summarise(imp_shock_d=mean(imp_shock_d, na.rm=T), exp_shock_d=mean(exp_shock_d, na.rm=T), migrate=sum(migrate, na.rm=T)) %>% ungroup()

data <- data %>% mutate(exp_imp = exp_shock_d - imp_shock_d)


data_imp <- data %>% arrange(desc(imp_shock_d))

data_imp <- data_imp %>% mutate(migrate=log(migrate))

data_imp <- data_imp %>% mutate(cz=1:33)

data_imp %>% ggplot(aes(cz, migrate)) +
  geom_col() +
  scale_x_continuous(name="CZ ordered by import expousre (1 - highest exposure, 33 - lowest)", breaks=1:33) +
  scale_y_continuous(name="log(migrate)") +
  theme_bw()

data_exp <- data %>% arrange(desc(exp_shock_d))

data_exp <- data_exp %>% mutate(migrate=log(migrate))

data_exp <- data_exp %>% mutate(cz=1:33)

data_exp %>% ggplot(aes(cz, migrate)) +
  geom_col() +
  scale_x_continuous(name="CZ ordered by export expousre (1 - highest exposure, 33 - lowest)", breaks=1:33) +
  scale_y_continuous(name="log(migrate)") +
  theme_bw()





data_all <- data %>% arrange(desc(exp_imp))

data_all <- data_all %>% mutate(migrate=log(migrate))

data_all <- data_all %>% mutate(cz=1:33)

data_all %>% ggplot(aes(cz, migrate)) +
  geom_col() +
  scale_x_continuous(name="CZ ordered by relative expousre (1 - highest exposure, 33 - lowest)", breaks=1:33) +
  scale_y_continuous(name="log(migrate)") +
  theme_bw()

# no log
data_ten <- data %>% mutate(imp_shock_d = imp_shock_d %>% sanghoon::classify(6, style = "quantile"),
                            exp_shock_d = exp_shock_d %>% sanghoon::classify(6, style = "quantile"),
                            exp_imp = exp_imp %>% sanghoon::classify(6, style = "quantile"))




data_ten_import <- data_ten %>% group_by(imp_shock_d) %>% 
  summarise(migrate=sum(migrate))

# data_ten_import <- data_ten_import %>% mutate(migrate=log(migrate))

data_ten_export <- data_ten %>% group_by(exp_shock_d) %>% 
  summarise(migrate=sum(migrate))

# data_ten_export <- data_ten_export %>% mutate(migrate=log(migrate))

data_ten_rel <- data_ten %>% group_by(exp_imp) %>% 
  summarise(migrate=sum(migrate))

# data_ten_rel <- data_ten_rel %>% mutate(migrate=log(migrate))

data_ten_export <- data_ten_export %>% arrange(desc(exp_shock_d))

data_ten_export %>% ggplot(aes(exp_shock_d, migrate)) +
  geom_col() +
  scale_x_discrete(name="CZ ordered by export expousre (left - lowest, right - highest)") +
  scale_y_continuous(name="Total number of in-Migration", labels=scales::label_comma(scale=0.001, suffix = "K")) +
  theme_bw()
ggsave("results/migration_map_exp.png", dpi=400)

data_ten_import <- data_ten_import %>% arrange(desc(imp_shock_d))

data_ten_import %>% ggplot(aes(imp_shock_d, migrate)) +
  geom_col() +
  scale_x_discrete(name="CZ ordered by import expousre (left - lowest, right - highest)") +
  scale_y_continuous(name="Total number of in-Migration", labels=scales::label_comma(scale=0.001, suffix = "K")) +
  theme_bw()
ggsave("results/migration_map_imp.png", dpi=400)

data_ten_rel <- data_ten_rel %>% arrange(desc(exp_imp))

data_ten_rel %>% ggplot(aes(exp_imp, migrate)) +
  geom_col() +
  scale_x_discrete(name="CZ ordered by relative expousre (left - lowest, right - highest)") +
  scale_y_continuous(name="Total number of in-Migration", labels=scales::label_comma(scale=0.001, suffix = "K")) +
  theme_bw()
ggsave("results/migration_map_rel.png", dpi=400)






# yes log
data_ten <- data %>% mutate(imp_shock_d = imp_shock_d %>% sanghoon::classify(6, style = "quantile"),
                            exp_shock_d = exp_shock_d %>% sanghoon::classify(6, style = "quantile"),
                            exp_imp = exp_imp %>% sanghoon::classify(6, style = "quantile"))




data_ten_import <- data_ten %>% group_by(imp_shock_d) %>% 
  summarise(migrate=sum(migrate))

data_ten_import <- data_ten_import %>% mutate(migrate=log(migrate))

data_ten_export <- data_ten %>% group_by(exp_shock_d) %>% 
  summarise(migrate=sum(migrate))

data_ten_export <- data_ten_export %>% mutate(migrate=log(migrate))

data_ten_rel <- data_ten %>% group_by(exp_imp) %>% 
  summarise(migrate=sum(migrate))

data_ten_rel <- data_ten_rel %>% mutate(migrate=log(migrate))

data_ten_export <- data_ten_export %>% arrange(desc(exp_shock_d))

data_ten_export %>% ggplot(aes(exp_shock_d, migrate)) +
  geom_col() +
  scale_x_discrete(name="CZ ordered by export expousre (left - lowest, right - highest)") +
  scale_y_continuous(name="Total number of in-Migration") +
  theme_bw()
ggsave("results/migration_map_exp_log.png", dpi=400)

data_ten_import <- data_ten_import %>% arrange(desc(imp_shock_d))

data_ten_import %>% ggplot(aes(imp_shock_d, migrate)) +
  geom_col() +
  scale_x_discrete(name="CZ ordered by import expousre (left - lowest, right - highest)") +
  scale_y_continuous(name="Total number of in-Migration") +
  theme_bw()
ggsave("results/migration_map_imp_log.png", dpi=400)

data_ten_rel <- data_ten_rel %>% arrange(desc(exp_imp))

data_ten_rel %>% ggplot(aes(exp_imp, migrate)) +
  geom_col() +
  scale_x_discrete(name="CZ ordered by relative expousre (left - lowest, right - highest)") +
  scale_y_continuous(name="Total number of in-Migration") +
  theme_bw()
ggsave("results/migration_map_rel_log.png", dpi=400)













####

  data <- data %>% mutate(imp_shock_o = imp_shock_o %>% sanghoon::classify(10, style = "quantile"), 
                          imp_shock_d = imp_shock_d %>% sanghoon::classify(10, style = "quantile"),
                          exp_shock_o = exp_shock_o %>% sanghoon::classify(10, style = "quantile"),
                          exp_shock_d = exp_shock_d %>% sanghoon::classify(10, style = "quantile"))


check <- expand.grid(cz_o=1:33, cz_d=1:33)

data <- check %>% left_join(data, by=c("cz_o", "cz_d"))

data <- data %>% mutate(imp_shock_o = imp_shock_o %>% sanghoon::classify(10, style = "quantile"), 
                        imp_shock_d = imp_shock_d %>% sanghoon::classify(10, style = "quantile"),
                        exp_shock_o = exp_shock_o %>% sanghoon::classify(10, style = "quantile"),
                        exp_shock_d = exp_shock_d %>% sanghoon::classify(10, style = "quantile"))

data <- data %>% group_by(exp_shock_o, exp_shock_d ) %>% 
  summarise(migrate=sum(migrate, na.rm=T)) %>% ungroup()

data <- data %>% mutate(migrate=log(migrate))

data <- as.data.frame(data)



