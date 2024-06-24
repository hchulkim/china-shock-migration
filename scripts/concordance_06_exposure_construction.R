

##############################
# creator: Hyoungchul Kim
# revised date: 06/24/2024
# description: This script constructs the local exposure of trade shock. We generally follow the procedure mentioned in Autor, Dorn, Hanson (2013) but use the method in Borusyak, Hull, and Jaravel (2022) to  clearly separate the exposure shares from the industry shocks, highlighting the shift-share structure of the instrument.
##############################

if (!require(pacman)) install.packages("pacman")
pacman::p_load(here, tidyverse)

#-------------------- 
# read in the data: comtrade data, industry data.
#-------------------- 
import_data <- read_rds(here("data", "temp", "import_data_ksic10.rds"))
export_data <- read_rds(here("data", "temp", "export_data_ksic10.rds"))

est_data <- read_rds(here("data", "temp", "est_region_industry_matched.rds"))


#-------------------- 
# construct s_{lnt} : share of manufacturing industry n in total employment in location l
#-------------------- 
s_lnt <- est_data %>% 
  group_by(iso, year) %>% 
  mutate(emp_total = sum(emp_all)) %>% 
  ungroup()

s_lnt <- s_lnt %>% 
  mutate(sh_lnt = emp_all / emp_total)


#-------------------- 
# construct g_{nt} :  industry n’s growth of imports from China in the eight comparable economies over period t (also expressed in $1,000 per U.S. worker) 
#-------------------- 

g_nt_list <- list(export = export_data, import = import_data)


process_gnt <- function(df) {
  df <- df %>% 
    group_by(cntry, ksiccode) %>% 
    mutate(value_lag = lag(value)) %>% 
    ungroup()
  
  # if the year is not 2001, value_lag NA values should be set to zero
  df <- df %>% 
    mutate(value_lag = if_else(is.na(value_lag), 0, value_lag))
 
  # get the change in imports and exports 
  df <- df %>% 
    mutate(m_nt = value - value_lag)
 # make the change value into $1000. 
  df <- df %>% 
    mutate(m_nt = m_nt / 1000)
}

g_nt <- map(g_nt_list, process_gnt) %>% 
  list_rbind(names_to = "type")


# only leave out 2010 - 2001 and 2019 - 2010 diff
g_nt <- g_nt %>% 
  filter(year>=2010, year<2020)

# separate main x variable korea and iv z variable
g_nt_kor <- g_nt %>% 
  filter(cntry == "KOR")

# iv cntry to use
country = c("AUS", "CHE", "DEU", "DNK", "ESP", "FIN", "NZL")

# iv construction first
g_nt_iv <- g_nt %>% 
  filter(cntry %in% country) %>% 
  group_by(type, year, ksiccode) %>% 
  summarise(m_nt = sum(m_nt, na.rm = T)) %>% 
  ungroup()


# get the e_{nt}
e_nt <- est_data %>% 
  group_by(year, ksic10) %>% 
  summarise(emp_nt = sum(emp_all)) %>% 
  ungroup()


e_nt_iv <- e_nt %>% filter(year == 1999) %>% 
  select(-year)

g_nt_iv <- g_nt_iv %>% 
  left_join(e_nt_iv, by=c("ksiccode"="ksic10")) %>% 
  mutate(emp_nt = if_else(emp_nt == 0, NA, emp_nt))

g_nt_iv <- g_nt_iv %>% 
  mutate(gr_nt_iv = m_nt / emp_nt)

g_nt_iv <- g_nt_iv %>% 
  mutate(gr_nt_iv = if_else(is.na(gr_nt_iv), 0, gr_nt_iv))


g_nt_iv <- g_nt_iv %>% 
  select(type, year, ksiccode, gr_nt_iv)


# now kor g_nt
g_nt_kor <- g_nt_kor %>% 
  left_join(e_nt, by=c("year", "ksiccode"="ksic10"))

g_nt_kor <- g_nt_kor %>% 
  mutate(emp_nt = if_else(emp_nt == 0, NA, emp_nt))

g_nt_kor <- g_nt_kor %>% 
  mutate(gr_nt = m_nt / emp_nt)


g_nt_kor <- g_nt_kor %>% 
  mutate(gr_nt = if_else(is.na(gr_nt), 0, gr_nt))


g_nt_kor <- g_nt_kor %>% 
  select(type, year, ksiccode, gr_nt)


check <- g_nt_kor %>% left_join(g_nt_iv, by=c("year", "ksiccode", "type"))


# now multiply s_lnt to get the final x_lt and z_lt
x_lt <- s_lnt %>% 
  left_join(g_nt_kor, by=c("year", "ksic10"="ksiccode"))

x_lt <- x_lt %>% 
  mutate(local_x_lt = sh_lnt * gr_nt) %>% 
  group_by(iso, year, type) %>% 
  summarise(local_x_lt = sum(local_x_lt, na.rm = T)) %>% 
  ungroup()
x_lt <- x_lt %>% filter(year>=2010, year<2020)

x_lt <- x_lt %>% filter(!is.na(year), !is.na(type))  

z_lt <- s_lnt %>% filter(year == 1999) %>% select(-year) %>%
  left_join(g_nt_iv, by=c("ksic10"="ksiccode"))

z_lt <- z_lt %>% 
  mutate(local_z_lt = sh_lnt * gr_nt_iv) %>% 
  group_by(iso, year, type) %>% 
  summarise(local_z_lt = sum(local_z_lt, na.rm = T)) %>% 
  ungroup()

z_lt <- z_lt %>% filter(!is.na(year))  


# merge the data and save it

final_data <- x_lt %>% 
  left_join(z_lt, by=c("iso", "year", "type"))
final_data %>% 
  write_rds(here("data", "temp", "local_exposure_data.rds"))
