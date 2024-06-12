# Note that this package requries the most recent dev version of data.table. To install, use the following:

# data.table::update_dev_pkg()


# To install ssaggregate, run the following:

# library(devtools)
# devtools::install_github("kylebutts/ssaggregate")

library(tidyverse)
library(fixest)
library(ssaggregate)

data("df")
data("shares")


# get our main dataset
df <- read_rds("data/final/shock/shock_main_data_sigungu_two_period.rds") %>% 
  select()


# we must first make the ind share

