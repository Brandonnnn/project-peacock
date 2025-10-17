# Data Ingestion

library(tidyverse)
library(data.table)

setwd("~/Projects/project-peacock")

dt_raw <- fread("./data/raw/acs2017_census_tract_data.csv")
dt_raw

load("./data/raw/demoData.RData")
sp_data_demo


dt_date
affinity_data <- affinity_data_demo
dt_brand_list <- dt_brand_list_demo
sp_data <- sp_data_demo
sp_levels <- sp_levels_demo

save(dt_date, affinity_data, dt_brand_list, sp_data, sp_levels, file="./data/raw/data_demo.RData")
