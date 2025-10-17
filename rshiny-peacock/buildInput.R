# date
create_date = "20220130"
setwd("/Users/bwang/projects/ds-shiny-tools")

# update dates
dt_date <- readRDS(sprintf("rds/data_update_date_%s.rds", create_date))

# brand affinity
affinity_data <- readRDS(sprintf("rds/brand_affinity_ingredient_data_%s.rds", create_date))
dt_brand_list <- unique(affinity_data[manufacturer != ' ', .(manufacturer, cat_desc, brand_desc)])[
  order(manufacturer, cat_desc, brand_desc)] 

# time affinity
affinity_time_date_data <- readRDS(sprintf("rds/brand_affinity_time_date_data_%s.rds", create_date))
dt_time_date_brand_list <- unique(affinity_time_date_data[manufacturer != ' ', .(manufacturer, cat_desc, brand_desc, shopper_personality)])[
  order(manufacturer, cat_desc, brand_desc, shopper_personality)] 

# personality audiences (shopper personalities)
sp_data_org <- readRDS(sprintf("rds/data_personalities_count_%s.rds", create_date))
full_sp_data <- readRDS(sprintf("rds/full_prsnly_count_%s.rds", create_date)) # make sure first column is segment_id

sp_data_status <- full_sp_data[, lapply(.SD, function(x){max(as.integer(x))}),
                               .SDcols = c("use_buyrvision", "use_tv", "use_in_store", "use_hub360","use_syndicated_programmatic", "royalty_owed"),
                               by = .(type, major_hierarchy, intermediate_hierarchy, personality_org)]
sp_data <- merge.data.table(sp_data_org, sp_data_status, 
                            by = c("type", "major_hierarchy", "intermediate_hierarchy", "personality_org"), all.x = TRUE)
sp_levels <- unique(sp_data[, .(type, major_hierarchy, intermediate_hierarchy, personality_name, personality_org, use_tv)])

full_desc <- unique(sp_data[, .(type, major_hierarchy, intermediate_hierarchy, personality_name, personality_desc)]) %>% 
  filter(personality_desc != 0)

sp_data <- sp_data %>% 
  select(-personality_desc) %>% 
  inner_join(full_desc, by = c("type", "major_hierarchy", "intermediate_hierarchy", "personality_name"))

## add active count and cumsum (11/17/2021)
sp_data[order(personality_level), id_act_cum:=cumsum(id_cnt_act),
        by = .(type,major_hierarchy,intermediate_hierarchy,personality_org, lgl_entity_nm)]

# demographic insights
demo_data <- readRDS(sprintf("rds/brand_demographic_%s.rds", create_date))
demo_brand_list <- unique(demo_data[manufacturer != ' ', .(manufacturer, cat_desc, brand_desc)])

# store personality (added 01/30/2022)
# store (add at 01/18/2022)
store_data <- readRDS(sprintf("rds/data_store_demographic_%s.rds", create_date))
tdlinx_data <- readRDS(sprintf("rds/data_storeinfo_%s.rds", create_date))
sp_demLevels <- unique(store_data[store_data$type=="Demographics", .(type, major_hierarchy, intermediate_hierarchy, personality_name)])


# save as a single file
save(dt_date,
     affinity_data, dt_brand_list, 
     affinity_time_date_data, dt_time_date_brand_list, 
     full_sp_data, sp_data, sp_levels, 
     demo_data, demo_brand_list,
     store_data, tdlinx_data, sp_demLevels, # added 01/30/2022
     file = sprintf("rds/inputData_%s.RData", create_date)
)
# # demo file
# save(dt_date,
#      affinity_data, dt_brand_list, 
#      affinity_time_date_data, dt_time_date_brand_list, 
#      full_sp_data, sp_data, sp_levels, 
#      demo_data, demo_brand_list,
#      file = sprintf("rds/inputData_%s_demo.RData", create_date)
# )
