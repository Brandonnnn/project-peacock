require(glue)
require(odbc)
require(data.table)
require(tidyverse)

# query needed to create a permanent help table map: ntwk_id to chn_nbr
"
  DROP TABLE IF EXISTS shiny_ntwk_chn;

  CREATE table shiny_ntwk_chn as
  select distinct 
      ntwk_id, 
      csdb_chn_nbr,
      lgl_entity_nm,
      lpad(csdb_chn_nbr::varchar, 3, '0') as csdb_chn_nbr_txt
  from py1uspa1.public.touchpoint_v
  where csdb_chn_nbr is not null
  ;
  
  GRANT SELECT ON shiny_ntwk_chn TO PUBLIC
  "

create_query_cameo_shopper <- function(prsnlty_name, major_level, network, retailer_name, use_static_filter, top_n){
  # Prepare data for CAMEO file
  # 
  # Args:
  # level: string
  # prsnlty_name: scalar:string 
  # prsnlth_name: scalar:string
  # networks: scalar:string (TV, BV)
  # retailer_name: vector:string
  # use_static_filter: bool
  # top_n: integers
  # 
  # Return: list
  # query: query used to export the target file
  # prsnlty_name: vector:string
  # retailers vector:string 
  # network: BV/TV network
  

  conn <- dbConnect(odbc(), dsn = "YB_conn", Database = "py1ussa1")
  
  if(!dbExistsTable(conn, "shiny_ntwk_chn")){stop("Missing help table : shiny_ntwk_chn")}
  if((top_n < 0) | (top_n%%1 != 0)){stop("top_n must be a non-negative integer")}
  # prepare ----
  
  ## where statement: network
  networks_where <- if(network == "BV"){
    "use_buyrvision = true"
  }else if(network == 'TV'){
    'use_tv = true'
  }else{
    stop("Networks can not be NULL and only be 'BV' or 'TV'!")
  }
  
  ## get personality basic information
  prsnlty_vec <- gsub("'", "''", prsnlty_name)
  prsnlty_vec <- toString(sprintf("'%s'", prsnlty_vec)) # collapse to sql code
  q_scoring_var <- "
    select personality_name, personality_key, major_hierarchy,
           case when scoring_rules like '%numeric_value1%' then 'numeric_value1'
                when scoring_rules like '%numeric_value2%' then 'numeric_value2'
                else 'null' end as scoring_var,
           lpad(segment_id::varchar(16),20,'0') as cnsmr_id_txt, 
           '444' as csdb_chn_nbr_txt
    from py1ussa1.public.personality_dim_v
    where personality_name in ({prsnlty_vec})
      and dimension = 'Shopper'
      and active = true
      and {networks_where}"
  personality_info <- dbGetQuery(conn, glue(q_scoring_var))
  
  ## reorder the table: primary personalities comes first
  ## set null score to 101
  personality_info <- personality_info[match(prsnlty_name, personality_info$personality_name), ] %>% 
    mutate(scoring_var = if_else(scoring_var == 'null' | is.null(personality_info$scoring_var), "101",
                                 scoring_var)) 
  
  ## check for duplications
  if(nrow(personality_info) != length(prsnlty_name)){
    personality_info <- filter(personality_info, major_hierarchy %in% major_level)
  }
  if(nrow(personality_info) != length(prsnlty_name)){
    stop("valueError: Could not find all the personalities informaiton")
  }
  
  ## limit statement: number of records to select
  if(top_n == 0){
    # select all the records
    limit_clause <- "--"
  }else{
    limit_clause <- glue("limit {top_n}")
  }
  
  ## where statement: retailers (multiple input)
  retailer_where_clause <- if(retailer_name[1] == '-All-'){
    "--- Pick all retailers"
  } else{
    retailer_vec <- gsub("'", "''", retailer_name) # escape names single quote
    retailer_vec <- toString(sprintf("'%s'", retailer_vec)) # collapse to sql code
    glue("and lgl_entity_nm in ({retailer_vec})")
  }
  
  ## where statement: static filter
  static_where_clause <- if(use_static_filter){
    "and IsConsistShop(a.shop_ord_hst_bmap, a.rcncy_ord_wk_cnt_nbr, a.rcncy_ord_prd_len_nbr,1,0)"
  } else{
    "--- Pick all shoppers"
  }
  
  # main query ----
  
  if(length(prsnlty_name) == 1){
    # single personality
    q_cameo <- glue("
      select a.cnsmr_id_txt, 
             max(b.csdb_chn_nbr_txt) as csdb_chn_nbr_txt,
             max({personality_info$scoring_var}) as score
      from py1ussa1.public.consumer_id_v a 
      join py1ussa1.public.shiny_ntwk_chn as b 
        on a.cnsmr_id_sys_ntwk_id = b.ntwk_id 
      join py1ussa1.public.shopper_personality_assn_v as c 
        on a.cnsmr_id_key = c.cnsmr_id_key 
      where c.personality_key = {personality_info$personality_key} --- {personality_info$personality_name}
        and a.abuser_ind='N'
        and a.cnsmr_id_key>0
        and a.unident_ind='N'
        {static_where_clause}
        {retailer_where_clause}
      group by 1
      order by 3 desc
      {limit_clause}
      ")
  } else{
    # multiple personalities
    
    ## build query for each personalities
    q_with_tables <- vector()
    q_with_join <- vector()
    q_with_order <- vector()
    for (i in seq_along(prsnlty_name)){
      pers_key <- personality_info$personality_key[i]
      pers_name <- personality_info$personality_name[i]
      score_value <- personality_info$scoring_var[i] 
      q_with_tables[i] <- glue("
      table{i} as (
      select a.cnsmr_id_txt, 
       max(b.csdb_chn_nbr_txt) as csdb_chn_nbr_txt,
       max({score_value}) as score
      from py1ussa1.public.consumer_id_v a 
      join py1ussa1.public.shiny_ntwk_chn as b 
        on a.cnsmr_id_sys_ntwk_id = b.ntwk_id 
      join py1ussa1.public.shopper_personality_assn_v as c 
        on a.cnsmr_id_key = c.cnsmr_id_key 
      where c.personality_key = {pers_key} --- {pers_name}
        and a.abuser_ind='N'
        and a.cnsmr_id_key>0
        and a.unident_ind='N'
        {static_where_clause}
        {retailer_where_clause}
      group by 1
      )")
      if (i == 1){next} # table 1 doesn't have join and order statements
      q_with_join[i] <- glue("join table{i} using (cnsmr_id_txt)")
      q_with_order[i] <- glue(",table{i}.score desc")
    }
    
    ## use frame to put all the queries together
    q_cameo <- glue("
    with 
    {paste(q_with_tables, collapse = ',\n')}
    select table1.*
    from table1
    {paste(q_with_join[!is.na(q_with_join)], collapse = '\n')}
    order by table1.score desc {paste(q_with_order[!is.na(q_with_order)], collapse = '')}
    {limit_clause}
    ")
  }
  
  # modify the query
  q_cameo_mod <- glue("
    SELECT *
    FROM (
    --- special first row: segment_id + '444'
    (select lpad(segment_id::varchar(16),20,'0') as cnsmr_id_txt
           ,'444' as csdb_chn_nbr_txt
           , 9999 as score
    from py1ussa1.public.personality_dim_v
    where personality_key = {personality_info$personality_key[1]} --- {personality_info$personality_name[1]}
    )
    UNION ALL
    --- main query
    ({q_cameo}
    )
    ) as input
    ORDER by score DESC
    ")
  
  on.exit(dbDisconnect(conn))
  return(list(
    query = q_cameo_mod,
    prsnlty_name = prsnlty_name, 
    retailers = retailer_name, 
    network = network
  ))
  
}

create_query_cameo_household <- function(prsnlty_name, major_level, network, retailer_name, top_n){
  # Prepare data for CAMEO file
  # 
  # Args:
  # level: string
  # prsnlty_name: vector:string
  # prsnlth_name: scalar:string
  # networks: scalar:string (TV, BV)
  # retailer_name: vector:string
  # top_n: integers
  # 
  # Return: list
  # query: query used to export the target file
  # prsnlty_name: vector:string
  # retailers vector:string 
  # network: BV/TV network
  
  require(glue)
  require(odbc)
  require(data.table)
  conn <- dbConnect(odbc(), dsn = "YB_conn", Database = "py1ussa1")
  
  if(!dbExistsTable(conn, "shiny_ntwk_chn")){stop("Missing help table : shiny_ntwk_chn")}
  if((top_n < 0) | (top_n%%1 != 0)){stop("top_n must be a non-negative integer")}
  
  # prepare ----
  
  ## where statement: network
  networks_where <- if(network == "BV"){
    "use_buyrvision = true"
  }else if(network == 'TV'){
    'use_tv = true'
  }else{
    stop("Networks can not be NULL and only be 'BV' or 'TV'!")
  }
  
  ## get personality basic information
  prsnlty_vec <- gsub("'", "''", prsnlty_name)
  prsnlty_vec <- toString(sprintf("'%s'", prsnlty_vec)) # collapse to sql code
  q_scoring_var <- "
    select personality_name, personality_key, major_hierarchy,
           case when scoring_rules like '%numeric_value1%' then 'numeric_value1'
                when scoring_rules like '%numeric_value2%' then 'numeric_value2'
                else 'null' end as scoring_var,
           lpad(segment_id::varchar(16),20,'0') as cnsmr_id_txt, 
           '444' as csdb_chn_nbr_txt
    from py1ussa1.public.personality_dim_v
    where personality_name in ({prsnlty_vec})
      and dimension = 'Shopper'
      and active = true
      and {networks_where}"
  personality_info <- dbGetQuery(conn, glue(q_scoring_var))
  if(nrow(personality_info) <= 0){
    stop(glue("valueError: Shopper Personlity {prsnlty_name} has no match in 'personality_dim_v' table"))
  }
  
  ## reorder the table: primary personalities comes first
  ## set null score to 101
  personality_info <- personality_info[match(prsnlty_name, personality_info$personality_name), ] %>% 
    mutate(scoring_var = if_else(scoring_var == 'null' | is.null(personality_info$scoring_var), "101",
                                 scoring_var)) 
  
  ## check for duplication
  if(nrow(personality_info) != length(prsnlty_name)){
    personality_info <- filter(personality_info, major_hierarchy %in% major_level)
  }
  if(nrow(personality_info) != length(prsnlty_name)){
    stop("valueError: Could not find all the personalities informaiton")
  }
  
  ## limit statement: number of records to select
  if(top_n == 0){
    # select all the records
    limit_clause <- "--"
  }else{
    limit_clause <- glue("limit {top_n}")
  }
  
  ## where statement: retailers (multiple input)
  retailer_where_clause <- if(retailer_name[1] == '-All-'){
    "--All"
  } else{
    retailer_vec <- gsub("'", "''", retailer_name) # escape names single quote
    retailer_vec <- toString(sprintf("'%s'", retailer_vec)) # collapse to sql code
    glue("and lgl_entity_nm in ({retailer_vec})")
  }
  
  ## static filter does NOT work for household level, since we are not using table py1ussa1.public.consumer_id_v
  
  # main query ----
  if(length(prsnlty_name) == 1){
    # single personality
    
    if(retailer_name[1] == '-All-'){
      # no need to get retailer information
      q_cameo <- glue("
          select lpad(cast(c.chhid as varchar), 14, '0') as chhid,
                 max({personality_info$scoring_var}) as score
          from py1ussa1.public.hh_personality_assn_v c
    	    where c.personality_key = {personality_info$personality_key} --- {personality_info$personality_name}
          group by 1
          order by 2 desc
          {limit_clause}
      ")
    } else {
      # need to join other tables for retailer information
      q_cameo <- glue("
          select lpad(cast(c.chhid as varchar), 14, '0') as chhid,
                 max({personality_info$scoring_var}) as score
          from py1ussa1.public.hh_personality_assn_v c
          join py1ussa1.public.shopper_personality_dim_v a
    	      on a.chhid = c.chhid
    	    join py1ussa1.public.shiny_ntwk_chn as d
    	      on d.csdb_chn_nbr_txt = a.max_csdb_chn_nbr_txt
    	    where c.personality_key = {personality_info$personality_key} --- {personality_info$personality_name}
            {retailer_where_clause}
          group by 1
          order by 2 desc
          {limit_clause}
      ")
    }
  } else {
    # multiple personalities
    
    ## build query for each personalities
    q_with_tables <- vector()
    q_with_join <- vector()
    q_with_order <- vector()
    for (i in seq_along(prsnlty_name)){
      pers_key <- personality_info$personality_key[i]
      pers_name <- personality_info$personality_name[i]
      score_value <- personality_info$scoring_var[i] 
      if(retailer_name[1] == "-All-"){
        # no need to get retailer information
        q_with_tables[i] <- glue("
          table{i} as (
            select lpad(cast(c.chhid as varchar), 14, '0') as chhid,
                     max({score_value}) as score
              from py1ussa1.public.hh_personality_assn_v c
        	    where c.personality_key = {pers_key} -- {pers_name}
              group by 1
          )")
      } else{
        # need to join other tables for retailer information
        q_with_tables[i] <- glue("
          table{i} as (
            select lpad(cast(c.chhid as varchar), 14, '0') as chhid,
                     max({score_value}) as score
              from py1ussa1.public.hh_personality_assn_v c
              join py1ussa1.public.shopper_personality_dim_v a
        	      on a.chhid = c.chhid
        	    join py1ussa1.public.shiny_ntwk_chn as d
        	      on d.csdb_chn_nbr_txt = a.max_csdb_chn_nbr_txt
        	    where c.personality_key = {pers_key} -- {pers_name}
                {retailer_where_clause}
              group by 1
          )")
      }
      if (i == 1){next} # table 1 doesn't have join and order statements
      q_with_join[i] <- glue("join table{i} using (chhid)")
      q_with_order[i] <- glue(",table{i}.score desc")
    }
    
    ## use frame to put all the queries together
    q_cameo <- glue("
    with 
    {paste(q_with_tables, collapse = ',\n')}
    select table1.*
    from table1
    {paste(q_with_join[!is.na(q_with_join)], collapse = '\n')}
    order by table1.score desc {paste(q_with_order[!is.na(q_with_order)], collapse = '')}
    {limit_clause}
    ")
    
  }
  
  on.exit(dbDisconnect(conn))
  return(list(
    query = q_cameo,
    prsnlty_name = prsnlty_name, 
    retailers = retailer_name, 
    network = network
  ))
  
}

ybunload_single_worker <- function(q_unload, prefix, download_dir, output_nm){
  # download large table from YB for cameo export
  #
  # Args:
  # q_unload: the select query used to extract information from YB
  # prefix: the prefix used to locate the downloaded table
  # download_dir: the directory used to save the output table
  # output_nm: the output table name (no file extension), since ybunload will automate add the .txt suffix
  #
  # Returns:
  # status: if success reture the transferred time, if not, return NA
  
  # download table using ybunload (ust have order statement)
  require(stringr)
  if(!grepl("order by", q_unload, ignore.case = T)){stop("q_unload_tbl must have an order statement")}
  q_unload <- shQuote(q_unload) # pass the whole string to the operating system
  result_str <- system(paste("./functions/ybunload_shiny.sh", download_dir, prefix, output_nm, q_unload),
                       intern = TRUE)
  
  # show ybunload status
  status <- str_split(result_str[length(result_str)], "\\[m\\] ")[[1]][2]
  
  # show number of records in the file
  file_path <- file.path(download_dir, output_nm)
  row_count <- system(paste0("wc -l ", file_path, ".txt"), intern = TRUE)
  n_rows <- as.numeric(str_split(str_trim(row_count, side = "both"), " ")[[1]][1])
  
  return(list(status = status, n_rows = n_rows))
}

export_cameo_table <- function(level, prsnlty_name, major_level, network, retailer_name, top_n,
                               use_static_filter, file_prefix, download_file_path){
  # use the function create_cameo_query and ybunload_single_worker to create and then
  # export the file to the destination
  #
  # Args: 
  # please check the docstring in create_cameo_query function
  # download_file_path (string): the directoyr used to save the output table
  # output_tbl_nm (string): the table name for the output (no file extension)
  # 
  # Returns:
  # List: return from create_caemo_query and n_rows
  
  conn <- dbConnect(odbc(), dsn = "YB_conn", Database = "py1ussa1")
  
  # prepare query
  if(level == "Shopper"){ # need to change to key map
    q_cameo = create_query_cameo_shopper(prsnlty_name, major_level, network, retailer_name, use_static_filter, top_n)
  } else if(level == "Household"){
    q_cameo = create_query_cameo_household(prsnlty_name, major_level, network, retailer_name, top_n)
  }
  
  # build table on YB (ybunload doesn't work with temp table)
  tbl_nm = paste0('SHINY_CAMEO_', format(Sys.time(), '%Y_%m_%d_%H%M'),'_',sample(0:1000,1),'_TEMP')
  dbGetQuery(conn, glue("create table {tbl_nm} as {q_cameo$query} 
                         DISTRIBUTE random"))
  
  # download table using ybunload tool
  if(level == "Shopper"){
    q_extract <- glue("select cnsmr_id_txt, csdb_chn_nbr_txt from {tbl_nm} order by score desc")
  } else {
    # Household
    q_extract <- glue("select chhid, score from {tbl_nm} order by score desc")
  }
  # file name convention (no suffix)
  output_nm <- paste(file_prefix, format(Sys.time(), '%Y%m%d_%H%M%S'), sep = "_")
  unload_rst <- ybunload_single_worker(q_unload = q_extract, 
                                       prefix = tbl_nm,
                                       download_dir = download_file_path, 
                                       output_nm = output_nm)

  # add attributes to the result
  q_cameo$nrows <- if_else(unload_rst$n_rows == 1, 0, unload_rst$n_rows) # empty file will also have 1 row
  q_cameo$level <- level
  q_cameo$query_status <- unload_rst$status
  q_cameo$output_tbl_nm <- paste0(output_nm, ".txt")
  q_cameo$time_run <- Sys.time() 
 
  # clean table
  on.exit(dbGetQuery(conn, glue("drop table {tbl_nm}")))
  
  return(q_cameo)
}


log_track_selection <- function(pageTab, value_lst, is_default = FALSE){
  # Funciton to track the selection of pageTab
  # Args:
  #   pageTab: Affinities / Demographics / Personalities
  #   value_lst: a list of values collapsed by "$$"
  #   is_default: TRUE if the value_lst is the default value set up by the portal
  
  require(glue)
  require(odbc)
  
  # get levels based on Tab
  levels <- if(pageTab %in% c("Affinities", "Demographics")){
    "manufacturer$$category$$brand"
  } else if (pageTab == "Personalities"){
    "type$$major$$intermediate$$personality"
  } else{
    stop(glue("No levels found on given pageTab: {pageTab}"))
  } 
  
  # track selection
  conn <- dbConnect(odbc(), dsn = "YB_conn", Database = "py1ussa1")
  
  rs <- dbSendStatement(
    conn,
    "INSERT INTO shiny_tracking_tab_log (tab_nm, view_time_utc, level, value, is_default) 
     VALUES (?, ?, ?, ?, ?)",
    params = list(
      pageTab,
      Sys.time(),
      levels, 
      paste(value_lst, collapse = "$$"),
      is_default)
  )
  
  # validate
  if(!dbHasCompleted(rs)){
    stop("Query has not completed, please check!")
  }
  if(dbGetRowsAffected(rs) <= 0){
    stop("No data has been ingested to the table, please check!")
  }
  
  # clean up
  dbClearResult(rs)
  dbDisconnect(conn)
}

log_track_export <- function(cameo_function_rst){
  # log CAMEO file extraction information to YB shiny_export_log
  
  require(glue)
  require(odbc)

  conn <- dbConnect(odbc(), dsn = "YB_conn", Database = "py1ussa1")
  
  rs <- dbSendStatement(
    conn,
    "INSERT INTO shiny_export_log (
      level, personality_nm, retailer_nm, extract_time_utc, output_rows
    )
     VALUES (?, ?, ?, ?, ?)",
    params = list(
      cameo_function_rst$level,
      cameo_function_rst$prsnlty_name,
      paste(cameo_function_rst$retailers, collapse = ','), 
      cameo_function_rst$time_run,
      cameo_function_rst$nrows
    )
  )
  
  # validate
  if(!dbHasCompleted(rs)){
    stop("Query has not completed, please check!")
  }
  if(dbGetRowsAffected(rs) <= 0){
    stop("No data has been ingested to the table, please check!")
  }
  
  # clean up
  dbClearResult(rs)
  dbDisconnect(conn)
}

log_export_destination <- function(cameo_function_rst, destination){
  # update table shiny_export_log if user export the file to a known destination
  # Args:
  #   cameo_function_rst: result from function export_cameo_table
  #   destination:
  #     value "local": user upload the file to the local dropbox
  #     value "cameo": user upload the file to the CAMEO system
  #     value "sdd": user upload the file to the SDD folder
  
  require(glue)
  require(odbc)
  require(dplyr)
  
  # check value
  if(!destination %in% c("local", "cameo", "sdd")){
    stop("InputError: destination can only be 'local', 'cameo', or 'sdd'")
  }
  
  # update data
  conn <- dbConnect(odbc(), dsn = "YB_conn", Database = "py1ussa1")
  
  to_where <- case_when(
    destination == "local" ~ "to_local",
    destination == "cameo" ~ "to_cameo",
    destination == "sdd"   ~ "to_sdd"
  )
  
  # handle the timestamp in the same way as log_track_export(...) 
  rs <- dbSendStatement(
    conn,
    glue("UPDATE shiny_export_log SET {to_where} = TRUE WHERE extract_time_utc = ?"),
    params = list(
      cameo_function_rst$time_run
    )
  )
  
  # validate
  if(!dbHasCompleted(rs)){
    stop("Query has not completed, please check!")
  }
  if(dbGetRowsAffected(rs) <= 0){
    stop("No data has been ingested to the table, please check!")
  }
  
  # clean up
  dbClearResult(rs)
  dbDisconnect(conn)
}
