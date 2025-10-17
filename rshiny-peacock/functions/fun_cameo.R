prepareCameoTable_odbc_new <- function(level, prsnlty_name, major_level, network, retailer_name, top_n){
  # Prepare data for CAMEO file
  # 
  # Args:
  # level: string
  # prsnlty_name: scalar:string 
  # prsnlth_name: scalar:string
  # networks: scalar:string (TV, BV)
  # retailer_name: vector:string
  # top_n: integers
  # 
  # Return: list
  # data: data.table[cnsmr_id_txt, csdb_chn_nbr, score]
  # scoring_rules: scalar:string
  # time_spend: scalar:POSIXct
  # level: scalar:string
  # prsnlty_name: vector:string
  # retailers vector:string 
  # n_rows: integers
  # query: scalar:string
  
  require(glue)
  require(odbc)
  require(data.table)
  conn <- dbConnect(odbc(), dsn = "YB_conn", Database = "py1ussa1")
  
  if((top_n < 0) | (top_n%%1 != 0)){stop("top_n must be a non-negative integer")}
  if(!dbExistsTable(conn, "shiny_ntwk_chn")){stop("Missing table : shiny_ntwk_chn")}
  
  startTime <- Sys.time()
  
  # get personality information
  
  ## create a permanent help table map: ntwk_id to chn_nbr
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
  
  ## where statement: network
  networks_where <- if(network == "BV"){
    "use_buyrvision = true"
  }else if(network == 'TV'){
    'use_tv = true'
  }else{
    stop("Networks can not be NULL and only be 'BV' or 'TV'!")
  }

  ## build query and run
  q_scoring_var <- "
    select personality_key, 
           major_hierarchy,
           case when scoring_rules like '%numeric_value1%' then 'numeric_value1'
                when scoring_rules like '%numeric_value2%' then 'numeric_value2'
                else 'null' end as scoring_var,
           lpad(segment_id::varchar(16),20,'0') as cnsmr_id_txt, 
           '444' as csdb_chn_nbr_txt
    from py1ussa1.public.personality_dim_v
    where personality_name = '{prsnlty_name}'
      and dimension = 'Shopper'
      and active = true
      and {networks_where}"
  personality_info <- dbGetQuery(conn, glue(q_scoring_var))
  
  if(nrow(personality_info) != 1){
    # use major_hierarchy to reduplicate
    personality_info <- filter(personality_info, major_hierarchy == major_level)
  }
  if(nrow(personality_info) != 1){
    stop("records variable error: there must have one and only one score rule")
  }
  
  # create query for cameo export 
  
  ## select statement: score
  if(is.null(personality_info$scoring_var) | personality_info$scoring_var == 'null'){
    warning("Scoring rule is empty")
      q_score <- 101
    } else{
      q_score <- glue("max(c.{personality_info$scoring_var})")
  }
  
  # personality key 
  # prsnlty_key_where_clause <- glue("personality_key = {personality_info$personality_key}")
  
  ## where statement: retailers (multiple input)
  retailer_where_clause <- if(retailer_name[1] == '-All-'){
    "--All"
  } else{
    retailer_vec <- gsub("'", "''", retailer_name) # escape names single quote
    retailer_vec <- toString(sprintf("'%s'", retailer_vec)) # collapse to sql code
    glue("and lgl_entity_nm in ({retailer_vec})")
  }
  
  ## main query
  if(level == 'Shopper') {
    # shopper cards level
    
    q_cameo <- glue("
      select a.cnsmr_id_txt, 
             max(b.csdb_chn_nbr_txt) as csdb_chn_nbr_txt,
             {q_score} as score
      from py1ussa1.public.consumer_id_v a 
      join py1ussa1.public.shiny_ntwk_chn as b 
        on a.cnsmr_id_sys_ntwk_id = b.ntwk_id 
      join py1ussa1.public.shopper_personality_assn_v as c 
        on a.cnsmr_id_key = c.cnsmr_id_key 
      where c.personality_key = {personality_info$personality_key}
        {retailer_where_clause}
      group by 1
      order by 3 desc
      limit {top_n}")
      # cnsmr_id_txt might have multiple score or chn_nbr, pick the max to avoid duplication
  } else {
    # household level
    
    if(retailer_where_clause == '--All') {
      # all retailer
      
      q_cameo <- glue("
          select lpad(cast(c.chhid as varchar), 14, '0') as chhid, 
                 {q_score} as score
          from py1ussa1.public.hh_personality_assn_v as c 
          where c.personality_key = {personality_info$personality_key}
          group by 1
          order by 2 desc
          limit {top_n}")
    } else {
      # need to identify retailers use id map
      
      if(network == "BV"){
        
        # BV network: use shopper_personality_assn_v
        q_cameo <- glue("
          select lpad(cast(d.cchid as varchar), 14, '0') as chhid,
                 {q_score} as score
          from py1ussa1.public.consumer_id_v a
          join py1ussa1.public.shiny_ntwk_chn as b
            on a.cnsmr_id_sys_ntwk_id = b.ntwk_id
          join py1ussa1.public.shopper_personality_assn_v as c
            on a.cnsmr_id_key = c.cnsmr_id_key
          join py1ussa1.public.experian_v d
            on c.cnsmr_id_key = d.cnsmr_id_key
          where c.personality_key = {personality_info$personality_key}
            {retailer_where_clause}
          group by 1
          order by 2 desc
          limit {top_n}")
      } else if (network == 'TV'){
        
        # TV network: use hh_personality_assn_v
        q_cameo <- glue("
          select lpad(cast(c.chhid as varchar), 14, '0') as chhid,
                 {q_score} as score
          from py1ussa1.public.hh_personality_assn_v c
          join py1ussa1.public.shopper_personality_dim_v a
    	      on a.chhid = c.chhid
    	    join py1ussa1.public.shiny_ntwk_chn as d
    	      on d.csdb_chn_nbr_txt = a.max_csdb_chn_nbr_txt
    	    where c.personality_key = {personality_info$personality_key}
            {retailer_where_clause}
          group by 1
          order by 2 desc
          limit {top_n}")
      }
    }
  }
  
  if(top_n < 5e6){
    # single query
    
    if(level != 'Household') {
      rst <- as.data.table(dbGetQuery(conn, q_cameo)) # cnsmr_id_txt, csdb_chn_nbr
      rst[, score := NULL]
    } else {
      
      rst <- as.data.table(dbGetQuery(conn, q_cameo)) # chhid, score
    }
    
  } else{
    # large query (split)
    
    # message("(2/3) ")
    tmp_tbl = paste0('SHINY_CAMEO_', format(Sys.time(), '%Y_%m_%d__%H%M'),'_',sample(0:1000,1),'_TEMP')
    n_batch <- ceiling(top_n / 4.9e6)
    
    if(level != 'Household') { 
      q_temp_tbl <- "
        create temp table {tmp_tbl} as
        with shiny_cameo_input as
        ( {q_cameo} )
        select cnsmr_id_txt,
               csdb_chn_nbr_txt,
               NTILE({n_batch}) OVER (ORDER BY score) as batch_nbr
        from shiny_cameo_input
        distribute on (batch_nbr) "
    } else {
      q_temp_tbl <- "
        create temp table {tmp_tbl} as
        with shiny_cameo_input as
        ( {q_cameo} )
        select chhid,
               score,
               NTILE({n_batch}) OVER (ORDER BY score) as batch_nbr
        from shiny_cameo_input
        distribute on (batch_nbr) "
    }
    
    dbGetQuery(conn, glue(q_temp_tbl))
    
    # write out
    rst_lst <- list()
    for (batch in seq(n_batch)){
      if(level != 'Household') {
        q_fetch <- "select cnsmr_id_txt, csdb_chn_nbr_txt from {tmp_tbl} where batch_nbr = {batch}"
      }  else {
        q_fetch <- "select chhid, score from {tmp_tbl} where batch_nbr = {batch}"
      }
      rst_lst[[batch]] <- as.data.table(dbGetQuery(conn, glue(q_fetch)))
    }
    rst <- rbindlist(rst_lst)
    rm(rst_lst)
  }
  
  if(level != 'Household') {
    # add tracking ID to CAMEO file
    rst <- rbind(as.data.table(personality_info[,c("cnsmr_id_txt", "csdb_chn_nbr_txt")]), rst)
    q_cameo <- glue("
    select coalesce(t1.cnsmr_id_txt, t2.cnsmr_id_txt) as cnsmr_id_txt,
           coalesce(t1.csdb_chn_nbr_txt, t2.csdb_chn_nbr_txt) as csdb_chn_nbr_txt
    from 
    ( {q_cameo} ) t1
    full join 
    (select lpad(segment_id::varchar(16),20,'0') as cnsmr_id_txt, 
            '444' as csdb_chn_nbr_txt
    from py1ussa1.public.personality_dim_v
    where personality_key = {personality_info$personality_key} ) t2
      on t1.cnsmr_id_txt = t2.cnsmr_id_txt
      and t1.csdb_chn_nbr_txt = t2.csdb_chn_nbr_txt 
    order by 1")
  }
  
  on.exit(dbDisconnect(conn))
  return(list(
    data = rst,
    scoring_rules = personality_info$scoring_var, 
    time_spend = difftime(Sys.time(), startTime, units = "secs"),
    time_run = Sys.time(),
    level = level,
    prsnlty_name = prsnlty_name, 
    retailers = retailer_name, 
    n_rows = nrow(rst),
    query = q_cameo,
    network = network
  ))
  
}

log_export <- function(cameo_function_rst){
  # log CAMEO file information to YB shiny_export_log
  require(glue)
  require(odbc)
  
  # ----Query used to create the Personality Logging table
  # create table shiny_export_log (
  #   level varchar(50),
  #   personality_nm varchar(255),
  #   retailer_nm varchar(2000),
  #   log_time TIMESTAMPTZ, /* this is a timestamp with TZ */
  #   query_rows int,
  #   query_exec_time_sec int,
  #   save_local bool,
  #   save_dropbox bool
  # );
  # grant select on shiny_export_log to public;
  # GRANT ALL PRIVILEGES ON TABLE shiny_export_log TO bwang, r_shiny_app;
  
  conn <- dbConnect(odbc(), dsn = "YB_conn", Database = "py1ussa1")
  q_log <- "
    insert into shiny_export_log (level, personality_nm, retailer_nm, log_time, query_rows, query_exec_time_sec)
    values(
      {cameo_function_rst$level},
      {cameo_function_rst$prsnlty_name},
      {paste(cameo_function_rst$retailers, collapse = ', ')},
      {cameo_function_rst$time_run},
      {cameo_function_rst$n_rows},
      {round(cameo_function_rst$time_spend, 0)})
    "
  dbSendQuery(conn, glue_sql(q_log, .con = conn))
}

update_log_export <- function(cameo_function_rst, mode){
  # update table shiny_export_log if user export the file
  # mode = "download": user download the file to the local
  # mode = "upload": user upload the file to the dropbox
  
  require(glue)
  require(odbc)
  
  if(!mode %in% c("download", "upload")){stop("InputError: mode can only be 'download' or 'upload'")}
  
  conn <- dbConnect(odbc(), dsn = "YB_conn", Database = "py1ussa1")
  act_save <- ifelse(mode == "download", 'save_local', 'save_dropbox')
  q_update <- "
  UPDATE shiny_export_log
  SET {`act_save`} = TRUE 
  WHERE log_time = {cameo_function_rst$time_run};
  "
  dbSendQuery(conn, glue_sql(q_update, .con = conn))
}
