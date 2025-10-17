################################################################################.
# Code to create CAMEO FILE under navbarMenu
# In this script include ui and server function to build several pages
# Updated: 2024-01-31
################################################################################.

plan(multisession)

# duplicated personalities
dup_pers <- sp_levels %>% 
  group_by(personality_org) %>% 
  summarise(n_count = n(), .groups = 'keep') %>% 
  filter(n_count > 1) %>% 
  pull(personality_org)
s_pers <- toString(sprintf("'%s'", dup_pers))

# map table: Demographics/Brand personality 
df_prsnlty_map <-  sp_levels %>% 
  filter(type %in% c("Brand Purchasing", "Demographics")) %>%  
  group_by(personality_name) %>% 
  summarise(pick_pers_nm = first(personality_org), .groups = 'keep') 

# fetch Message (added 2022-05-20)
conn <- dbConnect(odbc(), dsn = "YB_conn", Database = "py1ussa1")
df_in <- dbGetQuery(conn,
                    "select * from shiny_message_board 
                    where is_warning = TRUE") %>% 
  arrange(desc(public_date))

################################.
# UI ------
cameoPageUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$script(src="format_nbr.js"),
    sidebarPanel(width = 3,
                 div(title = "Select your output level.", # tooltip
                     style = "margin-top: 30px", # space to the action button
                     selectInput(ns('pick_level'), label = "Step 1 - Select a output level",
                       choices = c("Household", "Shopper"), 
                       selected = 'Shopper')
                  ),
                 # if pick household, need to pick BV/TV networks
                 conditionalPanel(
                   condition = "['Household'].includes(input.pick_level)",
                   ns = ns,
                   div(style = "margin-top: 30px",
                       selectizeInput(ns("pick_networks"),
                         label = shiny::HTML("Step 1b - Select a network group"),
                         choices = c(
                           "Digital Network" = 'BV',
                           "Digital-TV Network" = 'TV'),
                         multiple = FALSE,
                         selected = 'Digital Network'))
                 ),
                 div(title = "Select your searching type.", # tooltip
                     style = "margin-top: 30px", # space to the action button
                     selectizeInput(ns('pick_prsnlty'), 
                       label = shiny::HTML("<p>Step 2 - Select personalities 
                        <span style = 'font-weight:400'><br/>(You can select mutiple personalities)</span></p>"),
                       choices = NULL, multiple = TRUE, options= list(maxOptions = 2000))
                 ),
                div(title = "Allow user to put a small prefix for the file", # tooltip
                  style = "margin-top: 30px", # space to the action button
                  textInput(ns("file_prefix"),
                    label = shiny::HTML("<p>Step 3 - Add a prefix to the file <span style = 'font-weight:400'>
                      <br/>(Less than 100 letters)</span></p>"),
                    placeholder = "DataSol")
                ),
      
                  # use static filter
                  conditionalPanel(
                    condition = "input.pick_level == 'Shopper'", ns = ns,
                    div(style = "margin-top: 20px",
                      awesomeCheckbox(ns("use_static_filter"), label = "Pick Active Shoppers ", value = TRUE)
                    )
                  ),
                 div(title = "Input for number of unique IDs for the given personalities",
                     style = "margin-top: 20px",
                     awesomeCheckbox(ns("select_max_ids"), label = "Get all available IDs", value = TRUE)
                 ),
                 uiOutput(ns("input_num_ids")), # input: number of records
               
                 # duplicated personalities: pick intermediate level
                 conditionalPanel(
                   condition = glue("[{s_pers}].includes(input.pick_prsnlty)"),
                   ns = ns,
                   div(style = "margin-top: 30px",
                       selectizeInput(ns("pick_major_per"),
                                      label = shiny::HTML("Step 3b - Select a major category"),
                                      choices = NULL, multiple = TRUE))
                   # has to pick category for each personality (most time, no duplication)
                 ),
                 
                 div(title = "Show result on all retailers or selected retailers",
                     style = "margin-top: 20px",
                     awesomeCheckbox(ns("select_all_reta"), label = "Show result on all retailers", value = NULL)
                 ),
                 uiOutput(ns("show_reta")),# select retailers
                 
                 #  # prefix name (disabled)
                 # div(style = "margin-top: 20px",
                 #   awesomeCheckbox(ns("add_prefix"), label = "Use default file name", value = TRUE)
                 # ),
                 # uiOutput(ns("prefix_input")),
                
                 div(style = "margin-top:20px",
                     actionButton(ns('fetch_cameo'),
                                  label = "Prepare Data", icon = icon("fas fa-table"), class = "down")
                 )
    ),# end sidebar
    
    mainPanel(width = 8, 
              div(class= "depr-text-box",
                  div(class= "title", textOutput(ns("page_title"))),
                  div(class= "content", withSpinner(htmlOutput(ns("page_notes"))))
              ),
              br(),
              conditionalPanel(
                condition = glue("{nrow(df_in)} >= 1"),
                div(class= "depr-text-box",
                    div(class= "title", htmlOutput(ns("message_title"))),
                    div(class= "content", htmlOutput(ns("message_box")))
                )
              ),
              br(),
              uiOutput(ns("cameo_result_attr"))
    ) #end main panel
  )
}

################################.
# SERVER ------
cameoPage <- function(input, output, session, psnlty_output){

    # input ----
  
  # get initial values from personality page
  observe({
    updateAwesomeCheckbox(session, "select_all_reta", 
      value = psnlty_output$show_all_retailers())
  })
  
  pers_from_other <- reactive({
    if(psnlty_output$get_prsnly() %in% pull(df_prsnlty_map, personality_name)){
      # Demographics/Brand personality will use map table to transform to personality in yellowbrick with levels
      df_prsnlty_map %>%  
        filter(personality_name == psnlty_output$get_prsnly()) %>% 
        pull(pick_pers_nm) 
    } else{
      # normal case
      psnlty_output$get_prsnly()
    }
  })
  
  observe({
    if(input$pick_networks == 'BV'){
      vec_pers <- sort(sp_levels$personality_org)
    } else{
      # TV networks only have certain types of personalities
      vec_pers <- sp_levels %>% 
        filter(use_tv == 1) %>% 
        pull(personality_org)
    }
    updateSelectizeInput(session, "pick_prsnlty",
                      choices = c("Pick Personalities" = "", paste(vec_pers)),
                      selected = pers_from_other()
    )
  })
  
  # control to select number of IDs
  output$input_num_ids <- renderUI({
    req(!input$select_max_ids)
    
    div(style = "margin-top: 10px",
        numericInput(session$ns("cameo_num_attr"),
          label = shiny::HTML("<p>Step 4a - Select number of final IDs</p>"), value = 100000)
    )
  })
  
  # control to select retailers
  output$show_reta <- renderUI({
    req(!input$select_all_reta)
    
    if(input$pick_level == 'Shopper'){
      lst_reta <- sort(unique(sp_data[personality_org == input$pick_prsnlty & lgl_entity_nm != '-All-']$lgl_entity_nm))
    } else{
      # Household Level
      if(input$pick_networks == 'BV'){
        lst_reta <- sort(unique(sp_data[personality_org == input$pick_prsnlty 
                                        & lgl_entity_nm != '-All-'
                                        & BV_hh_tot_cnt > 0]$lgl_entity_nm))
      } else if(input$pick_networks == 'TV'){
        lst_reta <- sort(unique(sp_data[personality_org == input$pick_prsnlty 
                                        & lgl_entity_nm != '-All-'
                                        & TV_hh_tot_cnt > 0]$lgl_entity_nm))
      }
    }

    div(title = "Select one or more retailers", style = "margin-top: 10px",
      selectizeInput(session$ns("pick_reta"),
        label = shiny::HTML("<p>Step 4b - Select one or more retailers 
                  <span style = 'font-weight:400'><br/>(You can select mutiple retailers)</span></p>"),
        choices = c("Select retailers" = "", paste(lst_reta)),
        multiple = TRUE, selected = unlist(psnlty_output$get_retailers())))
  })
  
  d_sp_reta <- reactive({ # return a list, so need to use unlist(...) before use
    ifelse(input$select_all_reta == TRUE, '-All-',
           ifelse(is.null(input$pick_reta), '-All-', list(input$pick_reta)))
  })
  
  # # control to add a prefix to the file (disabled)
  # output$prefix_input <- renderUI({
  #   req(!input$add_prefix)
  #   
  #   div(title = "Allow user to put a small prefix for the file", style = "margin-top: 10px",
  #     textInput(session$ns("file_prefix"),
  #       label = shiny::HTML("<p>Step 5 - Add a prefix to the file 
  #                 <span style = 'font-weight:400'><br/>(Less than 50 letters)</span></p>"),
  #       placeholder = "userID")
  #   )
  # })

  # summary box -----
  output$page_title <- renderText({
    "Audience File Extraction"
  })
  
  output$page_notes <- renderUI({
    shiny::HTML(paste("<i><u>Reminder</u>: Retailer specific audiences and campaigns may require pre-approval.  Please review the Account level policies.</i>",
                      "Note: Please don't close the tab while fetching the data.",
                      sep = "<br/><br/>"))
  })
  
  # warning message box ----
  # add (2022-05-20)
  
  output$message_title <- renderUI({
    "Warnings"
  })
  output$message_box <- renderUI({
    # each row will be a separate message
    msg_vec <- c()
    for (i in seq(nrow(df_in))){
      msg_vec[i] <- paste0(df_in[i,"public_date"], ": ", df_in[i,"message"])
    }
    shiny::HTML(paste(msg_vec, collapse = " <br>"))
  })
  
  # asyn process ----
  
  # status indicators
  nclicks <- reactiveVal(0)
  first_click <- reactiveVal(0)
  result_val <- reactiveVal()
  
  observeEvent(input$fetch_cameo,{
    # # !!! Disable the CAMEO function (Need to comment this out if not wanted)
    # if(nclicks() >= 0){
    #   showNotification(glue("Due to data issues, this feature has been temporarily disabled. ({format(Sys.time(), format = '%Y-%m-%d')})"),
    #                    type = 'error', duration = 10)
    #   return(NULL)
    # }
    
    # Don't do anything if analysis is already being run
    if(nclicks() != 0){
      showNotification("There is a query running!", type = 'error', duration = 10)
      return(NULL)
    }
    
    # Increment clicks and prevent concurrent analyses
    nclicks(nclicks() + 1)
    first_click(first_click() + 1)
    result_val(data.frame(Status="Loading..."))
    # fire_running()
    
    # load reactive values (future can NOT read reactive values)
    level_in <- input$pick_level
    prsnlty_name_in <- input$pick_prsnlty # vector of character strings
    network_in <- ifelse(input$pick_level == "Shopper", "BV", input$pick_networks) # shopper only have BV networks
    major_level_in <- input$pick_major_per
    retailers_lst <- unlist(d_sp_reta())
    ## export_cameo_table will select all the IDs if top_n = 0
    top_n_in <- ifelse(input$select_max_ids == TRUE, 0, as.numeric(gsub(",", "", input$cameo_num_attr))) # js function change to string
    static_filter <- input$use_static_filter
    ## prefix by default is DataSol, and remove all punctuation characters
    prefix <- if_else(input$file_prefix == "", "DataSol",
      gsub("[[:punct:] ]",'_', input$file_prefix))
    
    # define output table information
    download_path_in <- "/data/dropbox" # do not end with /
    # download_path_in <- "/Users/bwang/projects/ds-shiny-tools/others/tmp"

    # **future func ----
    
    # create a promise to execute the query in the future
    fun_promise <- future(
      export_cameo_table(level = level_in,
                         prsnlty_name = prsnlty_name_in,
                         network = network_in,
                         major_level = major_level_in,
                         retailer_name = retailers_lst,
                         use_static_filter = static_filter,
                         top_n = top_n_in,
                         file_prefix = prefix,
                         download_file_path = download_path_in)
    ) %...>% result_val()
    
    # access result
    then(fun_promise,
        onFulfilled = function(){
            # if success
             nclicks(0)
             log_track_export(result_val()) # log future funcs result to YB updated 01/12/24
             showNotification("Done", type = "message", duration = 10)
           },
        onRejected = function(e){
            # if failed
             nclicks(0)
             result_val(NULL)
             showNotification(e$message, type = 'error', duration = 20)
           }
      )
    
    # Return something other than the promise so shiny remains responsive
    NULL
  })
  
  # control the UI
  output$cameo_result_attr <- renderUI({
    if(nclicks() == 0 & first_click() == 0){ # first time
      div(class = 'depr-text-box',
          div(class = 'content',
              shiny::HTML("Press the 'Prepare Data' button to fetch the data. <br><br>")
          ))
    } else if(nclicks() != 0 & first_click() != 0){ # running
      div(class = 'depr-text-box',
          div(class = 'content',
              shiny::HTML("Preparing the data...Please wait <br><br>")
          ))
    } else if(nclicks() == 0 & first_click() != 0){ # show result
      div(style = 'line-height:20px;',
          uiOutput(session$ns('cameo_info_table_attr'))
      )
    }
  })
  
  # # file name convension (disabled as no longer needed)
  # fileName <- reactive({
  #   req(result_val()$retailers)
  #   n_retailers <- length(result_val()$retailers)
  #   if(result_val()$retailers[1] == '-All-'){
  #     name_retailers <- "all"
  #   } else if(n_retailers <= 5){
  #     name_retailers <- gsub(' ','_', paste(tolower(result_val()$retailers), collapse = '_'))
  #   } else {
  #     name_retailers <- paste0("retailers_n", n_retailers)
  #   }
  #   paste0("DataSol_", gsub("[[:punct:] ]",'_', paste(tolower(result_val()$prsnlty_name), collapse = '_')),
  #          "_", name_retailers, "_", result_val()$n_rows, "_", tolower(result_val()$level), ".txt")
  # })
  
  
  # render tables
  output$cameo_info_table_attr <- renderUI({
    req(result_val()$level)
    div(class = 'depr-text-box',
        div(class = 'content',
            p(tags$b("Audience File Information")),
            shiny::HTML(paste0(
              "<b>Output Level</b>: ", result_val()$level, "<br>",
              if(result_val()$level == "Household"){paste0(
                "<b>Network</b>: ",ifelse(result_val()$network == "BV", "Digital Network", "Digital-TV Network"), "<br>")},
              "<b>Personality</b>: ", paste(result_val()$prsnlty_name, collapse = '; '), "<br>",
              "<b>Retailers</b>: ", paste(result_val()$retailers, collapse = '; '), "<br><br>",
              "File Name: ", result_val()$output_tbl_nm,  "<br>",
              "Number of Unique Records: ", prettyNum(result_val()$nrows, big.mark = ",", scientific=F), "<br>",
              "Dropbox Path: \\\\stpshared\\shared\\Program_Services\\Shared\\LM Cameo Files\\Drop Box\\Personalities\\Custom\\", "<br>",
              if(result_val()$level == "Household"){
                "SDD Path: //dlpreastus21016519tdcs.azuredatalakestore.net/segmentation/seed_src/others/type=hhid/<br>"
                },
              "Other information: ", result_val()$query_status, "<br>"
            )),
            div(style = "margin-top: 20px;",
              # shopper special: save to the cili dropbox
              if(result_val()$level == "Shopper"){
                actionButton(session$ns("dropbox_cili"), label = "Load to CAMEO",
                  icon = icon("fa-solid fa-upload"), class = "down")
              },
              # Save to Box
                actionButton(session$ns("dropbox_cameo"), label = "Save to Dropbox",
                             icon = icon("fab fa-dropbox"), class = "down"),
              # Household special: save to SDD folder using az
                if(result_val()$level == "Household"){
                  actionButton(session$ns("upload_TDC"), label = "Save to SDD",
                               icon = icon("fas fa-folder-open"), class = "down")
                },
              # get query
                actionButton(session$ns("get_query"), label = "SQL Queries", 
                             icon =icon("fas fa-database"), class = "down")
            )
        ))
  })
  
  # write out ----

  # export to dropbox
  observeEvent(input$dropbox_cameo, {
    # progress bar
    style <- isolate(input$style)
    withProgress(message = "Prepare Output", style = style, value = 0, {
      
      incProgress(0.1, message = "part1: Check ouput data")
      # filename <- fileName()
      print_address <- "\\\\stpshared\\shared\\Program_Services\\Shared\\LM Cameo Files\\Drop Box\\Personalities\\Custom\\"
      dropbox_address <- 'shinyuser@10.22.60.46:/cameo_dropbox/Personalities/Custom/'
      save_address <- paste0("/data/dropbox/", result_val()$output_tbl_nm)
      
      incProgress(0.5, message = "part2: Export data to Dropbox")
      # upload to dropbox and rm the local file
      system(paste('scp -i /home/shiny/.ssh/id_rsa', save_address, dropbox_address))

      incProgress(0.7, message = "part3: Update log and show Result")
      # log result
      log_export_destination(result_val(), destination = "local")
      showModal(modalDialog(
        shiny::HTML(paste0(
          "<b>Data Uploaded</b><br><br>",
          "<b>Address</b>: ", print_address, "<br>",
          "<b>Filename</b>: ", result_val()$output_tbl_nm)),
        footer = modalButton("Close (Esc)")
      ))
      
      incProgress(1, message = "Done")
    })
  })
  
  # export to SDD
  observeEvent(input$upload_TDC, {
    # progress bar
    style <- isolate(input$style)
    withProgress(message = "Prepare Output", style = style, value = 0, {
      
      incProgress(0.1, message = "part1: Check ouput data")
      # filename <- fileName()
      print_address <- "//dlpreastus21016519tdcs.azuredatalakestore.net/segmentation/seed_src/others/type=hhid/"
      save_address <- paste0("/data/dropbox/", result_val()$output_tbl_nm)
      logFile <- "/home/log.txt"
      
      incProgress(0.4, message = "part2: Upload data to Azure Data Lake")
      system(paste("/home/upload2Az.sh", save_address, result_val()$output_tbl_nm))
      txt <- readChar(logFile, file.info(logFile)$size)
      
      incProgress(0.8, message = "part3: Show result")
      log_export_destination(result_val(), destination = "sdd")
      showModal(modalDialog(
        shiny::HTML(paste0(
          "<b>Data Uploading</b><br><br>",
          "<b>Address</b>: ", print_address, "<br>",
          "<b>Filename</b>: ", result_val()$output_tbl_nm, "<br><br>",
          "Upload Result: <br>"
        )),
        shiny::HTML(gsub("\\n", "<br>", gsub("\\[#*\\]", "", txt))), # remove [####] and replace /n
        footer = modalButton("Close (Esc)")
      ))
      
      incProgress(1, message = "Done")
    })
  })
  
  # export to cameo folder
  observeEvent(input$dropbox_cili, {
    # progress bar
    style <- isolate(input$style)
    withProgress(message = "Prepare Output", style = style, value = 0, {
      
      incProgress(0.1, message = "part1: Check ouput data")
      print_address <- "\\\\prod_storage1\\targeting\\CAMEO\\PROD_LIST_IMPORT\\"
      # dropbox_address <- "shinyuser@10.22.60.46:/cameo_dropbox/Personalities/Custom/test_cili/" # test only
      dropbox_address <- 'shinyuser@10.22.60.46:/cameo_portal/PROD_LIST_IMPORT/'  # true address
      load_address <- paste0("/data/dropbox/", result_val()$output_tbl_nm)
      renamed_file <- paste(".customer", gsub(".txt", "", result_val()$output_tbl_nm), "shinyuser", sep = ".")
      
      incProgress(0.3, message = "part2: Export data to CAMEO")
      system(paste('scp -i /home/shiny/.ssh/id_rsa', load_address, paste0(dropbox_address, renamed_file)))
      
      incProgress(0.7, message = "part3: Rename file once finished")
      # The file is transferred to the folder with a dot at the beginning, then renamed to remove the dot once complete
      system(paste("./functions/rename_file.sh", renamed_file, gsub("^.", "", renamed_file)))
      
      incProgress(0.9, message = "part3: Update log and show Result")
      log_export_destination(result_val(), destination = "cameo")
      showModal(modalDialog(
        shiny::HTML(paste0(
          "<b>Data Uploaded</b><br><br>",
          "<b>Address</b>: ", print_address, "<br>",
          "<b>Filename</b>: ", renamed_file)),
        footer = modalButton("Close (Esc)")
        ))
      
      incProgress(1, message = "Done")
    })
  })
  
  
  # display query
  observeEvent(input$get_query, {
    showModal(
      modalDialog(title = "Queries",
                  br(),
                  # source
                  shiny::HTML(paste("
                  <b>Host</b>: orlpybvip01.catmktg.com <br>
                  <b>Warehouse</b>: Yellowbrick <br>
                  <b>Database</b>: py1ussa1")),
                  hr(),
                  # query
                  div(class = 'query', shiny::HTML(gsub("\\n", "<br>", result_val()$query)),
                      size = 'l', easyClose = TRUE, fade = FALSE, footer = modalButton("Close (Esc)"))))
  })
  
}
################ END.
