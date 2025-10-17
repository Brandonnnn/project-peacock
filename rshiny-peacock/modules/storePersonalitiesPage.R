################################################################################.
# Code to create Store Personality Audiences
# In this script include ui and server function to build the page
# Updated: 2021-12-08 
################################################################################.

################################.
# UI ------
storePersonalitiesPageUI  <- function(id){
  ns <- NS(id)
  
  
  tagList(
    # sidebar ----
    sidebarPanel(width = 3, #Filter options
                 actionButton(ns("help_sp"), label="Help", icon= icon('question-circle'), class ="down"),
                 actionButton(ns("defs_sp"),label="Definitions", icon= icon('info'), class ="down"),
                 
                 # select type
                 div(title="Select your searching type.", # tooltip
                     style = "margin-top: 20px", # space to the action button
                     selectInput(ns('store_search_type'), label = "Step 1 - Select a personality type",
                                 choices = sort(unique(sp_demLevels$type)),
                                 selected = 'Demographics')
                 ),
                 
                 # select major cate
                 div(style = "margin-top: 20px", # space above
                     selectInput(ns('store_pick_major'),
                                 shiny::HTML("<p>Step 2 - Select a major category 
                        <span style = 'font-weight:400'><br/>(hit backspace and start typing to search)</span></p>"),
                                 choices = NULL)),
                 
                 # select intermediate level
                 div(title = "Select the intermediate hierarchy",
                     style = "margin-top: 20px",
                     selectInput(ns("store_pick_inter"),
                                 label = shiny::HTML("<p>Step 3 - Select an intermediate category 
                        <span style = 'font-weight:400'><br/>(hit backspace and start typing to search)</span></p>"),
                                 choices = NULL)),
                 
                 # select personality
                 div(title = "Select the personality",
                 style = "margin-top: 20px",
                 selectInput(ns("store_pick_pers"),
                             label = shiny::HTML("<p>Step 4 - Select a personality 
                        <span style = 'font-weight:400'><br/>(hit backspace and start typing to search)</span></p>"),
                             choices = NULL)),
                 
                 # # retailer
                 # div(title = "Show result on all retailers or selected retailers",
                 #     style = "margin-top: 10px",
                 #     awesomeCheckbox(ns("sp_all_channels"), label = "Show result on all channels", value = TRUE)),
                 # uiOutput(ns("sp_show_channels")),# select retailers
                 
                 # button
           #      div(style = "margin-top:20px",
          #           # Cameo 
          #           #  CAMEO does not upload store files...
          #           actionButton(ns('open_cameo'), label = "Get Audience File", icon = icon("fas fa-file-excel"), class = "down"),
          #           # download
          #           downloadButton(ns('download_pers'),"Download Data", class = "down"),
          ##           # query
          #           actionButton(ns("get_query"), label = "SQL Queries", icon =icon("fas fa-database"), class = "down"),
          #           # Availability
          #           actionButton(ns("get_usage"), label = "Check Availability", icon =icon("fas fa-font-awesome"), class = "down")
          #       )
                 
                 div(style = "margin-top:20px",
                     # download
                     downloadButton(ns('download_pers'),"Download Data", class = "down"),
                     # query
                     actionButton(ns("get_query"), label = "SQL Queries", icon =icon("fas fa-database"), class = "down")
                 )
                 
    ), #end sidebar Panel
  
    # mainPanel ----
        mainPanel(width = 9,
              # Summary box:
              div(class= "depr-text-box",
                  div(class= "title", textOutput(ns("sp_table_title"))),
                  div(class= "content", withSpinner(htmlOutput(ns("sp_table_contents"))))
              ),
              # Personality Table
              div(style = "padding: 5px; padding-left: 5px; width: 98%; font-size: 95%",
                  htmlOutput(ns("sp_level_title")),
                  withSpinner(dataTableOutput(ns('sp_level_table')))
              ),
              
              div(style = "padding: 5px; padding-left: 5px; width: 98%; font-size: 95%",
                  #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
                  #absolutePanel(checkboxInput("catalina", "Catalina Network", TRUE),  checkboxInput("restofUS", "All Other", TRUE)
                  checkboxGroupInput(inputId =ns("mapNetworks"), label = "Networks",
                                     choices=c("Catalina Network"='pick_cat',  "All Other" = 'pick_others'),
                                     selected=c("pick_cat","pick_others"),
                                     inline = TRUE # put buttons horizontally
                  ),
                  tags$script("$('input[value=\"pick_cat\"]').parent().css('color','blue');"),
                  #tags$script("$('input[value=\"pick_cat\"]').parent().css('background-color','black');"),
                  tags$script("$('input[value=\"pick_others\"]').parent().css('color','green');"),
                  #tags$script("$('input[value=\"pick_others\"]').parent().css('background-color','black');"),
                  verbatimTextOutput("value"),
                  withSpinner(leafletOutput(ns( "mymap")))
              )
              
    ) #end main Panel
    
    
    
    
  )#end tagList
}


################################.

# SERVER ------
storePersonalitiesPage   <- function(input, output, session){
  
  # Sidebar Input ----
  observe({
    lst_major <- sort(unique(sp_demLevels[type == input$store_search_type, major_hierarchy]))
    updateSelectInput(session, "store_pick_major", 
                      choices = lst_major,
                      label = paste("Step 2 - picked type ", input$store_search_type),
                      selected = lst_major[1]
    )
  })
  
  observe({
    req(input$store_pick_major)
    lst_inter <- sort(unique(sp_demLevels[type == input$store_search_type & major_hierarchy == input$store_pick_major]$intermediate_hierarchy))
    updateSelectInput(session, "store_pick_inter", choices = lst_inter, selected = lst_inter[1])
  })
  
  observe({
    req(input$store_pick_inter)
    lst_pers <- sort(unique(sp_demLevels[type == input$store_search_type 
                                       & major_hierarchy == input$store_pick_major
                                       & intermediate_hierarchy == input$store_pick_inter]$personality_name))
    updateSelectInput(session, "store_pick_pers", choices = lst_pers, selected = lst_pers[1])
  })
  
  output$sp_show_channels <- renderUI({
    req(!input$sp_all_channels, input$store_pick_pers)
    lst_reta <- sort(unique(store_data[type == input$store_search_type 
                                    & major_hierarchy == input$store_pick_major
                                    & intermediate_hierarchy == input$store_pick_inter
                                    & personality_name == input$store_pick_pers]$channel))
    div(title = "Select one or more channels",
        selectizeInput(session$ns("sp_pick_reta"),
                       label = shiny::HTML("<p>Step 4b - Select one or more channels 
                <span style = 'font-weight:400'><br/>(You can select mutiple retailers)</span></p>"),
                       choices = c("Select channels" = "", paste(lst_reta)),
                       multiple = TRUE, selected = ""))
  })

  # page summary ----
  update_date <- reactive(dt_date[table_nm == "brand_demographic_table"]$date_end)  ##this is temporary
  
  # Render summary box (make sure personality name is unique)
  data_personality <- reactive({
    req(input$store_pick_major, input$store_pick_inter, input$store_pick_pers)

    cat(file=stderr(), "in data_personality function",  "\n")
    cat(file=stderr(), "\t\t intermediate>",  input$store_pick_inter , "<\n")
    cat(file=stderr(), "\t\t store_personality >",   input$store_pick_pers , "<\n")
    #cat(file=stderr(), "\t\t dataframe (1,5) >",   dfResults[1,5], "\n")

    dfResults = as.data.frame(store_data %>% filter( personality_name == input$store_pick_pers))
  })
  
  
  
  data_personality_group <- reactive({
    req(data_personality())
    
    cat(file=stderr(), "in data_personality_group function \n")

    dfResultsPivot = data_personality()
    dfResultsPivot1 = dfResultsPivot %>% group_by(  type ,  major_hierarchy ,intermediate_hierarchy, minor_hierarchy,   personality_name, personality_strength, channel  )  %>%  summarise(count = n_distinct(tdlinx_id), catalinacount = n_distinct(site_key, na.rm = TRUE)-1)
    dfResultsPivot2 = dfResultsPivot1 %>%  pivot_wider(  names_from=c(channel) , values_from=c(count, catalinacount))
    dfResultsPivot2 = dfResultsPivot2[c(2,1,3),]
    names(dfResultsPivot2) = c('type',  'major_hierarchy',   'intermediate_hierarchy', 'minor_hierarchy', 'personality_name', 'personality_strength', 'US_drug', 'US_Grocery' ,'US_Supercenter', 'cat_Drug', 'cat_Grocery', 'cat_SuperCenter')
    as.data.frame(dfResultsPivot2)
    
    # dfResultsPivot <- data_personality() %>% 
    #   group_by(type, major_hierarchy, intermediate_hierarchy, minor_hierarchy, personality_name, personality_strength, channel) %>%
    #   summarise(count = n_distinct(tdlinx_id),
    #             catalinacount = n_distinct(site_key, na.rm = TRUE)-1) %>% 
    #   pivot_wider(names_from=c(channel) , values_from=c(count, catalinacount)) %>% 
    #   arrange(factor(personality_strength, levels = c("strong", "moderate", "weak")), desc(personality_strength))
    # names(dfResultsPivot) = c('type',  'major_hierarchy',   'intermediate_hierarchy', 'minor_hierarchy',
    #                           'personality_name', 'personality_strength', 'US_drug', 'US_Grocery' ,'US_Supercenter',
    #                           'cat_Drug', 'cat_Grocery', 'cat_SuperCenter')
    # as.data.frame(dfResultsPivot)
  })
  
  
  output$sp_table_title <- renderText({
    paste0( input$store_search_type," : ", input$store_pick_inter," : ", input$store_pick_pers )
    
  })
  
  ###this is the datatable at the top (summary) 
  output$sp_table_contents <- renderUI({
    req(data_personality())
  
    dfToplineSummary = data_personality()
    dfTotalUSummary = dfToplineSummary %>% summarise(count = n_distinct(tdlinx_id)) 
    dfToplineCatalinaSummary = dfToplineSummary %>% summarise(count = n_distinct(site_key)) 
    
     div(style = "line-height: 17px",
          shiny::HTML(paste0(
            # line 1
            "<b>Audience Description</b>: ", tolower(data_personality()$personality_desc[1]), "<br/>",
            # line 2: Shopper Cards
            "Number of stores in the Total US is ", prettyNum(dfTotalUSummary[1,1], big.mark = ','), " for the 52 weeks ending at ", update_date(), "<br>",
            # line 3: BV Household
            "Number of stores in the Catalina Network is ", prettyNum(dfToplineCatalinaSummary[1,1], big.mark = ','), " for the 52 weeks ending at ", update_date(), "<br>")))
     })
  
  # main table ----
  
  # ** personality Table -------
  
  # render table
  ###this is the main data.....
  #brandon
  output$sp_level_table <- renderDT({
      cat(file=stderr(), "in render table \n")
      cat(file=stderr(), "      store_pick_per ", input$store_pick_pers, "\n")
      #cat(file=stderr(), "      data_personality_group ", data_personality_group(), "\n")
      
      req(input$store_pick_pers, data_personality_group())
    
      # select only one retailers, keep all the columns
      cat(file=stderr(), "made it through to render table\n")
    
      ## setup the frame
      container_dt= htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(class = 'dt-center', rowspan = 2, colspan = 1, "Intermediate"),
            th(class = 'dt-center', rowspan = 2, colspan = 1, "Personality Name"),
            th(class = 'dt-center', rowspan = 2, colspan = 1, "Strength"),
            th(class = 'dt-center', rowspan = 1, colspan = 3, "Total US"),
            th(class = 'dt-center', rowspan = 1, colspan = 3, "Catalina Network")
          ),
          lapply(c('Grocery', 'Supercenter', 'Drug', 'Grocery', 'Supercenter', 'Drug'), th)
        )
      ))
      
      ## build table
      data_personality_group() %>%
        select( intermediate_hierarchy,   personality_name, personality_strength ,
                US_Grocery ,US_Supercenter,  US_drug,  cat_Grocery, cat_SuperCenter ,cat_Drug) %>% 
        DT::datatable(
          options = list(pageLength = 10, dom = "tip"),
          container = container_dt,
          rownames = FALSE
        )  %>%
        formatRound("US_Grocery", digits = 0) %>%
        formatRound("US_Supercenter", digits = 0) %>%
        formatRound("US_drug", digits = 0)  %>%
        formatRound("cat_Grocery", digits = 0) %>%
        formatRound("cat_SuperCenter", digits = 0) %>%
        formatRound("cat_Drug", digits = 0) 
  })
  

  
  #create the map
  output$mymap <- renderLeaflet({
    mapData = data_personality() %>%    
      select(tdlinx_id, site_key, channel, latitude,  longitude )  

    leaflet(mapData) %>% addTiles() %>%
      fitBounds(-124.023438,24.686952,-66.541406,47.980217)
  })
  
  
  observe({

    mapData = data_personality() %>% select(tdlinx_id, site_key, personality_strength, channel, latitude, longitude)  
    mapData$colors = 
    
    cat(file=stderr(), "in mappers \n")
    cat(file=stderr(), "     tdlinx " , mapData[1,1] ,  "   lat ", mapData[1,4], '  long ', mapData[1,5], '\n' )
    
    leaflet( mapData) %>% clearMarkers()
           
    optRestOfUS = "pick_others"  %in% input$mapNetworks
    optCatalina = "pick_cat"  %in% input$mapNetworks
    cat(file=stderr(), "      Netowrks ", input$mapNetworks, "\n")
    cat(file=stderr(), "      catalina ", optCatalina, "\n")
    cat(file=stderr(), "      all others ", optRestOfUS, "\n")
    
    # tempMapData = mapData[!is.na(mapData$tdlinx_id) & (mapData$personality_strength =="strong"),]
    # leafletProxy("mymap",  session=session,  data = tempMapData) %>% clearShapes()
    leafletProxy("mymap",  session=session) %>% clearMarkers()
    
    if (optRestOfUS == TRUE) {
      tempMapData = mapData[is.na(mapData$site_key) & (mapData$personality_strength =="strong"),]
      leafletProxy("mymap",  session=session,  data = tempMapData)   %>% addCircleMarkers(lat=~latitude, lng=~longitude, weight=1, radius=3, color= "#006400" )              
      }
    
    if (optCatalina == TRUE) {
      tempMapData = mapData[!is.na(mapData$site_key) & (mapData$personality_strength =="strong"),]
      leafletProxy("mymap",  session=session,  data = tempMapData)   %>%  addCircleMarkers(lat=~latitude, lng=~longitude, weight=1, radius=3, color= "#0000FF" )
      }
  })

  
  # help button ----
  observeEvent(input$help_sp, {
    showModal(modalDialog(
      title = "How to use this page",
      p("This is page is aiming to help the user get a clear view of a selected brand's corresponding attributes
        like ingredient personalities, demographic status, etc."
      ),
      
      br(),
      shiny::HTML("<b>Retailer \ Shopper Personality Availability</b>:"),
      br(),
      shiny::HTML("<b>Product Attribute</b>: (aka Label Insight personalities) are available for <u>all</u> retailers and all shopper cards."),
      br(),
      shiny::HTML("<b>Demographics</b>: (with the exception of Generations) are only available for ATD retailers via in-store or digitally if you need to intersect with the retailer's PBT audiences (you can do demo only digital targeting if you want)."),
      br(),
      shiny::HTML("<b>Marketing Responsiveness</b>: are available for <u>all</u> retailers."),
      br(),
      shiny::HTML("<b>Category Buyer</b>: are available for <u>all</u> retailers."),
      br(),
      shiny::HTML("<b>Visit</b>: are available for ATD retailers via in-store or digitally if you need to intersect with the retailer's PBT audiences (you can do demo only digital targeting if you want)."),
      br(),
      shiny::HTML("<b>Time \ Date Ranges</b>: are available for <u>all</u> retailers."),
      br(),br(),
      
      p(column(6,
               "Select a type of personality in step 1. Then pick a personality from the step 2 drop-down list."),
        column(6,
               img(src = 'help_sp_1.png'))
      ),
      br(),br(),
      p(column(6,
               "The result is an aggregation of all retailers.
        If the user wants to explore the data for specific retailers (one or more),
        please unclick [show result on all retailers] to select."),
        column(6,
               img(src = 'help_sp_2.png'))
      ),
      br(), br(),
      p("Download button will export two tables, one summary table about the selected personality and
         one ranking table show the top 100 brands having such personality. "
      ),
      br(),
      size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)"))
    )
  })
  
  # definition button  ----
  observeEvent(input$defs_sp, {
    showModal(modalDialog(title = "Definitions",
                          HTML(paste(
                            "<b>Shopper Cards</b>: individual shopper cards information <br>",
                            "<b>Household</b>: linked shopper ids to form a household <br><br>",
                            "<b>Brand Affinity</b>: Affinity Metric. <br>
      Shows how likely this group is to buy this brand + category. <br>
      Affinities > 1 show a positive relationship and < 1 show a negative relationship.<br><br>")),
                          easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
    ))
  })
  
  # download  ----
  # download button for product attributes
  #dt_download_pers_attr <- reactive({
  #  data_personality() %>% 
  #    arrange(lgl_entity_nm, personality_level)
  #})
  #output$download_pers <- downloadHandler(
  #  filename = function() {paste0(gsub(' ','-',tolower(input$store_pick_pers)),'-','product-attributes',".csv")},
  #  content = function(file) {
  #    # attribute summary table
  #    write.table('------------------', file, col.names = F, row.names = F, sep = ',', append = T)
  #    write.table('Personality Table', file, col.names = F, row.names = F, sep = ',', append = T)
  #    write.table('------------------', file, col.names = F, row.names = F, sep = ',', append = T)
  #    write.table(dt_download_pers_attr(), file, row.names = F, sep = ',', append = T)
  #    # brand affinity table
  #    write.table('------------------', file, col.names = F, row.names = F, sep = ',', append = T)
  #    write.table('Brand Affinities', file, col.names = F, row.names = F, sep = ',', append = T)
  #    write.table('------------------', file, col.names = F, row.names = F, sep = ',', append = T)
  #    write.table(affinity_data[shopper_personality == input$store_pick_pers][order(-brand_affinity)][1:min(100, length(brand_affinity))],
  #                file, row.names = F, sep = ',', append = T)
  #  }
  #)

  
  # query ----
  query_store <- reactive({
    
    
    
    # main frame query
    build_query <- function( pers_name){
      
      cat(file=stderr(), "in build query  \n") 
      
      q_frame <- "
     select type, major_hierarchy, intermediate_hierarchy, minor_hierarchy, 
          personality_name,
      		case when numeric_value2 >= 1.5   then 'strong'
      		 when numeric_value2 >= -1.5   then 'moderate'
      		else 'weak' 
      	end as personality_strength,
      	b.tdlinx_id_key as tdlinx_id, 
      	site_key,
      	channel,
              latitude, longitude
      from personality_dim_v a
      join store_personality_assn_v b
      	ON a.personality_key = b.personality_key
      	and active
      join (select distinct td_acct_nbr, site_key, channel, latitude_nbr as latitude, longitude_nbr as longitude from  tt_store_personality_stores_v3)  c
            	ON b.tdlinx_id_key  = c.td_acct_nbr	
      where dimension = 'Store'
      and personality_name = '{pers_name}'
    ;
    
    "
    glue(q_frame)
    }
    
    cat(file=stderr(), "calling build query  \n")
    cat(file=stderr(), "    personality picked "  , input$store_pick_pers,  "  \n") 
    build_query(pers_name = input$store_pick_pers)
  })
  
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
                  div(class = 'query', shiny::HTML(gsub("\\n", "<br>", glue(query_store())))),
                  size = 'l', easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
      ))
  })
  
  # availability ----
  observeEvent(input$get_usage, {
    showModal(
      modalDialog(title = "Availability Table",
                  br(),
                  # table
                  renderDT({
                    data_personality() %>% 
                      select(personality_name,
                             `Buyer Vision` = use_buyrvision, 
                             `Digital/TV` = use_tv,
                             `In Store` = use_in_store,
                             `Hub 360` = use_hub360,
                             `Programmatic` = use_syndicated_programmatic,
                             `Royalty Owed` = royalty_owed) %>%
                      distinct() %>% 
                      pivot_longer(cols = -personality_name) %>% 
                      mutate(value = ifelse(value == 1, "True", "False")) %>% 
                      DT::datatable(
                        options = list(pageLength = 10, dom = "", lengthChange = FALSE),
                        rownames = FALSE,
                        colnames = c("Personality", "Availability", "Status")) %>% 
                      formatStyle('value', color = styleEqual(c('True', "False"), c('green', 'red')))
                  }),
                  hr(),
                  downloadButton(session$ns('download_full_usage'),"Download Full Shopper Personalities List", class = "down"),
                  easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")  
      ))
  })
  
  dt_storedownload <- reactive({
    cat(file=stderr(), "in storedownload function ",  "\n")
    dt_fileoutput = data_personality() %>%  select( tdlinx_id, site_key, latitude, longitude, personality_level, personality_strength) 
    dt_fileoutput2 = merge(x=dt_fileoutput, y=tdlinx_data ) %>%  select(personality_level, personality_strength, tdlinx_id, site_key, store_name , store_number,        address1  ,           city, state,   zipcode,   channel, latitude, longitude)  %>% arrange(personality_strength, tdlinx_id)
    as.data.frame(dt_fileoutput2)
  })
  
  
  # download button for full shopper list
  output$download_pers <- downloadHandler(
    filename = function() {paste0(gsub(' ','_',tolower(input$store_pick_pers)),'_','store_personalities.csv')},
    content = function(file) {write.csv(dt_storedownload(), file, row.names = FALSE)} 
  )
}
######################## END

