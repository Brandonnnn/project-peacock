################################################################################.
# Code to create page Brand Affinity
# In this script include ui and server function to build the page
# Created: 2020-03-24 15:00
# Last Update: 2024-01-08
################################################################################.

################################.
# UI ------
brandAffinityPageUI <- function(id){
  ns <- NS(id)
  
  tagList(
    
    sidebarPanel(width = 3, #Filter options
       actionButton(ns("help_affi"), label="Help", icon= icon('question-circle'), class ="down"),
       actionButton(ns("defs_affi"),label="Definitions", icon= icon('info'), class ="down"),
       
      #search method selection
       div(title="Select your brand searching method, from manufacturer, category or directly serach brand", # tooltip
           style = "margin-top: 20px", # space to the action button
           awesomeRadio(ns("search_type"), label= "Step 1 - Select your brand searching logic",
                        choices = list('Brand' = 'brand', 'Category' = 'cate', "Manufacturer" = 'manu'),
                        selected = 'brand', inline = TRUE, checkbox = TRUE)),
       
      # brand search panel
       conditionalPanel(condition = "input.search_type == 'brand'", ns = ns,
                        div(title = "Select a brand to see affinity information. 
                            Click in this box,hit backspace and start to type if you want to quickly find a brand", # tooltip
                            style = "margin-top: 20px", # space above
                            selectInput(ns('pick_brand'), 
                                        shiny::HTML("<p>Step 2 - Select a brand <span style = 'font-weight:400'><br/> 
                                                    (hit backspace and start typing to search)</span></p>"),
                                        choices = unique(dt_brand_list[, .(brand_desc)][order(brand_desc)]),
                                        selected = 'PEPSI')),
                        uiOutput(ns("show_brand_manu")), #print out manufacturer
                        uiOutput(ns("show_brand_cate")) # select category
       ),
      
       # category search
       conditionalPanel(condition = "input.search_type == 'cate'", ns = ns,
                        div(title = "Select a Category to check all the belonging brands,hit backspace and 
                            start to type if you want to quickly find a category", # tooltip
                            style = "margin-top: 20px", # space above
                            selectInput(ns('pick_cate'), 
                                        shiny::HTML("<p>Step 2 - Select a category <span style = 'font-weight:400'><br/>
      (hit backspace and start typing to search)</span></p>"),
                                        choices = unique(dt_brand_list[, .(cat_desc)][order(cat_desc)]),
                                        selected = 'SOFT DRINK-COLA MISC, INC DIET')),
                        uiOutput(ns("show_cate_brand")), # select brand
                        uiOutput(ns("show_cate_manu")) #print out manufacturer
       ),
      
       # manufacturer search
       conditionalPanel(condition = "input.search_type == 'manu'", ns = ns,
                        div(title = "Select a manufacturer to check all the belonging brands", # tooltip
                            style = "margin-top: 20px", # space above
                            selectInput(ns('pick_manu'),
                                        shiny::HTML("<p>Step 2 - Select a manufacturer <span style = 'font-weight:400'><br/>
      (hit backspace and start typing to search)</span></p>"),
                                        choices = unique(dt_brand_list[, .(manufacturer)])[order(manufacturer)],
                                        selected = 'PEPSICO INC')),
                        uiOutput(ns("show_manu_brand")), #select brand
                        uiOutput(ns("show_manu_cate")) #select category
       ),
      
        # Personality Type selection
        div(style = "margin-top: 20px",
          awesomeCheckbox(ns("all_sp_types"), label = "Show result on all personality types", value = TRUE)
        ),
        uiOutput(ns("show_sp_types")),# select personality types
        
         # button
         div(style = "margin-top:20px", #space above
             downloadButton(outputId = ns('download_affinity'),"Download Data", class = "down"),
             actionButton(ns("get_query"), label = "SQL Queries", icon =icon("fas fa-database"), class = "down"))
    ), #end Sidebar Panel
    
    mainPanel(width = 9,
      # definition button
      bsModal(id = ns('mod_defs_affinity'), title = "Definitions", trigger = "defs_affi"),
      #Overview: brand selection
      div(class= "depr-text-box",
        div(class= "title", textOutput(ns("affinity_table_title"))),
        div(class= "content", htmlOutput(ns("affinity_table_box")))),
      #datatable
      div( #class="depr-text-box",
        div(style = "padding: 5px; padding-left: 10px;font-size: 90%; width: 95%",
          htmlOutput(ns("affi_table_affinity_title")),
          withSpinner(dataTableOutput(ns('show_table_affinity')))))
    ) #end Main Panel
  ) #end tagList
}

################################.
# Server ------
brandAffinityPage <- function(input, output, session){
  
  # input----
  
  # control fro brand: select a manufacturer
  output$show_brand_manu <- renderUI({
    lst_manu <- sort(unique(dt_brand_list[brand_desc == input$pick_brand, manufacturer]))
    div(
      style = "margin-top: 10px",
      awesomeRadio(session$ns("pick_brand_manu"), label = "Check the manufacturer", 
                   choices = lst_manu, selected = lst_manu[1], inline = TRUE, checkbox = TRUE)
    )
  })
  
  # control for brand. Dynamic selection of category depending on brand
  output$show_brand_cate <- renderUI({
    req(input$pick_brand_manu)
    lst_cate <- sort(unique(dt_brand_list[brand_desc == input$pick_brand & manufacturer == input$pick_brand_manu, cat_desc]))
    div(title = "Select a category to see affinity for the brand",
        style = "margin-top: 20px", # space above
        selectInput(session$ns("pick_brand_cate"),
                    label = "Step 3 - Select a category", 
                    choices = lst_cate, selected = "SOFT DRINK-COLA MISC, INC DIET")
    )
  })
  
  
  
  # control for category. Dynamic selection of brand depending on category
  output$show_cate_brand <- renderUI({
    lst_brand <- sort(unique(dt_brand_list[cat_desc == input$pick_cate, brand_desc]))
    div(title = "Select a brand from the given category",
        style = "margin-top: 20px",
        selectInput(session$ns("pick_cate_brand"),
                    label = shiny::HTML("<p>Step 3 - Select a brand <span style = 'font-weight:400'><br/>
                (hit backspace and start typing to search)</span></p>"),
                    choices = lst_brand, selected = "PEPSI")
    )
  })
  
  # control for category. check the manufacturer
  output$show_cate_manu <- renderUI({
    req(input$pick_cate_brand)
    lst_manu <- sort(unique(dt_brand_list[cat_desc == input$pick_cate & brand_desc == input$pick_cate_brand, manufacturer]))
    div(
      style = "margin-top: 10px",
      awesomeRadio(session$ns("pick_cate_manu"), label = "Check the manufacturer", 
                   choices = lst_manu, selected = lst_manu[1], inline = TRUE, checkbox = TRUE)
    )
  })
  
  # control for manufacturer. Dynamic selection of brand depending on manufacturer
  output$show_manu_brand <- renderUI({
    lst_brand <- sort(unique(dt_brand_list[manufacturer == input$pick_manu, brand_desc]))
    div(title = "Select a brand from the given manufacturer",
        style = "margin-top: 20px",
        selectInput(session$ns("pick_manu_brand"),
                    label = shiny::HTML("<p>Step 3 - Select a brand <span style = 'font-weight:400'><br/>
                (hit backspace and start typing to search)</span></p>"),
                    choices = lst_brand, selected = "PEPSI")
    )
  })
  
  # control for manufacturer. check the category
  output$show_manu_cate <- renderUI({
    req(input$pick_manu_brand)
    lst_cate <- sort(unique(dt_brand_list[brand_desc == input$pick_manu_brand & manufacturer == input$pick_manu, cat_desc]))
    div(
      style = "margin-top: 10px",
      awesomeRadio(session$ns("pick_manu_cate"), label = "Check the category", 
                   choices = lst_cate, selected = lst_cate[1], inline = FALSE, checkbox = TRUE)
    )
  })
  
  # define input for data table
  d_manu <- reactive({
    if(input$search_type == 'brand') {
      input$pick_brand_manu
    } else if (input$search_type == 'cate'){
      input$pick_cate_manu
    } else if (input$search_type == 'manu'){
      input$pick_manu
    }
  })
  
  d_cate <- reactive({
    if(input$search_type == 'brand') {
      input$pick_brand_cate
    } else if (input$search_type == 'cate'){
      input$pick_cate
    } else if (input$search_type == 'manu'){
      input$pick_manu_cate
    }
  })
  
  d_brand <- reactive({
    if(input$search_type == 'brand') {
      input$pick_brand
    } else if (input$search_type == 'cate'){
      input$pick_cate_brand
    } else if (input$search_type == 'manu'){
      input$pick_manu_brand
    }
  })

  # pick personality types
  output$show_sp_types <- renderUI({
    req(!input$all_sp_types, d_manu(), d_cate(), d_brand())
    lst_types <- sort(unique(affinity_data[manufacturer == d_manu() & cat_desc == d_cate() & brand_desc == d_brand()]$type))
    div(title = "Select one or more retailers",
      selectizeInput(session$ns("select_sp_types"),
        label = shiny::HTML("<p>Step 4 - Select personality type(s)</p>"),
        choices = c("Select personality type(s)" = "", lst_types),
        multiple = TRUE, selected = ""))
  })
  
  d_sp_types <- reactive({
    all_types <- sort(unique(affinity_data$type))
    if(input$all_sp_types == TRUE | is.null(input$select_sp_types)){
      all_types
    } else {
      input$select_sp_types
    }
  })
  
  # page summary ----
  
  # data for brand affinity
  data_brand_affinity <- reactive({
    req(d_manu(), d_cate(), d_brand())
    affinity_data[manufacturer == d_manu() & cat_desc == d_cate() & brand_desc == d_brand() & type %in% d_sp_types(), 
                  ][order(-brand_affinity)]
  })

  # Summary box
  output$affinity_table_title <- renderText(
    paste0(d_manu(), " : ", d_brand())
  )
  
  output$affinity_table_box <- renderUI({
    req(data_brand_affinity()$t_spend)
    update_date <- dt_date[table_nm == "brand_affinity_ingredient_table"]$date_end
    t_spend <- max(data_brand_affinity()$t_spend)
    t_cards <- max(data_brand_affinity()$t_shoppers)
    div(style = "line-height: 17px",
        shiny::HTML(paste0(
          "The total brand sale in the category is $", prettyNum(t_spend, big.mark = ","),
          " for the 52 weeks ending at ", update_date, "<br>",
          "The total number of shopper cards in the category is ", prettyNum(t_cards, big.mark = ","),
          " for the 52 weeks ending at ",update_date, "<br>",
          "Brand affinities <b>only</b> use the <b>Very Strong</b> personality tier"))
    )
  })

  
  # main table ----
  
  # render table for brand affinity
  output$show_table_affinity <- renderDT({
    dt <- data_brand_affinity() %>%
      select(brand_desc, shopper_personality, type, brand_affinity, brand_spend,
             category_not_brand_shoppers, category_affinity, opportunity) %>%
      setNames(c("Brand", "Shopper Personality", "Personality Type","Brand Affinity", "Brand Spend",
        "Category not Brand Shopper Cards", "Category Affinity", "5% Opportunity"))
    dt %>%
      DT::datatable(
        options = list(
          pageLength = 12, dom = "tip", autoWidth = F,
          columnDefs = (list(list(width = '15%', targets =c(0))))
        ),
        selection = list(mode = "single"), rownames = F) %>%
      formatRound('Brand Affinity', digits = 2) %>%
      formatRound("Category Affinity", digits = 2) %>%
      formatCurrency("Brand Spend", digits = 0) %>%
      formatRound("Category not Brand Shopper Cards", digits = 0) %>%
      formatCurrency("5% Opportunity", digits = 0) %>%
      formatStyle('Brand Affinity', color = styleInterval(c(0.8, 2), c('red', 'goldenrod', 'green'))) %>%
      formatStyle('Category Affinity', color = styleInterval(c(0.8, 2), c('red', 'goldenrod', 'green')))
  })
  
  # button ----
  
  # help button
  observeEvent(input$help_affi, {
    showModal(modalDialog(
      title = "How to use this page",
      
      br(),
      shiny::HTML("<b>Retailer \ Shopper Personality Availability</b>:"),
      br(),
      shiny::HTML("<b>Product Attribute</b>: (aka Label Insight personalities) are available for <u>all</u> 
                  retailers and all shopper cards."),
      br(),
      shiny::HTML("<b>Demographics</b>: (with the exception of Generations) are only available for ATD retailers via 
                  in-store or digitally if you need to intersect with the retailer's PBT audiences 
                  (you can do demo only digital targeting if you want)."),
      br(),
      shiny::HTML("<b>Marketing Responsiveness</b>: are available for <u>all</u> retailers."),
      br(),
      shiny::HTML("<b>Category Buyer</b>: are available for <u>all</u> retailers."),
      br(),
      shiny::HTML("<b>Visit</b>: are available for ATD retailers via in-store or digitally if you need to
                  intersect with the retailer's PBT audiences (you can do demo only digital targeting if you want)."),
      br(),
      shiny::HTML("<b>Time \ Date Ranges</b>: are available for <u>all</u> retailers."),
      br(),br(),
      
      p(column(6,
               HTML("Select a <b>brand searching logic</b> in step 1 to decide the way to find the brand. <br><br>
          For <b>Brand</b>, please search the brand name in step 2 directly and then double-check the manufacturer. <br><br>
          For <b>Category</b>, please select the category you want to explore in step 2. <br>
          All the brands belong to the selected category will be available in step 3 search bar. <br><br>
          For <b>Manufacturer</b>, please select a manufacturer then pick one of the brands. <br>")),
        column(6,
               img(src = 'help_affi_1.png', height=70),
               HTML("<br><br><br><br><br><br><br><br><br><br>"))),
      
      br(), 
      
      size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)"))
    )}
  )
  
  # definition button
  observeEvent(input$defs_affi, {
    showModal(
      modalDialog(title = "Definitions",
                  br(),
                  HTML(paste("
        <b>Brand Affinity</b>: Affinity Metric. <br>
        Shows how likely this group is to buy this brand + category. <br>
        Affinities > 1 show a positive relationship and < 1 show a negative relationship.<br><br>",
                             "<b>Brand Spend</b>: Amount of spend for this category and brand by the shopper profile. <br><br>",
                             "<b>Category not Brand Shoppers</b>: Number of shoppers that are members of the shopper 
        personality that buy the category but not the brand. <br><br>",
                             "<b>Category Affinity</b>: Affinity Metric. <br>
        Shows how likely this group is to buy this category. <br><br>",
                             "<b>5% Opportunity</b>: Opportunity if converting 5% of the shopper personality category 
        buyers from non-brand buyers to buyers. <br>")),
                  br(),
                  size = 'l', easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
      ))
  })
  
  output$defs_text_affinity <- renderUI({
    
  })
  
  # download button
  dt_download_affinity <- reactive({
    data_brand_affinity() %>%
      select(manufacturer, cat_desc, brand_desc, shopper_personality, type, brand_affinity, brand_spend,
             category_not_brand_shoppers, category_affinity, category_spend, opportunity, t_spend, t_shoppers) %>% 
      setNames(c("Manufacturer", "Category","Brand", "Shopper Personality","Personality Type", "Brand Affinity", "Brand Spend",
                 "Category not Brand Shoppers", "Category Affinity", "Category Spend","5% Opportunity", "Brand Total Spend",
                 "Brand Total Shopper Cards"))
  })
  output$download_affinity <- downloadHandler(
    filename = function() { paste0(gsub(' ','-',tolower(d_brand())),'-','brand-affinities',".csv")},
    content = function(file) {write.csv(dt_download_affinity(), file, row.names = FALSE)}
  )
  
  # query button
  query_affinity <- reactive({
    q_affinty <- "
    select bnd.personality_name as shopper_personality
    	,per.type
    	,bnd.mfg_parent_corp_nm as manufacturer
    	,bnd.cat_desc
    	,bnd.brand_desc 
    	,round(bnd.spend) as brand_spend
    	,bnd.affinity as brand_affinity
    	,round(cat.spend) as category_spend
    	,cat.affinity as category_affinity
    	,cat.shoppers - bnd.shoppers as category_not_brand_shoppers
    	,round(((cat.shoppers::float - bnd.shoppers::float) * 0.05) * (bnd.spend::float / bnd.shoppers::float)) as opportunity
    	,round(tot.spend) as t_spend
    	,tot.shoppers as t_shoppers
    from affinity_brand_results_v2 as bnd
    join affinity_category_results_v2 as cat
    	on bnd.universe  = cat.universe 
    	and bnd.cat_nbr = cat.cat_nbr 
    	and bnd.personality_key = cat.personality_key
    join affinity_brand_totals_v2 as tot
    	on bnd.mfg_parent_corp_nm = tot.mfg_parent_corp_nm
    	and bnd.brand_key = tot.brand_key
    	and bnd.cat_nbr = tot.cat_nbr 
    	and bnd.universe = tot.universe
    join personality_dim_v as per
    	on bnd.personality_key = per.personality_key 
    where per.dimension = 'Shopper'
     	and bnd.hierarchy_type = 'C'
    	and bnd.universe like 'catalina_ntwk%'
    	and bnd.universe not like 'catalina_ntwk demo%'
    	and manufacturer = '{d_manu()}'
    	and bnd.cat_desc = '{d_cate()}'
      and bnd.brand_desc = '{d_brand()}'
    order by manufacturer, brand_desc, bnd.cat_desc, bnd.affinity desc
    distribute on (type)
    "
    glue(q_affinty)
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
        div(class = 'query',
          shiny::HTML( gsub("\\n", "<br>", glue(query_affinity())))),
        size = 'l', easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")

      ))
  })
  
  # tracking system -----
  # added 24/01/09
  
  
  observeEvent(ignoreInit = TRUE,
               input$pick_brand_cate,
               {
                 req(isTruthy(data_brand_affinity()))
                 # check default value
                 is_default <- if_else(d_brand() == 'PEPSI'
                                       & d_cate() == "SOFT DRINK-COLA MISC, INC DIET"
                                       & d_manu() == "PEPSICO INC",
                                       TRUE, FALSE)
                 # track selection
                 log_track_selection(
                   pageTab = "Affinities",
                   value_lst = list(d_manu(), d_cate(), d_brand()),
                   is_default)
               })
  
  observeEvent(ignoreInit = TRUE,
               input$pick_cate_manu,
               {
                 req(isTruthy(data_brand_affinity()))
                 # check default value
                 is_default <- if_else(d_brand() == 'PEPSI'
                                       & d_cate() == "SOFT DRINK-COLA MISC, INC DIET"
                                       & d_manu() == "PEPSICO INC",
                                       TRUE, FALSE)
                 # track selection
                 log_track_selection(
                   pageTab = "Affinities",
                   value_lst = list(d_manu(), d_cate(), d_brand()),
                   is_default)
               })
  
  observeEvent(ignoreInit = TRUE,
               input$pick_manu_cate,
               {
                 req(isTruthy(data_brand_affinity()))
                 # check default value
                 is_default <- if_else(d_brand() == 'PEPSI'
                                       & d_cate() == "SOFT DRINK-COLA MISC, INC DIET"
                                       & d_manu() == "PEPSICO INC",
                                       TRUE, FALSE)
                 # track selection
                 log_track_selection(
                   pageTab = "Affinities",
                   value_lst = list(d_manu(), d_cate(), d_brand()),
                   is_default)
               })
  # end updates
  
} #end server
################### END
