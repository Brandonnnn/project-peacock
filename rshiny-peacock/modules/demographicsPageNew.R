###############################################################################.
# Code to create page Demographic Insights (new)
# In this script include ui and server function to build the page
# Updated: 2020-04-06 17:00
################################################################################.

################################.
# UI ------
demographicsPageUI <- function(id){
  ns <- NS(id)
  
  tagList(
    sidebarPanel(width = 3, #Filter options
                 actionButton(ns("help_btn"), label="Help", icon= icon('question-circle'), class ="down"),
                 actionButton(ns("defs_btn"),label="Definitions", icon= icon('info'), class ="down"),
                 #search method selection
                 div(title="Select your brand searching method, from manufacturer, category or directly serach brand", # tooltip
                     style = "margin-top: 20px", # space to the action button
                     awesomeRadio(ns("search_type"), label= "Step 1 - Select your brand searching logic",
                                  choices = list('Brand' = 'brand', 'Category' = 'cate', "Manufacturer" = 'manu'),
                                  selected = 'brand', inline = TRUE, checkbox = TRUE)),
                 
                 # brand search panel
                 conditionalPanel(condition = "input.search_type == 'brand'", ns = ns,
                                  div(title = "Select a brand to see demographic information. 
            Click in this box,hit backspace and start to type if you want to quickly find a brand", # tooltip
                                      style = "margin-top: 20px", # space above
                                      selectInput(ns('pick_brand'), 
                                                  shiny::HTML("<p>Step 2 - Select a brand <span style = 'font-weight:400'><br/>
                (hit backspace and start typing to search)</span></p>"),
                                                  choices = unique(demo_brand_list[, .(brand_desc)])[order(brand_desc)],
                                                  selected = 'PEPSI')),
                                  uiOutput(ns("show_brand_manu")), #print out manufacturer
                                  uiOutput(ns("show_brand_cate")) # select category
                 ),
                 
                 # category search
                 conditionalPanel(condition = "input.search_type == 'cate'", ns = ns,
                                  div(title = "Select a Category to check all the belonging brands,hit backspace and start to type
          if you want to quickly find a category", # tooltip
                                      style = "margin-top: 20px", # space above
                                      selectInput(ns('pick_cate'), 
                                                  shiny::HTML("<p>Step 2 - Select a category <span style = 'font-weight:400'><br/>
                (hit backspace and start typing to search)</span></p>"),
                                                  choices = unique(demo_brand_list[, .(cat_desc)][order(cat_desc)]),
                                                  selected = 'SOFT DRINKS - COLA REGULAR')),
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
                                                  choices = unique(demo_brand_list[, .(manufacturer)])[order(manufacturer)],
                                                  selected = 'PEPSICO INC')),
                                  uiOutput(ns("show_manu_brand")), #select brand
                                  uiOutput(ns("show_manu_cate")) #select category
                 ),
                 
                 # select features
                 div(title = "Show result on all demographic features or selected features",
                     style = "margin-top: 10px",
                     awesomeCheckbox(ns("all_features_pick"), label = "Show all demographics", value = TRUE)),
                 uiOutput(ns("show_features")),
                 # button
                 div(style = "margin-top:20px", #space above
                     downloadButton(outputId = ns('download_btn'),"Download Data", class = "down"))
    ), #end Sidebar Panel
    
    # mainPanel -----
    mainPanel(width = 9,
              # definition button
              bsModal(id = ns('mod_defs_demo'), title = "Definitions", trigger = "defs_btn"),
              #Overview: brand selection
              div(class= "depr-text-box",
                  div(class= "title", textOutput(ns("page_summary_title"))),
                  div(class= "content", htmlOutput(ns("page_summmary_box")))),
              # demo detail grid
              div(style = "padding-left: 10px; padding-right: 10px",
                  uiOutput(ns("ui_demo_grids"))
              )
    ) #end Main Panel
  ) #end tagList
}


################################.
# Server ------
demographicsPage <- function(input, output, session){
  
  # input -----
  
  # control fro brand: select a manufacturer
  output$show_brand_manu <- renderUI({
    lst_manu <- sort(unique(demo_brand_list[brand_desc == input$pick_brand, manufacturer]))
    div(
      style = "margin-top: 10px",
      awesomeRadio(session$ns("pick_brand_manu"), label = "Check the manufacturer", 
                   choices = lst_manu, selected = lst_manu[1], inline = TRUE, checkbox = TRUE)
    )
  })
  
  # control for brand. Dynamic selection of category depending on brand
  output$show_brand_cate <- renderUI({
    req(input$pick_brand_manu)
    lst_cate <- sort(unique(demo_brand_list[brand_desc == input$pick_brand & manufacturer == input$pick_brand_manu, cat_desc]))
    div(title = "Select a category to see affinity for the brand",
        style = "margin-top: 20px", # space above
        selectInput(session$ns("pick_brand_cate"),
                    label = "Step 3 - Select a category", 
                    choices = lst_cate, selected = "SOFT DRINKS - COLA REGULAR")
    )
  })
  
  # control for category. Dynamic selection of brand depending on category
  output$show_cate_brand <- renderUI({
    lst_brand <- sort(unique(demo_brand_list[cat_desc == input$pick_cate, brand_desc]))
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
    lst_manu <- sort(unique(demo_brand_list[cat_desc == input$pick_cate & brand_desc == input$pick_cate_brand, manufacturer]))
    div(
      style = "margin-top: 10px",
      awesomeRadio(session$ns("pick_cate_manu"), label = "Check the manufacturer", 
                   choices = lst_manu, selected = lst_manu[1], inline = TRUE, checkbox = TRUE)
    )
  })
  
  # control for manufacturer. Dynamic selection of brand depending on manufacturer
  output$show_manu_brand <- renderUI({
    lst_brand <- sort(unique(demo_brand_list[manufacturer == input$pick_manu, brand_desc]))
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
    lst_cate <- sort(unique(demo_brand_list[brand_desc == input$pick_manu_brand & manufacturer == input$pick_manu, cat_desc]))
    div(
      style = "margin-top: 10px",
      awesomeRadio(session$ns("pick_manu_cate"), label = "Check the category", 
                   choices = lst_cate, selected = lst_cate[1], inline = FALSE, checkbox = TRUE)
    )
  })
  
  # control to select demo features
  output$show_features <- renderUI({
    req(!input$all_features_pick)
    lst_pers <- sort(unique(demo_data[brand_desc == 'PEPSI', personality_name]))
    # put a star in front of some features since it will be overlapping
    star_pers <- lst_pers %in% c("Adult Age", "Child Age", "Education", "Occupation", "Occupation Group", "Ethnicity", "Ethnic Assimilation")
    lst_pers[star_pers] <- paste0(lst_pers[star_pers], '*')
    div(title = "Select one or more features",
        selectizeInput(session$ns("pick_features"),
                       label = shiny::HTML("<p>Step 3b - Select one or more features 
                <span style = 'font-weight:400'><br/>(You can select mutiple features)</span></p>"),
                       choices = c("Select features" = "", paste(lst_pers)),
                       multiple = TRUE, selected = ""))
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
  
  # page summary  -------
  update_date <- dt_date[table_nm == "brand_demographic_table"]$date_end
  
  output$page_summary_title <- renderText(
    paste0(d_manu(), " :  ", d_brand())
  )
  
  output$page_summmary_box <- renderUI({
    div(style = "line-height: 17px", #line space
        shiny::HTML(paste0(
          "Demographic shoppers no longer include a static filter. <br>",
          "The numbers below are <b>ALL</b> matched shopper cards from Experian that purchase a brand updated at ",update_date, "<br>"),
          "Note: <i><b>*</b> Designates the attribute can be shared with more than one household member.  
        Due to this, percentages will exceed 100%. </i><br>")
    )
  })
  
  # demo tables -------
  
  # data for brand index
  data_brand_table <- reactive({
    req(d_manu(), d_cate(), d_brand())
    dt_brand <- demo_data[manufacturer == d_manu() & cat_desc == d_cate() & brand_desc == d_brand(),]
    dt_brand %>% 
      select(personality_name,
             personality_level,
             brand_index = shopper_penetration_index, 
             shoppers_count = raw_count_shoppers,
             brand_spend = raw_count_spend,
             percent_shoppers = percent_shoppers,
             percent_spend = percent_spend)
  })
  
  # function to render demo tables
  render_demo_table <- function(prsnly_name, dt_dom = ""){
    if(prsnly_name %in% c("Adult Age", "Adult Gender", "Child Age", "Education",
                          "Occupation",  "Occupation Group", "Ethnicity", "Ethnic Assimilation")){
      prsnly_name <- paste0(prsnly_name, "*")
    }
    return(renderDT({
      data_brand_table() %>%
        filter(personality_name == gsub("\\*", "", prsnly_name)) %>%
        arrange(personality_level) %>%
        select(personality_level, brand_index, shoppers_count, percent_shoppers, brand_spend, percent_spend) %>%
        DT::datatable(
          options = list(pageLength = 15, dom = dt_dom, ordering=F, 
                         initComplete = JS("function(settings, json) 
              {$(this.api().table().header()).css({'background-color':'#051e2c', 'color':'white'});}"), # formulate table header
                         autoWidth = FALSE,
                         columnDefs = (list(
                           list(width = '20%', targets =c(0, 1)),
                           list(width = '20%', targets =c(2, 4)),
                           list(width = '10%', targets =c(3, 5))))
          ),# end option
          rownames = FALSE,
          colnames = c(prsnly_name, "Shopper Penetration Index", "Number of Shopper Cards", "%Cards", "Total Brand Spend", "%Spend"),
          class = "display compact"
        ) %>%
        formatRound("brand_index", digits = 0) %>%
        formatRound("shoppers_count", digits = 0) %>%
        formatPercentage("percent_shoppers", digits = 1) %>%
        formatCurrency("brand_spend", digits = 0, currency = '$', before = TRUE) %>%
        formatPercentage("percent_spend", digits = 1) %>%
        formatStyle('brand_index',
                    color = styleInterval(c(80, 119.999), c('red', 'black', 'green'))) # right-closed interval
    }))
  }
  
  # output name need to be the lower case of the personality naem with connected with _
  output$t_adult_age <- render_demo_table("Adult Age")
  output$t_adult_gender <- render_demo_table("Adult Gender")
  output$t_child_age <- render_demo_table("Child Age")
  output$t_children <- render_demo_table("Children")
  output$t_education <- render_demo_table("Education")
  output$t_ethnic_assimilation <- render_demo_table("Ethnic Assimilation")
  output$t_ethnicity <- render_demo_table("Ethnicity")
  output$t_generation <- render_demo_table("Generation")
  output$t_household_size <- render_demo_table("Household Size")
  output$t_income <- render_demo_table("Income")
  output$t_language <- render_demo_table("Language")
  output$t_occupation <- render_demo_table("Occupation", dt_dom = "tip")
  output$t_occupation_group <- render_demo_table("Occupation Group")
  output$t_homeownership <- render_demo_table("Homeownership") # added at 12/19/2021
  
  # ui_table_grid
  output$ui_demo_grids <- renderUI({
    if(input$all_features_pick == TRUE){
      lst_pers <- sort(unique(demo_data[brand_desc == 'PEPSI', personality_name]))
      # put a star in front of some features since it will be overlapping
      star_pers <- lst_pers %in% c("Adult Age", "Adult Gender", "Child Age", "Education",
                                   "Occupation", "Occupation Group", "Ethnicity", "Ethnic Assimilation")
      lst_pers[star_pers] <- paste0(lst_pers[star_pers], '*')
      lapply(seq_along(lst_pers), function(i){
        div(style = 'padding-left: 5px; padding-right: 5px',
            fluidRow(div(class = "demo-table-box", withSpinner(DT::dataTableOutput(
              session$ns(paste0("t_", gsub(" ", "_", tolower(gsub("\\*", "", lst_pers[i]))))))))))
      })
    } else if(input$all_features_pick == FALSE){
      lapply(seq_along(input$pick_features), function(i){
        div(style = 'padding-left: 5px; padding-right: 5px',
            fluidRow(div(class = "demo-table-box", withSpinner(DT::dataTableOutput(
              session$ns(paste0("t_", gsub(" ", "_", tolower(gsub("\\*", "", input$pick_features[i]))))))))))
      })
    }
  })
  
  
  ##########################################.
  # button ----
  
  # help button
  observeEvent(input$help_btn, {
    showModal(modalDialog(
      title = "How to use this page",
      p("This page is trying to present detailed information about demographic attributes."
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
               HTML("Select a <b>brand searching logic</b> in step 1 to decide the way to find the brand. <br><br>
          For <b>Brand</b>, please search the brand name in step 2 directly and then double-check the manufacturer. <br><br>
          For <b>Category</b>, please select the category you want to explore in step 2. <br>
          All the brands belong to the selected category will be available in step 3 search bar. <br><br>
          For <b>Manufacturer</b>, please select a manufacturer then pick one of the brands. <br>")),
        column(6,
               img(src = 'help_demo_1.png', height=70),
               HTML("<br><br><br><br><br><br><br><br><br><br>"))),
      
      size = 'l', easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)"))
    )
  })
  
  # definition button
  observeEvent(input$defs_btn, {
    showModal(modalDialog(title = "Definitions",
                          div(style = "line-height: 20px", #line space
                              HTML(paste(
                                "<b>Note</b>: The numbers here are <b>ALL</b> matched shopper cards from Experian that purchase a brand. <br>",
                                "<b>Note</b>: Overlapping may occur for some categories. <br>",
                                "<b>Shopper Cards</b>: individual shopper card information. <br>",
                                "<b>Shopper Penetration Index</b>: Shows how likely this group is to buy this brand + category. <br>",
                                "<b>Brand Spend</b>: Amount of spend for this category and brand by the shopper profile. <br><br>"))),
                          easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
    ))
  })
  
  dt_download <- reactive({
    demo_data %>% 
      filter(manufacturer == d_manu(),
             cat_desc == d_cate(),
             brand_desc == d_brand()) %>% 
      select(manufacturer, cat_desc, brand_desc,
             demographics = personality_name,
             demographic_levels = personality_level,
             brand_index = shopper_penetration_index, 
             shoppers_count = raw_count_shoppers,
             percent_shoppers = percent_shoppers, 
             brand_spend = raw_count_spend,
             percent_spend = percent_spend)
  })
  
  output$download_btn <- downloadHandler(
    filename = function() {paste0(gsub(' ','_',tolower(d_brand())),'_','demographics.csv')},
    content = function(file) {write.csv(dt_download(), file, row.names = FALSE)}
  )
} #end server function


########### END
