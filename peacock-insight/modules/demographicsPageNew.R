###############################################################################.
# Code to create page Demographic Insights (new)
# In this script include ui and server function to build the page
# Updated: 2025-10-15
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
                 div(title="Choose how to search or match brands", # tooltip
                     style = "margin-top: 20px", # space to the action button
                     # awesomeRadio(ns("search_type"), label= "Step 1 - Search Strategy",
                     #              choices = list('Brand' = 'brand', 'Category' = 'cate', "Manufacturer" = 'manu'),
                     #              selected = 'brand', inline = TRUE, checkbox = TRUE),
                     selectInput(ns("search_type"), label= "Step 1 - Search Strategy",
                                  choices = list('Brand' = 'brand', 'Category' = 'cate', "Manufacturer" = 'manu'),
                                  selected = 'brand')
                     ),
                 
                 # brand search panel
                 conditionalPanel(condition = "input.search_type == 'brand'", ns = ns,
                                  div(title = "Select a brand to see demographic information. 
            Click in this box,hit backspace and start to type if you want to quickly find a brand", # tooltip
                                      style = "margin-top: 20px", # space above
                                      selectInput(ns('pick_brand'), 
                                                  shiny::HTML("<p>Step 2 - Brand Identification <span style = 'font-weight:400'><br/>
                (hit backspace and start typing to search)</span></p>"),
                                                  choices = unique(demo_brand_list[, .(brand_desc)])[order(brand_desc)],
                                                  selected = 'brand_001')),
                                  uiOutput(ns("show_brand_manu")), #print out manufacturer
                                  uiOutput(ns("show_brand_cate")) # select category
                 ),
                 
                 # category search
                 conditionalPanel(condition = "input.search_type == 'cate'", ns = ns,
                                  div(title = "Select a Category to check all the belonging brands,hit backspace and start to type
          if you want to quickly find a category", # tooltip
                                      style = "margin-top: 20px", # space above
                                      selectInput(ns('pick_cate'), 
                                                  shiny::HTML("<p>Step 2 - Product Category Identification <span style = 'font-weight:400'><br/>
                (hit backspace and start typing to search)</span></p>"),
                                                  choices = unique(demo_brand_list[, .(cat_desc)][order(cat_desc)]))),
                                  uiOutput(ns("show_cate_brand")), # select brand
                                  uiOutput(ns("show_cate_manu")) #print out manufacturer
                 ),
                 
                 # manufacturer search
                 conditionalPanel(condition = "input.search_type == 'manu'", ns = ns,
                                  div(title = "Select a manufacturer to check all the belonging brands", # tooltip
                                      style = "margin-top: 20px", # space above
                                      selectInput(ns('pick_manu'),
                                                  shiny::HTML("<p>Step 2 - Producer Identification <span style = 'font-weight:400'><br/>
                (hit backspace and start typing to search)</span></p>"),
                                                  choices = unique(demo_brand_list[, .(manufacturer)])[order(manufacturer)],
                                                  selected = 'manufacturer_001')),
                                  uiOutput(ns("show_manu_brand")), #select brand
                                  uiOutput(ns("show_manu_cate")) #select category
                 ),
                 
                 # select features
                 div(title = "Show result on all demographic features or selected features",
                     style = "margin-top: 10px",
                     awesomeCheckbox(ns("all_features_pick"), label = "Display all demographics", value = TRUE)),
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
      awesomeRadio(session$ns("pick_brand_manu"), label = "Step 2b - Producer Validation",
                   choices = lst_manu, selected = lst_manu[1], inline = TRUE, checkbox = TRUE)
      # selectInput(session$ns("pick_brand_manu"), label = "Producer Validation",
      #             choices = lst_manu, selected = lst_manu[1], multiple = FALSE)
    )
  })
  
  # control for brand. Dynamic selection of category depending on brand
  output$show_brand_cate <- renderUI({
    req(input$pick_brand_manu)
    lst_cate <- sort(unique(demo_brand_list[brand_desc == input$pick_brand & manufacturer == input$pick_brand_manu, cat_desc]))
    div(title = "Select a category to see affinity for the brand",
        style = "margin-top: 20px", # space above
        selectInput(session$ns("pick_brand_cate"),
                    label = "Step 3 - Product Category", 
                    choices = lst_cate, selected = lst_cate[1])
    )
  })
  
  # control for category. Dynamic selection of brand depending on category
  output$show_cate_brand <- renderUI({
    lst_brand <- sort(unique(demo_brand_list[cat_desc == input$pick_cate, brand_desc]))
    div(title = "Select a brand from the given category",
        style = "margin-top: 20px",
        selectInput(session$ns("pick_cate_brand"),
                    label = shiny::HTML("<p>Step 3 - Product Brand <span style = 'font-weight:400'><br/>
                (hit backspace and start typing to search)</span></p>"),
                    choices = lst_brand, selected = "brand_001")
    )
  })
  
  # control for category. check the manufacturer
  output$show_cate_manu <- renderUI({
    req(input$pick_cate_brand)
    lst_manu <- sort(unique(demo_brand_list[cat_desc == input$pick_cate & brand_desc == input$pick_cate_brand, manufacturer]))
    div(
      style = "margin-top: 10px",
      awesomeRadio(session$ns("pick_cate_manu"), label = "Step 3b - Producer Validation", 
                   choices = lst_manu, selected = lst_manu[1], inline = TRUE, checkbox = TRUE)
    )
  })
  
  # control for manufacturer. Dynamic selection of brand depending on manufacturer
  output$show_manu_brand <- renderUI({
    lst_brand <- sort(unique(demo_brand_list[manufacturer == input$pick_manu, brand_desc]))
    div(title = "Select a brand from the given manufacturer",
        style = "margin-top: 20px",
        selectInput(session$ns("pick_manu_brand"),
                    label = shiny::HTML("<p>Step 3 - Product Brand <span style = 'font-weight:400'><br/>
                (hit backspace and start typing to search)</span></p>"),
                    choices = lst_brand, selected = "brand_001")
    )
  })
  
  # control for manufacturer. check the category
  output$show_manu_cate <- renderUI({
    req(input$pick_manu_brand)
    lst_cate <- sort(unique(demo_brand_list[brand_desc == input$pick_manu_brand & manufacturer == input$pick_manu, cat_desc]))
    div(
      style = "margin-top: 10px",
      selectInput(session$ns("pick_manu_cate"), label = "Step 4 - Product Category", 
                   choices = lst_cate, selected = lst_cate[1], multiple = FALSE)
    )
  })
  
  # control to select demo features
  output$show_features <- renderUI({
    req(!input$all_features_pick)
    lst_pers <- sort(unique(demo_data[brand_desc == 'brand_001', personality_name]))
    # put a star in front of some features since it will be overlapping
    star_pers <- lst_pers %in% c("Adult Age Group", "Child Age", "Education", "Occupation", "Occupation Category", "Ethnicity")
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
    paste(d_manu(), d_cate(),  d_brand(), sep = " > ")
  )
  
  output$page_summmary_box <- renderUI({
    div(style = "line-height: 17px", #line space
        shiny::HTML(paste0(
          "Note: <i><b>*</b> Designates the attribute can be shared with more than one household member.Due to this, percentages will exceed 100%. </i><br>",
          "<b>Note</b>: The data displayed in this app is <b>synthetic</b> and for <b>demonstration purposes</b> only."))
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
    if(prsnly_name %in% c("Adult Age Group", "Child Age", "Education", "Occupation", "Occupation Category", "Ethnicity")){
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
              {$(this.api().table().header()).css({'background-color':'#41597D', 'color':'white'});}"), # formulate table header
                         autoWidth = FALSE,
                         columnDefs = (list(
                           list(width = '20%', targets =c(0, 1)),
                           list(width = '20%', targets =c(2, 4)),
                           list(width = '10%', targets =c(3, 5))))
          ),# end option
          rownames = FALSE,
          colnames = c(prsnly_name, "Preference Index", "Shopper Count", "% Count", "Total Brand Spend", "% Spend"),
          class = "display compact"
        ) %>%
        formatRound("brand_index", digits = 3) %>%
        formatRound("shoppers_count", digits = 0) %>%
        formatPercentage("percent_shoppers", digits = 2) %>%
        formatCurrency("brand_spend", digits = 0, currency = '$', before = TRUE) %>%
        formatPercentage("percent_spend", digits = 2) %>%
        formatStyle('brand_index',
                    backgroundColor = styleInterval(c(0.7, 1.29999), c('lightcoral', 'white', 'lightgreen'))) # right-closed interval
    }))
  }
  
  # output name need to be the lower case of the personality name with connected with _
  output$t_household_size <- render_demo_table("Household Size")
  output$t_household_income_bracket <- render_demo_table("Household Income Bracket")
  output$t_gender_of_primary_household <- render_demo_table("Gender of Primary Household")
  output$t_has_children <- render_demo_table("Has Children")
  output$t_adult_age_group <- render_demo_table("Adult Age Group")
  output$t_education <- render_demo_table("Education")
  output$t_ethnicity <- render_demo_table("Ethnicity")
  output$t_occupation_category <- render_demo_table("Occupation Category")
  
  # ui_table_grid
  output$ui_demo_grids <- renderUI({
    if(input$all_features_pick == TRUE){
      lst_pers <- sort(unique(demo_data[brand_desc == 'brand_001', personality_name]))
      # put a star in front of some features since it will be overlapping
      star_pers <- lst_pers %in% c("Adult Age Group", "Child Age", "Education", "Occupation", "Occupation Category", "Ethnicity")
      lst_pers[star_pers] <- paste0(lst_pers[star_pers], '*')
      lapply(seq_along(lst_pers), function(i){
        div(style = 'padding-left: 5px; padding-right: 10px',
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
      p("This page provides demographic personas for a given product brand, highlighting which target groups show the strongest preference for this product."
      ),
      
      br(),
      shiny::HTML("Pick a search strategy to identify the product brand you’re looking for."),
      br(),
      shiny::HTML("Since multiple producers may share the same brand name, please verify that the selected producer is the one you intend."),
      br(),
      shiny::HTML("Note that a single brand may have products across different categories."),
      br(),br(),
      
      p(column(6,
               HTML("Note: The results display all available demographic personas. 
                    To view or arrange specific personas, deselect the [Display all demographics] option 
                    and choose one or more personas manually.")
      ),
      column(6, img(src = 'help_demo_1.png', style = "max-width:100%; height:auto; display:block; margin:auto;"))
      ),
      
      size = 'l', easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)"))
    )
  })
  
  # definition button
  observeEvent(input$defs_btn, {
    showModal(modalDialog(title = "Definitions",
                          div(style = "line-height: 20px", #line space
                              shiny::HTML(paste(
                                "<b>Shopper Count</b>: The number of unique shopper IDs (or loyalty cards) associated with the selected persona. <br>",
                                br(), br(),
                                "<b>Preference Index</b>: A relative measure comparing the share of a specific persona 
          within a brand, category, or segment to its share in the total population. <br>
          Index = 1.0 (average alignment);<br>
          Index > 1.0 (higher-than-avearge preference);<br>
          Index < 1.0 (lower-than-average preference);<br>",
                                br(),br(),
                                "<b>Total Brand Spend</b>: Total Brand Spend represents the sum of all sales value on products
                                belonging to a given brand across store, regions, and time periods.<br>"
                              ))),
                          easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
    ))
  })
  
  dt_download <- reactive({
    demo_data %>% 
      filter(manufacturer == d_manu(),
             cat_desc == d_cate(),
             brand_desc == d_brand()) %>% 
      select(
        `Producer` = manufacturer,
        `Product Category` = cat_desc, 
        `Product Brand` = brand_desc,
        `Demographics Persona` = personality_name,
        `Persona Level`= personality_level,
        `Preference Index` = shopper_penetration_index, 
        `Shopper Count` = raw_count_shoppers,
        `% Count` = percent_shoppers, 
        `Total Brand Spend` = raw_count_spend,
        `% Spend`= percent_spend)
  })
  
  output$download_btn <- downloadHandler(
    filename = function() {paste0(gsub(' ','_',tolower(d_brand())),'_','demographics.csv')},
    content = function(file) {write.csv(dt_download(), file, row.names = FALSE)}
  )
} #end server function



########### END
