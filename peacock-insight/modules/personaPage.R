################################################################################.
# Code to create page Persona Audiences
# In this script include ui and server function to build the page
# Updated: 2025-10-15
################################################################################.

################################.
# UI ------
personaPageUI <- function(id){
  ns <- NS(id)
  
  tagList(
    # sidebar ----
    sidebarPanel(width = 3, #Filter options
                 actionButton(ns("help_sp"), label="Help", icon= icon('question-circle'), class ="down"),
                 actionButton(ns("defs_sp"),label="Definitions", icon= icon('info'), class ="down"),
                 
                 # select type
                 div(title="Select your searching type.", # tooltip
                     style = "margin-top: 20px", # space to the action button
                     selectInput(ns('sp_search_type'), label = "Step 1 - Select Data Source",
                                 choices = sort(unique(sp_levels$type)),
                                 selected = 'Modeling')
                 ),
                 
                 # select major cate
                 div(style = "margin-top: 20px", # space above
                     selectInput(ns('sp_pick_major'),
                                 shiny::HTML("<p>Step 2 - Select Main Segment 
                        <span style = 'font-weight:400'><br/>(hit backspace and start typing to search)</span></p>"),
                                 choices = NULL)),
                 
                 # select intermediate level
                 div(title = "Select the intermediate hierarchy",
                     style = "margin-top: 20px",
                     selectInput(ns("sp_pick_inter"),
                                 label = shiny::HTML("<p>Step 3 - Select Shopper Group 
                        <span style = 'font-weight:400'><br/>(hit backspace and start typing to search)</span></p>"),
                                 choices = NULL)),
                 
                 # select personality
                 div(title = "Select the personality",
                     style = "margin-top: 20px",
                     selectInput(ns("sp_pick_pers"),
                                 label = shiny::HTML("<p>Step 4 - Select Persona
                        <span style = 'font-weight:400'><br/>(hit backspace and start typing to search)</span></p>"),
                                 choices = NULL)),
                 
                 # retailer
                 div(title = "Show result on all retailers or selected retailers",
                     style = "margin-top: 10px",
                     awesomeCheckbox(ns("sp_all_reta"), label = "Display all retailers", value = TRUE)),
                 uiOutput(ns("sp_show_reta")),# select retailers
                 
                 # button
                 div(style = "margin-top:20px",
                     # Availability
                     actionButton(ns("get_usage"), label = "Use Environment", icon =icon("fas fa-font-awesome"), class = "down"),
                     # download
                     downloadButton(ns('download_pers'),"Download Data", class = "down")
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
              div(style = "padding: 5px; padding-left: 5px; width: 99%; font-size: 95%",
                  htmlOutput(ns("sp_level_title")),
                  withSpinner(dataTableOutput(ns('sp_level_table')))
              ),
              # Affinity Brands Table
              div(style = "padding: 5px; padding-left: 10px; margin-top: 30px; font-size: 90%; width: 98%",
                  htmlOutput(ns("sp_table_affinity_title")),
                  withSpinner(dataTableOutput(ns('sp_show_table_affinity'))))
    ) #end main Panel
  )#end tagList
}


################################.

# SERVER ------
personaPage <- function(input, output, session){
  
  # Sidebar Input ----
  observe({
    lst_major <- sort(unique(sp_levels[type == input$sp_search_type, major_hierarchy]))
    updateSelectInput(session, "sp_pick_major", 
                      choices = lst_major,
                      selected = lst_major[1]
    )
  })
  
  observe({
    req(input$sp_pick_major)
    lst_inter <- sort(unique(sp_levels[type == input$sp_search_type & major_hierarchy == input$sp_pick_major]$intermediate_hierarchy))
    updateSelectInput(session, "sp_pick_inter", choices = lst_inter, selected = lst_inter[1])
  })
  
  observe({
    req(input$sp_pick_inter)
    lst_pers <- sort(unique(sp_levels[type == input$sp_search_type 
                                      & major_hierarchy == input$sp_pick_major
                                      & intermediate_hierarchy == input$sp_pick_inter]$personality_name))
    updateSelectInput(session, "sp_pick_pers", choices = lst_pers, selected = lst_pers[1])
  })
  
  output$sp_show_reta <- renderUI({
    req(!input$sp_all_reta, input$sp_pick_pers)
    lst_reta <- sort(unique(sp_data[type == input$sp_search_type 
                                    & major_hierarchy == input$sp_pick_major
                                    & intermediate_hierarchy == input$sp_pick_inter
                                    & personality_name == input$sp_pick_pers
                                    & lgl_entity_nm != '-All-']$lgl_entity_nm))
    div(title = "Select one or more retailers",
        selectizeInput(session$ns("sp_pick_reta"),
                       label = shiny::HTML("<p>Step 4b - Select one or more retailers 
                <span style = 'font-weight:400'><br/>(You can select mutiple retailers)</span></p>"),
                       choices = c("Select retailers" = "", paste(lst_reta)),
                       multiple = TRUE, selected = ""))
  })
  
  d_sp_reta <- reactive({ # need to use unlist(...) before use since ifelse return input shape
    if(input$sp_all_reta == TRUE | is.null(input$sp_pick_reta)){
      "-All-"
    } else{
      input$sp_pick_reta
    }
  })
  
  
  # page summary ----
  update_date <- reactive(dt_date[table_nm == "personality_table"]$date_end)
  
  # Render summary box (make sure personality name is unique)
  data_personality <- reactive({
    req(input$sp_pick_major, input$sp_pick_inter, input$sp_pick_pers, d_sp_reta())
    sp_data %>% 
      filter(major_hierarchy == input$sp_pick_major,
             intermediate_hierarchy == input$sp_pick_inter,
             personality_name == input$sp_pick_pers) %>% 
      filter(lgl_entity_nm %in% d_sp_reta())
  })
  
  data_personality_group <- reactive({
    req(data_personality())
    data_personality() %>% 
      group_by(personality_level, personality_name) %>%
      summarise(id_cnt = sum(id_cnt), id_tot_cnt = sum(id_tot_cnt),
                id_cnt_act = sum(id_cnt_act), id_act_cum = sum(id_act_cum),
                BV_hh_cnt = sum(BV_hh_cnt), BV_hh_tot_cnt = sum(BV_hh_tot_cnt),
                TV_hh_cnt = sum(TV_hh_cnt), TV_hh_tot_cnt = sum(TV_hh_tot_cnt),
                .groups = 'keep') %>%
      mutate(id_cnt_p = id_cnt/id_tot_cnt,
             BV_hh_cnt_p = ifelse(BV_hh_tot_cnt ==0, 0, BV_hh_cnt/BV_hh_tot_cnt),
             TV_hh_cnt_p = ifelse(TV_hh_tot_cnt ==0, 0, TV_hh_cnt/TV_hh_tot_cnt)) %>%
      arrange(personality_level)
  })
  
  
  
  output$sp_table_title <- renderText({
    paste(data_personality()$type[1], input$sp_pick_major, input$sp_pick_pers, sep=" / ")
  })
  
  output$sp_table_contents <- renderUI({
    req(data_personality())
    div(style = "line-height: 17px",
        shiny::HTML(paste0(
          # line 1
          "<b>Target Group</b>: ", tolower(data_personality()$personality_desc[1]), "<br/>",
          # line 2: dates
          "Data collected from the last 26 weeks ending at ", update_date(), "<br>",
          # line 3: BV Household
          "<b>Note</b>: The data displayed in this app is <b>synthetic</b> and for <b>demonstration purposes</b> only."
        ))
    )
  })
  
  # main table ----
  
  # ** personality Table -------
  
  # render table
  output$sp_level_table <- renderDT({
    req(input$sp_pick_pers, d_sp_reta(), data_personality_group())

    if(length(d_sp_reta()) <= 1){
      # select only one retailers, keep all the columns
      
      ## setup the frame
      container_dt= htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(class = 'dt-center', rowspan = 2, colspan = 1, "Persona Name"),
            th(class = 'dt-center', rowspan = 2, colspan = 1, "Audience Level"),
            th(class = 'dt-center', colspan = 2, 'Shopper Count'),
            th(class = 'dt-center', colspan = 2, 'Household Count')
          ),
          lapply(c('# Count', '% Count', '# Count', '% Count'), th)
        )
      ))
      
      ## build table
      data_personality_group() %>%
        select(personality_name, personality_level, id_cnt, id_cnt_p, BV_hh_cnt, BV_hh_cnt_p) %>% 
        DT::datatable(
          options = list(pageLength = 10,
                         dom = "tip",
                         scrollX = TRUE
          ),
          container = container_dt,
          rownames = FALSE
        ) %>%
        formatRound("id_cnt", digits = 0) %>%
        formatRound("BV_hh_cnt", digits = 0) %>%
        formatPercentage("id_cnt_p", digits = 2) %>%
        formatPercentage("BV_hh_cnt_p", digits = 2)
  
    } else{
      # select multiple retailers, remove household counts columnns
      
      ## setup the frame
      container_dt= htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(class = 'dt-center', rowspan = 2, colspan = 1, "Persona Name"),
            th(class = 'dt-center', rowspan = 2, colspan = 1, "Audience Level"),
            th(class = 'dt-center', colspan = 2, 'Shopper Count')
          ),
          lapply(c('# Count', '% Count'), th)
        )
      ))
      
      ## build table
      data_personality_group() %>%
        select(personality_name, personality_level, id_cnt, id_cnt_p) %>% 
        DT::datatable(
          options = list(pageLength = 10,
                         dom = "tip",
                         scrollX = TRUE
          ),
          container = container_dt,
          rownames = FALSE
        ) %>%
        formatRound("id_cnt", digits = 0) %>%
        formatPercentage("id_cnt_p", digits = 2)
      
    }
  })
  
  # **affinity Table -----
  
  # rendar table for brand affinity ranking
  output$sp_table_affinity_title <- renderUI({
    p(tags$b(h5(glue("Top Brands for : {input$sp_pick_pers}"))))
  })
  
  output$sp_show_table_affinity <- renderDataTable({
    req(input$sp_pick_pers)
    dt <- affinity_data[shopper_personality == input$sp_pick_pers]
    dt %>% 
      select(manufacturer, cat_desc, brand_desc, brand_affinity, brand_spend, opportunity) %>% 
      arrange(-brand_affinity) %>% 
      DT::datatable(options = list(pageLength = 10, dom = "tip"),
                    colnames = c("Producer", "Product Category", "Brand Name", "Preference Index", "Spend Share",
                                 "High-Value Opportunity"),
                    rownames = FALSE
      ) %>% 
      formatRound("brand_affinity", digits = 3) %>% 
      formatCurrency("brand_spend", digits = 0, currency = "$", before = TRUE) %>%
      formatCurrency("opportunity", digits = 0, currency = "$", before = TRUE) %>%
      formatStyle('brand_affinity', color = styleInterval(c(0.8, 2), c('red', 'goldenrod', 'green')))
  })
  
  
  
  
  # help button ----
  observeEvent(input$help_sp, {
    showModal(modalDialog(
      title = "How to use this page",
      tags$p("Provides an overview of the selected brand’s attributes, such as ingredient composition, 
        demographic trends, and associated shopper personalities."
      ),
      
      br(),
      shiny::HTML("<b>Data Sources and Personas</b>: <br>"),
      br(),
      shiny::HTML("<b>Modeling</b>:
                  Modeling Persona uses predictive modeling to identify and score shoppers based on
                  their likelihood to engage, purchase, or respond — helping use target the right audiences more effectively.<br>"),
      br(),
      shiny::HTML("<b>Demographics</b>: provides insights into a brand’s audience based on key demographic attributes
        such as age, gender, income, household composition, and lifestyle.
        It helps users understand who the brand’s core shoppers are and how they differ from the broader population.
                  <br>"
                  ),
      br(),
      shiny::HTML("<b>Purchasing</b>: 
      Purchasing Persona highlights shopper groups based on their buying behavior — what they purchase, 
      how frequently they shop, and how much they spend. It helps identify key purchasing 
                  patterns that define a brand’s most valuable consumers.
                  <br>"),
      br(),
      shiny::HTML("<b>Visitation</b>:
      Visitation Persona provides insights into how and where shoppers engage with your brand. 
                  It highlights visit frequency, preferred locations, and touchpoints to reveal patterns 
                  in store and out-of-home interactions.
                  <br>"),
      br(),
      shiny::HTML("<b>Web Browsing</b>: 
      Web Browsing Persona provides insights into shoppers’ online behaviors — the websites they visit, 
      the content they engage with, and the interests they express online. 
      It helps identify digital patterns that shape brand affinity, awareness, and engagement potential.
                  <br>"),
      br(),br(),
      
      p(column(6,
        "Note: The result is an aggregation of all retailers.
        If the user wants to explore the data for specific retailers (one or more),
        please unclick [Display all retailers] button to select."
        ),
        column(6, img(src = 'help_sp_3.png', style = "max-width:100%; height:auto; display:block; margin:auto;"))
      ),
      br(), br(),
      p("Download button will export two tables, one summary table about the selected persona and
         one ranking table show the top 100 brands having such persona "
      ),
      br(),
      size = "l", easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)"))
    )
  })
  
  # definition button  ----
  observeEvent(input$defs_sp, {
    showModal(modalDialog(title = "Definitions",
                          HTML(paste(
      "<b>Shopper Count</b>: The number of unique shopper IDs (or loyalty cards) associated with the selected persona. <br>",
      br(), br(),
      "<b>Household Count</b>: The number of unique households linked to the selected persona.
              Each household may contain one or more shoppers, aggregated based on shared address or household ID. <br>",
      br(), br(),
      "<b>Preference Index</b>: A relative measure comparing the share of a specific persona 
          within a brand, category, or segment to its share in the total population. <br>
          Index = 1.0 (average alignment);<br>
          Index > 1.0 (higher-than-avearge preference);<br>
          Index < 1.0 (lower-than-average preference);<br>",
      br(),br(),
      "<b>Spend Share</b>: The total spending within a brand or category contributed by the selected persona.
          Indicates how much this audience contributes financially relative to other groups.<br>",
      br(), br(),
      "<b>High-Value Opportunity</b>: A segment representing the top percentile (e.g., top 5%) of consumers 
      or households with the greatest growth or conversion potential based on spend, frequency, or engagement metrics.
      Useful for targeting expansion or retention initiatives.")),
      easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")
    ))
  })
  
  # download  ----
  # download button for product attributes
  dt_download_pers_attr <- reactive({
    data_personality() %>% 
      arrange(lgl_entity_nm, personality_level)
  })
  output$download_pers <- downloadHandler(
    filename = function() {paste0(gsub(' ','-',tolower(input$sp_pick_pers)),'-','product-attributes',".csv")},
    content = function(file) {
      # attribute summary table
      write.table('------------------', file, col.names = F, row.names = F, sep = ',', append = T)
      write.table('Personality Table', file, col.names = F, row.names = F, sep = ',', append = T)
      write.table('------------------', file, col.names = F, row.names = F, sep = ',', append = T)
      write.table(dt_download_pers_attr() %>% 
                    select(
                      `Data Source` = type,
                      `Main Segment` = major_hierarchy,
                      `Shopper Group` = intermediate_hierarchy,
                      `Persona Name` = personality_name,
                      `Audience Level` = personality_level,
                      `Shopper Count` = id_cnt,
                      `Household Count` = BV_hh_cnt,
                      `Use: In Store` = is_in_store_used,
                      `Use: Dashboard` = is_dashboard_used,
                      `Use: Measurement` = is_measurement_used,
                      `Use: Online` = is_digital_used,
                      `Use: Out-of-Home` = is_ooh_used,
                    ), 
                  file, row.names = F, sep = ',', append = T)
      # brand affinity table
      write.table('------------------', file, col.names = F, row.names = F, sep = ',', append = T)
      write.table('Brand Affinities', file, col.names = F, row.names = F, sep = ',', append = T)
      write.table('------------------', file, col.names = F, row.names = F, sep = ',', append = T)
      write.table(affinity_data %>% 
                    filter(shopper_personality == input$sp_pick_pers) %>% 
                    arrange(desc(brand_affinity)) %>%
                    select(
                      `Persona Name` = shopper_personality,
                      `Data Source` = type,
                      `Producer` = manufacturer,
                      `Product Category` = cat_desc,
                      `Brand Name` = brand_desc,
                      `Preference Index` = brand_affinity,
                      `Spend Share` = brand_spend,
                      `High-Value Opportunity` = opportunity,
                    ) %>% 
                    slice_head(n=100),
                  file, row.names = F, sep = ',', append = T)
    }
  )
  
  
  
  # availability ----
  observeEvent(input$get_usage, {
    showModal(
      modalDialog(title = "Availability Table",
                  br(),
                  # table
                  renderDT({
                    data_personality() %>% 
                      select(personality_name,
                             `In Store` = is_in_store_used, 
                             `Dashboard` = is_dashboard_used,
                             `Measurement` = is_measurement_used,
                             `Digital/Online` = is_digital_used,
                             `Out-of-Home` = is_ooh_used) %>%
                      distinct() %>% 
                      pivot_longer(cols = -personality_name) %>% 
                      mutate(value = ifelse(value == 1, "True", "False")) %>% 
                      DT::datatable(
                        options = list(pageLength = 10, dom = "", lengthChange = FALSE),
                        rownames = FALSE,
                        colnames = c("Persona", "Environment", "Status")) %>% 
                      formatStyle('value', color = styleEqual(c('True', "False"), c('green', 'red')))
                  }),
                  hr(),
                  downloadButton(session$ns('download_full_usage'),"Download Full Persona List", class = "down"),
                  easyClose = TRUE, fade=FALSE, footer = modalButton("Close (Esc)")  
      ))
  })
  
  # download button for full shopper list
  output$download_full_usage <- downloadHandler(
    filename = function() {paste0("demo_full_persona_", gsub("-", "", today()), ".csv")},
    content = function(file) {
      write.csv(
        sp_data %>% 
          filter(lgl_entity_nm == "-All-") %>% 
          select(
            `Data Source` = type,
            `Main Segment` = major_hierarchy,
            `Shopper Group` = intermediate_hierarchy,
            `Persona Name` = personality_name,
            `Audience Level` = personality_level,
            `Shopper Count` = id_cnt,
            `Household Count` = BV_hh_cnt,
            `Use: In Store` = is_in_store_used,
            `Use: Dashboard` = is_dashboard_used,
            `Use: Measurement` = is_measurement_used,
            `Use: Online` = is_digital_used,
            `Use: Out-of-Home` = is_ooh_used,
          ),
          file, row.names = FALSE, na = "-"
        )
    }
  )
  
  # return -----
  return(list(
    jump_to_cameo = reactive(input$open_cameo),
    
    # export selected personality
    get_prsnly = reactive(input$sp_pick_pers),
    
    # export selected retailers
    show_all_retailers = reactive(input$sp_all_reta),
    
    get_retailers = d_sp_reta
    
  ))
}
######################## END
