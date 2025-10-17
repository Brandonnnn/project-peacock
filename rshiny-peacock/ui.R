# This script includes the user-interface definition of the app.

###############################################.
## Header ---- 
###############################################.

tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  introjsUI(),   # Required to enable introjs scripts
  navbarPage(id = "intabset", #needed for landing page
    title = div(tags$a(img(src="catalina_logo.png", height=40), href= 'https://www.catalina.com/insights/'),
      style = "position: relative; top: -5px;"), # Navigation bar
    windowTitle = "Catalina profiles", #title for browser tabexitq
    theme = shinytheme("spacelab"), #Theme of the app (https://bootswatch.com/3/spacelab/)
    collapsible = TRUE, #tab panels collapse into menu in small screens
     header =  
     tags$head(
        includeCSS("styles.css"), # load all css class definition
        HTML("<base target='_blank'>") # to make external links open a new tab
     ),
    
    # Landing page
    tabPanel(
      title = " Home", icon = icon("home"),
      mainPanel(width = 11, style="margin-left:4%; margin-right:4%",
        fluidRow( #Page title
          column(7,(h3("Welcome to the Catalina Data Strategy Toolbox", style="margin-top:0px;"))),
          column(4,actionButton("help_tour",label="Help: Take tour of the tool",icon=icon('question-circle'),class="down"))
        ),
        fluidRow( 
          #1st row of boxes
          #Brand Affinity
          column(4, class="landing-page-column",
            br(), #spacing
            lp_main_box(image_name= "landing_affinity", 
              button_name = 'jump_to_affinity', title_box = "Brand Affinity",
              description = 'Understand how much more likely different personality audiences are to buy a brand')
          ),     
          #Shopper Personality 
          column(4, class="landing-page-column",
            br(), #spacing
            lp_main_box(image_name= "landing_personalities", 
              button_name = 'jump_to_pers', title_box = "Personality Audiences",
              description = 'Review audience size by propensity tier and see which brands have the highest affinity')
          ),
          #Demographics 
          column(4, class="landing-page-column",
            br(), #spacing
            lp_main_box(image_name= "landing_demo", 
              button_name = 'jump_to_demo', title_box = "Demographic Insights",
              description = 'Review how key demographics impact category, manufacturer and brand sales')
            )
        ), #end 1st Row
        fluidRow( #2nd Row
          # Time / Date Personality
          column(4, class = 'landing-page-column',
            br(),
            lp_main_box(image_name = "landing_date",
              button_name = "jump_to_time", title_box = "Time Personalities",
              description = "Understand how much more likely audiences are to buy a brand at different times")
          ),
          # Store Personality
          column(4, class = 'landing-page-column',
            br(),
            lp_main_box(image_name = "landing_store",
              button_name = "jump_to_store", title_box = "Store Personalities",
              description = "Store based personalities")
          )
        ) #end 2nd Row
      ) #end mainPanel
    ), #end tabPanel
    
    # tabPanel Pages
    # tabPanel(
    #   title = "Affinities", icon = icon("fas fa-heartbeat"), value = "affinity",
    #   brandAffinityPageUI("brandAffinityPage")
    # ), 
    tabPanel(
      title = "Personalities", icon = icon("fas fa-address-card"), value = "personalities",
      personaPageUI("personaPage")
    ),
    # tabPanel(title = "Demographics", icon = icon("far fa-id-card-alt"), value = "demographics",
    #   demographicsPageUI("demographicsPage")
    # ),
    # tabPanel(
    #   title = "Time", icon = icon("far fa-calendar-alt"), value = "time_affinity",
    #   timeAffinityPageUI("timeAffinityPage")
    # ),
    # tabPanel(
    #   title = "Store", icon = icon("far fa-map-marked"), value = "store",
    #   storePersonalitiesPageUI("storePersonalitiesPage")
    # ),
    
    # navbar Pages
    navbarMenu("Info", icon = icon("info-circle"),
      tabPanel(
        title = "Tour of the tool", icon = icon("info-circle"), value = "tour",
        tourPageUI("tourPage")
      ),
      # tabPanel(
      #   title = "Audience File", icon = icon("fab fa-dropbox"), value = "cameo",
      #   cameoPageUI("cameoPage")
      # ),
      tabPanel(
        title = "Other Tools", icon = icon("fab fa-toolbox"), value = "tools",
        toolPageUI("toolPage")
      )
    )
  ), #end navbarPage
  
  ###############################################.             
  ## Footer----    
  ###############################################.
  div(style = "margin-bottom: 20px;"), # this adds breathing space between content and footer
  #Copyright warning
  tags$footer(
    column(3, "© Project Peacock 2025"), 
    column(6, tags$b("This application uses synthetic data for demo purposes only.")),
    column(2, tags$a(href="mailto:brandon.wangq@gmail.com", tags$b("Contact me!"), 
      class="externallink", style = "color: white; text-decoration: none")), 
    style = "
     position:fixed;
     text-align:center;
     left: 0;
     bottom:0;
     width:100%;
     z-index:1000;  
     height:30px; /* Height of the footer */
     color: white;
     padding: 10px;
     font-weight: NULL;
     background-color: #051e2c"
  )
) # end tagList
###END