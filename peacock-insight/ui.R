# This script includes the user-interface definition of the app.

###############################################.
## Header ---- 
###############################################.

tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs
  introjsUI(),   # Required to enable introjs scripts
  navbarPage(id = "intabset", #needed for landing page
    title = div(
      tags$a(
        img(
          src = "peacock_icon_white.png",
          style = "
        height:60px;
        width:80px;
        object-fit:contain;
        vertical-align:middle;
        margin-top:-10px;
      "
        ),
        href = 'https://www.catalina.com/insights/',
        target = "_blank"
      ),
      style = "
     display:flex;
    align-items:center;
    height:40px;
  "
    ),
    
    windowTitle = "bwang profiles", #title for browser tabexitq
    theme = shinytheme("paper"), #Theme of the app (https://bootswatch.com/3/spacelab/)
    collapsible = TRUE, #tab panels collapse into menu in small screens
     header =  
     tags$head(
        includeCSS("styles.css"), # load all css class definition
        HTML("<base target='_blank'>") # to make external links open a new tab,
     ),
    
    # Landing page
    tabPanel(
      title = " Home", icon = icon("home"),
      mainPanel(width = 11, style="margin-left:4%; margin-right:4%",
        fluidRow( #Page title
          column(11,(h4("Welcome to Project Peacock: The CPG Data Insights Toolbox", style="margin-top:3px;")))
          # column(4,actionButton("help_tour",label="Help: Take tour of the tool",icon=icon('question-circle'),class="down"))
        ),
        fluidRow( #1st row of boxes
          #Shopper Personality 
          column(6, class="landing-page-column",
            br(), #spacing
            lp_main_box(image_name= "landing_persona", 
              button_name = 'jump_to_pers', title_box = "Shopper Persona Insights",
              description = 'Identify key audience segmentation, revealing engagement levels, and which brands 
              resonate most with them')
          ),
          #Demographics 
          column(6, class="landing-page-column",
            br(), #spacing
            lp_main_box(image_name= "landing_demo", 
              button_name = 'jump_to_demo', title_box = "Brand's Demographic Reach",
              description = 'Explore demographic-based personas for each brand to reveal which audience segments show the strongest affinity')
            )
        ), #end 1st Row
        
        fluidRow( #2nd Row
          # Time / Date Personality
          column(6, class = 'landing-page-column',
            br(),
            lp_main_box(image_name = "landing_wip",
              button_name = "jump_to_time", title_box = "Retail Competitor Map",
              description = "Compare two retailers by county with an interactive color-coded map")
          ),
          # Store Personality
          column(6, class = 'landing-page-column',
            br(),
            lp_main_box(image_name = "landing_wip",
              button_name = "jump_to_store", title_box = "Explore More Tools",
              description = "Explore AI solutions that enhance product discovery and retail intelligence")
          )
        ) #end 2nd Row
        
      ) #end mainPanel
    ), #end tabPanel
    
    # tabPanel Pages
    tabPanel(
      title = "Personas", icon = icon("fas fa-address-card"), value = "personalities",
      personaPageUI("personaPage")
    )
    ,tabPanel(title = "Demographics", icon = icon("far fa-id-card-alt"), value = "demographics",
      demographicsPageUI("demographicsPage")
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
     background-color: #41597D"
  )
) # end tagList
###END