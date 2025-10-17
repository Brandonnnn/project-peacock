################################################################################.
# Code to create pages under navbarMenu
# In this script include ui and server function to build several pages
# Updated: 2020-03-24 16:00
################################################################################.

################################.
# UI ------

tourPageUI <- function(id){
  ns <- NS(id)
  
  tagList(
    sidebarPanel(width = 1),
    mainPanel(width = 10,
      fluidRow(p(h4("Welcome to the Catalina Data Strategy Toolbox"),
        h5("This interactive tool provides access to a range of marketing related indicators at 
            different categories including brand affinity, personality audiences, and demographic insights",style = "color:black;"),
        h5("There are different ways to navigate around the tool.", style = "color:black;"),
        h5("Different visualisations can be opened using the menu bar (the blue strip) at the top of the screen.",
          style = "color:black;"),
        img(src='intro_panel.png',width=300),
        br(),
        h5("The 'Home' option in the menu bar will return to the toolbox homepage.",
          style = "color:black;"),
        style = "font-size:20px")),
      hr(),
      fluidRow(column(6,
        h5("The 'Brand Affinity' page would help you understand how much more likely different personality audiences
              are to buy a brand. ",
          style = "color:black;")),
        column(6, img(src='tour_brandAffinity.png'))),
      hr(),
      fluidRow(column(3,
        h5("The 'Personality Audiences' present audience size by propensity tier and see which brands have the highest affinity.",
          style = "color:black;")),
        column(9, img(src='tour_personality.png'))),
      fluidRow(column(3,
        h5("The 'Demographic Insights'allow detailed exploration of one brand at a time.",
          style = "color:black;")),
        column(9, img(src='tour_demographic.png'))),
      hr(),
      fluidRow(column(8,
        p(h5("Throughout the tool look out for options in each window that provide",
          style = "color:black;"),
          tags$ul( 
            tags$li("Help button for how to use the page."),
            tags$li("Indicator Definitions or help to interpret a table or visualisation"),
            tags$li("Data Download options for individual tables,")))),
        column(4, img(src='tour_bottons.png'))),
      hr()
    )
  )#end tabPanel
  
  
}

################################.
# SERVER ------
tourPage <- function(input,output, session){
  
}