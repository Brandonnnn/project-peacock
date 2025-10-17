################################################################################.
# Code to create pages under navbarMenu
# In this script include ui and server function to build several pages
# Updated: 2023-08-31
################################################################################.

################################.
# UI ------

lp_tool_box <- function(title_box, image_name, button_name, description, onclick) {
  div(class="landing-page-box",
      div(title_box, class = "landing-page-box-title"),
      div(description, class = "landing-page-box-description"),
      div(class = "landing-page-icon", style= paste0("background-image: url(", image_name, ".png);
          background-size: auto 90%; background-position: center; background-repeat: no-repeat; ")),
      actionButton(button_name, NULL, class="landing-page-button", onclick=onclick)
  )
}

toolPageUI <- function(id){
  ns <- NS(id)
  
  tagList(
    mainPanel(width = 11, style="margin-left:4%; margin-right:4%",
      fluidRow( #Page title
        column(7,(h3("Other Tools", style="margin-top:0px;")))
      ),
      fluidRow( #1st row of boxes
        # Catalina Persona Tool
        column(6, class="landing-page-column",
               br(), #spacing
               lp_tool_box(image_name= "tool_explorer", 
                           button_name = 'link_pde',
                           title_box = "Catalina Persona Tool",
                           description = 'Personas use individual Shopper Personalities scores across types to create a more powerful audience',
                           onclick = "window.open('http://10.165.27.68:4000/persona-tool/', '_blank')")
        ),
        # Lookalike ModelR (LM)
        column(6, class="landing-page-column",
               br(), #spacing
               lp_tool_box(image_name= "tool_lookalike", 
                           button_name = 'link_lm',
                           title_box = "Lookalike ModelR",
                           description = 'Provide a streamlined and extensible approach to identify shopper audiences with lookalike modeling',
                           onclick = "window.open('http://orlpsasgdlv08.catmktg.com:3838/user/lookalike_modelr/', '_blank')")
        )
      ), #end 1st Row
      fluidRow( # 2nd row of boxes
        # AOA factor
        column(6, class="landing-page-column",
               br(), #spacing
               lp_tool_box(image_name= "tool_aoa", 
                           button_name = 'link_aoa',
                           title_box = "All-Outlet Adjustment",
                           description = 'Adjust insights to account for purchasing outside Catalina’s geographies and retail networks',
                           onclick = "window.open('http://10.165.27.68:3838/user/AOA/aoa_shiny/', '_blank')")
        )
      ) #end 2nd row
    ) #end mainPanel
  )#end tabPanel
  
}

################################.
# SERVER ------
toolPage <- function(input,output, session){

}
