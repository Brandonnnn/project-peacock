#In this script include all the server side functions: plots, reactive objects, etc.

###############################################.

## Define a server for the Shiny app
function(input, output, session) {
  
  # load moduels for each page
  
  callModule(personaPage, "personaPage")
  callModule(demographicsPage, "demographicsPage")
  
  observeEvent(input$jump_to_pers, {
    updateTabsetPanel(session, "intabset", selected = "personalities")
  })
  observeEvent(input$jump_to_demo, {
    updateTabsetPanel(session, "intabset", selected = "demographics")
  })
  
  

  
} # end server bracket
############################ END