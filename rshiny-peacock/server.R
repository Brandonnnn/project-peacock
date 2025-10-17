#In this script include all the server side functions: plots, reactive objects, etc.

###############################################.

## Define a server for the Shiny app
function(input, output, session) {
  
  # load moduels for each page
  # callModule(brandAffinityPage, "brandAffinityPage")
  # callModule(timeAffinityPage, "timeAffinityPage")
  # callModule(demographicsPage, "demographicsPage")
  callModule(tourPage, "tourPage")
  callModule(personaPage, "personaPage")
  # # callModule(cameoPage, "cameoPage", psnlty_output = psnlty_page)
  callModule(toolPage, "toolPage")
  # callModule(storePersonalitiesPage, "storePersonalitiesPage") # add at 01/19/2022
  # 
  # Landing Page ----
  # create event that switch tabs
  # observeEvent(input$jump_to_affinity, {
  #   updateTabsetPanel(session, "intabset", selected = "affinity")
  # })
  # observeEvent(input$jump_to_time, {
  #   updateTabsetPanel(session, "intabset", selected = "time_affinity")
  # })
  observeEvent(input$jump_to_pers, {
    updateTabsetPanel(session, "intabset", selected = "personalities")
  })
  # observeEvent(input$jump_to_demo, {
  #   updateTabsetPanel(session, "intabset", selected = "demographics")
  # })
  # ## add store personality 01/19/2022
  # observeEvent(input$jump_to_store, {
  #   updateTabsetPanel(session, "intabset", selected = "store")
  # }) 
  observeEvent(input$help_tour, {
    updateTabsetPanel(session, "intabset", selected = "tour")
  })
  # 
  # # connect cameo with personality module output
  # observeEvent(psnlty_page$jump_to_cameo(), {
  #   updateTabsetPanel(session, "intabset", selected = "cameo")
  # })
  

  
} # end server bracket
############################ END