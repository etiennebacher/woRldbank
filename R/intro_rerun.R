observeEvent(input$tabs, {
  if (input$tabs == "rerun"){
    introjs(session)
    updateNavbarPage(session = session, inputId = "tabs", selected = "Presentation")
  }
})