observeEvent(input$no_need, {
  toggleModal(session = session,
              modalId = "window",
              toggle = "close")
  updateNavbarPage(session = session, inputId = "tabs", selected = "Presentation")
})

observeEvent(input$need, {
  toggleModal(session = session,
              modalId = "window",
              toggle = "close")
  introjs(session)
  updateNavbarPage(session = session, inputId = "tabs", selected = "Presentation")
})