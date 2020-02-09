observeEvent(input$pasbesoin, {
  toggleModal(session = session,
              modalId = "window",
              toggle = "close")
})

observeEvent(input$besoin, {
  toggleModal(session = session,
              modalId = "window",
              toggle = "close")
  introjs(session)
})