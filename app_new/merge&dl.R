round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

result_merged <- reactive({
  req(input$to_merge)
  provisoire <- map(input$to_merge, ~{tables[[.x]]()})
  data <- reduce(provisoire, full_join, by = c("Code ISO", "Pays", "Année")) %>%
    arrange("Code ISO", "Pays", "Année") %>%
    select("Code ISO", "Pays", "Année", everything())
  round_df(data, 3)
})

observeEvent(input$apply_merge, {
  output$data_merged <- renderDataTable({
    result_merged()
  })
  
  if(!is.null(result_merged())){
    output$download_ui <- renderUI({
      downloadButton("download_data", "Télécharger la base fusionnée")
    })
  }
  else {}
})

output$download_data <- downloadHandler(
  filename = function() {
    paste("wb-data", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(result_merged(), file)
  }
)