result_merged <- reactive({
  req(input$to_merge)
  tmp <- map(input$to_merge, ~{tables[[.x]]()})
  data <- reduce(tmp, full_join, by = c("ISO Code", "Country", "Year", "Region")) %>%
    arrange("ISO Code", "Country", "Year") %>%
    select("ISO Code", "Country", "Year", "Region", everything()) %>%
    mutate_if(is.numeric, round,  3)
})

observeEvent(input$apply_merge, {
  output$data_merged <- renderDataTable({
    result_merged()
  })
  
  output$download_ui <- renderUI({
    downloadButton("download_data", "Download the full dataset")
  })
})

output$download_data <- downloadHandler(
  filename = function() {
    paste("wb-data-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(result_merged(), file)
  }
)