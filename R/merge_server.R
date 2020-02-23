result_merged <- reactive({
  req(input$to_merge)
  tmp <- map(input$to_merge, ~{tables[[.x]]()})
  data <- reduce(tmp, full_join, by = c("ISO_Code", "Country", "Region", "Year")) %>%
    complete(nesting(ISO_Code, Country, Region), Year) %>% 
    fill(ISO_Code, Region, .direction = "down") %>%
    ungroup() %>%
    arrange(ISO_Code, Country, Year) %>%
    select(ISO_Code, Country, Year, Region, everything()) %>%
    mutate_if(is.numeric, round,  3)
  data
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

# observeEvent(input$show_full_code, {
#   displayCodeModal(
#     expandChain(
#       paste0("x", count$value, "_data1")
#       
#     ), 
#     title = "R code to reproduce the table and the graph",
#     footer = tagList(),
#     size = "l",
#     easyClose = TRUE
#   )
# })

# #### "Merge" the single code modals in one big list object
# my_data <- reactive({
#   req(count$value)
# 
#   my_set <- 1:count$value
# 
#   ### lapply through the different name spaces so all are captured ###
#   final <- lapply(my_set, function(x){
#     temp <- callModule(newTab_server, paste0("tab", x))
#     return(temp[[2]])
#   })
# 
#   return(final)
# })
# 
# 
# observeEvent(input$show_full_code, {
# 
#   showModal(modalDialog(
#     renderPrint({
#       ### print the output of the combined module results
#       temp <- sapply(unlist(my_data()), function(x){
#         print(x)
#       })
#     })
#   ))
# })