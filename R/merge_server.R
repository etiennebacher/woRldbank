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


downloadname <- reactive({
  paste0("wb-data-", Sys.Date(), ".", input$format_dl)
})

downloadcontent <- function(){
  if(input$format_dl == "csv"){
    write.csv(result_merged(), file = downloadname())
  }
  else if(input$format_dl == "xlsx"){
    write.xlsx2(result_merged(), file = downloadname())
  }
  else if(input$format_dl == "dta"){
    write_dta(result_merged(), path = downloadname())
  }
  else if(input$format_dl == "sav"){
    write_sav(result_merged(), path = downloadname())
  }
  else if(input$format_dl == "sas7bdat"){
    write_sas(result_merged(), path = downloadname())
  }
}

output$download_data <- downloadHandler(
  filename = downloadname,
  content = function(file){
    downloadcontent()
    file.copy(downloadname(), file, overwrite=T)
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