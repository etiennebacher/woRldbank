# All IDs of the WDI
all_ID_WDI <- read.csv(here("all_ID_WDI.csv"))

newTab_server <- function(input, output, session){
  
  
  #### DEFINE THE DATASET IMPORTED FOR THE FIRST TIME ####
  #### This contains the "validate" messages, the importation of the base and the selection of columns. The two last things are embedded in a metaExpr to create the code to reproduce them.
  data_wb1 <- metaReactive2({
    req(input$import)
    validate(
      need(input$wdi_id %in% all_ID_WDI$x, 
           "Please specify the ID of one of the World Development Indicators")
      )
    validate(
      need(input$new_name != "", 
           "Please give a name to the variable")
    )
    isolate(metaExpr({
      tmp <- as.data.frame(WDI(country = "all", indicator = ..(input$wdi_id), extra = TRUE))
      colnames(tmp) <- c("iso2c", "Country", ..(input$new_name), 
                         "Year", "ISO Code", "Region", "capital", 
                         "longitude", "latitude", "income", "lending")
      tmp %>%
        select("ISO Code", "Country", "Region", "Year", ..(input$new_name))
    }))
  })
  
  
  #### BUTTON TO REPRODUCE THE TABLE SHOULD APPEAR ONLY IF THE TABLE IS NOT NULL ####
  # observe({
  #   if(!is.null(data_wb1())){
  #     output$new_button_code <- renderUI({
  #       actionButton(session$ns("show_code"),
  #                    "Show the code to reproduce this table on R",
  #                    width = '100%')
  #     })
  #   }
  # })
  
  ### INPUTS YEARS AND COUNTRIES ACCORDING TO THE TYPE OF DATA SELECTED ####
  observe({
    data_wb <- data_wb1()
    if(input$data_type == "Cross-sectional data"){
      updateSelectizeInput(session, 
                           inputId = "country", 
                           choices = split(data_wb$Country, data_wb$Region), 
                           selected = NULL)
      updateSelectizeInput(session, 
                           inputId = "year", 
                           choices = sort(unique(data_wb$Year)), 
                           selected = NULL,
                           options = list(maxItems = 1))
    }
    else if(input$data_type == "Time series"){
      updateSelectizeInput(session, 
                           inputId = "country", 
                           choices = split(data_wb$Country, data_wb$Region),
                           selected = NULL,
                           options = list(maxItems = 1))
      updateSliderInput(session,
                        inputId = "year2",
                        min = min(data_wb$Year),
                        max = max(data_wb$Year),
                        value = c(min(data_wb$Year), 
                                  max(data_wb$Year)),
                        step = 1
      )
    }
    else if (input$data_type == "Panel data"){
      updateSelectizeInput(session, 
                           inputId = "country", 
                           choices = split(data_wb$Country, data_wb$Region),
                           selected = NULL,
                           options = list(maxItems = 999999))
      updateSliderInput(session,
                        inputId = "year2",
                        min = min(data_wb$Year),
                        max = max(data_wb$Year),
                        value = c(min(data_wb$Year), 
                                  max(data_wb$Year)),
                        step = 1
      )
    }
  })
  
  
  #### APPLY THESE INPUTS TO THE TABLE IMPORTED ####
  # data_reac1 <- metaReactive2({
  data_reac1 <- reactive({
      data_wb  <- data_wb1()

    ## Three types of data: cross-sectional data, time series and panel data. 
    
    if(input$data_type == "Cross-sectional data"){
      data_wb  %>%
        filter(Country %in% input$country & Year %in% input$year) %>%
        arrange(Country, Year)
    }
    else if(input$data_type == "Time series"){
      data_wb  %>%
        filter(Country %in% input$country & 
                 Year >= input$year2[[1]] &
                 Year <= input$year2[[2]]) %>%
        arrange(Country, Year)
    }
    else if(input$data_type == "Panel data"){
      data_wb  %>%
        filter(Country %in% input$country & 
                 Year >= input$year2[[1]] &
                 Year <= input$year2[[2]]) %>%
        arrange(Country, Year)
      }
    
  })
  
  
  #### GENERATE THE LOGARITHM OF THE VARIABLE ####
  data_reac2 <- reactive({
    log_name <- paste0('ln(', input$new_name, ')')
    if (input$logarithm){
      data_reac1() %>%
        group_by(Country) %>%
        mutate(!!log_name := log(!!sym(input$new_name)))
    }
    else{
      data_reac1()
    }
  })
  
  #### GENERATE THE SQUARED VARIABLE ####
  data_reac3 <- reactive({
    square_name <- paste0(input$new_name, '^2')
    if (input$squared){
      data_reac2() %>%
        group_by(Country) %>%
        mutate(!!square_name := (!!sym(input$new_name))^2)
    }
    else{
      data_reac2()
    }
  })
  
  #### GENERATE THE LAGGED VARIABLE ####
  data_reac4 <- reactive({
    lagged_name <- paste0(input$new_name, '_lagged')
    if (input$lagged){
      data_reac3() %>%
        group_by(Country) %>%
        mutate(!!lagged_name := dplyr::lag(!!sym(input$new_name)))
    }
    else{
      data_reac3()
    }
  })
  
  #### DISPLAYED DATASET ####
  output$data_imported_tab  <- renderDataTable({
    data_reac4()
  },
  options = list(pageLength = 999999)
  )


  #### CREATE THE PLOT ####
  plot_dataset <- reactive({
    req(input$data_type)
    df <- data_reac1()
    if (input$data_type == "Cross-sectional data"){
      if (input$stata_style) {
        ggplot(data = df, aes(x = df[[input$new_name]])) +
          geom_line(stat = "density") +
          geom_vline(aes(xintercept = mean(df[[input$new_name]], na.rm = TRUE),
                         color = "Mean"),
                     linetype = "dashed",
                     size = 0.6) +
          labs(x = input$new_name,
               title = paste0(input$new_name, "'s density")) +
          scale_color_manual(name = "Legend", values = c(Mean = "black")) +
          theme_stata()
      }
      else {
        ggplot(data = df, aes(x = df[[input$new_name]])) +
          geom_line(stat = "density") +
          geom_vline(aes(xintercept = mean(df[[input$new_name]], na.rm = TRUE),
                         color = "Mean"),
                     linetype = "dashed",
                     size = 0.6) +
          labs(x = input$new_name,
               title = paste0(input$new_name, "'s density")) +
          scale_color_manual(name = "Legend", values = c(Mean = "black")) +
          theme_clean()
        
      }
    }
    else if (input$data_type == "Time series") {
      if (input$stata_style) {
        ggplot(data = df, aes(x = Year, y = df[[input$new_name]])) +
          geom_line() +
          labs(
            y = input$new_name,
            title = paste0(
              input$new_name,
              "'s evolution between ",
              min(df$Year),
              " and ",
              max(df$Year),
              ": ",
              unique(df$Country)
            )
          ) +
          theme_stata()
      }
      else {
        ggplot(data = df, aes(x = Year, y = df[[input$new_name]])) +
          geom_line() +
          labs(
            y = input$new_name,
            title = paste0(
              input$new_name,
              "'s evolution between ",
              min(df$Year),
              " and ",
              max(df$Year),
              ": ",
              unique(df$Country)
            )
          ) +
          theme_clean()
      }
    }
    else if (input$data_type == "Panel data"){
      if (input$stata_style) {
        ggplot(data = df, aes(
          x = Year,
          y = df[[input$new_name]],
          group = Country,
          color = Country
        )) +
          geom_line() +
          labs(
            y = input$new_name,
            title = paste0(
              input$new_name,
              "'s evolution between ",
              min(df$Year),
              " and ",
              max(df$Year)
            )
          ) +
          theme_stata()
      }
      else {
        ggplot(data = df, aes(
          x = Year,
          y = df[[input$new_name]],
          group = Country,
          color = Country
        )) +
          geom_line() +
          labs(
            y = input$new_name,
            title = paste0(
              input$new_name,
              "'s evolution between ",
              min(df$Year),
              " and ",
              max(df$Year)
            )
          ) +
          theme_clean()
      }
    }
    else {x <- NULL}
  })
  
  #### SHOW THE PLOT IF THE BUTTON IS CLICKED ####
  observeEvent(input$make_plot, {
    output$plot <- renderPlot({
      plot_dataset()
    })
  })
  
  #### DOWNLOAD THE MODIFIED TABLE ####
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("wb-plot-", input$new_name, ".png", sep = "")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot_dataset(), device = device)
    }
  )
  
  #### SHOW THE CODE TO REPRODUCE THE TABLE ####
  observeEvent(input$show_code, {
    output$rep_code <- renderPrint({
        expandChain(data_wb1()
                    # , data_reac1()
                    )
      })
  })
  
  return(data_reac1)

}
