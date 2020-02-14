# All IDs of the WDI
all_ID_WDI <- read.csv(here("all_ID_WDI.csv"))

newTab_server <- function(input, output, session){
  
  data_wb <- eventReactive(input$import, {
    validate(
      need(input$wdi_id %in% all_ID_WDI$x, 
           "Please specify the ID of one of the World Development Indicators")
      )
    validate(
      need(input$new_name != "", "Please give a name to the variable")
    )
    tmp <- as.data.frame(WDI(country = "all", indicator = input$wdi_id, extra = TRUE))
    colnames(tmp) <- c("iso2c", "Country", input$new_name, "Year", "ISO Code", "Region", "capital", "longitude", "latitude", "income", "lending")
    tmp %>%
      select("ISO Code", "Country", "Region", "Year", input$new_name)
  })
  
  ### INPUTS PAYS ET ANNEES
  observe({
    data_wb <- data_wb()
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
  
  data_reac <- reactive({
    data_wb  <- data_wb()
    log_name <- paste0('ln(', input$new_name, ')')
    square_name <- paste0(input$new_name, '^2')
    
    ## Three types of data: cross-sectional data, time series and panel data. In each of these three situations, we detail all possible combinations of logarithm and squared so that the order in which the checkboxes are ticked does not change anything.
    
    if(input$data_type == "Cross-sectional data"){
      data_wb <- data_wb  %>%
        filter(Country %in% input$country & Year %in% input$year) %>%
        arrange(Country, Year)
      
      if(input$logarithm){
        if(input$squared){
          data_wb <- data_wb %>%
            mutate(!!log_name := log(data_wb[, input$new_name])) %>%
            mutate(!!all_of(square_name) := data_wb[, input$new_name]^2) %>%
            select("ISO Code", "Country", "Region", "Year", input$new_name, all_of(square_name), all_of(log_name))
          
          data_wb
        }
        else {
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$new_name])) %>%
            select("ISO Code", "Country", "Region", "Year", input$new_name, all_of(log_name))
          
          data_wb
        }
      }
      else if (input$squared){
        if (input$logarithm){
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$new_name])) %>%
            mutate(!!all_of(square_name) := data_wb[, input$new_name]^2) %>%
            select("ISO Code", "Country", "Region", "Year", input$new_name, all_of(square_name), all_of(log_name))
          
          data_wb
        }
        else {
          data_wb <- data_wb %>%
            mutate(!!all_of(square_name) := data_wb[, input$new_name]^2) %>%
            select("ISO Code", "Country", "Region", "Year", input$new_name, all_of(square_name))
          
          data_wb
        }
      }
      else {data_wb}
    }
    else if(input$data_type == "Time series"){
      data_wb <- data_wb  %>%
        filter(Country %in% input$country & 
                 Year >= input$year2[[1]] &
                 Year <= input$year2[[2]]) %>%
        arrange(Country, Year)
      
      if(input$logarithm) {
        if (input$squared){
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$new_name])) %>%
            mutate(!!all_of(square_name) := data_wb[, input$new_name]^2) %>%
            select("ISO Code", "Country", "Region", "Year", input$new_name, all_of(square_name), all_of(log_name))
          
          data_wb
        }
        else {
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$new_name])) %>%
            select("ISO Code", "Country", "Region", "Year", input$new_name, all_of(log_name))
          
          data_wb
        }
      }
      else if (input$squared){
        if (input$logarithm){
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$new_name])) %>%
            mutate(!!all_of(square_name) := data_wb[, input$new_name]^2) %>%
            select("ISO Code", "Country", "Region", "Year", input$new_name, all_of(square_name), all_of(log_name))
          
          data_wb
        }
        else {
          data_wb <- data_wb %>%
            mutate(!!all_of(square_name) := data_wb[, input$new_name]^2) %>%
            select("ISO Code", "Country", "Region", "Year", input$new_name, all_of(square_name))
          
          data_wb
        }
      }
      else {data_wb}
    }
    else if(input$data_type == "Panel data"){
      data_wb <- data_wb  %>%
        filter(Country %in% input$country & 
                 Year >= input$year2[[1]] &
                 Year <= input$year2[[2]]) %>%
        arrange(Country, Year)
      
      if(input$logarithm) {
        if (input$squared){
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$new_name])) %>%
            mutate(!!all_of(square_name) := data_wb[, input$new_name]^2) %>%
            select("ISO Code", "Country", "Region", "Year", input$new_name, all_of(square_name), all_of(log_name))
          
          data_wb
        }
        else {
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$new_name])) %>%
            select("ISO Code", "Country", "Region", "Year", input$new_name, all_of(log_name))
          
          data_wb
        }
      }
      else if (input$squared){
        if (input$logarithm) {
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$new_name])) %>%
            mutate(!!all_of(square_name) := data_wb[, input$new_name]^2) %>%
            select("ISO Code", "Country", "Region", "Year", input$new_name, all_of(square_name), all_of(log_name))
          
          data_wb
        } 
        else {
          data_wb <- data_wb %>%
            mutate(!!all_of(square_name) := data_wb[, input$new_name]^2) %>%
            select("ISO Code", "Country", "Region", "Year", input$new_name, all_of(square_name))
          
          data_wb
        }
      }
      else {data_wb}
    }
    
    
  })
  
  data_reac2 <- reactive({
    lagged_name <- paste0(input$new_name, '_lagged')
    data_reac <- data_reac()
    if (input$lagged){
      data_reac2 <- data_reac %>%
        group_by(Country) %>%
        mutate(!!lagged_name := dplyr::lag(!!sym(input$new_name)))
    }
    else{
      data_reac2 <- data_reac
    }
  })
  
  ### DISPLAYED DATASET
  output$data_imported_tab  <- renderDataTable({
    data_reac2()
  },
  options = list(pageLength = 999999)
  )

  ### PLOT
  plot_dataset <- reactive({
    req(input$data_type)
    df <- data_reac()
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
  
  observeEvent(input$make_plot, {
    output$plot <- renderPlot({
      plot_dataset()
    })
  })
  
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
  
  return(data_reac)

}
