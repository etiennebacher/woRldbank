newTab_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      sidebarPanel(
        tags$h3(strong("Dataset to import")),
        textInput(ns("wdi_id"),
                  "ID of the World Development Indicator to import:",
                  placeholder = "Ex: NV.AGR.TOTL.ZS"),
        textInput(ns("new_name"), 
                  label = "Give a name to the variable:", 
                  placeholder = "Ex: Agriculture"),
        actionButton(ns("import"), 
                     strong("Import the dataset")),
        br(),
        
        tags$h3(strong("Choices")),
        
        ## Type of data
        selectInput(
          inputId = ns("data_type"),
          label = "Type of data:",
          choices = c("Cross-sectional data",
                      "Time series",
                      "Panel data"),
          selected = "Cross-sectional data",
          multiple = FALSE),
        
        ## Country choice
        selectizeInput(inputId = ns("country"), 
                       label = "Select the countries you want:", 
                       choices = "",
                       multiple = TRUE),
        
        ## Year choice
        conditionalPanel(
          condition = "input.data_type == 'Cross-sectional data'",
          ns = ns,
          selectizeInput(inputId = ns("year"), 
                         label = "Choose the year you want:", 
                         choices = "",
                         multiple = FALSE)
        ),
        conditionalPanel(
          condition = "input.data_type == 'Time series' || input.data_type == 'Panel data'",
          ns = ns,
          sliderInput(inputId = ns("year2"), 
                      label = "Choose the years you want:",
                      min = 1,
                      max = 2,
                      value = c(1, 2),
                      sep = "")
      ),
      
      ## Additional choices
      checkboxInput(
        inputId = ns("logarithm"), 
        label = "Compute the logarithm"),
      checkboxInput(
        inputId = ns("squared"), 
        label = "Compute the squared value"),
      br(),
      actionButton(
        ns("make_plot"), 
        "Generate a plot"),
      width = 3
    ),
    mainPanel(
      column(width = 12,
             busyIndicator(text = "Importing data from the WDI...", 
                           wait = 0),
             dataTableOutput(ns("data_imported_tab")),
             bsModal(ns("modal_plot"), "Graphical representation", 
                     trigger = ns("make_plot"),
                     plotOutput(ns("plot")),
                     checkboxInput(ns("stata_style"), "Apply Stata style"),
                     downloadButton(ns("download_plot"), label = "Download the plot"))
    ),
    width = 9)
  )
  )
}
