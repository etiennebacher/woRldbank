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
                     strong("Import the dataset"),
                     icon = icon("file-import")),
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
                       choices = " ",
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
      conditionalPanel(
        condition = "input.data_type == 'Time series' || input.data_type == 'Panel data'",
        ns = ns,
        checkboxInput(
          inputId = ns("lagged"),
          label = "Compute the one-period lagged value"
          )
      ),
      br(),
      actionButton(
        ns("make_plot"), 
        "Generate a plot",
        icon = icon("chart-bar")),
      width = 3
    ),
    mainPanel(
      column(width = 12,
             busyIndicator(text = "Importing data from the WDI...", 
                           wait = 0),
             column(4),
             column(4, 
                    conditionalPanel(condition = "input.import", 
                                     ns = ns,
                                     actionButton(ns("show_code"), 
                                                  "Show the R code to reproduce 
                                                  the table and the graph",
                                                  icon = icon("code")))),
             column(4),
             br(),
             br(),
             br(),
             br(),
             dataTableOutput(ns("data_imported_tab")),
             bsModal(ns("modal_plot"), "Graphical representation", 
                     trigger = ns("make_plot"),
                     plotOutput(ns("plot")),
                     selectInput(ns("graph_style"), 
                                 "Style of the graph", 
                                 choices = c("R", "Stata"),
                                 multiple = FALSE,
                                 selected = "R"),
                     downloadButton(ns("download_plot"), 
                                    label = "Download the plot")),
             # bsModal(ns("modal_code"), "R code to reproduce the table and the graph", 
             #         trigger = ns("show_code"),
             #         em("Note: The code to reproduce the graph will 
             #            be displayed only if the graph is downloaded."),
             #         br(),
             #         br(),
             #         verbatimTextOutput(ns("rep_code")))
    ),
    width = 9)
  )
  )
}
