library(shiny)
library(shinythemes)
library(shinyWidgets)
library(WDI)
library(DT)
library(dplyr)
library(shinysky)

presentationTab <- function(id){
  ns <- NS(id)
  tabPanel(title = introBox(icon("book"), "Presentation",
                            data.step = 1,
                            data.intro = "This first tab explains the interest of this application. It only is a descriptive tab: all the actions are to be made in the other tabs."),
           fluidRow(
             column(width = 2),
             column(width = 8, 
                    wellPanel(
                    h5("For applied economists (and likely for many other people), collecting and cleaning the data is very time-consuming. This Shiny app aims to reduce this issue by making it easier to collect and aggregate data from the World Development Indicators (WDI), some of the datasets provided by the World Bank. Indeed, it allows to import a unlimited number of WDI datasets and to use them more easily. You will see the details in the other tabs but we can summary the workflow with this app as following:",
                       br(),
                       tags$ul(
                         tags$li("generate a new tab"),
                         tags$li("write the ID of the data, give a name to the variable and import it"),
                         tags$li("once it is imported, choose the type of data you want and then choose the countries and time period"),
                         tags$li("choose whether you want the logarithm, squared and/or lagged value"),
                         tags$li("you may generate a plot and download it"),
                         tags$li("if you're done, go to the tab", strong("Merge and download"), "and download your data; if not, generate a new tab and repeat the same steps.")
                         ),
                    br(),
                  "I was able to build this Shiny application thanks to the previous work of", tags$a(href="http://arelbundock.com/", "Vincent Arel-Bundock"), "who made the", tags$a(href="https://cran.r-project.org/web/packages/WDI/WDI.pdf", "WDI package."), "This package allows to import the World Development Indicators with their ID and automatically reshapes them in the long format (as opposed to the wide format), which is more suited for econometrics.",
                  br(),
                  br(),
                  "The use of this application is quite intuitive and is explained in the introduction that you may have seen before this tab. If you have mistakenly closed the popup asking if you want to see the introduction (or if you have forgotten some info), you can launch it again with the tab", strong("Run the introduction"), "in the dropdown", strong("Other."),
                  br(),
                  br(),
                  "You can see the code of this application by clicking on", strong("View the code on GitHub,"), "also in the dropdown", strong("Other"), ".",
                       style = "line-height: 1.8;
                       text-align: justify;")
                    )),
             column(width = 2)
             ),
           value = "Presentation"
  )
}