library(shiny)
library(shinythemes)
library(shinyWidgets)
library(WDI)
library(DT)
library(dplyr)
library(shinysky)

presentationTab <- function(id){
  ns <- NS(id)
  tabPanel(title = introBox("Presentation",
                            data.step = 1,
                            data.intro = "This first tab explains the interest of this application. It only is a descriptive tab: all the actions are to be made in the other tabs."),
           fluidRow(
             column(width = 12, 
                    wellPanel(
                    h5("For economists (and likely for many other people), data gathering and cleaning is very time-consuming. This Shiny app aims to reduce this issue by making it easier to collect data and aggregate data from the World Development Indicators (WDI), the datasets provided by the World Bank. Indeed, it allows to import a unlimited number of WDI datasets and to use them more easily. You will see the details in the other tabs but we can summary the aim of this app with the following points: import datasets, determine their characteristics, plot them and finally, merge them. This allows to have a complete (and downloadable) dataset ready to be used for econometrics for example.",
                    br(),
                    br(),
                  "I was able to build this Shiny application thanks to the previous work of", tags$a(href="http://arelbundock.com/", "Vincent Arel-Bundock"), "who built the", tags$a(href="https://cran.r-project.org/web/packages/WDI/WDI.pdf", "WDI package"), ". This package allows to import the World Development Indicators with their ID. The use of this application is quite intuitive and is explained in the intro that you may have seen before this tab. If you have mistakenly closed the popup asking if you want to see the intro (or if you have forgotten some info), you can launch it again with the tab", strong("Run the presentation"), "in the dropdown", strong("Other"), ".",
                  br(),
                  br(),
                  "You may also see the code of this application by clicking on", strong("View the code on GitHub"), ", also in the dropdown", strong("Other"), ".",
                       style = "line-height: 1.5;
                       text-align: justify;")
                    ))),
           value = "Presentation"
  )
  

}