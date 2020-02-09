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
                    h4("For economists (and likely for many other people), data gathering and cleaning are very time-consuming. This Shiny app aims to reduce this issue by making it easier to collect data and aggregate data from the World Development Indicators (WDI), the datasets provided by the World Bank. Indeed, it allows to import a unlimited number of WDI datasets and to use them more easily. You will see the details in the other tabs but we can summary the aim of this app with the following points: import datasets, determine their characteristics, plot them and finally, merge them. This allows to have a complete (and downloadable) dataset ready to be used for econometrics for example.")
                    )),
           value = "Presentation"
  )

}