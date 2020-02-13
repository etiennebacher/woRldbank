library(shiny)
library(shinythemes)
library(shinyWidgets)
library(WDI)
library(DT)
library(dplyr)
library(shinysky)

mergeTab <- function(id){
  ns <- NS(id)

  tabPanel(title = introBox("Merge and download",
                            data.step = 4,
                            data.intro = "Once you have imported all the datasets you wanted, you can merge them and display the full dataset in this tab. This merged dataset is also downloadable."),
           value = "Merge and download",
           fluidRow(
             sidebarPanel(
               checkboxGroupInput("to_merge",
                                  label = strong("Datasets to merge"),
                                  choices = NULL),
               actionButton("select_all", strong("Select all / Unselect all")),
               br(),
               br(),
               actionButton("apply_merge", strong("Merge")),
               br(),
               br(),
               uiOutput("download_ui")
               ),
             mainPanel(
               column(12,
                      dataTableOutput("data_merged")
                      )
               )
  )
  )
}