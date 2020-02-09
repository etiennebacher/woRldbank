library(shiny)
library(shinythemes)
library(shinyWidgets)
library(WDI)
library(DT)
library(dplyr)
library(shinysky)

fusionTab <- function(id){
  ns <- NS(id)

  tabPanel(title = introBox("Fusion et exportation",
                            data.step = 4,
                            data.intro = "Une fois que vous avez importé toutes les bases souhaitées, vous pouvez les fusionner puis afficher la base complète dans cet onglet. Il sera également possible de la télécharger."),
           value = "Fusion et exportation",
           fluidRow(
             sidebarPanel(
               checkboxGroupInput("to_merge",
                                  label = strong("Bases à fusionner"),
                                  choices = NULL),
               actionButton("tout_select", strong("Tout sélectionner / déselectionner")),
               br(),
               br(),
               actionButton("apply_merge", strong("Fusionner")),
               br(),
               br(),
               uiOutput("download_ui")
               ),
             ## PRESENTATION ALTERNATIVE
             # dropdownButton(
             #   tags$h3("Fusionner les bases"),
             #   selectInput(inputId = ns("choix_fusion"),
             #               label = "Bases à fusionner :",
             #               choices = c("Base 1", "Base 2"),
             #               selected = "Base 1",
             #               multiple = TRUE),
             #   circle = TRUE, status = "primary",
             #   icon = icon("gear"),
             #   width = "300px",
             #   tooltip = tooltipOptions(title = "Outils")
             # ),
             mainPanel(
               column(12,
                      dataTableOutput("data_merged")
                      )
               )
  )
  )
}