library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(WDI)
library(DT)
library(dplyr)
library(shinysky)
library(tidyselect)
library(ggplot2)
library(ggthemes)
library(purrr)
library(here)
library(rintrojs)

source(here("popup_func.R"))
source(here("popup_ui.R"))
source(here("presentation.R"))
source(here("newTab_ui.R"))
source(here("newTab_server.R"))
source(here("fusion.R"))


### Comportement bizarre : à partir de 4-5 onglets, l'ordre des cases dans "Fusion" devient complètement aléatoire (?)
### Cliquer plusieurs fois de suite sur l'onglet "Less" ne supprime pas plusieurs tables à la suite, il faut cliquer sur un autre onglet entre chaque clic sur "Less"
### Faire un bouton pour créer une variable en t-1
### corrélation entre deux variables (nouvel onglet ?)
### un truc "select all" dans l'input pour choisir les pays + continents
### customiser l'onglet vide pour en faire un lanceur de la présentation pour pouvoir la faire tourner quand on veut
### mettre deux boutons au dessus du texte dans "Presentation": un pour aller voir le code sur github, l'autre pour relancer la présentation -> OU JUSTE UN MENU DEROULANT ??


##### BEGINNING OF UI #####

ui <- navbarPage(theme = shinytheme("sandstone"), 
                 id = "tabs",
                 title = strong(em("woRldbank"), ": facilitate the use of World Bank data"),
                 position = "static-top",
                 # setBackgroundColor(color = "Azure"),

                 # popup
                 tabPanel(title = "",
                          introjsUI(),
                          popup,
                          value = "for_popup"),
                 
                 ##### ONGLET PRESENTATION #####
                 presentationTab(1),

                 ##### ONGLET FUSION #####
                 fusionTab(1),
                 
                 ##### ONGLETS PLUS ET MOINS #####
                 tabPanel(
                  title = introBox(icon("plus"), "More",
                                   data.step = 2,
                                   data.intro = "Firstly, click on this button: it will create a new tab in which you can import a dataset from the World Development Indicators and define its characteristics. You can create as many as you want."),
                  value = "More"
                ), 
                 
                tabPanel(
                  title = introBox(icon("minus"), "Less",
                                   data.step = 3,
                                   data.intro = "If you have created too many tabs, you can delete the lastly created ones by clicking on this button."),
                  value = "Less"
                )
)
##### END OF UI #####


server <- function(input, output, session) {

  # adding "local = TRUE" is mandatory
  source(here("choice_pres.R"), local = TRUE)
    
  count <- reactiveValues(value = 0)
  tables <- reactiveValues()
  
  observeEvent(input$tabs, {
    if (input$tabs == "More"){
      count$value <- count$value + 1
      name = paste0("Dataset ", count$value)
      insertTab(inputId = "tabs",
                tabPanel(title = name,
                         newTab_ui(count$value), 
                         value = paste0("value", count$value)
                         ), 
                target = "Fusion and exportation", 
                position = "before",
                select = TRUE)
      
      x <- callModule(newTab_server, count$value)
      tables[[name]] <- x
    }
      
    else if (input$tabs == "Less") {
      removeTab(inputId = "tabs", target = paste0("value", count$value))
      count$value <- count$value - 1
    }
    
  })
  
  observe({
    updateCheckboxGroupInput(session = session,
                             inputId = "to_merge",
                             choices = names(tables),
                             selected = names(tables))
  })
  
  observe({
    if (input$select_all > 0) {
      if (input$select_all %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="to_merge",
                                 choices = names(tables),
                                 selected = names(tables))
        
      } else {
        updateCheckboxGroupInput(session=session, 
                                 inputId="to_merge",
                                 choices = names(tables),
                                 selected = "")
        
      }}
  })
  
  source(here("merge&dl.R"), local = TRUE)
  

}


shinyApp(ui = ui, server = server)
