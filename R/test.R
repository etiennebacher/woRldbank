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
### Cliquer plusieurs fois de suite sur l'onglet "Moins" ne supprime pas plusieurs tables à la suite, il faut cliquer sur un autre onglet entre chaque clic sur "Moins"
### Faire un bouton pour créer une variable en t-1
### graphiques dans popup -> mettre une légende pour geom_vline
### corrélation entre deux variables (nouvel onglet ?)


ui <- navbarPage(theme = shinytheme("sandstone"), 
                 id = "tabs",
                 title = strong("Traitement des données de la Banque Mondiale"),
                 position = "static-top",
                 # setBackgroundColor(color = "Azure"),

                 # popup
                 tabPanel("",
                          introjsUI(),
                          popup),
                 
                 ##### ONGLET PRESENTATION #####
                 presentationTab(1),

                 ##### ONGLET FUSION #####
                 fusionTab(1),
                 
                 ##### ONGLETS PLUS ET MOINS #####
                 tabPanel(
                  title = introBox(icon("plus"), "Plus",
                                   data.step = 2,
                                   data.intro = "Pour commencer, appuyez sur ce bouton : il créera un nouvel onglet dans lequel vous pourrez remplir les caractéristiques des données que vous souhaitez importer. Vous pouvez en créer autant que vous le souhaitez."),
                  value = "Plus"
                ), 
                 
                tabPanel(
                  title = introBox(icon("minus"), "Moins",
                                   data.step = 3,
                                   data.intro = "Si vous avez créé trop d'onglets, vous pouvez supprimer les derniers créés en appuyant sur ce bouton."),
                  value = "Moins"
                )
)
##### FIN UI #####


server <- function(input, output, session) {
  
  # ajouter "local = TRUE" obligatoire, causera peut être un problème pour le déploiement
  source(here("choice_pres.R"), local = TRUE)
    
  count <- reactiveValues(value = 0)
  tables <- reactiveValues()
  
  observeEvent(input$tabs, {
    if (input$tabs == "Plus"){
      count$value <- count$value + 1
      name = paste0("Base ", count$value)
      insertTab(inputId = "tabs",
                tabPanel(title = name,
                         newTab_ui(count$value), # juste mettre count$value va faire que les noms des inputs sont les memes entre onglets, sauf le numéro à la fin (1 pour l'onglet 1, 2 pour l'onglet 2, etc.)
                         value = paste0("value", count$value)
                         ), 
                target = "Fusion et exportation", 
                position = "before",
                select = TRUE)
      
      x <- callModule(newTab_server, count$value)
      tables[[name]] <- x
    }
      
    else if (input$tabs == "Moins") {
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
    if (input$tout_select > 0) {
      if (input$tout_select %% 2 == 0){
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
