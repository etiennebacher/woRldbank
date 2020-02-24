library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(shinysky)
library(shinymeta)
library(shinyAce)
library(rintrojs)
library(clipr)
library(WDI)
library(dplyr)
library(tidyr)
library(tidyselect)
library(ggplot2)
library(ggthemes)
library(purrr)
library(here)

source(here("popup_func.R"))
source(here("popup_ui.R"))
source(here("presentation.R"))
source(here("newTab_ui.R"))
source(here("newTab_server.R"))
source(here("merge_ui.R"))

### Comportement bizarre : à partir de 4-5 onglets, l'ordre des cases dans "Fusion" devient complètement aléatoire (?)
### Cliquer plusieurs fois de suite sur l'onglet "Less" ne supprime pas plusieurs tables à la suite, il faut cliquer sur un autre onglet entre chaque clic sur "Less"
### corrélation entre deux variables (nouvel onglet ?)
### un truc "select all" dans l'input pour choisir les pays + continents
### faire une case pour faire des moyennes sur 5 ans
### il faut que supprimer un onglet supprime aussi la case correspondante dans l'onglet fusion


############################## BEGINNING OF UI ############################## 
#############################################################################

ui <- navbarPage(theme = shinytheme("cerulean"), 
                 id = "tabs",
                 title = div(img(src="hex-woRldbank.png", height = '50px', width = '50px'), 
                             strong(em("woRldbank"), ": use WDI for econometrics"),
                             style = "position: relative; top: 50%; transform: translateY(-50%);"),
                 position = "static-top",

                 # popup
                 tabPanel(title = "",
                          introjsUI(),
                          popup,
                          value = "for_popup"),
                 
                 ##### TAB PRESENTATION #####
                 presentationTab(1),

                 ##### TAB MERGER #####
                 mergeTab(1),
                 
                 ##### TABS "MORE" AND "LESS" #####
                 tabPanel(
                  title = introBox(icon("plus"), "More",
                                   data.step = 2,
                                   data.intro = "Firstly, click on this button: it will create a new tab in which you can import a dataset from the World Development Indicators and define its characteristics. You may also generate a plot that represents the data you have chosen. You can create as many tabs as you want. Finally, the code to reproduce the plot and the table will be available and live-updated according to your modifications."),
                  value = "More"
                ), 
                 
                tabPanel(
                  title = introBox(icon("minus"), "Less",
                                   data.step = 3,
                                   data.intro = "If you have created too many tabs, you can delete the lastly created ones by clicking on this button."),
                  value = "Less"
                ),
                
                ##### TAB OTHER #####
                navbarMenu(title = "Other",
                           tabPanel("Run the introduction",
                                    value = "rerun"),
                           "----",
                           tabPanel(tagList(
                             a("View the code on GitHub", 
                               href = "https://github.com/etiennebacher/woRldbank"))
                             )
                           ),
                
                ##### FOOTER #####
                tags$footer(
                    h5(
                      "Created by", tags$a(href="https://github.com/etiennebacher", "Etienne Bacher"),
                      style = "
                       position:fixed;
                       text-align:center;
                       left: 0;
                       bottom: 0;
                       width:100%;
                       z-index:1000;  
                       height:30px; 
                       color: black;
                       padding: 10px;
                       background-color: #f5f5f5"
                    )
                  )
)

############################## END OF UI #################################### 
#############################################################################

######################### BEGINNING OF SERVER ############################### 
#############################################################################

server <- function(input, output, session) {

  # adding "local = TRUE" is mandatory
  source(here("intro_choice.R"), local = TRUE)
  source(here("intro_rerun.R"), local = TRUE)
    
  count <- reactiveValues(value = 0)
  tables <- reactiveValues()
  
  observeEvent(input$tabs, {
    if (input$tabs == "More"){
      count$value <- count$value + 1
      name <- paste0("Dataset ", count$value)
      insertTab(inputId = "tabs",
                tabPanel(title = name,
                         newTab_ui(paste0("tab", count$value)), 
                         value = paste0("value", count$value)
                         ), 
                target = "Merge and download", 
                position = "before",
                select = TRUE)
      
      x <- callModule(newTab_server, paste0("tab", count$value))
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
  
  source(here("merge_server.R"), local = TRUE)
  

}

############################ END OF SERVER ################################## 
#############################################################################

shinyApp(ui = ui, server = server)
