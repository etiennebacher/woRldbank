library(shiny)
library(shinythemes)
library(shinyWidgets)
library(WDI)
library(DT)
library(dplyr)
library(shinysky)

ui <- navbarPage(theme = shinytheme("flatly"), 
                 title = strong("Traitement des données de la Banque Mondiale"),
                 id = "tabs",
                 position = "static-top",
                 # setBackgroundColor(color = "Azure"),
                 
                 ##### ONGLET PRESENTATION #####
                 tabPanel(title = "Présentation",
                          fluidRow(
                            column(width = 12, 
                                   p("Le traitement de données est très chronophage, que ce soit au niveau de la récupération des données, de leur aggrégation puis de leur traitement. Cette application Shiny a pour but d'importer des bases de données depuis le site de la Banque Mondiale et de les traiter afin de les utiliser ensuite dans des régressions. Il est possible de traiter des données en coupe transversale, série temporelle ou des données de panel."),
                                   br(),
                                   h4(strong("Exemple d'utilisation de cette application")),
                                   br(),
                                   column(width = 3, 
                                          div(img(src = 'pres_1.png', 
                                                  align = "left",
                                                  height = 150, 
                                                  width = 400)),
                                          p(em("Image 1 : onglet 'Base 1'"))
                                   ),
                                   column(width = 1),
                                   column(width = 8,
                                          p("L'image ci-contre montre l'apparence de l'onglet 'Base 1' lorsque l'on ne touche à rien. La roue crantée permet d'afficher des paramètres sur l'importation et sur la manipulation des bases de données de la Banque Mondiale."))
                            ),
                            column(width = 12,
                                   column(width = 2,
                                          div(img(src = 'pres_2.png',
                                                  align = "left", 
                                                  height = 450,
                                                  width = 200)),
                                          p(em("Image 2 : paramètres"))
                                   ),
                                   column(width = 10, 
                                          br(),
                                          br(),
                                          p("On peut diviser ces paramètres en deux catégories : ceux permettant d'importer la base de données depuis le site de la Banque Mondiale et ceux permettant de manipuler plus en profondeur cette base de données."),
                                          br(),
                                          p("Au niveau de l'importation de la base, on doit d'abord spécifier le code type de la Banque Mondiale puis donner un nom à la variable que l'on veut importer. Une fois la base importée, on spécifie le type de données que l'on veut (coupe transversale, série temporelle, données de panel), le(s) pays désiré(s) et l(es)' année(s) désirée(s)."),
                                          br(),
                                          p("On peut également générer une colonne qui vaut le logarithme de la variable importée et une colonne qui vaut son carré, puisque ces deux manipulations sont souvent utilisées en économétrie."))
                            )
                          )
                 ),
                 
                 ##### ONGLET FUSION #####
                 
                 tabPanel(title = "Fusion et exportation",
                          fluidRow(
                                   dropdownButton(
                                     tags$h3("Fusionner les bases"),
                                     selectInput(inputId = "choix_fusion",
                                                 label = "Bases à fusionner :",
                                                 choices = c("Base 1", "Base 2"),
                                                 selected = "Base 1",
                                                 multiple = TRUE),
                                     circle = TRUE, status = "primary",
                                     icon = icon("gear"),
                                     width = "300px",
                                     tooltip = tooltipOptions(title = "Outils")
                                   ),
                                   column(12,
                                          dataTableOutput("data_merged")))
                          )
                 
                 ##### ONGLETS PLUS ET MOINS #####
                 ,
                 tabPanel(title = "Plus",
                          icon = icon("plus")),
                 tabPanel(title = "Moins",
                          icon = icon("minus"))
                 
                 
)
##### FIN UI #####



server <- function(input, output, session) {
  
  ### essai d'une fonction pour écrire une seule fois le code d'importation et traitement
  
  import_and_treat <- function(i){

    ### IMPORTATION
    assign(paste0("data_wb_tab", i), eventReactive(paste0("input$import", i), {
      if(!is.null(paste0("input$nom_base", i))){
        tmp <- as.data.frame(WDI(country = "all", indicator = get(paste0("input$nom_base", i))))
        colnames(tmp) <- c("Code ISO", "Pays", paste0("input$nouveau_nom", i), "Année")
        tmp
      }
      else{}
    }))
    
    ### INPUTS PAYS ET ANNEES
    observe({
      if(paste0("input$choix", i) == "Coupe transversale"){
        paste0("data_wb_tab", i) <- paste0("data_wb_tab", i)()
        updateSelectizeInput(session,
                             inputId = paste0("pays", i),
                             choices = unique(get(paste0("data_wb_tab", i))$Pays),
                             selected = NULL)
        updateSelectizeInput(session,
                             inputId = paste0("année", i),
                             choices = unique(get(paste0("data_wb_tab", i))$Année),
                             selected = NULL,
                             options = list(maxItems = 1))
      }
      else if(input$choix == "Série temporelle"){
        paste0("data_wb_tab", i) <- paste0("data_wb_tab", i)()
        updateSelectizeInput(session,
                             inputId = paste0("pays", i),
                             choices = unique(get(paste0("data_wb_tab", i))$Pays),
                             selected = NULL,
                             options = list(maxItems = 1))
        updateSliderInput(session,
                          inputId = paste0("année2", i),
                          min = min(get(paste0("data_wb_tab", i))$Année),
                          max = max(get(paste0("data_wb_tab", i))$Année),
                          value = c(min(get(paste0("data_wb_tab", i))$Année),
                                    max(get(paste0("data_wb_tab", i))$Année)),
                          step = 1
        )
      }
      else if (input$choix == "Données de panel"){
        paste0("data_wb_tab", i) <- paste0("data_wb_tab", i)()
        updateSelectizeInput(session,
                             inputId = paste0("pays", i),
                             choices = unique(get(paste0("data_wb_tab", i))$Pays),
                             selected = NULL,
                             options = list(maxItems = 999999))
        updateSliderInput(session,
                          inputId = paste0("année2", i),
                          min = min(get(paste0("data_wb_tab", i))$Année),
                          max = max(get(paste0("data_wb_tab", i))$Année),
                          value = c(min(get(paste0("data_wb_tab", i))$Année),
                                    max(get(paste0("data_wb_tab", i))$Année)),
                          step = 1
        )
      }
    })

    ### DONNEES AFFICHEES
    output[[paste0("data_imported_tab", i)]] <- renderDataTable({
      paste0("data_wb", i) <- paste0("data_wb_tab", i)()
      if (!is.null(paste0("data_wb", i))){
      log_name <- paste0('ln(',
                         paste0("input$nouveau_nom", i),
                         ')')
      square_name <- paste0(paste0("input$nouveau_nom", i),
                            '^2')


      ## Trois situations possibles : les données sont en coupe transversale, en série temporelle ou
      ## en données de panel.
      ## Dans chacune de ces trois situations, on détaille toutes les associations possibles
      ## de logarithme et carré, pour que l'ordre dans lequel on coche ne change rien.

      if(paste0("input$choix", i) == "Coupe transversale"){
        paste0("data_wb", i) %>%
          filter(Pays %in% paste0("pays", i)
                 & Année %in% paste0("année", i)) %>%
          arrange(Pays, Année)

        if(input$logarithme){
          if(input$carré){
            paste0("data_wb", i) %>%
              mutate(!!log_name := log(paste0("data_wb", i)
                                       [, paste0("input$nouveau_nom", i)]
              )
              ) %>%
              mutate(!!square_name := paste0("data_wb", i)
                     [, paste0("input$nouveau_nom", i)]^2
              ) %>%
              select("Code ISO",
                     "Pays",
                     "Année",
                     paste0("input$nouveau_nom", i),
                     square_name,
                     log_name)
          }
          else {
            paste0("data_wb", i) %>%
              mutate(!!log_name := log(paste0("data_wb", i)
                                       [, paste0("input$nouveau_nom", i)]
              )
              ) %>%
              select("Code ISO", "Pays", "Année", input$nouveau_nom, log_name)
          }
        }
        else if (input$carré){
          if (input$logarithme){
            paste0("data_wb", i) %>%
              mutate(!!log_name := log(paste0("data_wb", i)
                                       [, paste0("input$nouveau_nom", i)]
              )
              ) %>%
              mutate(!!square_name := paste0("data_wb", i)
                     [, paste0("input$nouveau_nom", i)]^2) %>%
              select("Code ISO",
                     "Pays",
                     "Année",
                     paste0("input$nouveau_nom", i),
                     square_name,
                     log_name)

          }
          else {
            paste0("data_wb", i) %>%
              mutate(!!square_name := paste0("data_wb", i)
                     [, paste0("input$nouveau_nom", i)]^2) %>%
              select("Code ISO",
                     "Pays",
                     "Année",
                     paste0("input$nouveau_nom", i),
                     square_name)
          }
        }
        else {paste0("data_wb", i)}
      }
      else if(paste0("input$choix", i) == "Série temporelle"){
        paste0("data_wb", i) %>%
          filter(Pays %in% paste0("pays", i)  &
                   Année >= paste0("année2", i)[[1]] &
                   Année <= paste0("année2", i)[[2]]) %>%
          arrange(Pays, Année)

        if(input$logarithme) {
          if (input$carré){
            paste0("data_wb", i) %>%
              mutate(!!log_name := log(paste0("data_wb", i)
                                       [, paste0("input$nouveau_nom", i)]
              )
              ) %>%
              mutate(!!square_name := paste0("data_wb", i)
                     [, paste0("input$nouveau_nom", i)]^2) %>%
              select("Code ISO",
                     "Pays",
                     "Année",
                     paste0("input$nouveau_nom", i),
                     square_name,
                     log_name)
          }
          else {
            paste0("data_wb", i) %>%
              mutate(!!log_name := log(paste0("data_wb", i)
                                       [, paste0("input$nouveau_nom", i)]
              )
              ) %>%
              select("Code ISO",
                     "Pays",
                     "Année",
                     paste0("input$nouveau_nom", i),
                     log_name)
          }
        }
        else if (input$carré){
          if (input$logarithme){
            paste0("data_wb", i) %>%
              mutate(!!log_name := log(paste0("data_wb", i)
                                       [, paste0("input$nouveau_nom", i)]
              )
              ) %>%
              mutate(!!square_name := paste0("data_wb", i)
                     [, paste0("input$nouveau_nom", i)]^2) %>%
              select("Code ISO",
                     "Pays",
                     "Année",
                     paste0("input$nouveau_nom", i),
                     square_name,
                     log_name)
          }
          else {
            paste0("data_wb", i) %>%
              mutate(!!square_name := paste0("data_wb", i)
                     [, paste0("input$nouveau_nom", i)]^2) %>%
              select("Code ISO",
                     "Pays",
                     "Année",
                     paste0("input$nouveau_nom", i),
                     square_name)

          }
        }
        else {paste0("data_wb", i)}
      }
      else if(input$choix == "Données de panel"){
        paste0("data_wb", i) %>%
          filter(Pays %in% input$pays &
                   Année >= paste0("année2", i)[[1]] &
                   Année <= paste0("année2", i)[[2]]) %>%
          arrange(Pays, Année)

        if(input$logarithme) {
          if (input$carré){
            paste0("data_wb", i) %>%
              mutate(!!log_name := log(paste0("data_wb", i)
                                       [, paste0("input$nouveau_nom", i)]
              )
              ) %>%
              mutate(!!square_name := paste0("data_wb", i)
                     [, paste0("input$nouveau_nom", i)]^2) %>%
              select("Code ISO",
                     "Pays",
                     "Année",
                     paste0("input$nouveau_nom", i),
                     square_name,
                     log_name)
          }
          else {
            paste0("data_wb", i) %>%
              mutate(!!log_name := log(paste0("data_wb", i)
                                       [, paste0("input$nouveau_nom", i)])) %>%
              select("Code ISO",
                     "Pays",
                     "Année",
                     paste0("input$nouveau_nom", i),
                     log_name)
          }
        }
        else if (input$carré){
          if (input$logarithme) {
            paste0("data_wb", i) %>%
              mutate(!!log_name := log(paste0("data_wb", i)
                                       [, paste0("input$nouveau_nom", i)]
              )
              ) %>%
              mutate(!!square_name := paste0("data_wb", i)
                     [, paste0("input$nouveau_nom", i)]^2) %>%
              select("Code ISO",
                     "Pays",
                     "Année",
                     paste0("input$nouveau_nom", i),
                     square_name,
                     log_name)
          }
          else {
            paste0("data_wb", i) %>%
              mutate(!!square_name := paste0("data_wb", i)
                     [, paste0("input$nouveau_nom", i)]^2) %>%
              select("Code ISO",
                     "Pays",
                     "Année",
                     paste0("input$nouveau_nom", i),
                     square_name)
          }
        }
        else {paste0("data_wb", i)}
      }
    }},
    options = list(pageLength = 999999)
    )
  }
  
  ##### PARTIE INTERACTIVITE DE L'UI ##### 
  
  count <- reactiveValues(value = 0)
  
  observeEvent(input$tabs, {
    if (input$tabs == "Plus"){
      count$value <- count$value + 1
      id = paste0("Base ", count$value)
      insertTab(inputId = "tabs",
                tabPanel(title = id,
                         fluidRow(
                           dropdownButton(
                             tags$h3(strong("Base à importer")),
                             textInput(paste0("nom_base", count$value),
                                       "Code de la base à importer",
                                       placeholder = "Ex : NV.AGR.TOTL.ZS"),
                             textInput(paste0("nouveau_nom", count$value), 
                                       label = "Quel nom voulez-vous donner 
                                  à la variable ?", 
                                       placeholder = "Ex : Agriculture"),
                             actionButton(paste0("import", count$value), "Importer la base"),
                             br(),
                             
                             tags$h3(strong("Manipulations")),
                             
                             ## Type de données
                             selectInput(
                               inputId = paste0("choix", count$value),
                               label = "Type de données désiré",
                               choices = c("Coupe transversale",
                                           "Série temporelle",
                                           "Données de panel"),
                               selected = "Coupe transversale",
                               multiple = FALSE),
                             
                             ## Choix du pays
                             selectizeInput(inputId = paste0("pays", count$value), 
                                            label = "Pays souhaités", 
                                            choices = "",
                                            multiple = TRUE),
                             
                             ## Choix de l'année
                             conditionalPanel(
                               condition = paste0("input.choix", 
                                                  count$value, 
                                                  " == 'Coupe transversale'"),
                               selectizeInput(inputId = paste0("année", count$value), 
                                              label = "Année souhaitée", 
                                              choices = "",
                                              multiple = FALSE)
                             ),
                             conditionalPanel(
                               condition = paste0("input.choix", 
                                                  count$value, 
                                                  " == 'Série temporelle' || input.choix == 'Données de panel'"),
                               sliderInput(inputId = paste0("année2", count$value), 
                                           label = "Années souhaitées",
                                           min = 1,
                                           max = 2,
                                           value = c(1, 2),
                                           sep = "")
                             ),
                             
                             ## Manipulations en plus
                             checkboxInput(
                               inputId = paste0("logarithme", count$value), 
                               label = "Générer le logarithme de la variable"),
                             checkboxInput(
                               inputId = paste0("carré", count$value), 
                               label = "Générer le carré de la variable"),
                             
                             circle = TRUE, status = "primary", 
                             icon = icon("gear"), 
                             width = "300px",
                             tooltip = tooltipOptions(title = "Outils")
                           ),
                           column(width = 12,
                                  busyIndicator(text = "Importation des données en cours", 
                                                wait = 0),
                                  dataTableOutput(paste0("data_imported_tab", count$value))
                           )
                         )), 
                target = "Fusion et exportation", 
                position = "before",
                select = TRUE)
      
      import_and_treat(i = count$value)
    }
    
    if (input$tabs == "Moins"){
      id = paste0("Base ", count$value)
      removeTab(inputId = "tabs",
                target = id
      )
      
      count$value <- count$value - 1
    }
  })
  
  
  # ##### PARTIE IMPORTATION, TRAITEMENT ET DONNEES ##### 
  # 
  # #### BASE 1 ####
  # 
  # 
  # #### BASE 2 ####
  # 
  # 
  # ### IMPORTATION
  # data_wb_tab2 <- eventReactive(input$import2, {
  #   tmp <- as.data.frame(WDI(country = "all", indicator = input$nom_base2))
  #   colnames(tmp) <- c("Code ISO", "Pays", input$nouveau_nom2, "Année")
  #   tmp
  # })
  # 
  # ### INPUTS PAYS ET ANNEES
  # observe({
  #   data_wb_tab2 <- data_wb_tab2()
  #   if(input$choix == "Coupe transversale"){
  #     updateSelectizeInput(session, 
  #                          inputId = "pays", 
  #                          choices = unique(data_wb_tab2$Pays), 
  #                          selected = NULL)
  #     updateSelectizeInput(session, 
  #                          inputId = "année", 
  #                          choices = unique(data_wb_tab2$Année), 
  #                          selected = NULL,
  #                          options = list(maxItems = 1))
  #   }
  #   else if(input$choix == "Série temporelle"){
  #     updateSelectizeInput(session, 
  #                          inputId = "pays", 
  #                          choices = unique(data_wb_tab2$Pays),
  #                          selected = NULL,
  #                          options = list(maxItems = 1))
  #     updateSliderInput(session,
  #                       inputId = "année2",
  #                       min = min(data_wb_tab2$Année),
  #                       max = max(data_wb_tab2$Année),
  #                       value = c(min(data_wb_tab2$Année), 
  #                                 max(data_wb_tab2$Année)),
  #                       step = 1
  #     )
  #   }
  #   else if (input$choix == "Données de panel"){
  #     updateSelectizeInput(session, 
  #                          inputId = "pays", 
  #                          choices = unique(data_wb_tab2$Pays),
  #                          selected = NULL,
  #                          options = list(maxItems = 999999))
  #     updateSliderInput(session,
  #                       inputId = "année2",
  #                       min = min(data_wb_tab2$Année),
  #                       max = max(data_wb_tab2$Année),
  #                       value = c(min(data_wb_tab2$Année), 
  #                                 max(data_wb_tab2$Année)),
  #                       step = 1
  #     )
  #   }
  # })
  # 
  # 
  # ### DONNEES AFFICHEES
  # output$data_imported_tab2 <- renderDataTable({
  #   data_wb_tab2 <- data_wb_tab2()
  #   log_name <- paste0('ln(', input$nouveau_nom, ')')
  #   square_name <- paste0(input$nouveau_nom, '^2')
  #   
  #   ## Trois situations possibles : les données sont en coupe transversale, en série temporelle ou
  #   ## en données de panel.
  #   ## Dans chacune de ces trois situations, on détaille toutes les associations possibles
  #   ## de logarithme et carré, pour que l'ordre dans lequel on coche ne change rien.
  #   
  #   if(input$choix == "Coupe transversale"){
  #     data_wb2 <- data_wb_tab2 %>%
  #       filter(Pays %in% input$pays & Année %in% input$année) %>%
  #       arrange(Pays, Année)
  #     
  #     if(input$logarithme){
  #       if(input$carré){
  #         data_wb3 <- data_wb2 %>%
  #           mutate(!!log_name := log(data_wb2[, input$nouveau_nom])) %>%
  #           mutate(!!square_name := data_wb2[, input$nouveau_nom]^2) %>%
  #           select("Code ISO", "Pays", input$nouveau_nom, square_name, log_name, "Année")
  #         
  #         data_wb3
  #       }
  #       else {
  #         data_wb3 <- data_wb2 %>%
  #           mutate(!!log_name := log(data_wb2[, input$nouveau_nom])) %>%
  #           select("Code ISO", "Pays", input$nouveau_nom, log_name, "Année")
  #         
  #         data_wb3
  #       }
  #     }
  #     else if (input$carré){
  #       if (input$logarithme){
  #         data_wb3 <- data_wb2 %>%
  #           mutate(!!log_name := log(data_wb2[, input$nouveau_nom])) %>%
  #           mutate(!!square_name := data_wb2[, input$nouveau_nom]^2) %>%
  #           select("Code ISO", "Pays", input$nouveau_nom, square_name, log_name, "Année")
  #         
  #         data_wb3
  #       }
  #       else {
  #         data_wb3 <- data_wb2 %>%
  #           mutate(!!square_name := data_wb2[, input$nouveau_nom]^2) %>%
  #           select("Code ISO", "Pays", input$nouveau_nom, square_name, "Année")
  #         
  #         data_wb3
  #       }
  #     }
  #     else {data_wb2}
  #   }
  #   else if(input$choix == "Série temporelle"){
  #     data_wb4 <- data_wb_tab2 %>%
  #       filter(Pays %in% input$pays & 
  #                Année >= input$année2[[1]] &
  #                Année <= input$année2[[2]]) %>%
  #       arrange(Pays, Année)
  #     
  #     if(input$logarithme) {
  #       if (input$carré){
  #         data_wb5 <- data_wb4 %>%
  #           mutate(!!log_name := log(data_wb4[, input$nouveau_nom])) %>%
  #           mutate(!!square_name := data_wb4[, input$nouveau_nom]^2) %>%
  #           select("Code ISO", "Pays", input$nouveau_nom, square_name, log_name, "Année")
  #         
  #         data_wb5
  #       }
  #       else {
  #         data_wb5 <- data_wb4 %>%
  #           mutate(!!log_name := log(data_wb4[, input$nouveau_nom])) %>%
  #           select("Code ISO", "Pays", input$nouveau_nom, log_name, "Année")
  #         
  #         data_wb5
  #       }
  #     }
  #     else if (input$carré){
  #       if (input$logarithme){
  #         data_wb5 <- data_wb4 %>%
  #           mutate(!!log_name := log(data_wb4[, input$nouveau_nom])) %>%
  #           mutate(!!square_name := data_wb4[, input$nouveau_nom]^2) %>%
  #           select("Code ISO", "Pays", input$nouveau_nom, square_name, log_name, "Année")
  #         
  #         data_wb5
  #       }
  #       else {
  #         data_wb5 <- data_wb4 %>%
  #           mutate(!!square_name := data_wb4[, input$nouveau_nom]^2) %>%
  #           select("Code ISO", "Pays", input$nouveau_nom, square_name, "Année")
  #         
  #         data_wb5
  #       }
  #     }
  #     else {data_wb4}
  #   }
  #   else if(input$choix == "Données de panel"){
  #     data_wb6 <- data_wb_tab2 %>%
  #       filter(Pays %in% input$pays & 
  #                Année >= input$année2[[1]] &
  #                Année <= input$année2[[2]]) %>%
  #       arrange(Pays, Année)
  #     
  #     if(input$logarithme) {
  #       if (input$carré){
  #         data_wb7 <- data_wb6 %>%
  #           mutate(!!log_name := log(data_wb6[, input$nouveau_nom])) %>%
  #           mutate(!!square_name := data_wb6[, input$nouveau_nom]^2) %>%
  #           select("Code ISO", "Pays", input$nouveau_nom, square_name, log_name, "Année")
  #         
  #         data_wb7
  #       }
  #       else {
  #         data_wb7 <- data_wb6 %>%
  #           mutate(!!log_name := log(data_wb6[, input$nouveau_nom])) %>%
  #           select("Code ISO", "Pays", input$nouveau_nom, log_name, "Année")
  #         
  #         data_wb7
  #       }
  #     }
  #     else if (input$carré){
  #       if (input$logarithme) {
  #         data_wb7 <- data_wb6 %>%
  #           mutate(!!log_name := log(data_wb6[, input$nouveau_nom])) %>%
  #           mutate(!!square_name := data_wb6[, input$nouveau_nom]^2) %>%
  #           select("Code ISO", "Pays", input$nouveau_nom, square_name, log_name, "Année")
  #         
  #         data_wb7
  #       } 
  #       else {
  #         data_wb7 <- data_wb6 %>%
  #           mutate(!!square_name := data_wb6[, input$nouveau_nom]^2) %>%
  #           select("Code ISO", "Pays", input$nouveau_nom, square_name, "Année")
  #         
  #         data_wb7
  #       }
  #     }
  #     else {data_wb6}
  #   }
  # },
  # options = list(pageLength = 999999)
  # )
  # 
  # 
  # 
  # ##### PARTIE FUSION #####
  # 
  # output$data_merged <- renderDataTable({
  #   
  # })
  # 
}


shinyApp(ui = ui, server = server)
