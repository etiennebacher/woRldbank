newTab_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      sidebarPanel(
        tags$h3(strong("Base à importer")),
        textInput(ns("nom_base"),
                  "Code de la base à importer",
                  placeholder = "Ex : NV.AGR.TOTL.ZS"),
        textInput(ns("nouveau_nom"), 
                  label = "Quel nom voulez-vous donner 
                                  à la variable ?", 
                  placeholder = "Ex : Agriculture"),
        actionButton(ns("import"), 
                     strong("Importer la base")),
        br(),
        
        tags$h3(strong("Manipulations")),
        
        ## Type de données
        selectInput(
          inputId = ns("choix"),
          label = "Type de données désiré",
          choices = c("Coupe transversale",
                      "Série temporelle",
                      "Données de panel"),
          selected = "Coupe transversale",
          multiple = FALSE),
        
        ## Choix du pays
        selectizeInput(inputId = ns("pays"), 
                       label = "Pays souhaités", 
                       choices = "",
                       multiple = TRUE),
        
        ## Choix de l'année
        conditionalPanel(
          condition = "input.choix == 'Coupe transversale'",
          ns = ns,
          selectizeInput(inputId = ns("année"), 
                         label = "Année souhaitée", 
                         choices = "",
                         multiple = FALSE)
        ),
        conditionalPanel(
          condition = "input.choix == 'Série temporelle' || input.choix == 'Données de panel'",
          ns = ns,
          sliderInput(inputId = ns("année2"), 
                      label = "Années souhaitées",
                      min = 1,
                      max = 2,
                      value = c(1, 2),
                      sep = "")
      ),
      
      ## Manipulations en plus
      checkboxInput(
        inputId = ns("logarithme"), 
        label = "Générer le logarithme de la variable"),
      checkboxInput(
        inputId = ns("carré"), 
        label = "Générer le carré de la variable"),
      br(),
      actionButton(
        ns("make_plot"), 
        "Générer un graphique"),
      width = 3
    ),
    mainPanel(
      column(width = 12,
             busyIndicator(text = "Importation des données en cours", 
                           wait = 0),
             dataTableOutput(ns("data_imported_tab")),
             bsModal(ns("modal_plot"), "Représentation graphique", 
                     trigger = ns("make_plot"),
                     plotOutput(ns("plot")),
                     checkboxInput(ns("stata_style"), "Appliquer le style 'Stata'"),
                     downloadButton(ns("download_plot"), label = "Télécharger le graphique"))
    ),
    width = 9)
  )
  )
}
