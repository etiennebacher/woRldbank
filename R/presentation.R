library(shiny)
library(shinythemes)
library(shinyWidgets)
library(WDI)
library(DT)
library(dplyr)
library(shinysky)

presentationTab <- function(id){
  ns <- NS(id)
  tabPanel(title = introBox("Présentation",
                            data.step = 1,
                            data.intro = "Ce premier onglet explique l'intérêt de cette application. Il ne s'agit que d'un onglet descriptif : toutes les manipulations techniques se font dans les autres onglets."),
           fluidRow(
             column(width = 12, 
                    h4("Le traitement de données est très chronophage, que ce soit au niveau de la récupération des données, de leur aggrégation puis de leur traitement. Cette application Shiny a pour but d'importer des bases de données depuis le site de la Banque Mondiale et de les traiter afin de les utiliser ensuite dans des régressions. Il est possible de traiter des données en coupe transversale, série temporelle ou des données de panel."),
                    br(),
                    h4(strong("Exemple d'utilisation de cette application")),
                    br(),
             ),
             column(width = 12,
                    br(),
                    br(),
                    h4("On peut diviser ces paramètres en deux catégories : ceux permettant d'importer la base de données depuis le site de la Banque Mondiale et ceux permettant de manipuler plus en profondeur cette base de données."),
                    br(),
                    h4("Au niveau de l'importation de la base, on doit d'abord spécifier le code type de la Banque Mondiale puis donner un nom à la variable que l'on veut importer. Une fois la base importée, on spécifie le type de données que l'on veut (coupe transversale, série temporelle, données de panel), le(s) pays désiré(s) et l(es)' année(s) désirée(s)."),
                    br(),
                    h4("On peut également générer une colonne qui vaut le logarithme de la variable importée et une colonne qui vaut son carré, puisque ces deux manipulations sont souvent utilisées en économétrie."))
             ),
           value = "Présentation"
           )

}