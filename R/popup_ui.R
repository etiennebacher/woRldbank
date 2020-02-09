popup <- bsModalNoClose("window", "Window",
               title="Voulez-vous faire le tour de l'application ?", size='small',
               actionBttn('besoin', 
                          'Oui', 
                          size = 'md', 
                          style = "material-flat",
                          icon = icon("check")
                          ),
               actionBttn('pasbesoin', 
                          'Non', 
                          size = 'md', 
                          style = "material-flat",
                          icon = icon("remove")
                          ),
               tags$head(tags$style("#window .modal-footer{display:none}
                                       .modal-header"),
                         tags$script("$(document).ready(function(){
                                        $('#window').modal();
                                        });")
               ))