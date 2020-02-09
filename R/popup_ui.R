popup <- bsModalNoClose("window", "Window",
               title="Would you like a presentation of the application?", size='small',
               actionBttn('need', 
                          'Yes', 
                          size = 'md', 
                          style = "material-flat",
                          icon = icon("check")
                          ),
               actionBttn('no_need', 
                          'No', 
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