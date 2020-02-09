newTab_server <- function(input, output, session){
  
  data_wb <- eventReactive(input$import, {
    validate(
      need(input$nom_base != "", "Veuillez renseigner un indicateur de la Banque Mondiale")
      )
    validate(
      need(input$nouveau_nom != "", "Veuillez donner un nom à la variable")
    )
    tmp <- as.data.frame(WDI(country = "all", indicator = input$nom_base))
    colnames(tmp) <- c("Code ISO", "Pays", input$nouveau_nom, "Année")
    tmp %>%
      select("Code ISO", "Pays", "Année", input$nouveau_nom)
  })
  
  ### INPUTS PAYS ET ANNEES
  observe({
    data_wb <- data_wb()
    if(input$choix == "Coupe transversale"){
      updateSelectizeInput(session, 
                           inputId = "pays", 
                           choices = unique(data_wb$Pays), 
                           selected = NULL)
      updateSelectizeInput(session, 
                           inputId = "année", 
                           choices = unique(data_wb$Année), 
                           selected = NULL,
                           options = list(maxItems = 1))
    }
    else if(input$choix == "Série temporelle"){
      updateSelectizeInput(session, 
                           inputId = "pays", 
                           choices = unique(data_wb$Pays),
                           selected = NULL,
                           options = list(maxItems = 1))
      updateSliderInput(session,
                        inputId = "année2",
                        min = min(data_wb$Année),
                        max = max(data_wb$Année),
                        value = c(min(data_wb$Année), 
                                  max(data_wb$Année)),
                        step = 1
      )
    }
    else if (input$choix == "Données de panel"){
      updateSelectizeInput(session, 
                           inputId = "pays", 
                           choices = unique(data_wb$Pays),
                           selected = NULL,
                           options = list(maxItems = 999999))
      updateSliderInput(session,
                        inputId = "année2",
                        min = min(data_wb$Année),
                        max = max(data_wb$Année),
                        value = c(min(data_wb$Année), 
                                  max(data_wb$Année)),
                        step = 1
      )
    }
  })
  
  data_reac <- reactive({
    data_wb  <- data_wb()
    log_name <- paste0('ln(', input$nouveau_nom, ')')
    square_name <- paste0(input$nouveau_nom, '^2')
    
    ## Trois situations possibles : les données sont en coupe transversale, en série temporelle ou
    ## en données de panel.
    ## Dans chacune de ces trois situations, on détaille toutes les associations possibles
    ## de logarithme et carré, pour que l'ordre dans lequel on coche ne change rien.
    
    if(input$choix == "Coupe transversale"){
      data_wb <- data_wb  %>%
        filter(Pays %in% input$pays & Année %in% input$année) %>%
        arrange(Pays, Année)
      
      if(input$logarithme){
        if(input$carré){
          data_wb <- data_wb %>%
            mutate(!!log_name := log(data_wb[, input$nouveau_nom])) %>%
            mutate(!!all_of(square_name) := data_wb[, input$nouveau_nom]^2) %>%
            select("Code ISO", "Pays", input$nouveau_nom, all_of(square_name), all_of(log_name), "Année")
          
          data_wb
        }
        else {
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$nouveau_nom])) %>%
            select("Code ISO", "Pays", input$nouveau_nom, all_of(log_name), "Année")
          
          data_wb
        }
      }
      else if (input$carré){
        if (input$logarithme){
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$nouveau_nom])) %>%
            mutate(!!all_of(square_name) := data_wb[, input$nouveau_nom]^2) %>%
            select("Code ISO", "Pays", input$nouveau_nom, all_of(square_name), all_of(log_name), "Année")
          
          data_wb
        }
        else {
          data_wb <- data_wb %>%
            mutate(!!all_of(square_name) := data_wb[, input$nouveau_nom]^2) %>%
            select("Code ISO", "Pays", input$nouveau_nom, all_of(square_name), "Année")
          
          data_wb
        }
      }
      else {data_wb}
    }
    else if(input$choix == "Série temporelle"){
      data_wb <- data_wb  %>%
        filter(Pays %in% input$pays & 
                 Année >= input$année2[[1]] &
                 Année <= input$année2[[2]]) %>%
        arrange(Pays, Année)
      
      if(input$logarithme) {
        if (input$carré){
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$nouveau_nom])) %>%
            mutate(!!all_of(square_name) := data_wb[, input$nouveau_nom]^2) %>%
            select("Code ISO", "Pays", input$nouveau_nom, all_of(square_name), all_of(log_name), "Année")
          
          data_wb
        }
        else {
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$nouveau_nom])) %>%
            select("Code ISO", "Pays", input$nouveau_nom, all_of(log_name), "Année")
          
          data_wb
        }
      }
      else if (input$carré){
        if (input$logarithme){
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$nouveau_nom])) %>%
            mutate(!!all_of(square_name) := data_wb[, input$nouveau_nom]^2) %>%
            select("Code ISO", "Pays", input$nouveau_nom, all_of(square_name), all_of(log_name), "Année")
          
          data_wb
        }
        else {
          data_wb <- data_wb %>%
            mutate(!!all_of(square_name) := data_wb[, input$nouveau_nom]^2) %>%
            select("Code ISO", "Pays", input$nouveau_nom, all_of(square_name), "Année")
          
          data_wb
        }
      }
      else {data_wb}
    }
    else if(input$choix == "Données de panel"){
      data_wb <- data_wb  %>%
        filter(Pays %in% input$pays & 
                 Année >= input$année2[[1]] &
                 Année <= input$année2[[2]]) %>%
        arrange(Pays, Année)
      
      if(input$logarithme) {
        if (input$carré){
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$nouveau_nom])) %>%
            mutate(!!all_of(square_name) := data_wb[, input$nouveau_nom]^2) %>%
            select("Code ISO", "Pays", input$nouveau_nom, all_of(square_name), all_of(log_name), "Année")
          
          data_wb
        }
        else {
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$nouveau_nom])) %>%
            select("Code ISO", "Pays", input$nouveau_nom, all_of(log_name), "Année")
          
          data_wb
        }
      }
      else if (input$carré){
        if (input$logarithme) {
          data_wb <- data_wb %>%
            mutate(!!all_of(log_name) := log(data_wb[, input$nouveau_nom])) %>%
            mutate(!!all_of(square_name) := data_wb[, input$nouveau_nom]^2) %>%
            select("Code ISO", "Pays", input$nouveau_nom, all_of(square_name), all_of(log_name), "Année")
          
          data_wb
        } 
        else {
          data_wb <- data_wb %>%
            mutate(!!all_of(square_name) := data_wb[, input$nouveau_nom]^2) %>%
            select("Code ISO", "Pays", input$nouveau_nom, all_of(square_name), "Année")
          
          data_wb
        }
      }
      else {data_wb}
    }
    
  })
  
  ### DONNEES AFFICHEES
  output$data_imported_tab  <- renderDataTable({
    data_reac()
  },
  options = list(pageLength = 999999)
  )

  
  plot_base <- reactive({
    req(input$choix)
    df <- data_reac()
    if (input$choix == "Coupe transversale"){
      if (input$stata_style) {
        ggplot(data = df, aes(x = df[[input$nouveau_nom]])) +
          geom_line(stat = "density") +
          geom_vline(aes(xintercept = mean(df[[input$nouveau_nom]], na.rm = TRUE),
                         color = "Moyenne"),
                     linetype = "dashed",
                     size = 0.6) +
          labs(x = input$nouveau_nom,
               title = paste0("Densité de ", input$nouveau_nom)) +
          scale_color_manual(name = "Légende") +
          theme_stata()
      }
      else {
        ggplot(data = df, aes(x = df[[input$nouveau_nom]])) +
          geom_line(stat = "density") +
          geom_vline(aes(xintercept = mean(df[[input$nouveau_nom]], na.rm = TRUE),
                         color = "Moyenne"),
                     linetype = "dashed",
                     size = 0.6) +
          labs(x = input$nouveau_nom,
               title = paste0("Densité de ", input$nouveau_nom)) +
          scale_color_manual(name = "Légende", values = c(Moyenne = "black")) +
          theme_clean()
        
      }
    }
    else if (input$choix == "Série temporelle") {
      if (input$stata_style) {
        ggplot(data = df, aes(x = Année, y = df[[input$nouveau_nom]])) +
          geom_line() +
          labs(
            y = input$nouveau_nom,
            title = paste0(
              "Evolution de ",
              input$nouveau_nom,
              " entre ",
              min(df$Année),
              " et ",
              max(df$Année),
              " : ",
              unique(df$Pays)
            )
          ) +
          theme_stata()
      }
      else {
        ggplot(data = df, aes(x = Année, y = df[[input$nouveau_nom]])) +
          geom_line() +
          labs(
            y = input$nouveau_nom,
            title = paste0(
              "Evolution de ",
              input$nouveau_nom,
              " entre ",
              min(df$Année),
              " et ",
              max(df$Année),
              " : ",
              unique(df$Pays)
            )
          ) +
          theme_clean()
      }
    }
    else if (input$choix == "Données de panel"){
      if (input$stata_style) {
        ggplot(data = df, aes(
          x = Année,
          y = df[[input$nouveau_nom]],
          group = Pays,
          color = Pays
        )) +
          geom_line() +
          labs(
            y = input$nouveau_nom,
            title = paste0(
              "Evolution de ",
              input$nouveau_nom,
              " entre ",
              min(df$Année),
              " et ",
              max(df$Année)
            )
          ) +
          theme_stata()
      }
      else {
        ggplot(data = df, aes(
          x = Année,
          y = df[[input$nouveau_nom]],
          group = Pays,
          color = Pays
        )) +
          geom_line() +
          labs(
            y = input$nouveau_nom,
            title = paste0(
              "Evolution de ",
              input$nouveau_nom,
              " entre ",
              min(df$Année),
              " et ",
              max(df$Année)
            )
          ) +
          theme_clean()
      }
    }
    else {x <- NULL}
  })

  
  observeEvent(input$make_plot, {
    output$plot <- renderPlot({
      plot_base()
    })
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("wb-plot-", input$nouveau_nom, ".png", sep = "")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot_base(), device = device)
    }
  )

}
