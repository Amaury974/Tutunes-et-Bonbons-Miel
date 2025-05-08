#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Server pour Tutune & Bonbon Miel

# A.Jorant - Nov 2024

# R version 4.4.2
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
double_identification <- ''

server <- function(input, output) {
  
  # setwd('../Source')
  # print(getwd())
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####               SERVER : 0 _ Chargement données              #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  cat('>> Préchargement Données _ 1\n')
  
  RV <- reactiveValues(data=NULL)
  RV$df_classif <- df_classif
  
  # cat('emplacement du fichier de sauvegarde', 'D:/apis_/Documents/R/Analyse des comptes bancaire TBM/Data/save.RDS', file = 'direction_sauvegarde.txt', sep = '\n')
  
  RDS_files <- try(readRDS(dir_sauvegarde), silent = TRUE)
  
  # print(list.files(dir_sauvegarde))
  if(!inherits(RDS_files, "try-error")) {
    cat('>> Initialisation > chargement sauvegarde\n')
    cat('                    depuis :', dir_sauvegarde, '\n')
    cat('                            ', str_c(names(RDS_files), collapse = '                            \n'), '\n')
    # load('save.RData')
    # RDS_files <- readRDS('save.RDS')
    for(i in names(RDS_files))
      RV[[i]] <- RDS_files[[i]]
  }
  
  cat('                         _ fin\n\n')
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                  SERVER : Page 1 - Data IN                 #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  #--------------------------------------------------------------------#
  #####                      __ Sauvegarde                         #####
  #--------------------------------------------------------------------#
  observeEvent(input$dir_sauvegarde, {
    cat('>> Direction Sauvegarde\n\n')
    dir_sauvegarde <<- input$dir_sauvegarde
    
  })
  
  observeEvent(input$save, {
    cat('>> Sauvegarde _ 1\n')
    print(dir_sauvegarde)
    test_sauvegarde <- try(saveRDS(list(df_classif=RV$df_classif, df_identifie=RV$df_identifie), file = dir_sauvegarde))
    print(test_sauvegarde)
    
    if(!inherits(test_sauvegarde, "try-error")) {
      cat('            _ Succès\n')
      print(getwd())
      cat('Emplacement du fichier de sauvegarde', dir_sauvegarde, file = 'Source/direction_sauvegarde.txt', sep = '\n')
    }
    
    cat('              _ fin\n\n')
    
  })
  
  
  #--------------------------------------------------------------------#
  #####                     __ Importation                         #####
  #--------------------------------------------------------------------#
  
  # ~~~~{    Depuis relevés de comptes    }~~~~
  observeEvent(
    input$input_releves,{
      cat('>> Importation > releves _ 1\n')
      # input_data <- input$input_releves
      # print(input_data)
      
      df_identifie <- f_diff_extraction(input$input_releves$datapath, .dir_name = input$input_releves$name) %>%
        fun_classif(RV$df_classif)
      
      # cat('                         _ 2\n')
      
      # df_resume_periode <- f_resume_trimestre(df_identifie)
      # cat('                         _ 3\n')
      # list_col <- f_couleurs(df_resume_periode)
      # 
      RV$df_identifie <- df_identifie
      # RV$df_resume_periode <- df_resume_periode
      # RV$list_col <- list_col
      
      cat('                         _ fin\n\n')
      
      
    })
  
  
  # ~~~~{    Depuis catégorisé    }~~~~
  observeEvent(
    input$input_identifie,{
      cat('>> Importation > Catégories _ 1\n')
      
      cat('                            _ 2\n')
      # print(input$input_identifie)
      df_identifie <- read.csv2(input$input_identifie$datapath)%>%
        mutate(Date = as.Date(Date))
      
      # df_resume_periode <- f_resume_trimestre(df_identifie)
      #
      # list_col <- f_couleurs(df_resume_periode)
      #
      RV$df_identifie <- df_identifie
      # RV$df_resume_periode <- df_resume_periode
      # RV$list_col <- list_col
      
      
      cat('                            _ fin\n\n')
    }
  )
  
  # # ~~~~{    Depuis résumé    }~~~~
  # observeEvent(
  #   input$input_resume,{
  #     cat('>> Importation > résumés _ 1\n')
  #     
  #     df_resume_periode <- read.csv2(input$input_resume$datapath) %>%
  #       mutate(trimestre = as.Date(trimestre))
  #     
  #     list_col <- f_couleurs(df_resume_periode)
  #     
  #     RV$df_resume_periode <- df_resume_periode
  #     RV$list_col <- list_col
  #     
  #     cat('                         _ fin\n\n')
  #   }
  # )
  
  
  
  #--------------------------------------------------------------------#
  #####                     __ Identification                      #####
  #--------------------------------------------------------------------#
  
  # ~~~~{    téléchargement fichier d'identification    }~~~~
  
  # ~~~~{    Importation    }~~~~
  observeEvent(
    input$upload_classif,{
      cat('>> Identification > Upload Classif _ 1\n')
      df_classif <<- read.csv2(input$upload_classif$datapath) %>%
        mutate(Date = as.Date(Date, format = '%d/%m/%Y'))
      
      RV$df_classif <- df_classif
      cat('                                   _ fin\n\n')
      
    })
  
  # ~~~~{    Exportation    }~~~~
  output$download_classif <- downloadHandler(
    filename = 'Classification_dépenses.csv',
    content = function(file) {
      cat('>> Identification > Download Classif\n\n')
      RV$dir_identification_download <- file
      RV$df_classif %>%
        mutate(Date = format(Date, '%d/%m/%Y')) %>%
        write.csv2(file, row.names = FALSE)
    }
  )
  
  
  
  # ~~~~{    bouton MaJ et Plus    }~~~~
  type_Maj_Classe <- 'MaJ_Classe'
  
  observeEvent(input$MaJ_Classe, {
    cat('>> Identification > click MaJ_Classe _ 1\n')
    type_Maj_Classe <<- 'MaJ_Classe'
    cat('                                     _ fin\n\n')
    
  })
  
  observeEvent(input$Plus_ligne, {
    cat('>> Identification > click Plus_ligne _ 1\n')
    type_Maj_Classe <<- 'Plus_ligne'
    cat('                                     _ fin\n\n')
    
  })
  
  #initialisation
  
  Nv_ligne <- c()
  
  observeEvent({input$MaJ_Classe ; input$Plus_ligne}, {
    cat('>> Identification > MaJ Classe _ 1\n')
    
    if(input$nv_Marq != ''){
      cat('                               _ 2\n')
      
      # !length(as.Date(character(0)))
      
      # print(input$nv_Date)
      
      Nv_ligne <<- data.frame(Super_Classe = str_extract(input$select_Classe, '^[^\\s]+'),
                              Classe = str_extract(input$select_Classe, '[^\\s]+$'),
                              Marqueur = toupper(input$nv_Marq),
                              Date = if(length(input$nv_Date)) input$nv_Date else NA)
      
      cat("                ajout d'une ligne :\n")
      print(Nv_ligne)
      
      RV$df_classif <- bind_rows(RV$df_classif, Nv_ligne)
      
    }
    
    
    # Reset des champs
    cat('                                _ 3\n')
    updateTextInput(inputId = 'nv_Marq', value = '')
    suppressWarnings( updateDateInput(inputId = 'nv_Date', value = NA ) )
    
    
    cat('                                _ fin\n\n')   
    
  })
  
  # ~~~~{    Update des identification    }~~~~
  observeEvent(RV$df_classif, {
    
    cat('>> Identification > Update des identifications _ 1\n')
    
    # ~~~~{    On ré-identifie tout    }~~~~
    df_identifie <- f_classif(RV$df_identifie, RV$df_classif, Nv_ligne, type_Maj_Classe)
    
    cat('                                               _ 2\n')   
    
    RV$df_classif <- RV$df_classif %>%
      arrange(Super_Classe, Classe)
    
    RV$df_identifie <- df_identifie
    
    
    
    cat('                                               _ fin\n\n')   
    
    
  })
  
  
  # ~~~~{    Champs de selection des Classes dispo    }~~~~
  observeEvent(RV$df_classif, {
    cat('>> Identification > Champs Selection _ 1\n')
    
    CHOICES <- RV$df_classif %>%
      group_by(Super_Classe) %>%
      mutate(N = length(Classe)) %>%
      group_by(Classe) %>%
      mutate(n = length(Classe),
             choices = paste(Super_Classe, Classe)) %>%
      arrange(-N, Super_Classe, -n, Classe) %>%
      pull(choices) %>%
      unique()
    
    updateSelectizeInput(inputId = 'select_Classe', choices = CHOICES)
    
    cat('                                     _ fin\n\n')   
    
  }
  
  )
  
  
  
  # ~~~~{    Tab Classification    }~~~~
  # output$tab_classif <- renderTable(arrange(RV$df_classif, Super_Classe, Classe))
  
  output$tab_classif <- renderDT(
    {
      
      cat('>> Identification > Render Classif\n\n')
      
      mutate(isolate(RV$df_classif),
             Date = format(Date, '%d/%m/%Y'),
             Classe = as.factor(Classe),
             Super_Classe = as.factor(Super_Classe))
    },
    selection = 'none',
    # editable = 'row',
    filter = 'top',
    # server = FALSE,
    
    options = list(
      pageLength = 40,
      autoWidth = TRUE,
      ordering = TRUE
    ),
    
  )
  
  #proxy de DT pour le manipuler sans recharger tout l'affichage
  proxy_DT <- dataTableProxy('tab_classif')
  
  observeEvent(RV$df_classif, {
    cat('>> Identification > MaJ tab Classif _ 1\n')
    
    if(type_Maj_Classe == 'Plus_ligne'){
      cat('                                    _ Plus_ligne\n')   
      proxy_DT %>% addRow(Nv_ligne)
    }
    
    if(type_Maj_Classe == 'MaJ_Classe'){
      cat('                                    _ MaJ_Classe\n')  
      replaceData(proxy_DT, RV$df_classif, resetPaging = TRUE)
    }
    
    cat('                                    _ fin\n\n')
  })
  
  
  # ~~~~{    Tab non assignés    }~~~~
  output$tab_nonIdentifies <- renderTable({
    # print(!is.null(RV$df_identifie))
    cat('>> Identification > non identifiés _ 1\n')
    # print(RV$df_identifie)
    
    tab <- NULL
    
    if(!is.null(RV$df_identifie)){
      tab <- RV$df_identifie %>%
        filter(is.na(Classe), !is.na(Montant)) %>%
        arrange(desc(abs(Montant))) %>%
        mutate(Date = format(Date, '%d/%m/%Y')) %>%
        select(Date, libelle, Montant, Compte)
    }
    cat('                                   _ fin\n\n')
    
    tab
  })
  
  # ~~~~{    message    }~~~~
  # R_double_identification <- reactive(double_identification)
  # 
  # output$erreur_id <- renderText({
  #   cat('>> Identification > msg erreur\n\n')
  #   print(R_double_identification())
  #   R_double_identification()
  # })
  
  
  # R_double_identification <- reactive(double_identification)
  # 
  # output$erreur_id <- renderText({
  #   cat('>> Identification > msg erreur\n\n')
  #   print(double_identification)
  #   double_identification
  # })
  
  
  
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####               SERVER : Page 2 - Graphiques                 #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  
  
  df_resume_periode <- reactive({    
    cat('>> Graphiques > df_resume_periode _ 1\n')
    
    tab <- NULL
    
    if(!is.null(RV$df_identifie))
      tab <-f_resume(RV$df_identifie, input$echelle)
    
    cat('                                  _ fin\n\n')
    
    tab
  })
  
  
  list_col  <- reactive({   
    cat('>> Graphiques > list_Col _ 1\n')
    tab <- NULL
    
    if(!is.null(RV$df_identifie))
      tab <-f_couleurs(df_resume_periode())
    
    cat('                           _ fin\n\n')
    
    tab
  })
  
  
  
  
  # ~~~~{    Mise à jour de la taille du curseur de dates    }~~~~
  # observeEvent(RV$df_identifie, {
  #   MIN <- min(RV$df_identifie$Date, na.rm = TRUE)
  #   MAX <- max(RV$df_identifie$Date, na.rm = TRUE)
  #   updateSliderInput(inputId = 'periode_subset', 
  #                     min = MIN, 
  #                     max = MAX,
  #                     value = c(MIN, MAX)
  #   )
  # })
  # ~~~~{    Periode par defaut    }~~~~
  
  observeEvent(df_resume_periode(), {
    cat('>> Graphiques > Periode par defaut _ 1\n')
    
    CHOICES <- periodifier(df_resume_periode()$periode, input$echelle, 'Court') %>%
      unique()
    
    cat('Tous les choix :', str_c(CHOICES, collapse =' ; '), '\n')
    
    SELECTED <- Periode_defaut(df_resume_periode(), RV$df_identifie)
    
    cat('Bornes pré-selectionnées :', str_c(SELECTED, collapse =' ; '),'\n')
    
    cat('                                   _ 2\n')
    
    updateSliderTextInput(inputId = 'periode_subset', 
                          choices = CHOICES,
                          selected =  SELECTED
    )
    cat('                                   _ fin\n\n')
    
  })
  
  
  # ~~~~{    Graphique    }~~~~
  output$graph <- renderGirafe({
    cat('>> Graphiques > graphique _ 1\n')
    cat('      ', input$typeGraph, '\n',
        '      ', input$echelle,'\n',
        '      ', as.character(input$periode_subset[1]),'à', as.character(input$periode_subset[2]), '\n')
    
    
    df_resume <- filter(df_resume_periode(),
                        periode >= periodifier_Court_to_Date(input$periode_subset[1], echelle = input$echelle),
                        periode <= periodifier_Court_to_Date(input$periode_subset[2], echelle = input$echelle))
    
    plot <- NULL
    cat('                          _ 2\n')
    # df_resume <- df_resume_periode()
    if(input$typeGraph == 'Verification_donnees') plot <- Verification_donnees(df_resume, list_col(), RV$df_identifie) else {
      if(!is.null(input$typeGraph))
        plot <- eval(call(input$typeGraph, df_resume_periode=df_resume, list_col=list_col()))
    }
    
    cat('                          _ fin\n\n')
    plot
  })
  
  
  # ~~~~{    Réaction au click : affichage des données    }~~~~
  giraph_select <- reactive({
    cat('>> Graphiques > clicked\n')
    input$graph_selected
  })
  
  output$clicked_tab <- renderTable({
    
    cat('>> Graphiques > clicked tab _ 1\n')
    print(giraph_select())
    tab <- NULL
    
    
    if(input$typeGraph == 'BonbonMiel_unique_giraph' & !is.null(giraph_select())){
      
      tab <- RV$df_identifie %>%
        filter(if(giraph_select() == 'NA') is.na(Classe) else Classe == giraph_select() | Super_Classe == giraph_select()) %>%
        arrange(desc(Montant)) %>%
        mutate(Date = as.character(Date)) %>%
        select(Date, libelle, Montant, Compte)
      
      return(tab)
    }
    
    if(input$typeGraph == 'histogramme_Fasse_a_Fasse' & !is.null(giraph_select())){
      print('ICI')
      encadrement_periode <- de_periodifier(as.Date(str_extract(giraph_select(), '^[^/]+')), input$echelle)
      selected_sens <- str_extract(giraph_select(), '[^/]+$')
      
      print(selected_sens)
      
      tab <- RV$df_identifie %>%
        filter((selected_sens == 'Credit') == (Montant > 0),
               Date >= encadrement_periode$deb, Date <= encadrement_periode$fin) %>%
        arrange(desc(abs(Montant))) %>%
        mutate(Date = as.character(Date)) %>%
        select(Date, libelle, Montant, Compte)
      
      return(tab)
    }
    
    
    
    if(!is.null(giraph_select())){
      
      select_Classe <- str_extract(giraph_select(), '^[^/]+')
      
      # début et fin de la période
      encadrement_periode <- de_periodifier(as.Date(str_extract(giraph_select(), '[^/]+$')), input$echelle)
      
      cat('deb:', as.character(encadrement_periode$deb), 'fin:', as.character(encadrement_periode$fin), '\n')
      
      tab <- RV$df_identifie %>%
        filter(if(select_Classe == 'NA') is.na(Classe) else Classe == select_Classe | Super_Classe == select_Classe,
               Date >= encadrement_periode$deb, Date <= encadrement_periode$fin) %>%
        arrange(desc(abs(Montant))) %>%
        mutate(Date = as.character(Date)) %>%
        select(Date, libelle, Montant, Compte)
    }
    
    cat('                         _ fin\n\n')
    
    tab
  })
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                SERVER : Page 3 - Data Display              #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  # ~~~~{    Tableau séléctionné    }~~~~
  tab <- reactive({
    
    if(input$data_shown == 'Resume_trimestriel')
      tab <- df_resume_periode() %>%
        mutate(periode = as.character(periode))
    
    if(input$data_shown == 'Releve_de_comptes_categorises')
      tab <- RV$df_identifie %>%
        mutate(Date = as.character(Date))
    
    tab
  })
  
  # ~~~~{    Affichage du dit tableau    }~~~~
  output$data_table <- renderTable(tab())
  
  
  # ~~~~{    Telechargement    }~~~~
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$data_shown, ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(tab(), file, row.names = FALSE)
    }
  )
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                       SERVER : GOUZOUS                     #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  output$gouzou1 <- renderPlot(GOUZOU_geom(type = 'brezel')+theme_void())
  
  output$logo_TBM <- renderImage(list(
    src = "../Source/logo Tutunes et Bonbons Miel.png",
    contentType = "image/png",
    alt = "logo Tutunes et Bonbons Miel",
    height = '300px'
  ))
  
}


