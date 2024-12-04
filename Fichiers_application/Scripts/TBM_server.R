#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Server pour Tutune & Bonbon Miel

# A.Jorant - Nov 2024

# R version 4.4.2
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

double_identification <- ''

server <- function(input, output) {
  
  setwd('../Source')
  print(getwd())
  
  RV <- reactiveValues(data=NULL)
  RV$df_classif <- df_classif
  
  print(list.files())
  if('save.RDS' %in% list.files()) {
    cat('>> Initialisation > chargement sauvegarde\n\n')
    
    # load('save.RData')
    RDS_files <- readRDS('save.RDS')
    for(i in names(RDS_files))
      RV[[i]] <- RDS_files[[i]]
  }
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                  SERVER : Page 1 - Data IN                 #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  #--------------------------------------------------------------------#
  #####                      __ Sauvegarde                         #####
  #--------------------------------------------------------------------#
  
  observeEvent(input$save, {
    cat('>> Sauvegarde\n\n')
    saveRDS(list(df_classif=RV$df_classif, df_identifie=RV$df_identifie, df_resume_trimestre=RV$df_resume_trimestre, list_col=RV$list_col), 'save.RDS')
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
      
      cat('                         _ 2\n')
      
      df_resume_trimestre <- f_resume_trimestre(df_identifie)
      cat('                         _ 3\n')
      list_col <- f_couleurs(df_resume_trimestre)
      
      RV$df_identifie <- df_identifie
      RV$df_resume_trimestre <- df_resume_trimestre
      RV$list_col <- list_col
      
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
      
      df_resume_trimestre <- f_resume_trimestre(df_identifie)
      
      list_col <- f_couleurs(df_resume_trimestre)
      
      RV$df_identifie <- df_identifie
      RV$df_resume_trimestre <- df_resume_trimestre
      RV$list_col <- list_col
      
      
      cat('                            _ fin\n\n')
    }
  )
  
  # ~~~~{    Depuis résumé    }~~~~
  observeEvent(
    input$input_resume,{
      cat('>> Importation > résumés _ 1\n')
      
      df_resume_trimestre <- read.csv2(input$input_resume$datapath) %>%
        mutate(trimestre = as.Date(trimestre))
      
      list_col <- f_couleurs(df_resume_trimestre)
      
      RV$df_resume_trimestre <- df_resume_trimestre
      RV$list_col <- list_col
      
      cat('                         _ fin\n\n')
    }
  )
  
  
  
  #--------------------------------------------------------------------#
  #####                     __ Identification                      #####
  #--------------------------------------------------------------------#
  
  # ~~~~{    téléchargement fichier d'identification    }~~~~
  
  # ~~~~{    Importation    }~~~~
  observeEvent(
    input$upload_classif,{
      cat('>> Identification > Upload Classif _ 1\n')
      df_classif <<- read.csv2(input$upload_classif$datapath) %>%
        mutate(Date = as.Date(Date))
      
      RV$df_classif <- df_classif
      cat('                                   _ fin\n\n')
      
    })
  
  # ~~~~{    Exportation    }~~~~
  output$download_classif <- downloadHandler(
    filename = 'Classification_dépenses.csv',
    content = function(file) {
      cat('>> Identification > Download Classif\n\n')
      RV$dir_identification_download <- file
      write.csv2(df_classif, file, row.names = FALSE)
    }
  )
  
  
  
  # ~~~~{    MaJ    }~~~~
  observeEvent(input$MaJ_classe, {
    cat('>> Identification > MaJ classe _ 1\n')
    
    if(input$nv_Marq != ''){
      cat('                             _ 2\n')
      
      # !length(as.Date(character(0)))
      
      
      # print(input$nv_Date)
      
      Nv_ligne <-data.frame(super_classe = input$nv_supClasse,
                            classe = input$nv_Classe,
                            lib_id = toupper(input$nv_Marq),
                            Date = if(length(input$nv_Date)) input$nv_Date else NA)
      cat("                ajout d'une ligne :")
      print(Nv_ligne)
      
      df_classif <<- bind_rows(df_classif, Nv_ligne)
    }
    
    
    cat('                              _ 3\n')   
    
    # ~~~~{    On ré-identifie tout    }~~~~
    df_identifie <- RV$df_identifie %>%
      select(Date, libelle, Debit, Compte) %>%
      fun_classif(df_classif)
    
    df_resume_trimestre <- f_resume_trimestre(df_identifie)
    
    list_col <- f_couleurs(df_resume_trimestre)
    
    RV$df_classif <- df_classif %>%
      # mutate(classe = factor(classe, levels(df_resume_trimestre$classe)),
      #        super_classe = factor(super_classe, levels(df_resume_trimestre$super_classe))) %>%
      arrange(super_classe, classe)
    
    RV$df_identifie <- df_identifie
    RV$df_resume_trimestre <- df_resume_trimestre
    RV$list_col <- list_col
    
    # ~~~~{    Reset des champs    }~~~~
    cat('                              _ 4\n')
    updateTextInput(inputId = 'nv_Marq', value = '')
    updateDateInput(inputId = 'nv_Date', value = as.Date(character(0)))
    
    
    cat('                              _ fin\n\n')   
    
  })
  
  
  # ~~~~{    Tab Classification    }~~~~
  # output$tab_classif <- renderTable(arrange(RV$df_classif, super_classe, classe))
  
  output$tab_classif <- renderDT(
    {
      cat('>> Identification > render classif\n\n')
      mutate(RV$df_classif,
             Date = format(Date, '%d/%m/%Y'),
             classe = as.factor(classe),
             super_classe = as.factor(super_classe))},
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
  
  
  # ~~~~{    Tab non assignés    }~~~~
  output$tab_nonIdentifies <- renderTable({
    # print(!is.null(RV$df_identifie))
    cat('>> Identification > non identifiés\n\n')
    # print(RV$df_identifie)
    
    tab <- NULL
    
    if(!is.null(RV$df_identifie)){
      tab <- RV$df_identifie %>%
        filter(is.na(lib_id), !is.na(Debit)) %>%
        arrange(desc(Debit)) %>%
        mutate(Date = format(Date, '%d/%m/%Y')) %>%
        select(Date, libelle, Debit, Compte)
    }
    
    tab
  })
  
  # ~~~~{    message    }~~~~
  R_double_identification <- reactive(double_identification)
  
  output$erreur_id <- renderText({
    cat('>> Identification > msg erreur\n\n')
    print(R_double_identification())
    R_double_identification()
  })
  
  
  
  
  
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####               SERVER : Page 2 - Graphiques                 #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  # ~~~~{    Mise à jour de la taille du curseur de dates    }~~~~
  observeEvent(RV$df_identifie, {
    MIN <- min(RV$df_identifie$Date, na.rm = TRUE)
    MAX <- max(RV$df_identifie$Date, na.rm = TRUE)
    updateSliderInput(inputId = 'trimestre_subset', 
                      min = MIN, 
                      max = MAX,
                      value = c(MIN, MAX)
    )
  })
  
  
  # ~~~~{    Graphique    }~~~~
  output$graph <- renderGirafe({
    cat('>> Graphiques > graphique _ 1\n')
    cat('      ', input$typeGraph, '\n')
    
    df_resume <- filter(RV$df_resume_trimestre,
                        trimestre >= input$trimestre_subset[1],
                        trimestre <=input$trimestre_subset[2])
    
    # df_resume <- df_resume_trimestre()
    
    if(input$typeGraph == 'BonbonMiel_tot')
      plot <- BonbonMiel_unique_giraph(df_resume, RV$list_col)
    
    
    if(input$typeGraph == 'BonbonMiel_trim')
      plot <- BonbonMiel_trimestriel_giraph(df_resume, RV$list_col)
    
    if(input$typeGraph == 'courbe')
      plot <- Courbe_empile_giraph(df_resume, RV$list_col)
    
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
    
    
    if(input$typeGraph == 'BonbonMiel_tot' & !is.null(giraph_select()))
      tab <- RV$df_identifie %>%
      filter(if(giraph_select() == 'NA') is.na(classe) else classe == giraph_select() | super_classe == giraph_select()) %>%
      arrange(desc(Debit)) %>%
      mutate(Date = as.character(Date)) %>%
      select(Date, libelle, Debit, Compte)
    
    if(input$typeGraph == 'BonbonMiel_trim' & !is.null(giraph_select())){
      
      select_classe <- str_extract(giraph_select(), '^[^/]+')
      
      # début et fin du trimestre
      centreTrimestre <- as.Date(str_extract(giraph_select(), '[^/]+$'))
      deb <- as.Date(paste0(format(centreTrimestre - 30, '%Y-%m-'), 01))
      fin <- as.Date(paste0(format(centreTrimestre + 60, '%Y-%m-'), 01))
      cat('deb:', as.character(deb), 'fin:', as.character(fin), '\n')
      
      tab <- RV$df_identifie %>%
        filter(if(select_classe == 'NA') is.na(classe) else classe == select_classe | super_classe == select_classe,
               Date >= deb, Date < fin) %>%
        arrange(desc(Debit)) %>%
        mutate(Date = as.character(Date)) %>%
        select(Date, libelle, Debit, Compte)
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
      tab <- RV$df_resume_trimestre %>%
        mutate(trimestre = as.character(trimestre))
    
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
  
  
  
}


