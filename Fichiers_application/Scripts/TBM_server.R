#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Server pour Tutune & Bonbon Miel

# A.Jorant - Nov 2024

# R version 4.4.2
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

double_identification <- ''

server <- function(input, output) {
  
  RV <- reactiveValues(data=NULL)
  RV$df_identification <- df_identification
  # RV$df_identifie <- df_identifie
  # RV$df_resume_trimestre <- df_resume_trimestre
  # RV$list_col <- list_col
  
  # RV$erreur_nvclassif <- ''
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                  SERVER : Page 1 - Data IN                 #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
 
  #--------------------------------------------------------------------#
  #####                     __ Importation                         #####
  #--------------------------------------------------------------------#
  
  # ~~~~{    Depuis relevés de comptes    }~~~~
  observeEvent(
    input$input_releves,{
      cat('Importation _ 1\n')
      input_data <- input$input_releves
      print(input_data)
      
      # ~~~~{    Banque Postale    }~~~~
      releve_Poste <- data.frame()
      for(dir_i in filter(input_data, type == 'application/pdf')$datapath){
        releve_Poste <- bind_rows(releve_Poste, extraction_Poste(dir_i))
        releve_Poste$Compte <- 'BP_Amaury'
      }
      
      # ~~~~{    Fortuneo    }~~~~
      
      releve_Fortuneo <- data.frame()
      # for(dir_i in filter(input_data, type == 'text/csv')$datapath){
      for(dir_i in filter(input_data, str_detect(name, 'csv$'))$datapath){
        
        releve_Fortuneo <- bind_rows(releve_Fortuneo, extraction_Fortuneo(dir_i))
        releve_Fortuneo$Compte <- 'Fortuneo_commun'
      }
      
      
      
      cat('            _ 2\n')
      
      # ~~~~{    mise en forme    }~~~~
      df_identifie <-
        bind_rows(releve_Poste, releve_Fortuneo) %>%
        f_identification(RV$df_identification)
      
      # print(df_identifie)
      # saveRDS(df_identifie, file='D:/apis_/Documents/R/Tutunes et Bonbon Miel/df_identifie.RData')
      
      cat('            _ 3\n')
      
      df_resume_trimestre <- f_resume_trimestre(df_identifie)
      cat('            _ 4\n')
      # list_col <- f_couleurs(df_resume_trimestre)
      
      RV$df_identifie <- df_identifie
      RV$df_resume_trimestre <- df_resume_trimestre
      # RV$list_col <- list_col
      
      cat('            _ fin\n')
      
      
    })
  
  
  # ~~~~{    Depuis catégorisé    }~~~~
  observeEvent(
    input$input_identifie,{
      cat('Load Cat _ 1\n')
      
      # print(input$input_identifie)
      df_identifie <- read.csv2(input$input_identifie$datapath)%>%
        mutate(Date = as.Date(Date))
      
      df_resume_trimestre <- f_resume_trimestre(df_identifie)
      
      # list_col <- f_couleurs(df_resume_trimestre)
      
      RV$df_identifie <- df_identifie
      RV$df_resume_trimestre <- df_resume_trimestre
      # RV$list_col <- list_col
      
      cat('         _ fin\n')
      
    })
  
  # ~~~~{    Depuis résumé    }~~~~
  observeEvent(
    input$input_resume,{
      cat('Load Res _ 1\n')
      
      df_resume_trimestre <- read.csv2(input$input_resume$datapath) %>%
        mutate(trimestre = as.Date(trimestre))
      
      # list_col <- f_couleurs(df_resume_trimestre)
      
      RV$df_resume_trimestre <- df_resume_trimestre
      # RV$list_col <- list_col
      
      cat('         _ fin\n')
      
    })
  
  
  observeEvent(RV$df_resume_trimestre,{
    cat('sauvegarde _ 1\n')
    RV$list_col <- f_couleurs(df_resume_trimestre)
    
    if(auto_save)
      save(df_identifie, df_resume_trimestre, list_col, file = 'dernier_ouvert.RData')
    cat('           _ fin\n')
  })
  
  #--------------------------------------------------------------------#
  #####                     __ Identification                      #####
  #--------------------------------------------------------------------#
  
  # ~~~~{    téléchargement fichier d'identification    }~~~~
  
  # ~~~~{    Importation    }~~~~
  observeEvent(
    input$upload_classif,{
      cat('Upload Classif _ 1\n')
      df_identification <<- read.csv2(input$upload_classif$datapath)
      RV$df_identification <- df_identification
      cat('         _ fin\n')
      
    })
  
  # ~~~~{    Exportation    }~~~~
  output$download_classif <- downloadHandler(
    filename = 'Classification_dépenses.csv',
    content = function(file) {
      write.csv2(df_identification, file, row.names = FALSE)
    }
  )
  
  
  
  # ~~~~{    MaJ    }~~~~
  observeEvent(input$MaJ_classe, {
    cat('MaJ classe _ 1\n')
    
    if(!any(c(input$nv_supClasse, input$nv_Classe, input$nv_Marq) == '')){
      cat('           _ 2\n')   
      
      # !length(as.Date(character(0)))
      
      
      print(input$nv_Date)
      
      Nv_ligne <-data.frame(super_classe = input$nv_supClasse,
                            classe = input$nv_Classe,
                            lib_id = toupper(input$nv_Marq),
                            Date = if(length(input$nv_Date)) input$nv_Date else NA)
      print(Nv_ligne)
      
      df_identification <<- bind_rows(df_identification, Nv_ligne)
    }
    
    
    cat('           _ 3\n')   
    
    # ~~~~{    On ré-identifie tout    }~~~~
    df_identifie <- RV$df_identifie %>%
      select(Date, libelle, Debit, Compte) %>%
      f_identification(df_identification)
    
    df_resume_trimestre <- f_resume_trimestre(df_identifie)
    
    list_col <- f_couleurs(df_resume_trimestre)
    
    RV$df_identification <- df_identification %>%
      # mutate(classe = factor(classe, levels(df_resume_trimestre$classe)),
      #        super_classe = factor(super_classe, levels(df_resume_trimestre$super_classe))) %>%
      arrange(super_classe, classe)
    
    RV$df_identifie <- df_identifie
    RV$df_resume_trimestre <- df_resume_trimestre
    RV$list_col <- list_col
    
    # ~~~~{    Reset des champs    }~~~~
    cat('           _ 4\n')
    updateTextInput(inputId = 'nv_Marq', value = '')
    updateDateInput(inputId = 'Date', value = as.Date(character(0)))
    
    
    cat('           _ fin\n')   
    
  })
  
  
  # ~~~~{    Tab Classification    }~~~~
  # output$tab_classif <- renderTable(arrange(RV$df_identification, super_classe, classe))
  
  output$tab_classif <- renderDT(
    mutate(RV$df_identification,
           classe = as.factor(classe),
           super_classe = as.factor(super_classe)),
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
  # 
  # observeEvent(input$tab_classif_cell_edit, {
  #   cat('edition DT\n')
  #   df_identification[input$tab_classif_cell_edit$row, input$tab_classif_cell_edit$col] <<- input$tab_classif_cell_edit$value
  # })
  
  
  # ~~~~{    Tab non assignés    }~~~~
  output$tab_nonIdentifies <- renderTable({
    cat('non identifiés _1\n')
    # print(RV$df_identifie)
    # print(!is.null(RV$df_identifie))
    tab <- NULL
    
    if(!is.null(RV$df_identifie)){
      tab <- RV$df_identifie %>%
        filter(is.na(lib_id), !is.na(Debit)) %>%
        arrange(desc(Debit)) %>%
        mutate(Date = as.character(Date)) %>%
        select(Date, libelle, Debit, Compte)
    }
    tab
    
  })
  
  # ~~~~{    message d'erreur    }~~~~
  R_double_identification <- reactive(double_identification)
  output$erreur_id <- renderText({
    cat('msg erreur\n')
    print(R_double_identification())
    R_double_identification()})
  
  
  
  
  
  
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
  
  
  
  output$graph <- renderGirafe({
    
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
    
    
    plot
  })
  
  giraph_select <- reactive({
    cat('clicked\n')
    input$graph_selected
  })
  
  output$clicked_tab <- renderTable({    
    
    cat('clicked tab : 1\n')
    print(giraph_select())
    tab <- NULL
    
    if(input$typeGraph == 'BonbonMiel_tot' & !is.null(giraph_select()))
      tab <- RV$df_identifie %>%
      filter(if(giraph_select() == 'NA') is.na(classe) else classe == giraph_select()) %>%
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
        filter(if(select_classe == 'NA') is.na(classe) else classe == select_classe, 
               Date >= deb, Date < fin) %>%
        arrange(desc(Debit)) %>%
        mutate(Date = as.character(Date)) %>%
        select(Date, libelle, Debit, Compte)
    }
    
    cat('            : fin\n')
    
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


