#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Suivi des dépenses depuis relevés de comptes
#            Banque postale et Fortuneo

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
rm(list=ls()); gc() ; options(warn = 1)

#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#
#************************************************************************************#

#####                              INITIALLISATION                               #####

#************************************************************************************#
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''#



library(dplyr); options(dplyr.summarise.inform = FALSE )
library(stringr)
library(tidyr)
library(ggplot2); theme_set(theme_light()) ; options(ggplot2.continuous.colour="viridis", ggplot2.continuous.fill = "viridis")
library(colorspace)
library(ggiraph)
# library(ggrepel)

library(shiny)
library(DT)
library(glue) # click sur un graphique interactif
# library(bslib)

source('TBM_extraction_comptes_BP.R')
source('TBM_extraction_comptes_Fortuneo.R')
source('TBM_identification_libelle.R')
source('TBM_manipulation_tableaux.R')
source('TBM_graph.R')
source('TBM_util.R')
source('../GOUZOU.R')

# Gouzou_showoff()



source('TBM_UI.R')
source('TBM_Server.R')

shinyApp(ui = ui, server = server)








#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#
#************************************************************************************#

#####                                     UI                                     #####

#************************************************************************************#
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''#

# 
# ui <- #page_fluid(
#   navbarPage(
#     
#     title ='Analyse des relevés de comptes v2',
#     
#     #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#     #####                   UI : Page 1 - Data IN                    #####
#     #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#     
#     
#     
#     tabPanel(
#       value = 'data_selection',
#       title = "Selection des données", 
#       
#       #--------------------------------------------------------------------#
#       #####                      __ Importation                        #####
#       #--------------------------------------------------------------------#
#       h2('Importation'),
#       tags$hr(),
#       
#       fluidRow(
#         column(8,
#                fileInput(
#                  inputId = 'input_releves',
#                  label = 'Relevés de compte (plusieurs fichiers .csv ou .pdf)',
#                  multiple = TRUE,
#                  width = '600px',
#                  accept = c("text/csv", 'text/pdf', '.csv', '.pdf')
#                ),
#                
#                
#                fileInput(
#                  inputId = 'input_identifie',
#                  label = "Relevé de comptes catégorisé",
#                  multiple = FALSE,
#                  width = '600px',
#                  accept = c("text/csv")
#                ),
#                
#                fileInput(
#                  inputId = 'input_resume',
#                  label = "Résumé Trimestriel",
#                  multiple = FALSE,
#                  width = '600px',
#                  accept = c("text/csv")
#                )
#         ),
#         
#         
#         column(4,
#                plotOutput('gouzou1', height = '300px', width = '300px')
#         ),
#       ),
#       
#       
#       
#       #--------------------------------------------------------------------#
#       #####                    __ Identification                       #####
#       #--------------------------------------------------------------------#
#       
#       h2('Identification'),
#       hr(),
#       
#       # ~~~~{    Importer/Exporter Classification    }~~~~
#       fluidRow(
#         column(4,    # à faire
#                fileInput(
#                  inputId = 'upload_classif',
#                  label = "Importer fichier d'identification",
#                  accept = c("text/csv"), 
#                  width = '500px'
#                )
#         ),
#         
#         column(4,    # à faire
#                br(),
#                downloadButton(
#                  outputId = "download_classif", 
#                  label = "Télécharger fichier d'identification")
#                
#         ),
#       ),
#       
#       
#       
#       fluidRow(
#         column(6,
#                h3('Classification'),
#                # ~~~~{    nouvel element de classification    }~~~~
#                p('Nouvel element de classification'),
#                fluidRow(
#                  column(3,
#                         textInput(
#                           inputId = 'supClasse',
#                           label = 'super Classe'
#                         )
#                  ), 
#                  
#                  column(4,
#                         textInput(
#                           inputId = 'Classe',
#                           label = 'Classe'
#                         )
#                  ), 
#                  
#                  column(4,
#                         textInput(
#                           inputId = 'Marq',
#                           label = "marqueur"
#                         )
#                  ),
#                  
#                  column(1,
#                         br(),
#                         actionButton(
#                           inputId = 'MaJ_classe',
#                           label = '',
#                           icon = icon('arrows-rotate')
#                         ),
#                         # textOutput('erreur_nvclassif')
#                  ),
#                ),
#                # ~~~~{    tableau classification    }~~~~
#                
#                DT::DTOutput("tab_classif")
#                
#                # tableOutput('tab_classif')
#         ),
#         
#         
#         
#         # ~~~~{    tableau reste à classer    }~~~~
#         
#         column(6,
#                h3('Lignes non classées'),
#                tableOutput('tab_nonIdentifies'),
#         ),
#         
#         
#       ),
#     ),
#     
#     
#     #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#     #####                  UI : Page 2 - Graphiques                  #####
#     #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#     
#     tabPanel(
#       value = 'graph',
#       title = "Graphiques", 
#       
#       fluidRow(
#         column(2),
#         
#         column(4,
#                
#                sliderInput(
#                  inputId = "trimestre_subset",
#                  label = "Période visualisée",
#                  min = deb.Trimestre(Sys.Date()-360),
#                  max = deb.Trimestre(Sys.Date()),
#                  value = c(deb.Trimestre(Sys.Date()-360), deb.Trimestre(Sys.Date())),
#                  step = 90
#                )
#         ), 
#         
#         column(4,
#                selectInput(
#                  inputId = 'typeGraph',
#                  label = 'Type de graphique',
#                  choices = list('résumé en un seul gros bonbon miel'='BonbonMiel_tot',
#                                 'un bonbon miel  par trimestre'='BonbonMiel_trim',
#                                 'courbes empilés'='courbe')
#                )
#         ), 
#       ),
#       
#       # plotOutput('graph') 
#       girafeOutput('graph', height = '800px', width = '100%'),
#       
#       # h3('T ou ?'),
#       tableOutput(outputId = 'clicked_tab'),
#       # h3('ben et alors')
#     ),
#     
#     
#     #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#     #####                  UI : Page 3 - Data Display                #####
#     #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#     
#     tabPanel(
#       
#       value = 'data_display',
#       title = "DATA", 
#       
#       column(2),
#       column(4,
#              selectInput(
#                inputId = 'data_shown',
#                label = 'Tableau de données',
#                choices = list('Relevé de comptes catégorisés' = 'Releve_de_comptes_categorises',
#                               'Résumé Trimestriel'='Resume_trimestriel')
#              ),
#       ),
#       
#       column(4,
#              tags$br(),
#              downloadButton("downloadData", "Download")
#       ),
#       column(2),
#       
#       column(12,
#              tableOutput(outputId = "data_table")
#       )
#     ),
#     
#     
#   )



#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#
#************************************************************************************#

#####                                   SERVER                                   #####

#************************************************************************************#
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''#



# server <- function(input, output) {
#   
#   RV <- reactiveValues(data=NULL)
#   RV$df_identification <- df_identification
#   RV$erreur_nvclassif <- ''
#   
#   #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#   #####                  SERVER : Page 1 - Data IN                 #####
#   #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#   
#   #--------------------------------------------------------------------#
#   #####                     __ Importation                         #####
#   #--------------------------------------------------------------------#
#   
#   # ~~~~{    Depuis relevés de comptes    }~~~~
#   observeEvent(
#     input$input_releves,{
#       cat('Importation _ 1\n')
#       input_data <- input$input_releves
#       print(input_data)
#       
#       # ~~~~{    Banque Postale    }~~~~
#       releve_Poste <- data.frame()
#       for(dir_i in filter(input_data, type == 'application/pdf')$datapath){
#         releve_Poste <- bind_rows(releve_Poste, extraction_Poste(dir_i))
#         releve_Poste$Compte <- 'BP_Amaury'
#       }
#       
#       # ~~~~{    Fortuneo    }~~~~
#       
#       releve_Fortuneo <- data.frame()
#       for(dir_i in filter(input_data, type == 'text/csv')$datapath){
#         releve_Fortuneo <- bind_rows(releve_Fortuneo, extraction_Fortuneo(dir_i))
#         releve_Fortuneo$Compte <- 'Fortuneo_commun'
#       }
#       
#       cat('            _ 2')
#       
#       # ~~~~{    mise en forme    }~~~~
#       df_identifie <-
#         bind_rows(releve_Poste, releve_Fortuneo) %>%
#         f_identification(RV$df_identification)
#       
#       df_resume_trimestre <- f_resume_trimestre(df_identifie)
#       
#       list_col <- f_couleurs(df_resume_trimestre)
#       
#       RV$df_identifie <- df_identifie
#       RV$df_resume_trimestre <- df_resume_trimestre
#       RV$list_col <- list_col
#       
#       cat('            _ fin')
#       
#       
#     })
#   
#   
#   # ~~~~{    Depuis catégorisé    }~~~~
#   observeEvent(
#     input$input_identifie,{
#       cat('Load Cat _ 1\n')
#       
#       # print(input$input_identifie)
#       df_identifie <- read.csv2(input$input_identifie$datapath)%>%
#         mutate(Date = as.Date(Date))
#       
#       df_resume_trimestre <- f_resume_trimestre(df_identifie)
#       
#       list_col <- f_couleurs(df_resume_trimestre)
#       
#       RV$df_identifie <- df_identifie
#       RV$df_resume_trimestre <- df_resume_trimestre
#       RV$list_col <- list_col
#       
#       cat('         _ fin\n')
#       
#     })
#   
#   # ~~~~{    Depuis résumé    }~~~~
#   observeEvent(
#     input$input_resume,{
#       cat('Load Res _ 1\n')
#       
#       df_resume_trimestre <- read.csv2(input$input_resume$datapath) %>%
#         mutate(trimestre = as.Date(trimestre))
#       
#       list_col <- f_couleurs(df_resume_trimestre)
#       
#       RV$df_resume_trimestre <- df_resume_trimestre
#       RV$list_col <- list_col
#       
#       cat('         _ fin\n')
#       
#     })
#   
#   
#   #--------------------------------------------------------------------#
#   #####                     __ Identification                      #####
#   #--------------------------------------------------------------------#
#   
#   # ~~~~{    téléchargement fichier d'identification    }~~~~
#   
#   # ~~~~{    Importation    }~~~~
#   observeEvent(
#     input$upload_classif,{
#       cat('Upload Classif _ 1\n')
#       df_resume_trimestre <- read.csv2(input$upload_classif$datapath)
#       RV$df_resume_trimestre <- df_resume_trimestre
#       cat('         _ fin\n')
#       
#     })
#   
#   # ~~~~{    Exportation    }~~~~
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       paste(input$download_classif, ".csv", sep = "")
#     },
#     content = function(file) {
#       write.csv2(df_resume_trimestre, file, row.names = FALSE)
#     }
#   )
#   
#   
#   # ~~~~{    MaJ    }~~~~
#   observeEvent(input$MaJ_classe, {
#     cat('MaJ classe _ 1\n')
#     
#     if(!any(c(input$supClasse, input$Classe, input$Marq) == '')){
#       cat('           _ 2\n')   
#       Nv_ligne <-data.frame(super_classe = input$supClasse,
#                             classe = input$Classe,
#                             lib_id = input$Marq)
#       
#       df_identification <<- bind_rows(df_identification, Nv_ligne)
#     }
#     
#     cat('           _ 3\n')   
#     
#     # ~~~~{    On ré-identifie tout    }~~~~
#     df_identifie <- RV$df_identifie %>%
#       select(Date, libelle, Debit, Compte) %>%
#       f_identification(df_identification)
#     
#     df_resume_trimestre <- f_resume_trimestre(df_identifie)
#     
#     list_col <- f_couleurs(df_resume_trimestre)
#     
#     RV$df_identification <- df_identification %>%
#       mutate(classe = factor(classe, levels(df_resume_trimestre$classe)),
#              super_classe = factor(super_classe, levels(df_resume_trimestre$super_classe))) %>%
#       arrange(super_classe, classe)
#     
#     RV$df_identifie <- df_identifie
#     RV$df_resume_trimestre <- df_resume_trimestre
#     RV$list_col <- list_col
#     cat('           _ fin\n')   
#     
#   })
#   
#   
#   # ~~~~{    Tab Classification    }~~~~
#   
#   output$tab_classif <- renderDT(
#     RV$df_identification,
#     selection = 'none',
#     editable = 'row', 
#     filter = 'top',
#     server = FALSE,
#     
#     options = list(
#       pageLength = 20,
#       autoWidth = TRUE,
#       ordering = TRUE
#     ),
#     
#   )
#   
#   observeEvent(input$tab_classif_cell_edit, {
#     cat('edition DT\n')
#     df_identification[input$tab_classif_cell_edit$row, input$tab_classif_cell_edit$col] <<- input$tab_classif_cell_edit$value
#   })
#   
#   
#   # ~~~~{    Tab non assignés    }~~~~
#   output$tab_nonIdentifies <- renderTable({
#     cat('non identifiés _1\n')
#     # print(RV$df_identifie)
#     # print(!is.null(RV$df_identifie))
#     tab <- NULL
#     
#     if(!is.null(RV$df_identifie)){
#       tab <- RV$df_identifie %>%
#         filter(is.na(lib_id), !is.na(Debit)) %>%
#         arrange(desc(Debit)) %>%
#         mutate(Date = as.character(Date)) %>%
#         select(Date, libelle, Debit, Compte)
#     }
#     tab
#     
#   })
#   
#   
#   
#   
#   
#   
#   
#   
#   #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#   #####               SERVER : Page 2 - Graphiques                 #####
#   #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#   
#   
#   output$graph <- renderGirafe({
#     
#     df_resume <- filter(RV$df_resume_trimestre,
#                         trimestre >= input$trimestre_subset[1],
#                         trimestre <=input$trimestre_subset[2])
#     
#     # df_resume <- df_resume_trimestre()
#     
#     if(input$typeGraph == 'BonbonMiel_tot')
#       plot <- BonbonMiel_unique_giraph(df_resume, RV$list_col)
#     
#     if(input$typeGraph == 'BonbonMiel_trim')
#       plot <- BonbonMiel_trimestriel_giraph(df_resume, RV$list_col)
#     
#     if(input$typeGraph == 'courbe')
#       plot <- Courbe_empile_giraph(df_resume, RV$list_col)
#     
#     
#     plot
#   })
#   
# giraph_select <- reactive({
#     cat('clicked\n')
#     input$graph_selected
#   })
#   
#   output$clicked_tab <- renderTable({    
#     
#     
#     cat('clicked tab : 1\n')
#     print(giraph_select())
#     tab <- NULL
#     
#     if(input$typeGraph == 'BonbonMiel_tot' & !is.null(giraph_select()))
#       tab <- RV$df_identifie %>%
#       filter(classe %in% giraph_select(), !is.na(Debit)) %>%
#       arrange(desc(Debit)) %>%
#       mutate(Date = as.character(Date)) %>%
#       select(Date, libelle, Debit, Compte)
#     
#     if(input$typeGraph == 'BonbonMiel_trim' & !is.null(giraph_select()))
#       tab <- RV$df_identifie %>%
#       filter(classe %in% str_extract(giraph_select(), '^[^/]+'), 
#              trimestre %in% str_extract(giraph_select(), '[^/]+$'), 
#              !is.na(Debit)) %>%
#       arrange(desc(Debit)) %>%
#       mutate(Date = as.character(Date)) %>%
#       select(Date, libelle, Debit, Compte)
#     
#     cat('            : fin\n')
#     
#     tab
#   })
#   
#   #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#   #####                SERVER : Page 3 - Data Display              #####
#   #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#   
#   # ~~~~{    Tableau séléctionné    }~~~~
#   tab <- reactive({
#     
#     if(input$data_shown == 'Resume_trimestriel')
#       tab <- RV$df_resume_trimestre %>%
#         mutate(trimestre = as.character(trimestre))
#     
#     if(input$data_shown == 'Releve_de_comptes_categorises')
#       tab <- RV$df_identifie %>%
#         mutate(Date = as.character(Date))
#     
#     tab
#   })
#   
#   # ~~~~{    Affichage du dit tableau    }~~~~
#   output$data_table <- renderTable(tab())
#   
#   
#   # ~~~~{    Telechargement    }~~~~
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       paste(input$data_shown, ".csv", sep = "")
#     },
#     content = function(file) {
#       write.csv2(tab(), file, row.names = FALSE)
#     }
#   )
#   #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#   #####                       SERVER : GOUZOUS                     #####
#   #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#   
#   output$gouzou1 <- renderPlot(GOUZOU_geom(type = 'brezel')+theme_void())
#   
#   
#   
# }




# shinyApp(ui = ui, server = server)






#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#
#************************************************************************************#

#####                                   OLD                                   #####

#************************************************************************************#
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''#
# fluidRow(
#   column(3,
#          selectInput(
#            inputId = 'input_supClasse',
#            label = 'selection de la super classe',
#            choices = c(unique(df_identification$super_classe), 'Nouvelle Super Classe')
#          ),
#          textInput(
#            inputId = 'nv_supClasse',
#            label = 'Ajouter une nouvelle super classe'
#          )
#   ), 
#   
#   column(3,
#          selectInput(
#            inputId = 'input_Classe',
#            label = 'selection de la classe',
#            choices = "selectionnez d'abord une super classe"
#          ),
#          textInput(
#            inputId = 'nv_Classe',
#            label = 'Ajouter une nouvelle classe'
#          )
#   ), 
#   
#   column(4,
#          textInput(
#            inputId = 'input_marq',
#            label = "nouveau marqueur d'identification"
#          )
#   ),
#   
#   column(2,
#          br(),br(),br(),
#          actionButton(
#            inputId = 'MaJ_classe',
#            label = 'Mise à jour'
#          ),
#          textOutput('erreur_nvclassif')
#   ),
# ),


# #--------------------------------------------------------------------#
# #####                     __ Identification                      #####
# #--------------------------------------------------------------------#
# 
# # ~~~~{    téléchargement fichier d'identification    }~~~~
# 
# ############# A FAIRE
# 
# 
# # ~~~~{    Nv element _ Select SuperClasse    }~~~~
# observeEvent(input$input_supClasse, {
#   cat('Nv Sclasse _ 1\n')
#   
#   Classe_IN <- filter(df_identification, super_classe == input$input_supClasse)$classe
#   updateSelectInput(inputId = 'input_Classe', choices = c(Classe_IN, 'Nouvelle Classe'))
#   
#   cat('           _ fin\n')
# })
# 
# 
# # ~~~~{    MaJ    }~~~~
# observeEvent(input$MaJ_classe, {
#   cat('MaJ classe _ 1\n')
#   
#   Nv_ligne <- data.frame(X=1)
#   Msg_erreur <- ''
#   
#   # ~~~~{    Super Classe    }~~~~
#   cat('           _ 2 superclasse\n')   
#   SupCl_select <- input$input_supClasse %in% unique(df_identification$super_classe)
#   SupCl_nv <- input$nv_supClasse != ''
#   
#   Msg_erreur <- paste0(Msg_erreur, 'erreur dans le choix de super Classe (0 ou 2 remplis)\n'[SupCl_select == SupCl_nv])
#   
#   Nv_ligne$super_classe <- first(c(c(input$input_supClasse, 
#                                      input$nv_supClasse)[c(SupCl_select, SupCl_nv)],
#                                    NA))
#   
#   
#   # ~~~~{    Classe    }~~~~
#   cat('           _ 3 classe\n')   
#   classe <- input$input_supClasse
#   
#   Cl_select <- input$input_Classe %in% unique(df_identification$classe)
#   Cl_nv <- input$nv_Classe != ''
#   
#   Cl_Erreur <- Cl_select == Cl_nv
#   RV$erreur_nvclassif <- paste0(Msg_erreur, 'erreur dans le choix de Classe (0 ou 2 remplis)\n'[Cl_select == Cl_nv])
#   
#   Nv_ligne$classe <- first(c(str_c(Nv_ligne$super_classe,
#                                    c(input$input_Classe, input$nv_Classe))[c(Cl_select, Cl_nv)],
#                              NA))
#   
#   # ~~~~{    Nouvelle Element    }~~~~
#   cat('           _ 4 element\n')   
#   Nv_ligne$lib_id <- first(c(toupper(input$input_marq), NA))
#   
#   
#   Nv_ligne <- Nv_ligne[,-1]
#   
#   print(Nv_ligne)
#   cat('erreur? _ ', Msg_erreur,'_\n')
#   
#   if(Msg_erreur == '') {
#     df_identification <<- bind_rows(df_identification, Nv_ligne)
#     
#     RV$df_identification <- df_identification
#     
#     # ~~~~{    On ré-identifie tout    }~~~~
#     df_identifie <- RV$df_identifie %>%
#       select(Date, libelle, Debit, Compte) %>%
#       f_identification(df_identification)
#     
#     df_resume_trimestre <- f_resume_trimestre(df_identifie)
#     
#     list_col <- f_couleurs(df_resume_trimestre)
#     
#     RV$df_identification <- df_identification
#     RV$df_identifie <- df_identifie
#     RV$df_resume_trimestre <- df_resume_trimestre
#     RV$list_col <- list_col
#     cat('           _ fin\n')   
#   }
# })
# 
# output$erreur_nvclassif <- renderText(RV$erreur_nvclassif)



