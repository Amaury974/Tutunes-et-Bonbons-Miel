#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Interface Utilisateur pour Tutune & Bonbon Miel

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤



ui <- navbarPage(
    
    title ='Tutunes et Bonbons Miel',
    
    #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
    #####                   UI : Page 1 - Data IN                    #####
    #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
    
    tags$img(
        src = "logo_TBM_dessature.png",
        alt = 'logo TnBBM',
        style = 'position: fixed ; right: 10% ; z-index: -1',
        height = '90%'
      ),
    
    tabPanel(
      value = 'data_selection',
      title = "Selection des données", 
      
      
      
      #--------------------------------------------------------------------#
      #####                       __ Sauvegarde                        #####
      #--------------------------------------------------------------------#
      fluidRow(
        column(1,
               actionButton(inputId = 'save',
                            label = NULL,
                            icon = icon('floppy-disk'))
        ),
        column(3,
               checkboxInput(inputId = 'sauv_auto',
                             label = "sauvegarde automatique ttes les 5mn (non implémenté)",
                             value = TRUE,
                             width = '100%'),
        ),
        
        column(6,
               textInput(inputId = 'dir_sauvegarde',
                         label = NULL,
                         value = dir_sauvegarde,
                         width = '100%')
        )
      ),
      #--------------------------------------------------------------------#
      #####                      __ Importation                        #####
      #--------------------------------------------------------------------#
      h2('Importation'),
      tags$hr(),
      
      # fluidRow(
      #   column(8,
               fileInput(
                 'input_releves',
                 label = 'Relevés de compte (plusieurs fichiers .csv ou .pdf)',
                 # title = 'Relevés de compte (plusieurs fichiers .csv ou .pdf)',
                 multiple = TRUE,
                 width = '600px',
                 accept = c("text/csv", 'text/pdf', '.csv', '.pdf')
               ),
               
               fileInput(
                 'input_identifie',
                 label = 'Relevé de comptes catégorisé',
                 # title = 'Relevé de comptes catégorisé',
                 multiple = FALSE,
                 width = '600px',
                 accept = c("text/csv")
               ),
               
               # fileInput(
               #   'input_resume',
               #   label = 'Résumé Trimestriel',
               #   # title = 'Résumé Trimestriel',
               #   multiple = FALSE,
               #   width = '600px',
               #   accept = c("text/csv")
               # )
        # ),
        
        
        # column(4,
               # imageOutput('logo_TBM', height = '300px')
               # plotOutput('gouzou1', height = '300px', width = '300px')
        # ),
      # ),
      
      
      
      #--------------------------------------------------------------------#
      #####                    __ Identification                       #####
      #--------------------------------------------------------------------#
      
      h2('Identification'),
      hr(),
      
      # ~~~~{    Importer/Exporter Classification    }~~~~
      fluidRow(
        column(4,
               fileInput(
                 inputId = 'upload_classif',
                 label = "Importer fichier d'identification",
                 accept = c("text/csv"), 
                 width = '500px'
               )
        ),
        
        column(4,
               br(),
               downloadButton(
                 outputId = "download_classif", 
                 label = "Enregistrer fichier d'identification")
               
        ),
        
        column(4,
               textOutput(outputId = 'erreur_id')
               
        ),
      ),
      
      
      
      fluidRow(
        column(6,
               h3('Classification'),
               # ~~~~{    nouvel element de classification    }~~~~
               
               p(HTML('<p>Nouvel element de classification : <i> <small> laisser super Classe vide pour masquer </small> </i> </p>')), 
               fluidRow(
                 column(8,
                        fluidRow(
                          column(3,
                                 textInput(
                                   inputId = 'nv_supClasse',
                                   label = 'super Classe'
                                 )
                          ), 
                          
                          column(3,
                                 textInput(
                                   inputId = 'nv_Classe',
                                   label = 'Classe'
                                 )
                          ), 
                          
                          column(6,
                                 textInput(
                                   inputId = 'nv_Marq',
                                   label = "marqueur"
                                 )
                          ),
                        )
                 ),
                 column(4,
                        fluidRow(
                          # column(1,
                          #        br(),
                          #        checkboxInput(
                          #          inputId = 'use_date',
                          #          label = '')
                          # ),
                          column(8,
                                 dateInput(
                                   inputId = 'nv_Date',
                                   label = "(Date)",
                                   format = 'dd/mm/yyyy',
                                   language = 'fr',
                                   value = '',
                                   weekstart = 1
                                 )
                          ),
                          
                          column(3,
                                 br(),
                                 actionButton(
                                   inputId = 'MaJ_Classe',
                                   label = '',
                                   icon = icon('arrows-rotate')
                                 ),
                                 # textOutput('erreur_nvclassif')
                          )
                        )
                 )
               ),
               
               # ~~~~{    tableau classification    }~~~~
               
               DT::DTOutput("tab_classif")
               
               # tableOutput('tab_classif')
        ),
        
        
        
        # ~~~~{    tableau reste à Classer    }~~~~
        
        column(6,
               h3('Lignes non classées'),
               tableOutput('tab_nonIdentifies'),
        ),
        
        
      ),
    ),
    
    
    #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
    #####                  UI : Page 2 - Graphiques                  #####
    #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
    
    tabPanel(
      value = 'graph',
      title = "Graphiques", 
      
      # ~~~~{    en-tête : options    }~~~~
      fluidRow(
        column(2),
        
        column(2,
               sliderTextInput(inputId = "periode_subset",
                               label = "Période visualisée",
                               choices = letters, 
                               selected = c('a','z')
                               )
               
               
               # sliderInput(
               #   inputId = "periode_subset",
               #   label = "Période visualisée",
               #   min = deb.Trimestre(Sys.Date()-360), # sera mis à jour lorsqu'on importe des données
               #   max = deb.Trimestre(Sys.Date()),     # idem
               #   value = c(deb.Trimestre(Sys.Date()-360), deb.Trimestre(Sys.Date())),
               #   step = 30
               # )
        ), 
        column(2,
               selectInput(
                 inputId = 'echelle',
                 label = 'Echelle',
                 choices = list('Mois',
                                'Trimestre',
                                'Semestre',
                                'An (in progress)' = 'An'),
                 selected = 'Trimestre'
               )
        ),
        
        column(4,
               selectInput(
                 inputId = 'typeGraph',
                 label = 'Type de graphique',
                 choices = list(#'résumé en un seul gros bonbon miel'='Gro_BonbonMiel',
                                'un bonbon miel par periode'='Ti_BonbonMiel',
                                'histogramme par periode'='histogramme_periode',
                                'histogramme comparaison par Classe'='histogramme_Classe',
                                'courbes empilés (in proogress)'='Courbe_empile_giraph',
                                'Vérification des données'='Verification_donnees')
               )
        ), 
      ),
      
      # ~~~~{    Graphique    }~~~~
      girafeOutput('graph'), #height = '700px', width = '100%'),
      
      # ~~~~{    tableau détail selectionné    }~~~~
      h3('Lignes sélectionnées'),
      hr(),
      tableOutput(outputId = 'clicked_tab'),
    ),
    
    
    #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
    #####                  UI : Page 3 - Data Display                #####
    #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
    
    tabPanel(
      
      value = 'data_display',
      title = "DATA", 
      

      column(2),
      column(4,
             selectInput(
               inputId = 'data_shown',
               label = 'Tableau de données',
               choices = list('Relevé de comptes catégorisés' = 'Releve_de_comptes_categorises',
                              'Résumé Trimestriel'='Resume_trimestriel')
             ),
      ),
      
      column(4,
             tags$br(),
             downloadButton("downloadData", "Télécharger")
      ),
      column(2),
      
      column(12,
             tableOutput(outputId = "data_table")
      )
    ),
    
    
  )





