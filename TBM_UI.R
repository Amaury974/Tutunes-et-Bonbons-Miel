#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Interface Utilisateur pour Tutune & Bonbon Miel

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤







ui <- #page_fluid(
  navbarPage(
    
    title ='Tutunes et Bonbons Miel',
    
    #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
    #####                   UI : Page 1 - Data IN                    #####
    #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
    
    
    
    tabPanel(
      value = 'data_selection',
      title = "Selection des données", 
      
      #--------------------------------------------------------------------#
      #####                      __ Importation                        #####
      #--------------------------------------------------------------------#
      h2('Importation'),
      tags$hr(),
      
      fluidRow(
        column(8,
               fileInput(
                 inputId = 'input_releves',
                 label = 'Relevés de compte (plusieurs fichiers .csv ou .pdf)',
                 multiple = TRUE,
                 width = '600px',
                 accept = c("text/csv", 'text/pdf', '.csv', '.pdf')
               ),
               
               
               fileInput(
                 inputId = 'input_identifie',
                 label = 'Relevé de comptes catégorisé',
                 multiple = FALSE,
                 width = '600px',
                 accept = c("text/csv")
               ),
               
               fileInput(
                 inputId = 'input_resume',
                 label = 'Résumé Trimestriel',
                 multiple = FALSE,
                 width = '600px',
                 accept = c("text/csv")
               )
        ),
        
        
        column(4,
               plotOutput('gouzou1', height = '300px', width = '300px')
        ),
      ),
      
      
      
      #--------------------------------------------------------------------#
      #####                    __ Identification                       #####
      #--------------------------------------------------------------------#
      
      h2('Identification'),
      hr(),
      
      # ~~~~{    Importer/Exporter Classification    }~~~~
      fluidRow(
        column(4,    # à faire
               fileInput(
                 inputId = 'upload_classif',
                 label = "Importer fichier d'identification",
                 accept = c("text/csv"), 
                 width = '500px'
               )
        ),
        
        column(4,    # à faire
               br(),
               downloadButton(
                 outputId = "download_classif", 
                 label = "Télécharger fichier d'identification")
               
        ),
      ),
      
      
      
      fluidRow(
        column(6,
               h3('Classification'),
               # ~~~~{    nouvel element de classification    }~~~~
               p('Nouvel element de classification'),
               fluidRow(
                 column(3,
                        textInput(
                          inputId = 'supClasse',
                          label = 'super Classe'
                        )
                 ), 
                 
                 column(4,
                        textInput(
                          inputId = 'Classe',
                          label = 'Classe'
                        )
                 ), 
                 
                 column(4,
                        textInput(
                          inputId = 'Marq',
                          label = "marqueur"
                        )
                 ),
                 
                 column(1,
                        br(),
                        actionButton(
                          inputId = 'MaJ_classe',
                          label = '',
                          icon = icon('arrows-rotate')
                        ),
                        # textOutput('erreur_nvclassif')
                 ),
               ),
               # ~~~~{    tableau classification    }~~~~
               
               DT::DTOutput("tab_classif")
               
               # tableOutput('tab_classif')
        ),
        
        
        
        # ~~~~{    tableau reste à classer    }~~~~
        
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
      
      fluidRow(
        column(2),
        
        column(4,
               
               sliderInput(
                 inputId = "trimestre_subset",
                 label = "Période visualisée",
                 min = deb.Trimestre(Sys.Date()-360),
                 max = deb.Trimestre(Sys.Date()),
                 value = c(deb.Trimestre(Sys.Date()-360), deb.Trimestre(Sys.Date())),
                 step = 90
               )
        ), 
        
        column(4,
               selectInput(
                 inputId = 'typeGraph',
                 label = 'Type de graphique',
                 choices = list('résumé en un seul gros bonbon miel'='BonbonMiel_tot',
                                'un bonbon miel  par trimestre'='BonbonMiel_trim',
                                'courbes empilés'='courbe')
               )
        ), 
      ),
      
      # plotOutput('graph') 
      girafeOutput('graph', height = '700px', width = '100%'),
      
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
             downloadButton("downloadData", "Download")
      ),
      column(2),
      
      column(12,
             tableOutput(outputId = "data_table")
      )
    ),
    
    
  )





