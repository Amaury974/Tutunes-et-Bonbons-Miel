#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Interface Utilisateur pour Tutune & Bonbon Miel

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤



ui <- fluidPage(
  style = "padding: 0px;", # no gap in navbar
  
  #--------------------------------------------------------------------#
  #####                       __ Sauvegarde                        #####
  #--------------------------------------------------------------------#
  actionButton(inputId = 'save',
               label = NULL,
               icon = icon('floppy-disk', class = 'fa-regular  fa-xl'),
               style = "position: absolute; top: 10px; right: 10px; z-index:10000;"),
  
  div(
    style = "position: absolute; top: 10px; right: 60px; z-index: 10000; ",
    textInput(inputId = 'dir_sauvegarde',
              label = NULL,
              value = dir_sauvegarde,
              # resize = 'horizontal',
              updateOn = 'blur',
              # style = "position: absolute; top: 10px; right: 50px; z-index:10000;"
    )
  ),
  
  # ~~~~{    image d'arrière plan    }~~~~
  
  tags$img(
    src = "logo_TBM_dessature.png",
    alt = 'logo TBM',
    style = 'position: fixed ; right: 10% ;top: 10% ;  z-index: -1',
    height = '90%'
  ),
  
  # ~~~~{    navbar page    }~~~~
  
  navbarPage(
    
    title ='Tutunes et Bonbons Miel',
    
    #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
    #####                   UI : Page 1 - Data IN                    #####
    #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
    
    
    tabPanel(
      value = 'data_selection',
      title = "Selection des données",
      
      # contenu rassemblé dans une colone centrale de 1100 px max.
      # Les marges absorbent le redimensionnement de la fenètre
      div(style = "max-width: 900px; margin: 0 auto;",
          
          p(HTML(
            "<p>Tutunes et Bonbons Miel est une application d'analyse de relevés de compte. Pour l'utiliser vous devez suivre les 3 étapes suivantes:</p>
        <ol>
        <li> Importation et extraction des relevés de compte. <i>(Banque Postale (.pdf ou .csv), Fortuneo ou Société Générale)</i></li>
        <li> Identification et classification des dépenses </li>
        <li> Visualisation des foyers de dépenses et de leur évolution dans le second onglet </li> 
        </ol>
        <p>Les mouvements bancaires sont rangés dans deux échelons de catégorisation:</p>
        <ol>
        <li> Les <i>Super Classe</i>, qui définissent des thèmes de dépenses: Déplacements, Alimentation, etc. </li>
        <li> Les <i>Classes</i>, qui viennent préciser la catégorie: voiture, vélo, transp.commun pour la Super Classe Déplacements par exemple.
        </ol>"
          )),
          
          
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
          
          
          #--------------------------------------------------------------------#
          #####                    __ Identification                       #####
          #--------------------------------------------------------------------#
          
          h2('Classification'),
          hr(),
          p(HTML(
            "<p> La classification permet de ranger chaque mouvement bancaire dans une catégorie de dépenses.
        Ces catégories sont constuées d'une <i>Super Classe</i> (premier élément) et d'une <i>Classe</i> (deuxième élément). ex:</p>
        <ul>
        <li>Deplac. velo</li>
        <li>Deplac. transp.commun</li>
        <li>Fixe loyer</li>
        </ul>
        La classification est basée sur l'identification d'une chaine de charactères (<i>marqueur</i>) dans les libellés des lignes de relevés de comptes.</p>
        
        <p>Une clef de classification est proposée par défaut. Vous devrez cependant ajouter des éléments corespondants à vos propres dépenses.<br>
        Pour ajouter un nouvel élément de classification, veuillez renseigner les éléments suivants :</p>
        
        <h4>Choix de la Classe</h4>
        <p>lorsque cela est possible, sélectionnez une Classe parmis celles déjà enregistrées.<br>
        Si vous souhaitez en ajouter une nouvelle, tapez son intitulé directement dans le menu déroulant <i>Sélection de la Classe</i>.<br>
        Veuillez toutefois à le renseigner comme suit :</p>
        
        <ul>
        <li>Le premier élément/mot, la <i>Super Classe</i>, sert de classe globale, ex: Déplacements, Alimentation, etc., 
        puis le second, la <i>Classe</i>, sert de classification précise. Pour permettre la dictinction de ces deux éléments, 
        il doit y avoir un seul et unique espace, entre la <i>Super Classe</i> et la <i>Classe</i></li>
        <li>Il ne doit pas y avoir d'accent </li>
        <li>Si vous souhaitez ne pas faire apparaitre un mouvement bancaire dans l'analyse, nommez la <i>Super Classe</i> dans laquel vous l'identifiez \"Transferts\".</li>
        </ul>
       

        <h4>Marqueur</h4> 
        <p>Le marqueur est la chaine de charactères qui permet d'identifier le ou les libellés que vous souhaitez classer.
        Ainsi, ce doit être une partie ou l'entièreté d'un libellé de ligne de compte.</p> 
        <p>Option supplémentaire:</p>
        <ul>
        <li>Si vous souhaitez indiquer qu'un marqueur est prioritaire par rapport aux autres, ajoutez 1/ avant le marqueur.<br>
        ex: 1/AUCHAN CAR pour classer le debit de carburant différemment du magasin AUCHAN</li>
        <li>Si vous  souhaitez amortir une dépense sur 10 ans, ajoutez AMORTI10/ avant le marqueur. AMORTI5/ pour 5 ans, etc. Son montant sera réparti par mois sur les x années suivantes.</li>
        </ul>
        
        <h4>Date (facultatif)</h4>
        <p>Enfin vous pouvez ajouter une date et ainsi spécifier une date pour laquelle le marqueur sera recherché.</p> <br>")),
          
          
          
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
          
      ),
      
          
          
          
          fluidRow(
            column(6,
                   # ~~~~{    nouvel element de classification    }~~~~
                   h3('Nouvel élément de classification'),
                   
                   fluidRow(
                     column(8,
                            fluidRow(
                              column(6,
                                     selectizeInput(
                                       inputId = 'select_Classe',
                                       label = 'Selection de la Classe',
                                       choices = 'aucune classe préchargée',
                                       options = list(placeholder = 'Veuillez choisir une Classe', 
                                                      onInitialize = I('function() { this.setValue(""); }'),
                                                      create = TRUE)
                                     )
                              ),
                              
                              column(6,
                                     textInput(
                                       inputId = 'nv_Marq',
                                       label = "Marqueur"
                                     )
                              ),
                            )
                     ),
                     column(4,
                            fluidRow(
                              column(8,
                                     suppressWarnings(dateInput(
                                       inputId = 'nv_Date',
                                       label = "(Date)",
                                       format = 'dd/mm/yyyy',
                                       language = 'fr',
                                       value = '',
                                       weekstart = 1
                                     ))
                              ),
                              
                              column(2,
                                     br(),
                                     actionButton(
                                       inputId = 'MaJ_Classe',
                                       label = '',
                                       icon = icon('arrows-rotate')
                                     ),
                              ),
                              
                              column(2,
                                     br(),
                                     actionButton(
                                       inputId = 'Plus_ligne',
                                       label = '',
                                       icon = icon('plus')
                                     ),
                              )
                              
                            )
                     )
                   ),
                   
                   
                   
                   # ~~~~{    tableau classification    }~~~~
                   h3("Clef de classification active"),
                   DT::DTOutput("tab_classif")
                   
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
          column(1),
          
          column(1,
                 
                 switchInput(
                   inputId = 'ShowTransferts',
                   label = 'Afficher les transferts',
                   value = FALSE,
                   onLabel = "OUI",
                   offLabel = "NON",
                   size = 'small'
                 )
          ),
          
          column(3,
                 sliderTextInput(inputId = "periode_subset",
                                 label = "Période visualisée",
                                 choices = letters, 
                                 selected = c('a','z'),
                                 force_edges = TRUE
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
          
          column(3,
                 selectInput(
                   inputId = 'typeGraph',
                   label = 'Type de graphique',
                   choices = list(#'résumé en un seul gros bonbon miel'='Gro_BonbonMiel',
                     'Vérification des données'='Verification_donnees',
                     'comparaison Fesse à Fesse'='Fesses',
                     'un bonbon miel par periode'='Ti_BonbonMiel',
                     'histogramme par periode'='histogramme_periode',
                     'histogramme comparaison par Classe'='histogramme_Classe',
                     'comparaison des dépenses et recettes'='histogramme_Fasse_a_Fasse',
                     'courbes empilés (in proogress)'='Courbe_empile_giraph')
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
      )
      
      
    )
  )
  
  
  
  
  