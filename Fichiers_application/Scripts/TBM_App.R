#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Suivi des dépenses depuis relevés de comptes
#            Banque postale et Fortuneo

# A.Jorant - Nov 2024

# R version 4.4.1

# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# rm(list=ls()); gc() ; options(warn = 1)
# setwd('D:/apis_/Documents/R/Analyse des comptes bancaire TBM/Git Tutunes et Bonbon Miel/Fichiers_application/Scripts')

 
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#
#************************************************************************************#

#####                              INITIALLISATION                               #####

#************************************************************************************#
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''#

#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####                    Init. Dépendances                    #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

# ~~~~{    chargement des packages    }~~~~

needed_packages <- c(#'Rtools', # utilisé par certains packages
  'dplyr', # manipulation de données / tableaux
  'stringr', # manipulation de chaines de caractères
  'pdftools', #lecture de pdf : relevés de compte banque postale
  'tidyr', # manipulation de données / tableaux
  'ggplot2', # graphiques
  'colorspace', #nuances de couleurs au sein des super classes
  'ggiraph', # interactivité des graphiques
  'shiny', # application
  'DT' # tableaux interactifs, modifiables
  # 'shinyFiles' # chargement des fichiers non securisé mais laissant une trace de l'emplacement
)


install.packages(setdiff(needed_packages, rownames(installed.packages())))


for(pack_i in needed_packages)
  require(pack_i, character.only = TRUE)


# options dpyr
options(dplyr.summarise.inform = FALSE )
# options ggplot2
theme_set(theme_light()) ; options(ggplot2.continuous.colour="viridis", ggplot2.continuous.fill = "viridis")




# ~~~~{    chargement des fonctions    }~~~~
print(getwd())
# setwd('Scripts')
source('TBM_diff_extraction.R')
source('TBM_extraction_comptes_BP.R')
source('TBM_extraction_comptes_Fortuneo.R')
source('TBM_identification_libelle.R')
source('TBM_manipulation_tableaux.R')
source('TBM_graph.R')
source('TBM_util.R')
source('GOUZOU.R') # Gouzou_showoff()

# ~~~~{    chargement des elements de l'application    }~~~~
source('TBM_ui.R')
source('TBM_server.R')
# source('TBM_sources.R')

# setwd('../Source')

# ~~~~{    chargement de l'application    }~~~~
shinyApp(ui = ui, server = server)







