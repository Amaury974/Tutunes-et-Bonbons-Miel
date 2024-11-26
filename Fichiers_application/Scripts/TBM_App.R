#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Suivi des dépenses depuis relevés de comptes
#            Banque postale et Fortuneo

# A.Jorant - Nov 2024

# R version 4.4.1

# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# rm(list=ls()); gc() ; options(warn = 1)
# setwd('D:/apis_/Documents/R/Analyse des comptes bancaire TBM/Git Tutunes et Bonbon Miel/Fichiers_application')


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
)



install.packages(setdiff(needed_packages, rownames(installed.packages())))


for(pack_i in needed_packages)
  require(pack_i, character.only = TRUE)


# options dpyr
options(dplyr.summarise.inform = FALSE )
# options ggplot2
theme_set(theme_light()) ; options(ggplot2.continuous.colour="viridis", ggplot2.continuous.fill = "viridis")




# ~~~~{    chargement des fonctions    }~~~~
# print(getwd())
source('Scripts/TBM_extraction_comptes_BP.R')
source('Scripts/TBM_extraction_comptes_Fortuneo.R')
source('Scripts/TBM_identification_libelle.R')
source('Scripts/TBM_manipulation_tableaux.R')
source('Scripts/TBM_graph.R')
source('Scripts/TBM_util.R')
source('Scripts/GOUZOU.R')

# Gouzou_showoff()


source('Scripts/TBM_ui.R')
source('Scripts/TBM_server.R')



# fichiers_dispo <- list.files('Source')


# df_identifie
# df_resume_trimestre
# list_col
# if('dernier_ouvert.RData' %in% fichiers_dispo) load('source/dernier_ouvert.RData') else load('source/data_vide.RData')



# df_identification <- read.csv2('Source/classification_defaut.csv')


shinyApp(ui = ui, server = server)







