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



# options dpyr
options(dplyr.summarise.inform = FALSE )
# options ggplot2
theme_set(theme_light())
# theme_update(
#   panel.background = element_rect(fill='transparent'), #transparent panel bg
#   plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
#   # panel.grid.major = element_blank(), #remove major gridlines
#   # panel.grid.minor = element_blank(), #remove minor gridlines
#   legend.background = element_rect(fill='transparent'), #transparent legend bg
#   legend.box.background = element_rect(fill='transparent') #transparent legend panel
# )
options(ggplot2.continuous.colour="viridis", ggplot2.continuous.fill = "viridis")




# ~~~~{    chargement des fonctions    }~~~~
cat('>> Chargement Fonctions \n\n')

# print(getwd())
setwd('Scripts')
# print(getwd())

source('TBM_diff_extraction.R')
source('TBM_extraction_comptes_BP_pdf.R')
source('TBM_extraction_comptes_BP.R')
source('TBM_extraction_comptes_SG.R')
source('TBM_extraction_comptes_Fortuneo.R')

source('TBM_identification_libelle.R')
source('TBM_manipulation_tableaux.R')
source('TBM_graph.R')
source('TBM_util.R')
# source('GOUZOU.R') # Gouzou_showoff()

# ~~~~{    chargement des elements de l'application    }~~~~
cat('>> Chargement Application \n\n')

source('TBM_sources.R')
source('TBM_ui.R')
source('TBM_server.R')

setwd('..')
# print(getwd())
# setwd('../Source')

# ~~~~{    chargement de l'application    }~~~~
shinyApp(ui = ui, server = server)







