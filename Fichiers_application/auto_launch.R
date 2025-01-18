#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : lance l'application

# A.Jorant - Nov 2024

# R version 4.4.1

# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤



# la direction du script est donnée en argument dans l'appel
wd <- commandArgs(trailingOnly = TRUE)
# print(wd)
wd <- paste(wd, collapse = ' ')
# ~~~~{    chargement des packages    }~~~~
cat('>> Chargement Packages \n\n')

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
  require(pack_i, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)

setwd(wd)

runApp('TBM_App.R', launch.browser = TRUE)









