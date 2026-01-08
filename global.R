#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Charger l'environnement de l'appli (librairies, variables, etc.)

# A.Jorant - Dec 2024

# R version 4.4.1

# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# ~~~~{    chargement des données source    }~~~~



# ~~~~{    chargement des packages    }~~~~
cat('\n>> GLOBAL > Installation des packages \n')

library(methods) # non chargé en execution ligne de commande avec RScript alors qu'il l'est pas défaut R. Potentiel résoltion d'un problème de chargement des packages

needed_packages <- c(#'Rtools', # utilisé par certains packages
  'dplyr', # manipulation de données / tableaux
  'stringr', # manipulation de chaines de caractères
  'pdftools', #lecture de pdf : relevés de compte banque postale
  'tidyr', # manipulation de données / tableaux
  'ggplot2', # graphiques
  'colorspace', #nuances de couleurs au sein des super classes
  'ggiraph', # interactivité des graphiques
  'shiny', # application
  'DT', # tableaux interactifs, modifiables
  'shinyWidgets',
  'shinyjs' # update date input avec possiblilité de laisser vide
  # 'shinyFiles' # chargement des fichiers non securisé mais laissant une trace de l'emplacement
)


install.packages(setdiff(needed_packages, rownames(installed.packages())))

cat('\n>> GLOBAL > chargement des packages \n')

for(pack_i in needed_packages)
  require(pack_i, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)


# ~~~~{    options des packages    }~~~~
cat('\n>> GLOBAL > options des packages \n')

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



# ~~~~{    directions    }~~~~
cat('\n>> GLOBAL > direction sauvegarde \n')

dir_sauvegarde <- NULL
if(file.exists('data/direction_sauvegarde.txt')) 
  dir_sauvegarde <- scan('data/direction_sauvegarde.txt', 
                         skip = 1, 
                         what = 'character', 
                         sep = '$')

df_classif <- read.csv2('data/classification_defaut.csv') %>%
  mutate(Date = as.Date(Date))

cat('\n>> GLOBAL > fin\n\n')



