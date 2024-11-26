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
dir_projet <- 'D:/apis_/Documents/R/Analyse des comptes bancaire TBM/'
dir_scripts <- paste0(dir_projet, 'Git Tutunes et Bonbon Miel/Fichiers_application/Scripts')
dir_data <- paste0(dir_projet, 'Data')

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
  'ggiraph' # interactivité des graphiques
)



install.packages(setdiff(needed_packages, rownames(installed.packages())))  


for(pack_i in needed_packages)
  require(pack_i, character.only = TRUE)
 
# options dpyr
options(dplyr.summarise.inform = FALSE )
# options ggplot2
theme_set(theme_light()) ; options(ggplot2.continuous.colour="viridis", ggplot2.continuous.fill = "viridis")



# ~~~~{    chargement des fonctions    }~~~~
setwd(dir_scripts)
source('TBM_extraction_comptes_BP.R')
source('TBM_extraction_comptes_Fortuneo.R')
source('TBM_identification_libelle.R')
source('TBM_manipulation_tableaux.R')
source('TBM_graph.R')
source('TBM_util.R')


#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####                    Init. Importation                    #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

# ~~~~{    Banque Postale    }~~~~

dir_Poste <- loca_dossier('.', 'releve_CCP2006014Y038')



# dir = 'releve_CCP2006014Y038_20240112.pdf'

releve_Poste <- data.frame()
for(dir in dir_Poste)
  releve_Poste <- bind_rows(releve_Poste, extraction_Poste(dir))

releve_Poste$Compte <- 'BP_Amaury'


# ~~~~{    Fortuneo    }~~~~

dir_Fortuneo <- loca_dossier('.', 'HistoriqueOperations_018720443542')

releve_Fortuneo <- data.frame()
for(dir in dir_Fortuneo)
  releve_Fortuneo <- bind_rows(releve_Fortuneo, extraction_Fortuneo(dir))

releve_Fortuneo$Compte <- 'Fortuneo_commun'


# ~~~~{    Jointure    }~~~~

releve <- bind_rows(releve_Poste, releve_Fortuneo)


#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####            Init. Classification des dépenses            #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

setwd(dir_data)
df_identification <- read.csv2('Classification_dépenses.csv')

df_identifie <- f_identification(releve, df_identification)


setwd(dir_data)
write.csv2('Releve_de_comptes_categorises.csv')
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####              Init. Manipulation des tableaux            #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

setwd(dir_data)
df_identifie <- read.csv2('Releve_de_comptes_categorises.csv') %>%
  mutate(Date = as.Date(Date))

df_resume_trimestre <- f_resume_trimestre(df_identifie)
list_col <- f_couleurs(df_resume_trimestre)


#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#
#************************************************************************************#

#####                                 GRAPHIQUES                                 #####

#************************************************************************************#
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''#

#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####                   Graph . Bonbons Miel                  #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

BonbonMiel_unique_giraph(df_resume_trimestre, list_col)

BonbonMiel_trimestriel_giraph(df_resume_trimestre, list_col)

#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####            Graph . evolution des dépense                #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

Courbe_empile_giraph(df_resume_trimestre, list_col)



