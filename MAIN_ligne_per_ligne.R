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
dir_fig <- paste0(dir_projet, 'Figures')

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

source('TBM_diff_extraction.R')
source('TBM_extraction_comptes_BP_pdf.R')
source('TBM_extraction_comptes_BP.R')
source('TBM_extraction_comptes_SG.R')
source('TBM_extraction_comptes_Fortuneo.R')

source('TBM_identification_libelle.R')
source('TBM_manipulation_tableaux.R')
source('TBM_graph.R')
source('TBM_util.R')


#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####                    Init. Importation                    #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #


all_dir <- loca_dossier(dir_data,'\\d{7}')
releve <- f_diff_extraction(all_dir[28:40])
releve <- f_diff_extraction(dir = all_dir[1:5])
releve <- f_diff_extraction(all_dir[c(1:5,30:54)])

# releve2=releve
# releve=releve2

#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####            Init. Classification des dépenses            #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

setwd(dir_data)
df_classif <- read.csv2('Classification_dépenses.csv')%>%
  mutate(Date = as.Date(Date, format = '%d/%m/%Y'))



df_identifie <- fun_classif(releve, df_classif)


#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####              Init. Manipulation des tableaux            #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

echelle = 'Trimestre'
ShowTransferts = F


df_resume_periode <- f_resume(df_identifie, echelle)
list_col <- f_couleurs(df_resume_periode)



periode_subset <- Periode_defaut(df_resume_periode, df_identifie)

df_resume_periode <- filter(df_resume_periode,
                    periode >= periodifier_Court_to_Date(periode_subset[1], echelle = echelle),
                    periode <= periodifier_Court_to_Date(periode_subset[2], echelle = echelle))

if(!ShowTransferts) df_resume_periode <- filter(df_resume_periode, !Super_Classe %in% c('RTransferts', 'DTransferts'))

#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#
#************************************************************************************#

#####                                 GRAPHIQUES                                 #####

#************************************************************************************#
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''#
setwd(dir_fig)

Verification_donnees(df_resume_periode, list_col, df_identifie)

# Gro_BonbonMiel(df_resume_periode, list_col)

Ti_BonbonMiel(df_resume_periode, list_col)

Fesses(df_resume_periode, list_col)





