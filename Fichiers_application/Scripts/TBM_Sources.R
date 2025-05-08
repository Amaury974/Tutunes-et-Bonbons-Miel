#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Charger les emplacement de fichier utilisés précedement

# A.Jorant - Dec 2024

# R version 4.4.1

# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# ~~~~{    chargement des données source    }~~~~
cat('>> SOURCE _ 1\n')

dir_sauvegarde <- scan('../Source/direction_sauvegarde.txt', skip = 1, what = 'character', sep = '$')

df_classif <- read.csv2('../Source/classification_defaut.csv') %>%
  mutate(Date = as.Date(Date))

cat('       _ fin\n')



