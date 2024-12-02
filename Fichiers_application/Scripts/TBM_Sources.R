#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Charger les emplacement de fichier utilisés précedement

# A.Jorant - Dec 2024

# R version 4.4.1

# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# ~~~~{    chargement des données source    }~~~~
cat('>> SOURCE _ 1\n')

setwd('../Source')

# ~~~~{    defaut    }~~~~
df_localisation <- data.frame(fichier = c('Non_def', 'identification'),
                              emplacement = c('Non_def', 'classification_defaut.csv'))

if('localisation.csv' %in% list.files()) df_localisation <- read.csv2('../Source/localisation.csv')

print(df_localisation)


df_identification <- read.csv2(df_localisation[2, 'emplacement']) %>%
  mutate(Date = as.Date(Date))



if(df_localisation[1,'fichier'] == 'releves') 
  df_identifie <- f_diff_extraction(df_localisation[1,'emmplacement']) %>%
  f_identification(df_identification)

if(df_localisation[1,'fichier'] == 'identifie') 
  df_identifie <- read.csv2(df_localisation[1,'emmplacement'])


# les autres fichiers sont chargés dans Server


cat('       _ fin\n')
