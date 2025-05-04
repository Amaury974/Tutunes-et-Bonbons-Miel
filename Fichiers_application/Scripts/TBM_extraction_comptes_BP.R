#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Extraction et pré-formatage des extraction de 
#            compte Banque Postale en csv
# 
#     IN : dir
#     OUT: releve[c('Date', 'libelle', 'Montant', 'Compte')]
#
# A.Jorant - Dec 2024
#
# R version 4.4.
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# dir=all_dir[2]

extraction_BP <- function(dir){
  
  # ~~~~{    Importation    }~~~~
  
  releve <- read.csv2(dir, header=T, encoding = 'latin1', skip = 6)
  
  
  # ~~~~{    modifications de base    }~~~~
  
  names(releve) <- str_replace_all(names(releve), 'é', 'e')
  
  releve$Date <- as.Date(releve$Date, format = '%d/%m/%Y')
  
  releve <- releve %>%
    mutate(Debit = -Montant.EUROS.) %>%
    filter(Debit > 0) %>%
    select(Date,
           libelle = Libelle,
           Debit)
  
  
  releve$Compte <- paste('LBP', read.csv2(dir, header=F, encoding = 'latin1')[1,2])
  
  releve
}












