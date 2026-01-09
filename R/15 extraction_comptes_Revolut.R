#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Extraction et pré-formatage des extraction de 
#            compte Relvolut
# 
# 
#       IN : dir
#       OUT: releve[c('Date', 'libelle', 'Montant', 'Compte')]
# 
# A.Jorant - Janv 2026

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# dir="D:/apis_/Documents/R/Analyse des comptes bancaire TBM/Data/Relevés/HistoriqueOperations_018720443542_du_01_01_2023_au_31_12_2023.csv"
# dir="D:/apis_/Documents/R/Analyse des comptes bancaire TBM/Data/Relevés/HistoriqueOperations_018720443542_du_05_10_2023_au_05_11_2024.csv"

extraction_Revolut <- function(dir){
  
  # ~~~~{    Importation    }~~~~
  releve <- read.csv(dir, header=T,  fileEncoding = 'utf-8')
  
  # ~~~~{    standardisation    }~~~~
  releve$Date <- str_extract(releve$Date.de.début, '[^\\s]+') %>%
    as.Date()
  
  releve$Compte <- 'Compte Revolut inconnu'
  
  releve$libelle <- paste(releve$Type,'-', releve$Description)
  
  releve <- select(releve, libelle, Date, Montant, Compte)
  
  releve
}
# releve$libelle <- str_replace(releve$libelle, '(\\*)|(\\&)', '_')
# releve$lib <- str_remove(releve$libelle, '^.+\\d{2}/\\d{2}\\s')