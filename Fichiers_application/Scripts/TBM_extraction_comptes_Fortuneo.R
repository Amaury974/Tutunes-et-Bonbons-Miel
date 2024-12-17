#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Extraction et pré-formatage du classeur de relevé de 
#            compte Fortuneo
# 
# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# dir="D:/apis_/Documents/R/Analyse des comptes bancaire TBM/Data/Relevés/HistoriqueOperations_018720443542_du_01_01_2023_au_31_12_2023.csv"
# dir="D:/apis_/Documents/R/Analyse des comptes bancaire TBM/Data/Relevés/HistoriqueOperations_018720443542_du_05_10_2023_au_05_11_2024.csv"

extraction_Fortuneo <- function(dir, .force_compte = NULL){
  # ~~~~{    Importation    }~~~~
  
  releve <- read.csv2(dir, header=T, encoding = 'utf-8')
  
  
  # ~~~~{    modifications de base    }~~~~
  
  names(releve) <- str_replace_all(names(releve), 'é', 'e')
  
  releve$Date <- as.Date(releve$Date.operation, format = '%d/%m/%Y')
  releve <- select(releve, -Date.operation, -Date.valeur, -Credit)
  
  
  # releve$moyen <- str_extract(releve$libelle,'(^.+(?=\\d{2}/\\d{2}))|(^\\S+)')
  
  releve$Debit <- -releve$Debit
  releve <- filter(releve, !is.na(Debit))
  
  releve$Compte <- if(!is.null(.force_compte)) .force_compte else paste('Fortuneo', str_extract(dir, '\\d+'))
  
  releve
}
# releve$libelle <- str_replace(releve$libelle, '(\\*)|(\\&)', '_')
# releve$lib <- str_remove(releve$libelle, '^.+\\d{2}/\\d{2}\\s')