#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Extraction et pré-formatage des extraction de 
#            compte Societe Generale
# 
# A.Jorant - Dec 2024

# R version 4.4.
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# dir=all_dir[1]

extraction_SG <- function(dir, .force_compte = NULL){
  
  # ~~~~{    Importation    }~~~~
  
  releve <- read.csv2(dir, header=T, encoding = 'latin1', skip = 1)
  
  
  # ~~~~{    modifications de base    }~~~~
  
  names(releve) <- str_replace_all(names(releve), 'é', 'e')
  
  releve$Date <- as.Date(releve$Date.de.l.operation, format = '%d/%m/%Y')
  
  releve <- releve %>%
    mutate(Debit = -Montant.de.l.operation) %>%
    filter(Debit > 0) %>%
    select(Date,
           libelle = Detail.de.l.ecriture,
           Debit)
    
  releve$Compte <- read.csv2(dir, header=F, encoding = 'latin1')[1,1] %>% str_extract('\\d+')
  releve$Compte <- paste('SG', releve$Compte)
  
  releve
}












