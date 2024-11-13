#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Extraction et pré-formatage du classeur de relevé de 
#            compte Fortuneo
# 
# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤




extraction_Fortuneo <- function(dir){
  # ~~~~{    Importation    }~~~~
  
  releve <- read.csv2(dir, header=T, encoding = 'utf-8')
  
  
  # ~~~~{    modifications de base    }~~~~
  
  names(releve) <- str_replace_all(names(releve), 'é', 'e')
  
  releve$Date <- as.Date(releve$Date.operation, format = '%d/%m/%Y')
  releve <- select(releve, -Date.operation, -Date.valeur, -Credit)
  
  
  # releve$moyen <- str_extract(releve$libelle,'(^.+(?=\\d{2}/\\d{2}))|(^\\S+)')
  
  releve$Debit <- -releve$Debit
  releve <- filter(releve, !is.na(Debit))
  
}
# releve$libelle <- str_replace(releve$libelle, '(\\*)|(\\&)', '_')
# releve$lib <- str_remove(releve$libelle, '^.+\\d{2}/\\d{2}\\s')