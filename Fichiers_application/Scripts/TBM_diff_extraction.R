#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Différenciation des types de relevés et appel 
#            aux fonctions d'extraction et de formatage

#     IN : all_dir
#     OUT: releve[c('Date', 'libelle', 'Debit', 'Compte')]

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤


f_diff_extraction <- function(dir, .dir_name=dir){
  
  # print(.dir_name)
  
  # ~~~~{    Banque Postale    }~~~~
  releve_Poste <- data.frame()
  for(i in which(str_detect(.dir_name, 'releve.+pdf$'))){
    print(.dir_name[i])
    releve_Poste <- bind_rows(releve_Poste, extraction_Poste(dir[i]))
  }
  
  
  # ~~~~{    Fortuneo    }~~~~
  releve_Fortuneo <- data.frame()
  # for(dir_i in filter(input_data, type == 'text/csv')$datapath){
  for(i in which(str_detect(.dir_name, 'HistoriqueOperations.+csv$'))){
    print(.dir_name[i])
    releve_Fortuneo <- bind_rows(releve_Fortuneo, extraction_Fortuneo(dir[i], paste('Fortuneo', str_extract(.dir_name[i], '\\d+'))))
  }
  
  # ~~~~{    Jointure    }~~~~
  releve <- bind_rows(releve_Poste, releve_Fortuneo)
  
  
}










