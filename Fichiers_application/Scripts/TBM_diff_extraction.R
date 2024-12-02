#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Différenciation des types de relevés et appel 
#            aux fonctions d'extraction et de formatage

#     IN : all_dir
#     OUT: releve[c('Date', 'libelle', 'Debit', 'Compte')]

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤



f_diff_extraction <- function(all_dir){
  
  # ~~~~{    Banque Postale    }~~~~
  releve_Poste <- data.frame()
  for(dir_i in all_dir[str_detect(all_dir, '/releve.+pdf$')]){
    releve_Poste <- bind_rows(releve_Poste, extraction_Poste(dir_i))
    print(dir_i)
    # releve_Poste$Compte <- 'BP_Amaury'
  }
  
  
  # ~~~~{    Fortuneo    }~~~~
  releve_Fortuneo <- data.frame()
  # for(dir_i in filter(input_data, type == 'text/csv')$datapath){
  for(dir_i in  all_dir[str_detect(all_dir, '/HistoriqueOperations.+csv$')]){
    releve_Fortuneo <- bind_rows(releve_Fortuneo, extraction_Fortuneo(dir_i))
    print(dir_i)
  }
  
  # ~~~~{    Jointure    }~~~~
  releve <- bind_rows(releve_Poste, releve_Fortuneo)
  
  
}










