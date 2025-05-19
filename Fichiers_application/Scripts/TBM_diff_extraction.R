#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Différenciation des types de relevés et appel 
#            aux fonctions d'extraction et de formatage

#     IN : all_dir
#     OUT: releve[c('Date', 'libelle', 'Montant', 'Compte')]

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# dir=all_dir[51:55]
f_diff_extraction <- function(dir, .dir_name=dir){
  
  .dir_name <- str_c('/', .dir_name)
  # print(.dir_name)
  
  # ~~~~{    releve PDF Banque Postale    }~~~~
  print('PDF_BP')
  # i=5
  releve_Poste_PDF <- data.frame()
  for(i in which(str_detect(.dir_name, 'releve.+pdf$'))){
    print(.dir_name[i])
    releve_Poste_PDF <- bind_rows(releve_Poste_PDF, extraction_BP_pdf(dir[i]))
  }
  
  
  # ~~~~{    Banque postale csv    }~~~~
  print('BP')
  # ex : '0456399A0381730071860072.csv'
  releve_Poste <- data.frame()
  # for(dir_i in filter(input_data, type == 'text/csv')$datapath){
  for(i in which(str_detect(.dir_name, '/\\d{7}\\w\\d{16}.csv$'))){
    print(.dir_name[i])
    releve_Poste <- bind_rows(releve_Poste, extraction_BP(dir[i]))
  }
  
  # ~~~~{    Societe Generale    }~~~~
  print('SG')
  # ex : '00056002117.csv'
  releve_SG <- data.frame()
  # for(dir_i in filter(input_data, type == 'text/csv')$datapath){
  for(i in which(str_detect(.dir_name, '/\\d{11}.csv$'))){
    print(.dir_name[i])
    releve_SG <- bind_rows(releve_SG, extraction_SG(dir[i]))
  }
  
  # ~~~~{    Fortuneo    }~~~~
  print('Fortuneo')
  releve_Fortuneo <- data.frame()
  # for(dir_i in filter(input_data, type == 'text/csv')$datapath){
  for(i in which(str_detect(.dir_name, 'HistoriqueOperations.+csv$'))){
    print(.dir_name[i])
    releve_Fortuneo <- bind_rows(releve_Fortuneo, extraction_Fortuneo(dir = dir[i], .force_compte = paste('Fortuneo', str_extract(.dir_name[i], '\\d+'))))
  }
  
  # ~~~~{    Jointure    }~~~~
  releve <- bind_rows(releve_Poste_PDF, releve_Poste, releve_SG, releve_Fortuneo)
  
  
  
  # ~~~~{    Retrait des Doublons    }~~~~
  # compliqué parce qu'on limite les risques de supprimer deux dépense identique le même jour qui ne serraient pas des doublons
  N1 <- nrow(releve)
  
  releve <- releve %>%
    group_by(Compte, Date) %>%
    summarize(Depense_unique = length(unique(str_c(libelle, Montant))),
              Depense_tt = length(libelle)) %>%
    filter(Depense_tt == 2*Depense_unique) %>%
    mutate(Jour_Doublon = TRUE) %>%
    select(-c(Depense_unique, Depense_tt)) %>%
    ungroup() %>%
    right_join(releve, by = join_by(Compte, Date)) %>%
    group_by(Compte, Date, libelle, Montant) %>%
    mutate(Ligne_Doublon = cumsum(!is.na(Montant))) %>% # cumsum de n'importe quoi
    filter(Ligne_Doublon == 1 | is.na(Jour_Doublon)) %>%
    select(-c(Jour_Doublon, Ligne_Doublon)) %>%
    ungroup()
  
  N_Doublons <- N1-nrow(releve)
  
  if(N_Doublons) print(paste('doublons retirés :', N_Doublons))
  
  
  releve
}










