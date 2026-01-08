#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Identification des libellées de dépenses issus de relevé de comptes et 
#            catégorisation

#     IN : releve[c('Date', 'libelle', 'Montant', 'Compte')]
#     OUT: df_identifie[c('Date', 'libelle', 'Montant', 'Compte', 'Marqueur', 'Classe', 'Super_Classe')]

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# df_classif2=df_classif
# df_classif=df_classif2



# plus_ligne <- function(df_identifie, Nv_ligne){
#   filter(df_identifie, is.na(Classe)) %>%
#     select(Compte, Date, libelle, Montant) %>%
#   fun_classif(Nv_ligne)
# }


fun_classif <- function(releve, df_classif){
  
  
  # print(releve)
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                   Reconstruction du releve                 #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  # on peut fournir le df_identifié en tant que releve mais il faut recalculer les amortis
  if('Classe' %in% names(releve)){
    # print('re-identification')
    releve <- releve %>%
      filter(!str_detect(libelle, '^Amortissement')) %>%
      mutate(Montant = if_else(str_detect(libelle, 'amortis en'),
                               as.numeric(str_extract(libelle, '.+(?=...amortis en)')),
                               Montant),
             libelle = str_remove(libelle, '^.+mois\\s\\-\\s')) %>%
      select(Date, libelle, Montant, Compte)
    
  }
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                     Priorité et amorti                     #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  # print(25)
  
  df_classif2 <- rename(df_classif, Date_id = Date) %>% # permet de joindre sans tenir compte de la date
    mutate(regle = str_extract(Marqueur, '^[^//]*\\d(?=/)'), # extrait l'indicateur de priorite au debut du marqueur ex: 1//AUCHAN CAR -> 1
           priorite = str_extract(regle, '^\\d'),
           amorti_mois = as.numeric2(str_extract(regle, '(?<=AMORTI)\\d+'))*12,
           Marqueur = str_extract(Marqueur, '[^//]+$')) %>% # retire les indicateurs de priorité ex : 1//AUCHAN CAR -> AUCHAN CAR
    arrange(priorite) %>%
    select(-c(regle, priorite))
  
  
  
  # filter(df_classif2, !is.na(regle))
  
  # print(39)
  
  # df_classif2 <- rename(df_classif, Date_id = Date) %>% # permet de joindre sans tenir compte de la date
  #   mutate(priorite = str_extract(Marqueur, '^\\d(?=/)'), # extrait l'indicateur de priorite au debut du marqueur ex: 1//AUCHAN CAR -> 1
  #          Marqueur = str_extract(Marqueur, '[^//]+$')) %>% # retire les indicateurs de priorité ex : 1//AUCHAN CAR -> AUCHAN CAR
  #   arrange(priorite) %>%
  #   select(-priorite)
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                        Identification                      #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  df_identifie <- releve
  
  
  df_identifie$libelle <- str_replace(df_identifie$libelle, '(\\*)|(\\&)', '_')
  
  # print(56)
  # ~~~~{    identification generale    }~~~~
  df_id_general <- filter(df_classif2, is.na(Date_id)) # les filtres globaux (sans date)
  
  c_Marqueur <- paste0('(', paste(df_id_general$Marqueur, collapse = ')|('),')') # Marqueurs concaténés
  
  # print(62)
  
  df_identifie$Marqueur <- str_extract(toupper(df_identifie$libelle), c_Marqueur) # extraction du premier (/!\) Marqueur
  
  # print(66)
  # ~~~~{    identification des exceptions    }~~~~
  # on ne compare que les libellés avec les bonnes dates
  
  df_id_exception <- filter(df_classif2, !is.na(Date_id))
  i=2
  for(i in (0:nrow(df_id_exception))[-1]){
    bonne_date <- df_identifie$Date == df_id_exception[i, 'Date_id'] & !is.na(df_identifie$Date)
    bon_marqueur <- str_detect(toupper(df_identifie$libelle), df_id_exception[i,'Marqueur'])
    df_identifie[bonne_date & bon_marqueur, 'Marqueur'] <- paste(df_id_exception[i,'Marqueur'], df_id_exception[i, 'Date_id'])
  }
  
  # print(77)
  
  df_classif2 <- df_classif2 %>%
    mutate(Marqueur = case_match(Date_id, NA ~ Marqueur, .default = paste(Marqueur, Date_id)))
  
  # summary(as.factor(df_classif2$Marqueur))
  # ~~~~{    verification des doublons    }~~~~
  # à faire
  
  # ~~~~{    jointure    }~~~~
  
  
  df_identifie2 <- df_identifie %>%
    left_join(df_classif2, by = join_by(Marqueur)) 
  
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                           Amortis                          #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  df_a_amortir <- filter(df_identifie2, !is.na(amorti_mois)) %>%
    mutate(Montant_original = Montant,
           Montant = Montant / amorti_mois) %>%
    as.data.frame()
  
  
  df_amorti <- df_a_amortir %>%
    mutate(Montant = 0,
           libelle = paste(Montant_original, '€ amortis en', amorti_mois, 'mois -', libelle))
  
  # print(df_a_amortir)
  # print(105)
  # i=1
  for(i in (0:nrow(df_a_amortir))[-1]){
    # j=1
    for(j in 1:df_a_amortir[i, "amorti_mois"]-1){
      df_amorti_i <- df_a_amortir[i,]
      
      
      df_amorti_i$libelle <- paste('Amortissement de',df_amorti_i$Montant_original, '€ le', format(df_amorti_i$Date, '%d/%m/%Y'), 'mois', j, '/', df_a_amortir[i, "amorti_mois"], '-', 
                                   df_amorti_i$libelle )
      
      M <- as.numeric(format(df_amorti_i$Date, '%m')) + j
      Y <- as.numeric(format(df_amorti_i$Date, '%Y')) + floor((M-1)/12)
      M <- 1+(M-1) %% 12
      
      df_amorti_i$Date <- as.Date(str_c(Y, M, '01', sep = '-')) # amorti le 1er du mois, c'est plus sûr.
      # print(df_amorti_i)
      df_amorti <- bind_rows(df_amorti, df_amorti_i)
    }
  }
  
  df_identifie2 <- filter(df_identifie2, is.na(amorti_mois)) %>%
    bind_rows(df_amorti)
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                       Auto Transferts                      #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  Auto_Transf <- filter(df_identifie2, is.na(Classe), Montant >= 100) %>%
    select(Date, Montant_plus = Montant) %>%
    left_join(df_identifie2, by = join_by(Date)) %>%
    filter(Montant == -Montant_plus) %>%
    select(Date, Montant) %>%
    mutate(auto_transfert = TRUE)
  
  Auto_Transf <- Auto_Transf %>%
    mutate(Montant = - Montant) %>%
    bind_rows(Auto_Transf)
    
  df_identifie2 <- df_identifie2 %>%
    left_join(Auto_Transf, by = join_by(Date, Montant)) %>%
    mutate(Super_Classe = if_else(!is.na(auto_transfert), 'Transferts', Super_Classe),
           Classe = if_else(!is.na(auto_transfert), 'auto', Classe))
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                             NA                             #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  df_identifie2[df_identifie2$Montant > 0 & is.na(df_identifie2$Classe), 'Super_Classe'] <- 'NA'
  df_identifie2[df_identifie2$Montant > 0 & is.na(df_identifie2$Classe), 'Classe'] <- 'non attribue'
  df_identifie2[df_identifie2$Montant < 0 & is.na(df_identifie2$Classe), 'Super_Classe'] <- 'NA'
  df_identifie2[df_identifie2$Montant < 0 & is.na(df_identifie2$Classe),  'Classe'] <- 'non attribue'
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                            sens                            #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  df_identifie2$Super_Classe <- with(df_identifie2, str_c(ifelse(Montant>0, 'R', 'D'), Super_Classe))
  
  # print(124)
  df_identifie2 %>%
    mutate(Classe = str_c(Super_Classe, Classe, sep = ' : ')) %>%   ### ICI <---
    select('Date', 'libelle', 'Montant', 'Compte', 'Marqueur', 'Classe', 'Super_Classe')
  
}





f_classif <- function(releve, df_classif, Nv_ligne, type_Maj_Classe){
  
  if(type_Maj_Classe == 'MaJ_Classe'){
    df_identifie <- fun_classif(releve, df_classif)
  }
  
  if(type_Maj_Classe == 'Plus_ligne'){
    # df_identifie <- filter(releve, is.na(Classe)) %>%
    df_identifie <- filter(releve, Super_Classe %in% c('RNA', 'DNA')) %>%
      fun_classif(Nv_ligne) %>%
      bind_rows(filter(releve, !Super_Classe %in% c('RNA', 'DNA')))
  }
  
  df_identifie
  
}




























