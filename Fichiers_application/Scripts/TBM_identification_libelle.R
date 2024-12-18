#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Identification des libellées de dépenses issus de relevé de comptes et 
#            catégorisation

#     IN : releve[c('Date', 'libelle', 'Debit', 'Compte')]
#     OUT: df_identifie[c('Date', 'libelle', 'Debit', 'Compte', 'Marqueur', 'Classe', 'Super_Classe')]

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

# df_classif2=df_classif
# df_classif=df_classif2

fun_classif <- function(releve, df_classif){
  
  
  # print(releve)
  
  df_classif2 <- rename(df_classif, Date_id = Date) %>% # permet de joindre sans tenir compte de la date
    mutate(priorite = str_extract(Marqueur, '^\\d(?=/)'), # extrait l'indicateur de priorite au debut du marqueur ex: 1//AUCHAN CAR -> 1
           Marqueur = str_extract(Marqueur, '[^//]+$')) %>% # retire les indicateurs de priorité ex : 1//AUCHAN CAR -> AUCHAN CAR
    arrange(priorite) %>%
    select(-priorite)
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                        Identification                      #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  df_identifie <- releve
  
  
  df_identifie$libelle <- str_replace(df_identifie$libelle, '(\\*)|(\\&)', '_')
  
  
  # ~~~~{    identification generale    }~~~~
  df_id_general <- filter(df_classif2, is.na(Date_id)) # les filtres globaux (sans date)
  
  c_Marqueur <- paste0('(', paste(df_id_general$Marqueur, collapse = ')|('),')') # Marqueurs concaténés
  df_identifie$Marqueur <- str_extract(toupper(df_identifie$libelle), c_Marqueur) # extraction du premier (/!\) Marqueur
  
  
  # ~~~~{    identification des exceptions    }~~~~
  # on ne compare que les libellés avec les bonnes dates
  
  df_id_exception <- filter(df_classif2, !is.na(Date_id))
  i=2
  for(i in (0:nrow(df_id_exception))[-1]){
    bonne_date <- df_identifie$Date == df_id_exception[i, 'Date_id'] & !is.na(df_identifie$Date)
    bon_marqueur <- str_detect(toupper(df_identifie$libelle), df_id_exception[i,'Marqueur'])
    df_identifie[bonne_date & bon_marqueur, 'Marqueur'] <- paste(df_id_exception[i,'Marqueur'], df_id_exception[i, 'Date_id'])
  }
  
  df_classif2 <- df_classif2 %>%
    mutate(Marqueur = case_match(Date_id, NA ~ Marqueur, .default = paste(Marqueur, Date_id)))
  
  # summary(as.factor(df_classif2$Marqueur))
  # ~~~~{    verification des doublons    }~~~~
  # à faire
  
  # ~~~~{    synthèse    }~~~~
  
  
  df_identifie <- df_identifie %>%
    left_join(df_classif2, by = join_by(Marqueur)) %>%
    select('Date', 'libelle', 'Debit', 'Compte', 'Marqueur', 'Classe', 'Super_Classe')
  
  df_identifie
}































