#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Identification des libellées de dépenses issus de relevé de comptes et 
#            catégorisation

#     IN : releve[c('Date', 'libelle', 'Debit', 'Compte')]
#     OUT: df_identifie[c('Date', 'libelle', 'Debit', 'Compte', 'lib_id', 'lib_nice', 'classe', 'super_classe']

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤


#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#####                     Liste des dépenses                     #####
#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #


fun_classif <- function(releve, df_classif){
  
  
  # print(releve)
  
  df_classif <- rename(df_classif, Date_id = Date)
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                        Identification                      #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  
  
  releve$libelle <- str_replace(releve$libelle, '(\\*)|(\\&)', '_')
  
  # releve2=releve 
  
  # ~~~~{    identification generale    }~~~~
  df_id_general <- filter(df_classif, is.na(Date_id)) 
  # releve$lib_id <- str_extract(toupper(releve$libelle), paste0('(',df_id_general$lib_id,')', collapse = '|'))
  
  tab_id <- releve
  i=1
  
  for(lib_i in df_id_general$lib_id)
    tab_id[,lib_i] <- str_extract(toupper(releve$libelle), lib_i)
  
  
  # tab_id[i,] 
  
  # ~~~~{    identification des exceptions    }~~~~
  # on ne compare que les libbllés avec les bonnes dates
  
  df_id_exception <- filter(df_classif, !is.na(Date_id))
  
  i=356
  for(i in (0:nrow(df_id_exception))[-1]){
    tab_id[tab_id$Date == df_id_exception[i, 'Date_id'], df_id_exception[i, 'lib_id']] <- 
      str_extract(toupper(tab_id[tab_id$Date == df_id_exception[i, 'Date_id'], 'libelle']), df_id_exception[i,'lib_id'])
  }
  
  
  
  # ~~~~{    synthèse et verification des doublons    }~~~~
  double_identification <- ''
  # tab_id2 <- data.frame()
  # i=72
  for(i in 1:nrow(tab_id)){
    X <- tab_id[i, df_classif$lib_id]
    X2 <- df_classif[!is.na(X), ]
    
    X2 <- distinct(X2, super_classe, classe, Date_id, .keep_all = TRUE) %>%
      arrange(Date_id)
    releve[i,c('super_classe', 'classe', 'lib_id')] <- X2[1,-4]
    
    if(nrow(X2) >1) {
      
      ligne <- releve[i, c('Date', 'libelle', 'Debit', 'Compte')]
      ligne$Date <- as.character(ligne$Date)
      ligne$Debit <- paste0(ligne$Debit, '€')
      avertissement_i <- paste('\n', i, str_flatten_comma(ligne, na.rm = TRUE), '\n   ==> identifié comme :', paste(lapply(as.data.frame(t(X2)), str_flatten_comma, na.rm = TRUE), collapse = '\n                         ')) 
      
      double_identification <- paste(double_identification, avertissement_i)
    }
    
    
  }
  
  if(double_identification != '') double_identification <- paste0('/!\\ Risque de mauvaise identification', double_identification)
  double_identification <<- double_identification
  
  # cat(double_identification)
  
  
  # # ~~~~{    TEST des non identifiés    }~~~~
  # 
  # filter(releve,is.na(lib_id), !is.na(Debit)) %>%
  #   arrange(Debit) %>%
  #   select(Date, libelle, Debit)
  
  
  
  # filter(releve, str_detect(toupper(libelle), 'VILLE DE LYON' ))
  
  
  # ~~~~{    Jointure des classes de dépense    }~~~~
  
  # df_identifie <- releve %>%
  #   left_join(df_classif, by = join_by(lib_id))
  
  releve
  # # ~~~~{    Ménage    }~~~~
  # rm(list = c('releve', 'releve_Poste', 'releve_Fortuneo', 'list_classes', 'df_classif'))
  
}