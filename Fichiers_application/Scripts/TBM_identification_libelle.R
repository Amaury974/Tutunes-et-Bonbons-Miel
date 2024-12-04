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

df_classif <- read.csv2('../Source/classification_defaut.csv')

# # ~~~~{    Classes de dépense par defaut    }~~~~
# list_classes <- list(
#   alim_general = c('E.LECLERC', 'elsa boulanger', 'intermarche', 'aldi', 'auchan', 'carrefour', 'RUN Market', 'SPAR', 'Monop', 'asia store', "l'alizee", 'Larbre a pains', 
#                    'la case a pains', 'Vival', 'Casino', 'SAS SEMOI', 'Discash', 'SARL Miamax', 'Provanille', 'Distr Venissieux', 'Armanni Nadege', #SAS SEMOI = Picard    Discash = LeaderPrice   SARL Miamax = un autre supermarche à BoubonLancy
#                    'Aroma-Zone'), # pas alim mais tous les cosmétiques sont là dedans de toute façon
#   alim_bio = c('Biocoop', 'epi centre', 'bioloco', 'alimentation bio', 'naturalia', 'la ptite distrib', 'bio'),
#   
#   matos_cadeaux = c('il etait une foi', 'Zettle__Cacao', 'Cobalt', 'la poste', 'Cadeau', 'PEPINIERE GROM', 'EXEMPLAIRE'),
#   matos_equip. = c('decathlon', 'au vieux campeur', 'amazon', 'bleu cerise', 'Marebam Primat', 'SAS Autrement'), #Marebam Primat = Decathlon re     SAS Autrement = Libraire
#   matos_vetements = c('Frip attitude', 'HetM043', 'HAV SAINT ANDRE', 'www.loom.fr'), # HetM043 = H&M   HAV= hall aux vetements
#   
#   lieu_amenag. = c('muji','bricorama', 'Ravate', 'Conforama', 'IKEA', 'Fermes et jardin', 'FERMES_JARDINS', 'LBD', 'Banian Tissus', 'La Droguerie', 'S Center', 'Leroy Mer', 'Au monde des ani', 'Matin ste clotilde'), #LBD = First deco   Matin = bazar
#   lieu_deplac. = c('SNCF', 'transdev', 'tropic auto', 'ADA locations', 'keolis', 'CYCLEXPERTSLYON', 'Bike24', 'Sodiparc', 'RATP', 'inter location', 'VITO', 'EURL DJM SERVICE', 'OLA ENERGY', 'Avion', 'Station ZAC', 'cool location'),
#   lieu_demenag. = c('dossier 244049', 'VIR INST jorant backenstrass out', 'ecocarton', 'ville de lyon', 'Sixt'),
#   
#   
#   loisir_activite = c('camping',  'airbnb', 'kelonia', 'Cinepalme', 'Labyrinte Champ', 'Cite du volcan', 'Maison Folio', 'domaine du cafe', 'Billetterie web', 'Plein Champ', 'SUMUP _PAYET'), #Billeterie web = HOSHI ??    Plein Champ = Comoedia
#   loisir_resto = c('igloo', 'sapna', 'le caudan', 'SO THAI', 'ROYAL FOOD TRADI', 'MJC MONPLAISIR', 'FEUILLETTE', 'Paul', 'zoi kitchen', 'House pizza', 
#                    'La Cardinale', 'Snack', 'LyBeyrouth', 'le cafe potager', 'restaurant','la cascade grourm', 'le petit creux', 'Mattsam Restaura', 
#                    'carre jardin', 'Mafate C', 'Papichulo', 'The Merveilles', 'Chez Louna', 'Terre Adelice', 'cochi   co', 'villa Marthe', 'La Cascade gourm', 'Silver one',
#                    'Zettle__Recup et', 'SumUp  _DELIS A', 'A Bis', 'Jojo', 'Galeria Restaura', '513102 ID TRATTO', 'o divin plaisir', 'Kiwa', 'Kotozafy Randria',
#                    'Queen Fr', "Tart'in", 'le relais Kreol', 'Bacchus', 'Le vieux press'),
#   
#   fixes_Loyer = c('Loyer',  'Regie Targe'),
#   fixes_abonn. = c('EDF', 'prlv', 'COTISATION TRIMESTRIELLE'),
#   
#   retraits_cheques = c('CHQ' ), #retraits d'especes et cheques
#   retraits_especes = c('RET DAB', 'Retrait'),
#   
#   autre_divers = c('Ciudin', 'essomo laundry', 'Pcie', 'Phie', 'caution', 'LAURA CAPONY', 'TIMBRE FISCAL') # Pcie = Pharmacie
# )
# 
# # ~~~~{    Agrégation des classes    }~~~~
# 
# df_classif <- data.frame()
# for(i in names(list_classes)){
#   vec_i <- list_classes[[i]]
#   df_classif <- 
#     data.frame(super_classe = str_extract(i, '^[^_]+'),
#                classe = i,
#                lib_id = toupper(vec_i),
#                Date = as.Date(NA)
#                # lib_nice = vec_i,
#                
#   #   )
#   # data.frame(super_classe = as.factor(str_extract(i, '^[^_]+')),
#   #                classe = as.factor(i),
#   #                lib_id = toupper(vec_i),
#   #                Date = as.Date(NA)
#   #                # lib_nice = vec_i,
#                  
#     ) %>%
#     bind_rows(df_classif)
# }
# 
# # df_classif <- df_classif[nrow(df_classif):1,]
# 
# 
# 
# # ~~~~{    ajout manuel de dépenses mals identifiées    }~~~~
# # 
# # # montant identiques
# # releve[is.na(releve$lib_id) & releve$moyen == 'VIR' & releve$Debit %in% c(644.83), 'lib_id'] <- 'LOYER'
# # 
# # # virement internes
# # releve <- filter(releve, !str_detect(libelle, 'VIR INST Miaou|VIREMENT PERMANENT POUR BACKENSTRASS & JORANT|JORANT')) %>%
# #   filter(!(libelle == 'VIR Virement avec Fortuneo Compt' & Debit %in% c(300)))
# # 
# # # frais remboursé
# # releve <- filter(releve, !(lib_id == 'SIXT' & Date %in% as.Date(c('2024-04-18', '2024-04-24'))))
# # 
# # # virement interne pour achat de billets avec carte Claire
# # releve[(releve$libelle == 'VIR Virement avec Fortuneo Compt' & releve$Debit ==2000), c('lib_id')] <- 'AVION'
# # 
# 
# df_classif <- bind_rows(df_classif,
#                                data.frame(super_classe = 'lieu',
#                                           classe = 'lieu_deplac.',
#                                           lib_id = toupper('VIR Virement avec Fortuneo Compt'),
#                                           Date = as.Date('2024-07-20')))
# 
# 
# df_classif$classe <- str_extract(df_classif$classe,'[^_]+$')
# df_classif$classe <- str_to_lower(df_classif$classe)
# df_classif$super_classe <- str_to_title(df_classif$super_classe)
# write.csv2(df_classif, 'classification_default.csv')



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