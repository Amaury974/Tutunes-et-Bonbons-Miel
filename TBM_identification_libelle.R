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

# ~~~~{    Classes de dépense par defaut    }~~~~
list_classes <- list(
  alim_general = c('E.LECLERC', 'elsa boulanger', 'intermarche', 'aldi', 'auchan', 'carrefour', 'RUN Market', 'SPAR', 'Monop', 'asia store', "l'alizee", 'Larbre a pains', 
                   'la case a pains', 'Vival', 'Casino', 'SAS SEMOI', 'Discash', 'SARL Miamax', 'Provanille', 'Distr Venissieux', 'Armanni Nadege', #SAS SEMOI = Picard    Discash = LeaderPrice   SARL Miamax = un autre supermarche à BoubonLancy
                   'Aroma-Zone'), # pas alim mais tous les cosmétiques sont là dedans de toute façon
  alim_bio = c('Biocoop', 'epi centre', 'bioloco', 'alimentation bio', 'naturalia', 'la ptite distrib', 'bio'),
  
  matos_cadeaux = c('il etait une foi', 'Zettle__Cacao', 'Cobalt', 'la poste', 'Cadeau', 'PEPINIERE GROM', 'EXEMPLAIRE'),
  matos_equip. = c('decathlon', 'au vieux campeur', 'amazon', 'bleu cerise', 'Marebam Primat', 'SAS Autrement'), #Marebam Primat = Decathlon re     SAS Autrement = Libraire
  matos_vetements = c('Frip attitude', 'HetM043', 'HAV SAINT ANDRE', 'www.loom.fr'), # HetM043 = H&M   HAV= hall aux vetements
  
  lieu_amenag. = c('muji','bricorama', 'Ravate', 'Conforama', 'IKEA', 'Fermes et jardin', 'FERMES_JARDINS', 'LBD', 'Banian Tissus', 'La Droguerie', 'S Center', 'Leroy Mer', 'Au monde des ani', 'Matin ste clotilde'), #LBD = First deco   Matin = bazar
  lieu_deplac. = c('SNCF', 'transdev', 'tropic auto', 'ADA locations', 'keolis', 'CYCLEXPERTSLYON', 'Bike24', 'Sodiparc', 'RATP', 'inter location', 'VITO', 'EURL DJM SERVICE', 'OLA ENERGY', 'Avion', 'Station ZAC', 'cool location'),
  lieu_demenag. = c('dossier 244049', 'VIR INST jorant backenstrass out', 'ecocarton', 'ville de lyon', 'Sixt'),
  
  
  loisir_activite = c('camping',  'airbnb', 'kelonia', 'Cinepalme', 'Labyrinte Champ', 'Cite du volcan', 'Maison Folio', 'domaine du cafe', 'Billetterie web', 'Plein Champ', 'SUMUP _PAYET'), #Billeterie web = HOSHI ??    Plein Champ = Comoedia
  loisir_resto = c('igloo', 'sapna', 'le caudan', 'SO THAI', 'ROYAL FOOD TRADI', 'MJC MONPLAISIR', 'FEUILLETTE', 'Paul', 'zoi kitchen', 'House pizza', 
                   'La Cardinale', 'Snack', 'LyBeyrouth', 'le cafe potager', 'restaurant','la cascade grourm', 'le petit creux', 'Mattsam Restaura', 
                   'carre jardin', 'Mafate C', 'Papichulo', 'The Merveilles', 'Chez Louna', 'Terre Adelice', 'cochi   co', 'villa Marthe', 'La Cascade gourm', 'Silver one',
                   'Zettle__Recup et', 'SumUp  _DELIS A', 'A Bis', 'Jojo', 'Galeria Restaura', '513102 ID TRATTO', 'o divin plaisir', 'Kiwa', 'Kotozafy Randria',
                   'Queen Fr', "Tart'in", 'le relais Kreol', 'Bacchus', 'Le vieux press'),
  
  fixes_Loyer = c('Loyer',  'Regie Targe'),
  fixes_abonn. = c('EDF', 'prlv', 'COTISATION TRIMESTRIELLE'),
  
  retraits_cheques = c('CHQ' ), #retraits d'especes et cheques
  retraits_especes = c('RET DAB', 'Retrait'),
  
  autre_divers = c('Ciudin', 'essomo laundry', 'Pcie', 'Phie', 'caution', 'LAURA CAPONY', 'TIMBRE FISCAL') # Pcie = Pharmacie
)

# ~~~~{    Agrégation des classes    }~~~~

df_identification <- data.frame()
for(i in names(list_classes)){
  vec_i <- list_classes[[i]]
  df_identification <- 
    data.frame(super_classe = as.factor(str_extract(i, '^[^_]+')),
               classe = as.factor(i),
               lib_id = toupper(vec_i)
               # lib_nice = vec_i,
               
               ) %>%
    bind_rows(df_identification)
}

# df_identification <- df_identification[nrow(df_identification):1,]











# list_classes


f_identification <- function(releve, df_identification){
  
  
  # print(releve)
  
  
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                        Identification                      #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  
  
  releve$libelle <- str_replace(releve$libelle, '(\\*)|(\\&)', '_')
  
  
  # ~~~~{    identification    }~~~~
  
  releve$lib_id <- str_extract(toupper(releve$libelle), paste0('(',df_identification$lib_id,')', collapse = '|'))
  
  
  # ~~~~{    ajout manuel de dépenses mals identifiées    }~~~~
  
  # montant identiques
  releve[is.na(releve$lib_id) & releve$moyen == 'VIR' & releve$Debit %in% c(644.83), 'lib_id'] <- 'LOYER'
  
  # virement internes
  releve <- filter(releve, !str_detect(libelle, 'VIR INST Miaou|VIREMENT PERMANENT POUR BACKENSTRASS & JORANT|JORANT')) %>%
    filter(!(libelle == 'VIR Virement avec Fortuneo Compt' & Debit %in% c(300)))
  
  # frais remboursé
  releve <- filter(releve, !(lib_id == 'SIXT' & Date %in% as.Date(c('2024-04-18', '2024-04-24'))))
  
  # virement interne pour achat de billets avec carte Claire
  releve[(releve$libelle == 'VIR Virement avec Fortuneo Compt' & releve$Debit ==2000), c('lib_id')] <- 'AVION'
  
  
  
  # ~~~~{    TEST des non identifiés    }~~~~
  
  # filter(releve,is.na(lib_id), !is.na(Debit)) %>%
  #   arrange(Debit) %>%
  #   select(Date, libelle, Debit)
  
  
  
  # filter(releve, str_detect(toupper(libelle), 'VILLE DE LYON' ))
  
  
  # ~~~~{    Jointure des classes de dépense    }~~~~
  
  df_identifie <- releve %>%
    left_join(df_identification, by = join_by(lib_id))
  
  
  # # ~~~~{    Ménage    }~~~~
  # rm(list = c('releve', 'releve_Poste', 'releve_Fortuneo', 'list_classes', 'df_identification'))
  
}