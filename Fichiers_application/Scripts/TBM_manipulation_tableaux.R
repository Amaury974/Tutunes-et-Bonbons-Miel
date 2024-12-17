#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Formatage des données bancaires identifié

#      IN : df_identifie[c('Date', 'libelle', 'Debit', 'Compte', 'Marqueur', 'Classe', 'Super_Classe']
#      OUT: df_resume_periode[c('Super_Classe','Classe', 'periode', 'Debit', 'Label_Trimestre')]
#           list_col list(Classe=hex_color)

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤



f_resume <- function(df_identifie, echelle = 'Semestre'){
  
  # print('str(df_identifie)') 
  # print(str(df_identifie))
  # 
  # print('summary(df_identifie)')
  # print(summary(df_identifie))
  
  
  
  
  # ~~~~{    Summarize    }~~~~
  
  # print(df_identifie)
  df_identifie2 <- filter(df_identifie, (Super_Classe != '' | is.na(Super_Classe))) %>%
    mutate(Classe = str_c(Super_Classe, '_', Classe))
  
  resume_periode <- df_identifie2 %>%
    mutate(periode = periodifier(Date, echelle, 'Date')) %>%
    group_by(Super_Classe, Classe, periode) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    ungroup()
  
  # ~~~~{    Toutes les Classes représentées tous les periodes    }~~~~
  resume_periode <- expand.grid(Classe = unique(resume_periode$Classe),
                                periode = unique(resume_periode$periode)) %>%
    mutate(Super_Classe = str_extract(Classe, '^[^_]+'),
           Debit = 0) %>%
    bind_rows(resume_periode) %>%
    arrange(desc(Debit)) %>%
    distinct(Classe, periode, .keep_all = TRUE)
  
  
  
  
  # ~~~~{    Mise en forme noms periodes    }~~~~
  
  # '2023-11-15' => 'Oct. Nov. Dec. 2023 \n 7500 €'
  
  resume_periode <- resume_periode %>%
    group_by(periode) %>%
    summarize(Total = round(sum(Debit))) %>%
    arrange(periode) %>%
    mutate(Label_periode = paste(periodifier(periode, echelle, 'Long'), '\n', Total, '€'),
           Label_periode = factor(Label_periode, unique(Label_periode))) %>%
    select(-Total) %>%
    right_join(resume_periode, by = 'periode')
  
  
  
  # # ~~~~{    Périodes incomplètes ?    }~~~~
  # Test_periode <- data.frame(periode = unique(resume_periode$periode)) # date centrale de toutes les périodes représentées dans le résumé
  # 
  # Test_periode[,c('deb', 'fin')] <- de_periodifier(Test_periode$periode, echelle) # dates du début et de la fin de chaque période
  # 
  # Test_periode$deb <- periodifier(Test_periode$deb, 'Mois', 'Date') # date du centre du mois de début puis de la fin de chaque période
  # Test_periode$fin <- periodifier(Test_periode$fin, 'Mois', 'Date')
  # 
  # 
  # tous_les_mois <- unique(periodifier(unique(df_identifie$Date), 'Mois', 'Date')) # date du centre de chaque mois représentés dans les données
  # 
  # Test_periode$deb_ok <- Test_periode$deb %in% tous_les_mois
  # Test_periode$fin_ok <- Test_periode$fin %in% tous_les_mois
    
    
  
  
  
  
  
  summary(resume_periode)
  
  
  
  # ~~~~{    Classement des dépenses par Super_Classe (primaire)    }~~~~
  ordre_resume_sup <-resume_periode %>%
    group_by(Super_Classe) %>%
    summarize(ordre_sup = -sum(Debit, na.rm = TRUE)/sd(Debit/mean(Debit))) # les dépenses très variables sont pénalisées
  
  # ~~~~{    Classement des dépenses par Classe (secondaire)    }~~~~
  
  df_resume_periode <-
    resume_periode %>%
    group_by(Super_Classe, Classe) %>%
    summarize(ordre_inf = -sum(Debit, na.rm = TRUE)/sd(Debit/mean(Debit))) %>%
    ungroup() %>%
    left_join(ordre_resume_sup, by = join_by(Super_Classe)) %>%
    left_join(resume_periode, by = join_by(Super_Classe, Classe)) %>%  # + Debit, periodes
    arrange(ordre_sup, ordre_inf) %>%
    mutate(Super_Classe = factor(Super_Classe, unique(Super_Classe)),
           Classe = str_extract(Classe,'[^_]+$'), # on retire la super Classe de la Classe
           Classe = factor(Classe, unique(Classe))) %>%
    select(Super_Classe, Classe, periode, Debit, Label_periode)
  
  
  # ~~~~{    Périodes incomplètes ?    }~~~~
  
  
  
  # # ~~~~{    Ménage    }~~~~
  # rm(list = c('ordre_resume_sup', 'resume_periode'))
  
}


#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#####                   Synthèse par Trimestres                  #####
#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #

# f_resume_trimestre <- function(df_identifie){
#   
#   # print('str(df_identifie)') 
#   # print(str(df_identifie))
#   # 
#   # print('summary(df_identifie)')
#   # print(summary(df_identifie))
#   
#   
#   
#   
#   # ~~~~{    Summarize    }~~~~
#   
#   # print(df_identifie)
#   df_identifie2 <- filter(df_identifie, (Super_Classe != '' | is.na(Super_Classe))) %>%
#     mutate(Classe = str_c(Super_Classe, '_', Classe))
#   
#   resume_trim <- df_identifie2 %>%
#     mutate(trimestre = ceiling(as.numeric(format(Date, '%m'))/3),
#            trimestre = as.Date(paste(format(Date, '%Y'),3*trimestre-1, '15', sep='-'))) %>%
#     group_by(Super_Classe, Classe, trimestre) %>%
#     summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
#     ungroup()
#   
#   # ~~~~{    Toutes les Classes représentées tous les trimestres    }~~~~
#   resume_trim <- expand.grid(Classe = unique(resume_trim$Classe),
#                              trimestre = unique(resume_trim$trimestre)) %>%
#     mutate(Super_Classe = str_extract(Classe, '^[^_]+'),
#            Debit = 0) %>%
#     bind_rows(resume_trim) %>%
#     arrange(desc(Debit)) %>%
#     distinct(Classe, trimestre, .keep_all = TRUE)
#   
#   
#   
#   
#   # ~~~~{    Mise en forme noms trimestres    }~~~~
#   
#   # '2023-11-15' => 'Oct. Nov. Dec. 2023 \n 7500 €'
#   
#   resume_trim <- resume_trim %>%
#     group_by(trimestre) %>%
#     summarize(Total = round(sum(Debit))) %>%
#     arrange(trimestre) %>%
#     mutate(Label_Trimestre = paste(format(trimestre -30, '%b'), format(trimestre , '%b'), format(trimestre +30, '%b'), format(trimestre, '%Y'), '\n', Total, '€'),
#            Label_Trimestre = factor(Label_Trimestre, unique(Label_Trimestre))) %>%
#     select(-Total) %>%
#     right_join(resume_trim, by = 'trimestre')
#   
#   
#   
#   
#   # ~~~~{    Classement des dépenses par Super_Classe (primaire)    }~~~~
#   ordre_resume_sup <-resume_trim %>%
#     group_by(Super_Classe) %>%
#     summarize(ordre_sup = -sum(Debit, na.rm = TRUE)/sd(Debit/mean(Debit))) # les dépenses très variables sont pénalisées
#   
#   # ~~~~{    Classement des dépenses par Classe (secondaire)    }~~~~
#   
#   df_resume_trimestre <-
#     resume_trim %>%
#     group_by(Super_Classe, Classe) %>%
#     summarize(ordre_inf = -sum(Debit, na.rm = TRUE)/sd(Debit/mean(Debit))) %>%
#     ungroup() %>%
#     left_join(ordre_resume_sup, by = join_by(Super_Classe)) %>%
#     left_join(resume_trim, by = join_by(Super_Classe, Classe)) %>%  # + Debit, trimestres
#     arrange(ordre_sup, ordre_inf) %>%
#     mutate(Super_Classe = factor(Super_Classe, unique(Super_Classe)),
#            Classe = str_extract(Classe,'[^_]+$'), # on retire la super Classe de la Classe
#            Classe = factor(Classe, unique(Classe))) %>%
#     select(Super_Classe, Classe, trimestre, Debit, Label_Trimestre)
#   
#   # # ~~~~{    Ménage    }~~~~
#   # rm(list = c('ordre_resume_sup', 'resume_trim'))
#   
# }


#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#####                          Couleurs                          #####
#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #


f_couleurs <- function(df_resume_periode){
  # ~~~~{    Assignation de couleurs aux super-Classes    }~~~~
  df_sup_col <- 
    data.frame(Super_Classe = unique(df_resume_periode$Super_Classe),
               Super_col = qualitative_hcl(length(unique(df_resume_periode$Super_Classe)), 
                                           palette = "Dark 3"))
  
  df_couleur <- df_resume_periode %>%
    distinct(Super_Classe, Classe) %>%
    # distinct() %>%
    left_join(df_sup_col, by = join_by(Super_Classe)) 
  
  
  # ~~~~{    Variations de la couleur par "sous" Classe    }~~~~
  
  col_dev <- function(col, N){
    palette <- c()
    for(i in 0.5 *(0:(N-1))/N)
      palette <- c(palette, lighten(col, i))
    
    palette
  }
  
  df_couleur$col <- NA
  # i=unique(ordre_resume_col$Super_col)[1]
  for(i in unique(df_couleur$Super_col))
    df_couleur[df_couleur$Super_col == i, 'col'] <- col_dev(i, nrow(df_couleur[df_couleur$Super_col == i,])) 
  
  df_couleur <- as.data.frame(select(df_couleur, Classe, col))
  
  
  # ~~~~{    Liste des couleurs pour utilisation dans scale_._manual()    }~~~~
  
  list_col <- list()
  i=1
  for(i in 1:nrow(df_sup_col))
    list_col[[as.character(df_sup_col[i,'Super_Classe'])]] <- darken(df_sup_col[i,'Super_col'])
  
  for(i in 1:nrow(df_couleur))
    list_col[[as.character(df_couleur[i,'Classe'])]] <- df_couleur[i,'col']
  
  
  
  # # ~~~~{    test    }~~~~
  # df_resume_periode %>%
  #   distinct(Super_Classe, Classe) %>%
  #   ggplot() +
  #   geom_bar(aes(x=Classe, fill = Classe), y=1) +
  #   geom_bar(aes(x=Classe, fill = Super_Classe, y=0.3),stat = 'identity', color = 'black')+
  #   scale_fill_manual(values = list_col) +
  #   guides(fill='none') +
  #   theme(axis.text.x = element_text(angle = (330), hjust=0))
  
  # # ~~~~{    Ménage    }~~~~
  # rm(list = c('df_couleur', 'col_dev', 'df_sup_col'))
  
  list_col
  
}

















