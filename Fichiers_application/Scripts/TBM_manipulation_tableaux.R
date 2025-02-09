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
  
  
  # # ~~~~{    Ménage    }~~~~
  # rm(list = c('ordre_resume_sup', 'resume_periode'))
  
}


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




#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#####                           Période                          #####
#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #


Periode_defaut <- function(df_resume_periode, df_identifie){
  
echelle <- quelle_periode(label = df_resume_periode[1,"Label_periode"])


df_verif <- df_identifie %>%
  # select(Date, Compte) %>%
  arrange(Date) %>%
  mutate(periode = periodifier(Date, echelle, 'Court'),
         periode = factor(periode, unique(periode)),
         Mois = format(Date, '%m')) %>%
  group_by(periode, Compte) %>%
  summarize(N_ligne = length(Date),
            Date_min = min(Date),
            Date_max = max(Date),
            nbr_mois = length(unique(Mois))) %>%
  ungroup() %>%
  # mutate(couleur_periode = nbr_mois == max(nbr_mois),
  #        nbr_mois = if_else(couleur_periode, as.character(nbr_mois), str_c('bold(underline(',nbr_mois,'))'))) %>%
  group_by(Compte) %>%
  mutate(couleur_lignes = abs(N_ligne - mean(N_ligne))/mean(N_ligne),
         couleur_lignes = case_when(couleur_lignes > 0.6 ~ 'FALSE', couleur_lignes > 0.3 ~ 'suspect', .default = 'TRUE')) %>%
  
  mutate(# nombre de mois
    couleur_duree = nbr_mois == max(nbr_mois)) %>% # si une periode a moins de mois, elle est problablement tronquée 
    # nbr_mois = if_else(couleur_duree, as.character(nbr_mois), str_c('bold(underline(',nbr_mois,'))'))) %>%
  ungroup() %>%
  mutate( # date du debut
    couleur_deb = Date_min - de_periodifier(periodifier( Date_min, echelle,'Date' ), echelle)$deb  ,
    # couleur_deb = case_when(couleur_deb > 10 ~ 'FALSE', couleur_deb > 5 ~ 'suspect', .default = 'TRUE'),
    # label_deb = str_c('phantom(',nbr_mois, '~mois~du)~',format_plotmath(Date_min)),
    
    # date de fin
    couleur_fin = de_periodifier(periodifier( Date_max, echelle,'Date' ), echelle)$fin - Date_max)
    # couleur_fin = case_when(couleur_fin > 10 ~ 'FALSE', couleur_fin > 5 ~ 'suspect', .default = 'TRUE'),
    # label_fin = str_c('phantom(', nbr_mois, '~mois~du~',format_plotmath(Date_min),'~au)~', format_plotmath(Date_max)),
    
    # label_periode =  str_c(nbr_mois, 'mois~du', format_plotmath(Date_min), 'au', format_plotmath(Date_max), sep = '~'))



Periode_out <- df_verif %>%
  group_by(periode) %>%
  summarise(ok_deb_fin = !any(couleur_deb > 10 | couleur_fin > 10)) %>%
  filter(ok_deb_fin) %>%
  pull(periode)

if(length(Periode_out) == 0) Periode_out <- unique(df_verif$periode)


as.character(c(first(Periode_out), last(Periode_out)))

}







