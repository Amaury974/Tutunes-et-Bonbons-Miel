#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Formatage des données bancaires identifié

#      IN : df_identifie[c('Date', 'libelle', 'Montant', 'Compte', 'Marqueur', 'Classe', 'Super_Classe']
#      OUT: df_resume_periode[c('Super_Classe','Classe', 'periode', 'Montant', 'Label_periode')]
#           list_col list(Classe=hex_color)

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤



f_resume <- function(df_identifie, echelle = 'Semestre'){
  
  # ~~~~{    Summarize    }~~~~
  
  # print(df_identifie)
  # les transferts d'argent sont ignoré pour l'instant
  df_identifie2 <- filter(df_identifie, (Super_Classe != 'Transferts' | is.na(Super_Classe)))
  # %>%
  #   mutate(Classe = str_c(Super_Classe, '_', Classe))
  
  resume_periode <- df_identifie2 %>%
    mutate(periode = periodifier(Date, echelle, 'Date')) %>%
    group_by(Super_Classe, Classe, periode) %>%
    summarise(Montant = sum(Montant, na.rm = TRUE)) %>%
    ungroup()
  
  # ~~~~{    Toutes les Classes représentées tous les periodes    }~~~~
  resume_periode <- expand.grid(Classe = unique(resume_periode$Classe),
                                periode = unique(resume_periode$periode)) %>%
    mutate(Super_Classe = str_extract(Classe, '^[^_]+'),
           Montant = 0) %>%
    bind_rows(resume_periode) %>%
    arrange(desc(abs(Montant))) %>%
    distinct(Classe, periode, .keep_all = TRUE) %>%
    mutate(Direction = if_else(Montant > 0, 'Credit', 'Debit'))
  
  
  
  
  # ~~~~{    Mise en forme noms periodes    }~~~~
  
  # '2023-11-15' => 'Oct. Nov. Dec. 2023 \n 7500 €'
  
  resume_periode <- resume_periode %>%
    group_by(periode, Direction) %>%
    summarize(Total = round(sum(Montant))) %>%
    arrange(periode) %>%
    mutate(Label_periode = paste(periodifier(periode, echelle, 'Long'), '\n', Total, '€'),
           Label_periode = factor(Label_periode, unique(Label_periode))) %>%
    select(-Total) %>%
    right_join(resume_periode, by = c('periode', 'Direction'))
  
  
  # ~~~~{    Classement des dépenses par Super_Classe (primaire)    }~~~~
  ordre_resume_sup <- resume_periode %>%
    group_by(Super_Classe) %>%
    summarize(ordre_sup = -sum(abs(Montant), na.rm = TRUE)/sd(Montant/mean(Montant))) # les dépenses très variables sont pénalisées
  
  # ~~~~{    Classement des dépenses par Classe (secondaire)    }~~~~
  
  df_resume_periode <-
    resume_periode %>%
    group_by(Super_Classe, Classe) %>%
    summarize(ordre_inf = -sum(abs(Montant), na.rm = TRUE)/sd(Montant/mean(Montant))) %>%
    ungroup() %>%
    left_join(ordre_resume_sup, by = join_by(Super_Classe)) %>%
    left_join(resume_periode, by = join_by(Super_Classe, Classe)) %>%  # + Montant, periodes
    arrange(ordre_sup, ordre_inf) %>% #distinct(Super_Classe, Classe, ordre_inf, ordre_sup)
    mutate(Super_Classe = factor(Super_Classe, unique(Super_Classe)),
           # Classe = str_extract(Classe,'[^_]+$'), # on retire la super Classe de la Classe
           Classe = factor(Classe, unique(Classe))) %>%
    select(Super_Classe, Classe, periode, Montant, Label_periode)

  # # ~~~~{    Ménage    }~~~~
  # rm(list = c('ordre_resume_sup', 'resume_periode'))
  
}




#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#####                          Couleurs                          #####
#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
f_couleurs <- function(df_resume_periode){
  list_col1 <- 
    filter(df_resume_periode, Montant <0)%>%
    f_couleurs_part(Palette='Dark2')

    list_col2 <- 
    filter(df_resume_periode, Montant >0) %>%
    f_couleurs_part(Palette='Set1')
  
  c(list_col1, list_col2)
}

# RColorBrewer::brewer.pal()

# df_resume_periode<-filter(df_resume_periode, Montant <0)

f_couleurs_part <- function(df_resume_periode, Palette){
  # ~~~~{    Assignation de couleurs aux super-Classes    }~~~~
  df_sup_col <- data.frame(Super_Classe = unique(df_resume_periode$Super_Classe))
  
  df_sup_col$Super_col <- RColorBrewer::brewer.pal(max(3,nrow(df_sup_col)), Palette)[1:nrow(df_sup_col)] # minimal value for n is 3, returning requested palette with 3 different levels
                 # qualitative_hcl(length(unique(df_resume_periode$Super_Classe)), 
                 #                           palette = Palette))

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
    # list_col[[paste(df_couleur[i,'Super_Classe'], df_couleur[i,'Classe'])]] <- df_couleur[i,'col']
  
  # unique(df_couleur$Classe)
  
  # ~~~~{    test    }~~~~
  df_resume_periode %>%
    distinct(Super_Classe, Classe) %>%
    ggplot() +
    geom_bar(aes(x=Classe, fill = Classe), y=1) +
    geom_bar(aes(x=Classe, fill = Super_Classe, y=0.3),stat = 'identity', color = 'black')+
    scale_fill_manual(values = list_col) +
    guides(fill='none') +
    theme(axis.text.x = element_text(angle = (330), hjust=0))
  
  # # ~~~~{    Ménage    }~~~~
  # rm(list = c('df_couleur', 'col_dev', 'df_sup_col'))
  
  list_col
  
}
# for(i in 1:16){
#   pal = palette.pals()[i]
#   print(pal)
# (data.frame(A=1:6, B=LETTERS[1:6])%>%
# ggplot(aes(x=A, color= B))+
#   geom_point(y=1,size = 5)+
#   scale_color_manual(values = palette(pal))+
#     labs(title=pal)) %>%
#     
#     print()
# }

#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#####                           Période                          #####
#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #


Periode_defaut <- function(df_resume_periode, df_identifie){
  
  echelle <- quelle_periode(label = df_resume_periode[1,"Label_periode"])
  
  # écart entre la première et la dernière transaction de la période et l'étendue théorique de cette période
  df_verif <- df_identifie %>%
    arrange(Date) %>%
    mutate(periode = periodifier(Date, echelle, 'Court'),
           periode = factor(periode, unique(periode))) %>%
    group_by(periode, Compte) %>%
    summarize(Date_min = min(Date),
              Date_max = max(Date)) %>%
    mutate( # date du debut
      ecart_deb = Date_min - de_periodifier(periodifier( Date_min, echelle,'Date' ), echelle)$deb  ,
      ecart_fin = de_periodifier(periodifier( Date_max, echelle,'Date' ), echelle)$fin - Date_max)
  
  # les périodes pour lesquels l'écart ne dépasse pas 10 jours, donc pas plus de 10 jours sans transactions en bord de période.
  # pour tous les comptes présents.
  Periode_out <- df_verif %>%
    group_by(periode, Compte) %>%
    summarise(ok_deb_fin = !any(ecart_deb > 10 | ecart_fin > 10)) %>%
    group_by(periode) %>%
    summarize(nbr_comptes_ok = sum(as.numeric(ok_deb_fin))) %>%
    filter(nbr_comptes_ok == max(nbr_comptes_ok)) %>%
    pull(periode)
  
  # si aucune période n'est jugée bonne, on garde tout
  if(length(Periode_out) == 0) Periode_out <- unique(df_verif$periode)
  
  # unquement la première et ladernière période présente.
  # /!\ pas de détection prévue si il y a un troue dans les données
  as.character(c(first(Periode_out), last(Periode_out)))
  
}







