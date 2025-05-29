#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Graphiques Camemberts et courbes empilés

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####             Graph . Vérification des données            #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #



Verification_donnees <- function(df_resume_periode, list_col, df_identifie){
  
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
      couleur_duree = nbr_mois == max(nbr_mois), # si une periode a moins de mois, elle est problablement tronquée 
      nbr_mois = if_else(couleur_duree, as.character(nbr_mois), str_c('bold(underline(',nbr_mois,'))'))) %>%
    ungroup() %>%
    mutate( # date du debut
      couleur_deb = Date_min - de_periodifier(periodifier( Date_min, echelle,'Date' ), echelle)$deb  ,
      couleur_deb = case_when(couleur_deb > 10 ~ 'FALSE', couleur_deb > 5 ~ 'suspect', .default = 'TRUE'),
      label_deb = str_c('phantom(',nbr_mois, '~mois~du)~',format_plotmath(Date_min)),
      
      # date de fin
      couleur_fin = de_periodifier(periodifier( Date_max, echelle,'Date' ), echelle)$fin - Date_max,
      couleur_fin = case_when(couleur_fin > 10 ~ 'FALSE', couleur_fin > 5 ~ 'suspect', .default = 'TRUE'),
      label_fin = str_c('phantom(', nbr_mois, '~mois~du~',format_plotmath(Date_min),'~au)~', format_plotmath(Date_max)),
      
      label_periode =  str_c(nbr_mois, 'mois~du', format_plotmath(Date_min), 'au', format_plotmath(Date_max), sep = '~'))
  
  
  # couleur_debu=with(df_verif2, Date_min - de_periodifier(periodifier( Date_min, echelle,'Date' ), echelle)$deb)
  # case_when(couleur_debu > 10 ~ 'FALSE', couleur_debu > 5 ~ 'suspect', .default = 'TRUE')
  # 
  # 
  # de_periodifier(periodifier(df_verif$Date_min, echelle,'Date'))
  
  
  
  # estetique
  df_verif <- df_verif %>%
    mutate(an = str_extract(periode, '.{4}'))
  
  # colone période
  df_verif$base_Y <- 1:nrow(df_verif)
  
  df_verif <- df_verif %>%
    group_by(periode) %>%
    mutate(Y_periode = mean(base_Y))
  
  
  
  # cadre sélection
  selectionnees <- data.frame(periode = periodifier(range(df_resume_periode$periode), echelle, format = 'Court')) %>%
    left_join(select(df_verif, periode, base_Y), by = join_by(periode)) %>%
    pull(base_Y) %>%
    range()
  
  
  
  graph_verif <-
    ggplot(df_verif) +
    geom_rect(aes(ymin = base_Y -0.5, ymax = base_Y +0.5, fill = periode), xmin = 0.9, xmax = 10, alpha = 0.25) +
    geom_text(aes(label = periode,  y = Y_periode), x = 1, hjust = 0) + #color = an,
    geom_text(aes(label = Compte,  y = base_Y), x = 2, hjust = 0) + #color = Compte,
    
    # label periode
    geom_text(aes(label =label_periode, y = base_Y), x = 6, hjust = 0, parse = TRUE) +
    # nbr mois
    geom_text(aes(label =nbr_mois, color = couleur_duree, y = base_Y), x = 6, hjust = 0, parse = TRUE) +
    # date du debut
    geom_text(aes(label = label_deb, color = couleur_deb, y = base_Y), x = 6, hjust = 0, parse = TRUE) +
    geom_text(aes(label = label_fin, color = couleur_fin, y = base_Y), x = 6, hjust = 0, parse = TRUE) +
    
    # nombre de ligne dans les relevés
    geom_text(aes(label = paste(N_ligne, 'lignes'), y = base_Y, color = couleur_lignes), x = 9, hjust = 0) +
    
    # cadre autour de la période sélectionnée
    annotate(geom = 'rect', 
             ymin = selectionnees[1]-0.5, ymax = selectionnees[2]+0.5,
             xmin = 0.9, xmax = 10, 
             alpha = 0, color = 'grey50', linewidth = 1)+
    
    scale_color_manual(values = c('TRUE' = 'black', 'FALSE' = 'red', 'suspect' = 'gold2')) +
    guides(color = 'none', fill = 'none') +
    theme_void() +
    xlim(c(0, 11)) +
    scale_y_reverse()
  
  
  
  girafe(ggobj = graph_verif,
         bg = "transparent",
         height_svg = nrow(df_verif)*0.5,
         width_svg = 10,)
  
  
}

#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####                        BonbonMiel                       #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

# ~~~~{    Unique    }~~~~
Gro_BonbonMiel <- function(df_resume_periode, list_col, sens = 'Debit'){
  print('bbm unique')
  hsize = 3
  
  df_bbm_periode_0 <- if(sens == 'Debit') filter(df_resume_periode, Montant < 0) else filter(df_resume_periode, Montant > 0)
  
  
  
  df_camembert <- df_bbm_periode_0 %>%
    mutate(Montant = abs(Montant)) %>%
    group_by(Super_Classe, Classe) %>%
    summarise(Montant = sum(Montant, na.rm = TRUE)) %>%
    ungroup() %>%
    
    arrange(Classe) %>%
    mutate(ylab = cumsum(Montant)-0.5*Montant,
           Classe_num = as.numeric(Classe),
           Classe_label = paste(str_remove(Classe, '.'),'\n', round(Montant), '€')) %>%
    group_by(Super_Classe) %>%
    mutate(Super_Classe_label = paste(str_remove(Super_Classe,'.'),'\n', round(sum(Montant)), '€')) %>%
    as.data.frame()
  
  df_camembert_lab <- df_camembert %>%
    group_by(Super_Classe) %>%
    summarise(Montant = sum(Montant, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(Montant)) %>%
    mutate(ylab = cumsum(Montant)-0.5*Montant,
           hjust_dir = 0.5-sinpi(2*ylab/total)/2,
           vjust_dir = 0.5-cospi(2*ylab/total)/2) %>%
    mutate(Super_Classe = ifelse(is.na(Super_Classe), 'NA', as.character(Super_Classe)))
  
  
  
  
  myplot <-
    ggplot(df_camembert) +
    
    # bande externe : superClasse
    geom_bar_interactive(aes(x = 1.2+hsize,
                             y = Montant,
                             fill = Super_Classe,
                             tooltip = Super_Classe_label,
                             data_id = Super_Classe),
                         width = 0.4, stat = "identity", position = position_stack(reverse = TRUE)) +
    
    # bande interne : Classe
    geom_bar_interactive(aes(x = hsize, 
                             y = Montant,
                             fill = Classe,
                             tooltip=Classe_label, 
                             data_id = Classe),
                         width = 2, stat = "identity", position = position_stack(reverse = TRUE)) +
    
    scale_fill_manual(values = list_col) +
    
    geom_text(data = df_camembert_lab, 
              aes(y = ylab, label = Super_Classe, hjust = hjust_dir, vjust = vjust_dir), 
              x = 1.6 + hsize) +
    
    
    guides(fill = 'none') +
    coord_polar("y", start=0, clip = 'off') +
    xlim(c(0, 1.5+hsize)) +
    theme_void() + 
    theme(panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = element_rect(fill='transparent')) #transparent legend panel
  
  
  
  
  
  interactive_plot <- girafe(ggobj = myplot,
                             bg = "transparent",
                             options = list(
                               opts_hover(css = "filter: brightness(95%);
                                        cursor: pointer;"),
                               opts_hover_inv(css = "opacity:0.4;"),
                               # reactive = TRUE,
                               opts_selection(css = 'stroke:black',
                                              type = "single")
                             ))
  interactive_plot
  
}
BonbonMiel_unique_giraph <- Gro_BonbonMiel

# ~~~~{    Par periode    }~~~~

Ti_BonbonMiel <- function(df_resume_periode, list_col, sens = 'Debit'){
  hsize = 3
  
  # ordre dans lequel on veut voir apparaitre les colonnes du graphiques : periode sans l'année
  ordre_factor_periodes <- c(format(as.Date(str_c('01-', 1:12,'-2000')),'%B'), # tous les mois
                             'janv. févr. mars', 'avr. mai juin', 'juil. août sept.', 'oct. nov. déc.', # tous les trimestres
                             'janvier à juin', 'juiller à décembre' # semestres
  )
  
  
  df_bbm_periode_0 <- if(toupper(sens) == 'DEBIT') filter(df_resume_periode, Montant < 0) else filter(df_resume_periode, Montant > 0)
  
  
  df_bbm_periode <- df_bbm_periode_0 %>%
    mutate(Montant = abs(Montant)) %>%
    group_by(periode) %>%
    arrange(Classe) %>% # les Classes sont des facteurs ordonnées selon le cout total de leurs Super_Classes respectives
    mutate(Classe_label = paste(str_remove(Classe, '.'),'\n', round(Montant), '€'), # utilisé quand on passe la souris sur une zone
           Classe_periode = paste0(Classe,'/',periode)) %>%      # utilisé pour identifier les zones
    
    group_by(Super_Classe, periode) %>%
    mutate(Super_Classe_label = paste(str_remove(Super_Classe, '.'),'\n', round(sum(Montant)), '€'), # utilisé quand on passe la souris sur une marge
           Super_Classe_periode = paste0(Super_Classe,'/',periode)) %>%           # utilisé pour identifier les zones
    mutate(an = format(periode, '%Y'),
           periode_an = str_trim(str_extract(Label_periode, '^.{3,4}\\D+')), # on isole la premiere partie du Label, avant l'année
           periode_an = factor(periode_an, ordre_factor_periodes)) %>%
    as.data.frame()
  
  echelle <- quel_periode(df_resume_periode$Label_periode)
  
  N_col <- if( echelle != 'An' ) length(unique(df_bbm_periode$periode_an)) else length(unique(df_bbm_periode$an))
  N_lig <- if( echelle != 'An' ) length(unique(df_bbm_periode$an)) else 1
  
  # print('ICI_2')
  
  myplot <-
    ggplot(df_bbm_periode) +
    
    # bande externe : superClasse
    geom_bar_interactive(aes(x       = 1.2 + hsize,
                             y       = Montant,
                             fill    = Super_Classe,
                             tooltip = Super_Classe_label,
                             data_id = Super_Classe_periode),
                         width   = 0.4, 
                         stat    = "identity", 
                         position = position_stack(reverse = TRUE)) +
    
    # bande interne : Classe
    geom_bar_interactive(aes(x       = hsize, 
                             y       = Montant,
                             fill    = Classe,
                             tooltip = Classe_label, 
                             data_id = Classe_periode),
                         width    = 2, 
                         stat     = "identity", 
                         position = position_stack(reverse = TRUE)) +
    
    scale_fill_manual(values = list_col) +
    
    # total € au milieu
    geom_text(data = distinct(df_bbm_periode, Label_periode, periode, .keep_all = TRUE),
              aes(label = str_extract(Label_periode, '\\d+ €$')),
              x = 0, y = 0) +#, size = 10, size.unit = 'pt')+ # size = ifelse(N_col>10, 7, 11)
    guides(fill = 'none') +
    
    
    coord_polar("y", start=0, clip = 'off') +
    xlim(c(0, 1.5+hsize)) +
    theme_void() + 
    theme(strip.text.y = element_text( angle = 270, vjust = 1),
          strip.text.x = element_text( vjust = 1))
  
  # # Ajout des labels sauf si l'echelle est le mois
  # if(N_col<10)
  #   myplot <- myplot +
  #   geom_text(data = df_bbm_lab, 
  #             aes(y     = ylab, 
  #                 label = Classe, 
  #                 hjust = hjust_dir, 
  #                 vjust = vjust_dir), 
  #             x    = 1.6 + hsize, 
  #             size = 7, size.unit = 'pt')
  
  # grille si annee ou autre
  if(is.na(first(df_bbm_periode$periode_an))){ # echelle == annee
    myplot <- myplot + facet_grid(.~paste(an, '\n')) 
  } else {
    myplot <- myplot + facet_grid(paste(an, '\n')~periode_an) 
  }
  
  
  # print('ICI_3')
  
  
  interactive_plot <- girafe(ggobj = myplot,
                             bg = "transparent",
                             height_svg = N_lig * 2,
                             width_svg  = N_col * 2,
                             # height_svg = 2*length(unique(df_bbm_periode$an)),
                             # width_svg  = max(10,N_col*2),
                             # pointsize  = 5,
                             options    = list(
                               opts_hover(css = "filter: brightness(95%)"),
                               opts_hover_inv(css = "opacity:0.4;"),
                               opts_selection(css = 'stroke:black',
                                              type = "single")
                             ))
  interactive_plot
  
  # htmltools::save_html(interactive_plot, "BonbonMiel_trimestriel.html")
  
  
}
BonbonMiel_trimestriel_giraph <- Ti_BonbonMiel





# ~~~~{    Comparaison en fesse à fesse    }~~~~

Fesses <- function(df_resume_periode, list_col, sens = 'Debit'){

  # ordre dans lequel on veut voir apparaitre les colonnes du graphiques : periode sans l'année
  ordre_factor_periodes <- c(format(as.Date(str_c('01-', 1:12,'-2000')),'%B'), # tous les mois
                             'janv. févr. mars', 'avr. mai juin', 'juil. août sept.', 'oct. nov. déc.', # tous les trimestres
                             'janvier à juin', 'juiller à décembre' # semestres
  )
   
  
  df_bbm_periode <- df_resume_periode %>%
    filter(Montant != 0) %>%
    # mutate(Montant = abs(Montant)) %>%
    group_by(periode) %>%
    arrange(Classe) %>% # les Classes sont des facteurs ordonnées selon le cout total de leurs Super_Classes respectives
    mutate(Classe_label = paste(str_remove(Classe, '.'),'\n', round(Montant), '€'), # utilisé quand on passe la souris sur une zone
           Classe_periode = paste0(Classe,'/',periode)) %>%      # utilisé pour identifier les zones
    
    group_by(Super_Classe, periode) %>%
    mutate(Super_Classe_label = paste(str_remove(Super_Classe, '.'),'\n', round(sum(Montant)), '€'), # utilisé quand on passe la souris sur une marge
           Super_Classe_periode = paste0(Super_Classe,'/',periode)) %>%           # utilisé pour identifier les zones
    mutate(an = format(periode, '%Y'),
           periode_an = str_trim(str_extract(Label_periode, '^.{3,4}\\D+')), # on isole la premiere partie du Label, avant l'année
           periode_an = factor(periode_an, ordre_factor_periodes)) %>%
    as.data.frame()
  
  
  echelle <- quel_periode(df_resume_periode$Label_periode)
  
  N_col <- if( echelle != 'An' ) length(unique(df_bbm_periode$periode_an)) else length(unique(df_bbm_periode$an))
  N_lig <- if( echelle != 'An' ) length(unique(df_bbm_periode$an)) else 1
  
  # print('ICI_2')
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####               Adaptation à la courbe spéciale              #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  # ~~~~{    les bords des classes    }~~~~
  
  df_bbm_periode2 <- df_bbm_periode %>%
    # filter(periode %in% c('2024-02-15', '2024-08-15')) %>%
    group_by(periode) %>%
    summarize(somme_periode = max(sum(pmax(0,Montant)), sum(pmax(0, -Montant)))) %>% # à chaque période, le plus grand des recettes ou des déppenses remplira totalement son cadrant. On calcul ici la plus grande des deux valeurs
    inner_join(df_bbm_periode, by = join_by(periode)) %>%
    mutate(part_de_periode = Montant/somme_periode)%>%
    group_by(periode, sens = Montant>0) %>%
    arrange(periode, sens, is.na(Classe), Classe)  %>%
    mutate(som_cu = cumsum(part_de_periode)) %>%
    ungroup()
  
  # manips pour séparer recettes et dépenses et faire démarrer en bas
  df_bbm_periode2 <- df_bbm_periode2  %>%
    mutate(X1 = c(0, df_bbm_periode2$som_cu[1:(nrow(df_bbm_periode2)-1)]), 
           X1 = 1+X1,
           X1 = ifelse((X1 > 1) == (som_cu > 0), X1, 1), #test si c'est la même direction recette ou dépense. Sinon, c'est la première valeur de son sens/période
           
           X2 = 1+som_cu,
           Xmax = pmax(X1, X2),
           Xmin = pmin(X1, X2))
  
  
  # ~~~~{    le tracé de la courbe    }~~~~
  #définitioin très faible pour la courbe de base : seulement 40 points suffisent
  A <- B <- data.frame(X=c(0:20/20)) %>%
    mutate(Y=1-(X-0.5)^2)
  
  B$X <- B$X+1
  
  AB <- bind_rows(A,B) %>% distinct()
  
  #on rajoute tout de même des points à chaque intersection de classe pour qu'elles se touchent  
  joints <- data.frame(X= unique(df_bbm_periode2$X2)) %>%
    mutate(Y=1-(ifelse(X > 1, X-1, X)-0.5)^2)
  
  XY <- bind_rows(AB, joints) %>% distinct()
  
  # la courbe nue :
  # ggplot(XY, aes(X,Y)) +
  #   geom_line(linewidth = 2, lineend = "round") +
  #   # geom_point() +
  #   coord_polar("x", start=0, clip = 'off') +
  #   ylim(c(0, 1))
  #
  
  # ~~~~{    jointure des coordonnées des classes et de la courbe    }~~~~
  
  # la trame de base, pour les  catégorie étendues
  test_trame <- expand.grid(AB$X, df_bbm_periode2$Classe_periode) %>%
    rename(X=Var1, Classe_periode=Var2) %>%
    inner_join(df_bbm_periode2, by = join_by(Classe_periode)) %>%
    filter(X <= Xmax, X>=Xmin) 
  
  # les bordures des classes
  test_bords1 <- rename(df_bbm_periode2, X = X1)
  test_bords2 <- rename(df_bbm_periode2, X = X2)
  
  # tout
  test <- bind_rows(test_trame, test_bords1, test_bords2) %>%
    inner_join(XY, by = join_by(X))
  
  
  
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  #####                           GGPLOT                           #####
  #  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
  
  myplot <- #filter(test, periode == '2024-08-15') %>% # select(X, Y, Classe, Super_Classe, Montant) %>% View()
    ggplot(test, aes(X, Y)) +   
    geom_ribbon(data = AB, aes(ymin = Y-0.3, ymax=Y+0.05), fill = 'grey95')+
    geom_ribbon_interactive(aes(ymin = Y-0.3, ymax=Y, 
                                fill = Classe,
                                tooltip = Classe_label, 
                                data_id = Classe_periode))+
    geom_ribbon(aes(ymin = Y, ymax= Y+0.05,
                                fill = Super_Classe))+
    
    # geom_text(aes(label = paste(X,signif(Y, 2))), color = 'black')+
    # geom_point() +
    scale_fill_manual(values = list_col) +
    guides(fill = 'none') +
    coord_polar("x", start=0, clip = 'off') +
    theme_void() +
    ylim(c(0, 1.05)) +
    xlim(c(0,2)) +
    # total € au milieu
    geom_text(data = distinct(test, Label_periode, periode, .keep_all = TRUE),
              aes(label = paste0(ifelse(sens, 'Recettes\n', 'Dépenses\n'), str_extract(Label_periode, '\\d+ €$')), 
                  x = ifelse(sens, 1.5,0.5),
                  hjust =as.numeric(sens)),
              y = 0.05, , size = 7, size.unit = 'pt') + # size = ifelse(N_col>10, 7, 11)
    theme(strip.text.y = element_text( angle = 270, vjust = 1),
          strip.text.x = element_text( vjust = 1))
  
  # grille si annee ou autre
  if(is.na(first(df_bbm_periode$periode_an))){ # echelle == annee
    myplot <- myplot + facet_grid(.~paste(an, '\n')) 
  } else {
    myplot <- myplot + facet_grid(paste(an, '\n')~periode_an) 
  }

  interactive_plot <- girafe(ggobj = myplot,
                             bg = "transparent",
                             height_svg = N_lig * 2,
                             width_svg  = N_col * 2,
                             # height_svg = 2*length(unique(df_bbm_periode$an)),
                             # width_svg  = max(10,N_col*2),
                             # pointsize  = 5,
                             options    = list(
                               opts_hover(css = "filter: brightness(95%)"),
                               opts_hover_inv(css = "opacity:0.4;"),
                               opts_selection(css = 'stroke:black',
                                              type = "single")
                             ))
  interactive_plot
  
  
}

#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####                       histogramme                       #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

# ~~~~{    Par periode    }~~~~
histogramme_periode <- function(df_resume_periode, list_col, sens= 'Debit'){
  
  # ordre dans lequel on veut voir apparaitre les colonnes du graphiques : periode sans l'année
  ordre_factor_periodes <- c(format(as.Date(str_c('01-', 1:12,'-2000')),'%B'), # tous les mois
                             'janv. févr. mars', 'avr. mai juin', 'juil. août sept.', 'oct. nov. déc.', # tous les trimestres
                             'janvier à juin', 'juiller à décembre' # semestres
  )
  
  
  df_bbm_periode_0 <- if(sens == 'Debit') filter(df_resume_periode, Montant < 0) else filter(df_resume_periode, Montant > 0)
  
  
  df_bbm_periode <- df_bbm_periode_0 %>%
    mutate(Montant = abs(Montant)) %>%
    group_by(periode) %>%
    arrange(Classe) %>% # les Classes sont des facteurs ordonnées selon le cout total de leurs Super_Classes respectives
    mutate(Classe_label = paste(str_remove(Classe, '.'),'\n', round(Montant), '€'), # utilisé quand on passe la souris sur une zone
           Classe_periode = paste0(Classe,'/',periode)) %>%      # utilisé pour identifier les zones
    
    group_by(Super_Classe, periode) %>%
    mutate(Super_Classe_label = paste(str_remove(Super_Classe, '.'),'\n', round(sum(Montant)), '€'), # utilisé quand on passe la souris sur une marge
           Super_Classe_periode = paste0(Super_Classe,'/',periode)) %>%           # utilisé pour identifier les zones
    mutate(an = format(periode, '%Y'),
           periode_an = str_trim(str_extract(Label_periode, '^.{4}\\D+')), # on isole la premiere partie du Label, avant l'année
           periode_an = factor(periode_an, ordre_factor_periodes)) %>%
    as.data.frame()
  
  myplot <-
    ggplot(df_bbm_periode) +
    geom_bar_interactive(aes(x = Classe, 
                             y = Montant,
                             fill = Classe,
                             tooltip=Classe_label, 
                             data_id = Classe_periode),
                         stat = "identity") +
    
    scale_fill_manual(values = list_col) +
    
    guides(fill = 'none') +
    facet_grid(an~periode_an) + # facet_grid(.~Label_periode) 
    theme(axis.text.x = element_text( angle = 45, hjust = 1),
          strip.text.y = element_text(size = 12, angle = 270, vjust = 1),
          strip.text.x = element_text(size = 12, vjust = 1),
          panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = element_rect(fill='transparent')) #transparent legend panel
  
  
  interactive_plot <- girafe(ggobj = myplot,
                             bg = "transparent",
                             height_svg = 3,
                             width_svg = 10,
                             options = list(
                               opts_hover(css = "filter: brightness(95%)"),
                               opts_hover_inv(css = "opacity:0.4;"),
                               opts_selection(css = 'stroke:black',
                                              type = "single")
                             ))
  interactive_plot
  
  # htmltools::save_html(interactive_plot, "BonbonMiel_trimestriel.html")
  
  
}
histogramme_trimestriel_giraph <- histogramme_periode



# ~~~~{    Un seul graphique, mêmes Classes côte à côte    }~~~~
histogramme_Classe <- function(df_resume_periode, list_col, sens = 'Debit'){
  hsize = 3
  # df_bbm_periode[1,"Label_periode"]
  
  
  
  df_bbm_periode_0 <- if(sens == 'Debit') filter(df_resume_periode, Montant < 0) else filter(df_resume_periode, Montant > 0)
  
  
  df_bbm_periode <- df_bbm_periode_0 %>%
    mutate(Montant = abs(Montant)) %>%
    arrange(Classe) %>%
    mutate(Label_periode = str_c(format(periode, '%Y'),' T',round(as.numeric(format(periode, '%m'))/3)),
           # sC_C = str_c(Super_Classe, '_', Classe),
           sC_C = Classe,
           sC_C = factor(sC_C, unique(sC_C))) %>%
    group_by(Super_Classe, Classe, sC_C, Label_periode, periode) %>%
    summarise(Montant = sum(Montant, na.rm = TRUE)) %>%
    
    group_by(Label_periode) %>%
    arrange(sC_C) %>%
    mutate(Classe_label = paste(str_remove(Classe, '.'),'\n', round(Montant), '€'), # utilisé quand on passe la souris sur une zone
           Classe_periode = paste0(Classe,'/',periode)) %>%      # utilisé pour identifier les zones
    
    group_by(Super_Classe, periode) %>%
    mutate(Super_Classe_label = paste(str_remove(Super_Classe, '.'),'\n', round(sum(Montant)), '€'), # utilisé quand on passe la souris sur une marge
           Super_Classe_periode = paste0(Super_Classe,'/',periode)) %>%           # utilisé pour identifier les zones
    mutate(an = str_extract(Label_periode, '\\d{4}'),
           periode_an = str_trim(str_extract(Label_periode, '^\\D+')),
           periode_an = factor(periode_an, c('janv. févr. mars', 'avr. mai juin', 'juil. août sept.', 'oct. nov. déc.')) ) %>%
    as.data.frame()
  
  
  myplot <-
    ggplot(df_bbm_periode) +
    geom_bar_interactive(aes(x = sC_C, 
                             y = Montant,
                             fill = Label_periode,
                             tooltip = Classe_label, 
                             data_id = Classe_periode),
                         stat = "identity", position = position_dodge()) +
    labs(fill = 'periode', y='Débit (€)') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          legend.background = element_rect(fill='transparent'), #transparent legend bg
          legend.box.background = element_rect(fill='transparent')) #transparent legend panel
  
  
  interactive_plot <- girafe(ggobj = myplot,
                             bg = "transparent",
                             height_svg = 3,
                             width_svg = 10,
                             options = list(
                               opts_hover(css = "filter: brightness(95%)"),
                               opts_hover_inv(css = "opacity:0.4;"),
                               opts_selection(css = 'stroke:black',
                                              type = "single")
                             ))
  interactive_plot
  
  # htmltools::save_html(interactive_plot, "BonbonMiel_trimestriel.html")
  
}

histogramme_Classe_giraph <- histogramme_Classe



# ~~~~{    Comparaison Crédits Débits    }~~~~

histogramme_Fasse_a_Fasse <- function(df_resume_periode, list_col){
  
  # ordre dans lequel on veut voir apparaitre les colonnes du graphiques : periode sans l'année
  # ordre_factor_periodes <- c(format(as.Date(str_c('01-', 1:12,'-2000')),'%B'), # tous les mois
  #                            'janv. févr. mars', 'avr. mai juin', 'juil. août sept.', 'oct. nov. déc.', # tous les trimestres
  #                            'janvier à juin', 'juiller à décembre' # semestres
  # )
  
  echelle <- quelle_periode(label = df_resume_periode[1,"Label_periode"])
  
  
  df_fasse_a_fasse <- df_resume_periode %>%
    mutate(sens = if_else(Montant > 0, 'Credit', 'Debit'),
           periode_date = periode,
           periode = periodifier(periode, echelle = echelle)) %>%
    
    group_by(periode_date, periode, sens) %>%
    summarise(Montant = sum(Montant, na.rm = TRUE)) %>%
    mutate(label_montant = paste(abs(round(Montant)), '€'),
           id = paste0(periode_date, '/', sens))
  
  
  
  myplot <- ggplot(df_fasse_a_fasse, aes(x = Montant, y = periode, fill = sens)) +
    geom_col(aes(x=-Montant), fill= 'grey', orientation = 'y') +
    geom_col_interactive(aes(tooltip = label_montant,
                             data_id = id),
                         orientation = 'y') +
    scale_fill_brewer(palette = 'Dark2') +
    geom_text(data=filter(df_fasse_a_fasse, sens== 'Debit'), aes(label = periodifier(periode_date, echelle, 'Long')), x = 0) +
    scale_y_discrete(limits = rev) +
    scale_x_continuous(position = 'top') +
    theme(axis.text.y=element_blank(),  #remove y axis labels
          axis.ticks.y=element_blank(),  #remove y axis ticks
          axis.title.y = element_blank()
    ) +
    guides(fill = 'none')
  
  
  interactive_plot <- girafe(ggobj = myplot,
                             bg = "transparent",
                             height_svg = length(unique(df_fasse_a_fasse$periode))*0.6,
                             width_svg = 10,
                             options = list(
                               opts_hover(css = "filter: brightness(95%)"),
                               opts_hover_inv(css = "opacity:0.4;"),
                               opts_selection(css = 'stroke:black',
                                              type = "single")
                             ))
  
  interactive_plot
  
}





#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####                   Les graphs pas ouf                    #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #


Courbe_empile <- function(df_resume_periode, list_col){
  
  fact_SC <- c(levels(df_resume_periode$Super_Classe), '_NA')
  fact_C <- c(levels(df_resume_periode$Classe), '_NA')
  
  df_courbe_empile <- as.data.frame(df_resume_periode)
  df_courbe_empile[,c('Classe','Super_Classe')] <- apply(df_courbe_empile[,c('Classe','Super_Classe')], 2, as.character)
  df_courbe_empile[is.na(df_courbe_empile$Classe), c('Classe','Super_Classe')] <- '_NA'
  
  df_courbe_empile <- df_courbe_empile %>%
    mutate(Super_Classe = factor(Super_Classe, fact_SC),
           Classe = factor(Classe, fact_C)) %>%
    group_by(Super_Classe, Classe, periode) %>%
    summarise(Montant = sum(Montant, na.rm = TRUE)) %>%
    group_by(periode) %>%
    arrange(Super_Classe, Classe) %>%
    mutate(YMAX = cumsum(Montant),
           YMIN = YMAX - Montant)  %>%
    as_tibble()
  
  list_col$'_NA' <- 'grey45'
  
  
  myplot <-
    ggplot(df_courbe_empile) +
    # geom_area_interactive(position = position_stack(reverse = TRUE)) +
    geom_ribbon_interactive(aes(x = periode,
                                fill    = Classe,
                                tooltip = Classe, 
                                data_id = Classe,
                                ymin = YMIN, 
                                ymax = YMAX)) +
    
    scale_fill_manual(values = list_col) +
    guides(fill = 'none')
  
  
  interactive_plot <-
    girafe(ggobj = myplot,
           bg = "transparent",
           options = list(
             opts_hover(css = "filter: brightness(95%)"),
             opts_hover_inv(css = "opacity:0.4;")
           )
    )
  interactive_plot
  
  # htmltools::save_html(interactive_plot, "Courbe_empile.html")
}



# radar <- function(df_resume_periode, list_col){
#   
#   echelle <- quel_periode(label = df_resume_periode[1,"Label_periode"])
#   
#   
#   # ggplot ne comprend pas la jointure entre les deux bouts du cercle
#   annees <- unique(df_resume_periode$periode) %>%
#     format('%Y') %>%
#     unique()
#   
#   annees_chevauche <- annees[-1]
#   
#   deb_janv_chev <- annees_chevauche %>%
#     str_c('-01-01') %>%
#     as.Date()
#   
#   
#   # les debuts et fins d'année problématiques avec leurs périodes
#   df_jointures1 <- data.frame(deb_janv = deb_janv_chev) %>%
#     mutate(fin_dec = deb_janv-1,
#            periode_deb = periodifier(deb_janv, echelle, 'Date'),
#            periode_fin = periodifier(fin_dec, echelle, 'Date'))
#   
#   # pour toutes les classes
#   df_jointures2 <- expand.grid(deb_janv=df_jointures1$deb_janv, Classe=unique(df_resume_periode$Classe)) %>%
#     left_join(df_jointures1)
#   
#   
#   # ajout des débits
#   # pour la première période de l'année
#   df_jointures2 <- df_resume_periode %>%
#     select(periode_deb = periode, Montant_janv = Montant, Classe) %>%
#     right_join(df_jointures2)
#   
#   # pour la dernière période de l'année
#   df_jointures2 <- df_resume_periode %>%
#     select(periode_fin = periode, Montant_dec = Montant, Classe) %>%
#     right_join(df_jointures2)
#   
#   # Montant de jointure = moyenne des deux
#   df_jointures2 <- df_jointures2 %>%
#     mutate(Montant = (Montant_janv+Montant_dec)/2)
#   
#   # lignes à ajouter
#   df_jointures_fin <- df_jointures2 %>%
#     select(periode=fin_dec, Classe, Montant)
#   
#   df_jointures_deb <- df_jointures2 %>%
#     select(periode=deb_janv, Classe, Montant)
#   
#   
#   # jointure
#   df_plot <- bind_rows(df_resume_periode, df_jointures_fin, df_jointures_deb) %>%
#     mutate(an = format(periode, '%Y'),
#            periode_an = une_annee(periode, mois_deb = 0))
#   
#   
#   
#   
#   
#   
#   myplot <-
#     df_plot %>%
#     arrange(periode) %>%
#     ggplot(aes(x = Montant,
#                y = periode_an,
#                color = Classe,
#                group = str_c(Classe,an))) +
#     geom_path(linewidth = 1) +
#     scale_color_manual(values = unlist(list_col)) +
#     
#     coord_polar("y") 
#   
#   guides(color = 'none')
#   
#   
# }






































