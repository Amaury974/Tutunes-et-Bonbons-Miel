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
  
  echelle <- quel_periode(label = df_resume_periode[1,"Label_periode"])
  # 
  print(echelle)
  # 
  
  
  # # ~~~~{    Nombre de comptes d'origine    }~~~~
  # df_identifie %>%
  #   select(Date, Compte) %>%
  #   mutate(periode = periodifier(Date, echelle, 'Court')) %>%
  #   group_by(periode) %>%
  #   summarize(N_releves = length(unique(Compte)))
  
  # ~~~~{    Nombre de lignes de dépense    }~~~~
  
  df_verif <- df_identifie %>%
    # select(Date, Compte) %>%
    arrange(Date) %>%
    mutate(periode = periodifier(Date, echelle, 'Court'),
           periode = factor(periode, unique(periode)),
           Mois = format(Date, '%m')) %>%
    group_by(periode, Compte) %>%
    summarize(N_ligne = length(unique(Date)),
              Date_min = min(Date),
              date_max = max(Date),
              nbr_mois = length(unique(Mois))) %>%
    ungroup() %>%
    mutate(couleur_periode = nbr_mois == max(nbr_mois),
           nbr_mois = if_else(couleur_periode, as.character(nbr_mois), str_c('bold(underline(',nbr_mois,'))'))) %>%
    group_by(Compte) %>%
    mutate(couleur_lignes = abs(N_ligne - mean(N_ligne))/mean(N_ligne),
           couleur_lignes = case_when(couleur_lignes > 0.3 ~ 'FALSE', couleur_lignes > 0.1 ~ 'suspect', .default = 'TRUE'),
           lab_date = )
  
  # estetique
  df_verif <- df_verif %>%
    mutate(an = str_extract(periode, '.{4}'))
  
  # colone période
  df_verif$base_Y <- 1:nrow(df_verif)
  
  df_verif <- df_verif %>%
    group_by(periode) %>%
    mutate(Y_periode = mean(base_Y))
  
  
  myplot <- ggplot(df_verif) +
    geom_rect(aes(ymin = base_Y -0.5, ymax = base_Y +0.5, fill = periode), xmin = 0.9, xmax = 10, alpha = 0.2) +
    geom_text(aes(label = periode,  y = Y_periode), x = 1, hjust = 0) + #color = an,
    geom_text(aes(label = Compte,  y = base_Y), x = 2, hjust = 0) + #color = Compte,
    geom_text(aes(label = str_c(nbr_mois, 'mois~du', format_plotmath(Date_min), 'au', format_plotmath(date_max), sep = '~'), color = couleur_periode, y = base_Y), x = 6, hjust = 0, parse = TRUE) +
    geom_text(aes(label = paste(N_ligne, 'lignes'), y = base_Y, color = couleur_lignes), x = 9, hjust = 0) +
    
    scale_color_manual(values = c('TRUE' = 'black', 'FALSE' = 'red', 'suspect' = 'darkorange')) +
    guides(color = 'none', fill = 'none') +
    theme_void() +
    xlim(c(0, 11)) +
    scale_y_reverse()
  
  
  
  girafe(ggobj = myplot,
         bg = "transparent",
         height_svg = nrow(df_verif),
         width_svg = 10,)
  
  
}

#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####                        BonbonMiel                       #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

# ~~~~{    Unique    }~~~~
Gro_BonbonMiel <- function(df_resume_periode, list_col){
  print('bbm unique')
  hsize = 3
  
  df_camembert <- df_resume_periode %>%
    group_by(Super_Classe, Classe) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    ungroup() %>%
    
    arrange(Classe) %>%
    mutate(ylab = cumsum(Debit)-0.5*Debit,
           Classe_num = as.numeric(Classe),
           Classe_label = paste(Classe,'\n', round(Debit), '€')) %>%
    group_by(Super_Classe) %>%
    mutate(Super_Classe_label = paste(Super_Classe,'\n', round(sum(Debit)), '€')) %>%
    as.data.frame()
  
  df_camembert_lab <- df_camembert %>%
    group_by(Super_Classe) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(Debit)) %>%
    mutate(ylab = cumsum(Debit)-0.5*Debit,
           hjust_dir = 0.5-sinpi(2*ylab/total)/2,
           vjust_dir = 0.5-cospi(2*ylab/total)/2) %>%
    mutate(Super_Classe = ifelse(is.na(Super_Classe), 'NA', as.character(Super_Classe)))
  
  
  
  
  myplot <-
    ggplot(df_camembert) +
    
    # bande externe : superClasse
    geom_bar_interactive(aes(x = 1.2+hsize,
                             y = Debit,
                             fill = Super_Classe,
                             tooltip = Super_Classe_label,
                             data_id = Super_Classe),
                         width = 0.4, stat = "identity", position = position_stack(reverse = TRUE)) +
    
    # bande interne : Classe
    geom_bar_interactive(aes(x = hsize, 
                             y = Debit,
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

Ti_BonbonMiel <- function(df_resume_periode, list_col){
  hsize = 3
  
  # ordre dans lequel on veut voir apparaitre les colonnes du graphiques : periode sans l'année
  ordre_factor_periodes <- c(format(as.Date(str_c('01-', 1:12,'-2000')),'%B'), # tous les mois
                             'janv. févr. mars', 'avr. mai juin', 'juil. août sept.', 'oct. nov. déc.', # tous les trimestres
                             'janvier à juin', 'juiller à décembre' # semestres
  )
  
  
  
  df_bbm_periode <- df_resume_periode %>%
    group_by(periode) %>%
    arrange(Classe) %>% # les Classes sont des facteurs ordonnées selon le cout total de leurs Super_Classes respectives
    mutate(Classe_label = paste(Classe,'\n', round(Debit), '€'), # utilisé quand on passe la souris sur une zone
           Classe_periode = paste0(Classe,'/',periode)) %>%      # utilisé pour identifier les zones
    
    group_by(Super_Classe, periode) %>%
    mutate(Super_Classe_label = paste(Super_Classe,'\n', round(sum(Debit)), '€'), # utilisé quand on passe la souris sur une marge
           Super_Classe_periode = paste0(Super_Classe,'/',periode)) %>%           # utilisé pour identifier les zones
    mutate(an = format(periode, '%Y'),
           periode_an = str_trim(str_extract(Label_periode, '^.{4}\\D+')), # on isole la premiere partie du Label, avant l'année
           periode_an = factor(periode_an, ordre_factor_periodes)) %>%
    as.data.frame()
  
  
  df_bbm_lab <- df_bbm_periode %>%
    # group_by(Super_Classe, Label_periode, periode) %>%
    # summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    group_by(Label_periode) %>%
    mutate(total_trim = sum(Debit)) %>%
    mutate(ylab = cumsum(Debit)-0.5*Debit) %>%
    # décallage en fonction de la position dans le cercle
    ungroup() %>%
    mutate(max_total = max(total_trim),
           # direction du text par rapport au point d'ancrage -> vers l'extérieur du cercle :
           hjust_dir = 0.5-sinpi(2*ylab/max_total)/2, 
           vjust_dir = 0.5-cospi(2*ylab/max_total)/2) %>% # 0 : vers le haut ; 1 : vers le bas
    mutate(an = format(periode, '%Y'),
           periode_an = str_trim(str_extract(Label_periode, '^.{4}\\D+')), # on isole la premiere partie du Label, avant l'année
           periode_an = factor(periode_an, ordre_factor_periodes)) %>%
    # retrait des trop petites Classes pour eviter les chevauchements de label
    # filter(Debit > 0.02*max_total)
    mutate(Super_Classe = str_c(signif(Debit / (0.02*max_total), 3),
                                signif(pmax(0.3, abs(2*vjust_dir-1)),3),
                                signif((Debit / (0.02*max_total)) / pmax(0.3, abs(2*vjust_dir-1)) ,3), sep = ' ')) %>%
    # filter(((Debit / (0.02*max_total)) / pmax(0.2, abs(2*vjust_dir-1))) > 1)
    filter((Debit / (0.08*max_total*pmax(0.2, abs(2*vjust_dir-1)))) > 1)
  
  # ((Debit / a) / 2*pmax(0.2, abs(2*vjust_dir-1)))
  # (Debit / (a*2*pmax(0.2, abs(2*vjust_dir-1))))
  # 
  # (48648/5464)/65484
  # 48648/(5464*65484)
  N_col <- length(unique(df_bbm_periode$periode_an))
  
  
  myplot <-
    ggplot(df_bbm_periode) +
    
    # bande externe : superClasse
    geom_bar_interactive(aes(x       = 1.2 + hsize,
                             y       = Debit,
                             fill    = Super_Classe,
                             tooltip = Super_Classe_label,
                             data_id = Super_Classe_periode),
                         width   = 0.4, 
                         stat    = "identity", 
                         position = position_stack(reverse = TRUE)) +
    
    # bande interne : Classe
    geom_bar_interactive(aes(x       = hsize, 
                             y       = Debit,
                             fill    = Classe,
                             tooltip = Classe_label, 
                             data_id = Classe_periode),
                         width    = 2, 
                         stat     = "identity", 
                         position = position_stack(reverse = TRUE)) +
    
    scale_fill_manual(values = list_col) +
    
    geom_text(aes(label = str_extract(Label_periode, '\\d+ €$')),
              x = 0, y = 0)+ # size = ifelse(N_col>10, 7, 11), size.unit = 'pt'
    guides(fill = 'none') +
    
    
    coord_polar("y", start=0, clip = 'off') +
    xlim(c(0, 1.5+hsize)) +
    theme_void() + 
    theme(strip.text.y = element_text( angle = 270, vjust = 1),
          strip.text.x = element_text( vjust = 1),
          
          panel.background = element_rect(fill='transparent', color=NA), #transparent panel bg
          plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
          legend.background = element_rect(fill='transparent', color=NA), #transparent legend bg
          legend.box.background = element_rect(fill='transparent', color=NA))
  # size = 12,
  
  # Ajout des labels sauf si l'echelle est le mois
  if(N_col<10)
    myplot <- myplot +
    geom_text(data = df_bbm_lab, 
              aes(y     = ylab, 
                  label = Classe, 
                  hjust = hjust_dir, 
                  vjust = vjust_dir), 
              x    = 1.6 + hsize)
  
  # grille si annee ou autre
  if(is.na(first(df_bbm_periode$periode_an))){ # echelle == annee
    myplot <- myplot + facet_grid(.~paste(an, '\n')) 
  } else {
    myplot <- myplot + facet_grid(paste(an, '\n')~periode_an) 
  }
  
  
  
  
  interactive_plot <- girafe(ggobj      = myplot,
                             bg = "transparent",
                             height_svg = 2*length(unique(df_bbm_periode$an)),
                             width_svg  = max(10,N_col*2),
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


#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####                       histogramme                       #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

# ~~~~{    Par periode    }~~~~
histogramme_periode <- function(df_resume_periode, list_col){
  
  # ordre dans lequel on veut voir apparaitre les colonnes du graphiques : periode sans l'année
  ordre_factor_periodes <- c(format(as.Date(str_c('01-', 1:12,'-2000')),'%B'), # tous les mois
                             'janv. févr. mars', 'avr. mai juin', 'juil. août sept.', 'oct. nov. déc.', # tous les trimestres
                             'janvier à juin', 'juiller à décembre' # semestres
  )
  
  df_bbm_periode <- df_resume_periode %>%
    group_by(periode) %>%
    arrange(Classe) %>% # les Classes sont des facteurs ordonnées selon le cout total de leurs Super_Classes respectives
    mutate(Classe_label = paste(Classe,'\n', round(Debit), '€'), # utilisé quand on passe la souris sur une zone
           Classe_periode = paste0(Classe,'/',periode)) %>%      # utilisé pour identifier les zones
    
    group_by(Super_Classe, periode) %>%
    mutate(Super_Classe_label = paste(Super_Classe,'\n', round(sum(Debit)), '€'), # utilisé quand on passe la souris sur une marge
           Super_Classe_periode = paste0(Super_Classe,'/',periode)) %>%           # utilisé pour identifier les zones
    mutate(an = format(periode, '%Y'),
           periode_an = str_trim(str_extract(Label_periode, '^.{4}\\D+')), # on isole la premiere partie du Label, avant l'année
           periode_an = factor(periode_an, ordre_factor_periodes)) %>%
    as.data.frame()
  
  myplot <-
    ggplot(df_bbm_periode) +
    geom_bar_interactive(aes(x = Classe, 
                             y = Debit,
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
histogramme_Classe <- function(df_resume_periode, list_col){
  hsize = 3
  # df_bbm_periode[1,"Label_periode"]
  
  
  df_bbm_periode <- df_resume_periode %>%
    arrange(Classe) %>%
    mutate(Label_periode = str_c(format(periode, '%Y'),' T',round(as.numeric(format(periode, '%m'))/3)),
           sC_C = str_c(Super_Classe, '_', Classe),
           sC_C = factor(sC_C, unique(sC_C))) %>%
    group_by(Super_Classe, Classe, sC_C, Label_periode, periode) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    
    group_by(Label_periode) %>%
    arrange(sC_C) %>%
    mutate(Classe_label = paste(Label_periode, sC_C,'\n', round(Debit), '€'),
           Classe_periode = paste0(Classe,'/',periode)) %>%
    
    group_by(Super_Classe, periode) %>%
    mutate(Super_Classe_label = paste(Super_Classe,'\n', round(sum(Debit)), '€'),
           Super_Classe_periode = paste0(Super_Classe,'/',periode)) %>%
    mutate(an = str_extract(Label_periode, '\\d{4}'),
           periode_an = str_trim(str_extract(Label_periode, '^\\D+')),
           periode_an = factor(periode_an, c('janv. févr. mars', 'avr. mai juin', 'juil. août sept.', 'oct. nov. déc.')) ) %>%
    as.data.frame()
  
  
  myplot <-
    ggplot(df_bbm_periode) +
    geom_bar_interactive(aes(x = sC_C, 
                             y = Debit,
                             fill = Label_periode,
                             tooltip=Classe_label, 
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
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    group_by(periode) %>%
    arrange(Super_Classe, Classe) %>%
    mutate(YMAX = cumsum(Debit),
           YMIN = YMAX - Debit)  %>%
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



radar <- function(df_resume_periode, list_col){
  
  echelle <- quel_periode(label = df_resume_periode[1,"Label_periode"])
  
  
  # ggplot ne comprend pas la jointure entre les deux bouts du cercle
  annees <- unique(df_resume_periode$periode) %>%
    format('%Y') %>%
    unique()
  
  annees_chevauche <- annees[-1]
  
  deb_janv_chev <- annees_chevauche %>%
    str_c('-01-01') %>%
    as.Date()
  
  
  # les debuts et fins d'année problématiques avec leurs périodes
  df_jointures1 <- data.frame(deb_janv = deb_janv_chev) %>%
    mutate(fin_dec = deb_janv-1,
           periode_deb = periodifier(deb_janv, echelle, 'Date'),
           periode_fin = periodifier(fin_dec, echelle, 'Date'))
  
  # pour toutes les classes
  df_jointures2 <- expand.grid(deb_janv=df_jointures1$deb_janv, Classe=unique(df_resume_periode$Classe)) %>%
    left_join(df_jointures1)
  
  
  # ajout des débits
  # pour la première période de l'année
  df_jointures2 <- df_resume_periode %>%
    select(periode_deb = periode, Debit_janv = Debit, Classe) %>%
    right_join(df_jointures2)
  
  # pour la dernière période de l'année
  df_jointures2 <- df_resume_periode %>%
    select(periode_fin = periode, Debit_dec = Debit, Classe) %>%
    right_join(df_jointures2)
  
  # Debit de jointure = moyenne des deux
  df_jointures2 <- df_jointures2 %>%
    mutate(Debit = (Debit_janv+Debit_dec)/2)
  
  # lignes à ajouter
  df_jointures_fin <- df_jointures2 %>%
    select(periode=fin_dec, Classe, Debit)
  
  df_jointures_deb <- df_jointures2 %>%
    select(periode=deb_janv, Classe, Debit)
  
  
  # jointure
  df_plot <- bind_rows(df_resume_periode, df_jointures_fin, df_jointures_deb) %>%
    mutate(an = format(periode, '%Y'),
           periode_an = une_annee(periode, mois_deb = 0))
  
  
  
  
  
  
  # myplot <-
  df_plot %>%
    arrange(periode) %>%
    ggplot(aes(x = Debit,
               y = periode_an,
               color = Classe,
               group = str_c(Classe,an))) +
    geom_path(linewidth = 1) +
    scale_color_manual(values = unlist(list_col)) +
    
    coord_polar("y") 
    
    guides(color = 'none')
  
  
  
  
  
  
  
  
}




