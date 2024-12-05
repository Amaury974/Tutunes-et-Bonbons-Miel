#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Graphiques Camemberts et courbes empilés

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤

#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####                 Graph . BonbonMiel ggiraph              #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

# ~~~~{    Unique    }~~~~
BonbonMiel_unique_giraph <- function(df_resume_trimestre, list_col){
  print('bbm unique')
  hsize = 3
  
  df_camembert <- df_resume_trimestre %>%
    group_by(super_classe, classe) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    ungroup() %>%
    
    arrange(classe) %>%
    mutate(ylab = cumsum(Debit)-0.5*Debit,
           classe_num = as.numeric(classe),
           classe_label = paste(classe,'\n', round(Debit), '€')) %>%
    group_by(super_classe) %>%
    mutate(super_classe_label = paste(super_classe,'\n', round(sum(Debit)), '€')) %>%
    as.data.frame()
  
  df_camembert_lab <- df_camembert %>%
    group_by(super_classe) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(Debit)) %>%
    mutate(ylab = cumsum(Debit)-0.5*Debit,
           hjust_dir = 0.5-sinpi(2*ylab/total)/2,
           vjust_dir = 0.5-cospi(2*ylab/total)/2)
  
  
  
  
  myplot <-
    ggplot(df_camembert) +
    
    # bande externe : superClasse
    geom_bar_interactive(aes(x = 1.2+hsize,
                             y = Debit,
                             fill = super_classe,
                             tooltip = super_classe_label,
                             data_id = super_classe),
                         width = 0.4, stat = "identity", position = position_stack(reverse = TRUE)) +
    
    # bande interne : classe
    geom_bar_interactive(aes(x = hsize, 
                             y = Debit,
                             fill = classe,
                             tooltip=classe_label, 
                             data_id = classe),
                         width = 2, stat = "identity", position = position_stack(reverse = TRUE)) +
    
    scale_fill_manual(values = list_col) +
    
    geom_text(data = df_camembert_lab, 
              aes(y = ylab, label = super_classe, hjust = hjust_dir, vjust = vjust_dir), 
              x = 1.6 + hsize) +
    
    
    guides(fill = 'none') +
    coord_polar("y", start=0, clip = 'off') +
    xlim(c(0, 1.5+hsize)) +
    theme_void()
  
  
  
  
  interactive_plot<-girafe(ggobj = myplot,
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

# ~~~~{    Par trimestre    }~~~~
BonbonMiel_trimestriel_giraph <- function(df_resume_trimestre, list_col){
  hsize = 3
  
  df_camembert_trim <- df_resume_trimestre %>%
    group_by(super_classe, classe, Label_Trimestre, trimestre) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    
    group_by(Label_Trimestre) %>%
    arrange(classe) %>%
    mutate(classe_label = paste(classe,'\n', round(Debit), '€'),
           classe_trimestre = paste0(classe,'/',trimestre)) %>%
    
    group_by(super_classe, trimestre) %>%
    mutate(super_classe_label = paste(super_classe,'\n', round(sum(Debit)), '€'),
           super_classe_trimestre = paste0(super_classe,'/',trimestre)) %>%
    mutate(an = str_extract(Label_Trimestre, '\\d{4}'),
           trimestre_an = str_trim(str_extract(Label_Trimestre, '^\\D+')),
           trimestre_an = factor(trimestre_an, c('janv. févr. mars', 'avr. mai juin', 'juil. août sept.', 'oct. nov. déc.'))) %>%
    as.data.frame()
  
  df_camembert_lab <- df_camembert_trim %>%
    group_by(super_classe, Label_Trimestre) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    group_by(Label_Trimestre) %>%
    mutate(total_trim = sum(Debit)) %>%
    mutate(ylab = cumsum(Debit)-0.5*Debit) %>%
    # décallage en fonction de la position dans le cercle
    ungroup() %>%
    mutate(max_total = max(total_trim),
           hjust_dir = 0.5-sinpi(2*ylab/max_total)/2,
           vjust_dir = 0.5-cospi(2*ylab/max_total)/2,
           super_classe = if_else(Debit < 0.02*max_total, '', super_classe)) %>% # retrait des trop petites classes pour eviter les chevauchements de label
    mutate(an = str_extract(Label_Trimestre, '\\d{4}'),
           trimestre_an = str_trim(str_extract(Label_Trimestre, '^\\D+')),
           trimestre_an = factor(trimestre_an, c('janv. févr. mars', 'avr. mai juin', 'juil. août sept.', 'oct. nov. déc.')))
  
  myplot <-
    ggplot(df_camembert_trim) +
    
    # bande externe : superClasse
    geom_bar_interactive(aes(x       = 1.2 + hsize,
                             y       = Debit,
                             fill    = super_classe,
                             tooltip = super_classe_label,
                             data_id = super_classe_trimestre),
                         width   = 0.4, 
                         stat    = "identity", 
                         position = position_stack(reverse = TRUE)) +
    
    # bande interne : classe
    geom_bar_interactive(aes(x       = hsize, 
                             y       = Debit,
                             fill    = classe,
                             tooltip = classe_label, 
                             data_id = classe_trimestre),
                         width    = 2, 
                         stat     = "identity", 
                         position = position_stack(reverse = TRUE)) +
    
    scale_fill_manual(values = list_col) +
    
    geom_text(data = df_camembert_lab, 
              aes(y     = ylab, 
                  label = super_classe, 
                  hjust = hjust_dir, 
                  vjust = vjust_dir), 
              x    = 1.6 + hsize) +
    
    geom_text(aes(label = str_extract(Label_Trimestre, '\\d+ €$')),
              x = 0, y = 0)+
    
    guides(fill = 'none') +
    facet_grid(an~trimestre_an) + # facet_grid(.~Label_Trimestre) 
    coord_polar("y", start=0, clip = 'off') +
    xlim(c(0, 1.5+hsize)) +
    theme_void() + 
    theme(strip.text.y = element_text( angle = 270, vjust = 1),
          strip.text.x = element_text( vjust = 1))
  # size = 12,
  
  
  
  interactive_plot <- girafe(ggobj      = myplot,
                             height_svg = 2*length(unique(df_camembert_trim$an)),
                             width_svg  = 10,
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



#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####                   Graph . histogramme                   #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

# ~~~~{    Par trimestre    }~~~~

histogramme_trimestriel_giraph <- function(df_resume_trimestre, list_col){
  hsize = 3
  
  df_camembert_trim <- df_resume_trimestre %>%
    group_by(super_classe, classe, Label_Trimestre, trimestre) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    
    group_by(Label_Trimestre) %>%
    arrange(classe) %>%
    mutate(classe_label = paste(classe,'\n', round(Debit), '€'),
           classe_trimestre = paste0(classe,'/',trimestre)) %>%
    
    group_by(super_classe, trimestre) %>%
    mutate(super_classe_label = paste(super_classe,'\n', round(sum(Debit)), '€'),
           super_classe_trimestre = paste0(super_classe,'/',trimestre)) %>%
    mutate(an = str_extract(Label_Trimestre, '\\d{4}'),
           trimestre_an = str_trim(str_extract(Label_Trimestre, '^\\D+')),
           trimestre_an = factor(trimestre_an, c('janv. févr. mars', 'avr. mai juin', 'juil. août sept.', 'oct. nov. déc.')) ) %>%
    as.data.frame()
  
  
  myplot <-
    ggplot(df_camembert_trim) +
    geom_bar_interactive(aes(x = classe, 
                             y = Debit,
                             fill = classe,
                             tooltip=classe_label, 
                             data_id = classe_trimestre),
                         stat = "identity") +
    
    scale_fill_manual(values = list_col) +
    
    guides(fill = 'none') +
    facet_grid(an~trimestre_an) + # facet_grid(.~Label_Trimestre) 
    theme(axis.text.x = element_text( angle = 45, hjust = 1),
          strip.text.y = element_text(size = 12, angle = 270, vjust = 1),
          strip.text.x = element_text(size = 12, vjust = 1))
  
  interactive_plot <- girafe(ggobj = myplot,
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


# ~~~~{    Un seul graphique, mêmes classes côte à côte    }~~~~
histogramme_classe_giraph <- function(df_resume_trimestre, list_col){
  hsize = 3
  # df_camembert_trim[1,"Label_Trimestre"]
  
  
  df_camembert_trim <- df_resume_trimestre %>%
    arrange(classe) %>%
    mutate(Label_Trimestre = str_c(format(trimestre, '%Y'),' T',round(as.numeric(format(trimestre, '%m'))/3)),
           sC_C = str_c(super_classe, '_', classe),
           sC_C = factor(sC_C, unique(sC_C))) %>%
    group_by(super_classe, classe, sC_C, Label_Trimestre, trimestre) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    
    group_by(Label_Trimestre) %>%
    arrange(sC_C) %>%
    mutate(classe_label = paste(Label_Trimestre, sC_C,'\n', round(Debit), '€'),
           classe_trimestre = paste0(classe,'/',trimestre)) %>%
    
    group_by(super_classe, trimestre) %>%
    mutate(super_classe_label = paste(super_classe,'\n', round(sum(Debit)), '€'),
           super_classe_trimestre = paste0(super_classe,'/',trimestre)) %>%
    mutate(an = str_extract(Label_Trimestre, '\\d{4}'),
           trimestre_an = str_trim(str_extract(Label_Trimestre, '^\\D+')),
           trimestre_an = factor(trimestre_an, c('janv. févr. mars', 'avr. mai juin', 'juil. août sept.', 'oct. nov. déc.')) ) %>%
    as.data.frame()
  
  
  myplot <-
    ggplot(df_camembert_trim) +
    geom_bar_interactive(aes(x = sC_C, 
                             y = Debit,
                             fill = Label_Trimestre,
                             tooltip=classe_label, 
                             data_id = classe_trimestre),
                         stat = "identity", position = position_dodge()) +
    labs(fill = 'Trimestre', y='Débit (€)') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank())
  
  interactive_plot <- girafe(ggobj = myplot,
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


#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####            Graph . evolution des dépense                #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

Courbe_empile <- function(df_resume_trimestre, list_col){
  
  df_courbe_empile <- df_resume_trimestre %>%
    group_by(super_classe, classe, trimestre) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE))
  
  
  ggplot(df_courbe_empile, aes(x=trimestre, y = Debit, fill = classe)) +
    geom_area(position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = list_col) +
    guides(fill = guide_legend(reverse=T))
  
  
}


# ~~~~{    GGirafe    }~~~~

Courbe_empile_giraph <- function(df_resume_trimestre, list_col){
  
  
  fact_SC <- c(levels(df_resume_trimestre$super_classe), '_NA')
  fact_C <- c(levels(df_resume_trimestre$classe), '_NA')
  
  df_courbe_empile <- as.data.frame(df_resume_trimestre)
  df_courbe_empile[,c('classe','super_classe')] <- apply(df_courbe_empile[,c('classe','super_classe')], 2, as.character)
  df_courbe_empile[is.na(df_courbe_empile$classe), c('classe','super_classe')] <- '_NA'
  
  df_courbe_empile <- df_courbe_empile %>%
    mutate(super_classe = factor(super_classe, fact_SC),
           classe = factor(classe, fact_C)) %>%
    group_by(super_classe, classe, trimestre) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    group_by(trimestre) %>%
    arrange(super_classe, classe) %>%
    mutate(YMAX = cumsum(Debit),
           YMIN = YMAX - Debit)  %>%
    as_tibble()
  
  list_col$'_NA' <- 'grey45'
  
  
  myplot <-
    ggplot(df_courbe_empile, aes(x=trimestre,  fill = classe,
                                 tooltip=classe, data_id = classe)) +
    # geom_area_interactive(position = position_stack(reverse = TRUE)) +
    geom_ribbon_interactive(aes(ymin = YMIN, ymax = YMAX)) +
    
    scale_fill_manual(values = list_col) +
    guides(fill = 'none')
  
  
  interactive_plot<-girafe(ggobj = myplot,
                           options = list(
                             opts_hover(css = "filter: brightness(95%)"),
                             opts_hover_inv(css = "opacity:0.4;")
                           )
  )
  interactive_plot
  
  # htmltools::save_html(interactive_plot, "Courbe_empile.html")
}


