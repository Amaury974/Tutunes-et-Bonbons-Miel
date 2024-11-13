#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Graphiques Camemberts et courbes empilés

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤



# #  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
# #####                    Graph . Camembert                    #####
# #  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
# 
# 
# # ~~~~{    Unique    }~~~~
# BonbonMiel_unique <- function(df_resume_trimestre, list_col){
#   
#   df_camembert <- df_resume_trimestre %>%
#     group_by(classe) %>%
#     summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
#     ungroup() %>%
#     
#     arrange(classe) %>%
#     mutate(ylab = cumsum(Debit)-0.5*Debit,
#            classe_num = as.numeric(classe)) %>%
#     as.data.frame()
#   
#   hsize = 2
#   
# ggplot(df_camembert, aes(x=hsize, y=Debit, fill=classe)) +
#   geom_bar(width = 1, stat = "identity", position = position_stack(reverse = TRUE)) +
#   scale_fill_manual(values = list_col) +
# 
#   ggrepel::geom_text_repel(aes(y = ylab,
#                                label = paste(str_extract(classe,'[^_]+$'),'\n', round(Debit), '€')),
#                            point.size = NA,
#                            min.segment.length = 0.2,
#                            max.overlaps = 15,
#                            # xlim = c(1.5,NA),
#   ) +
#   guides(fill = 'none') +
#   coord_polar("y", start=0) +
#   xlim(c(0.2, hsize + 0.5)) +
#   theme_void()
# 
# }
# 
# 
# 
# # ~~~~{    Par trimestre    }~~~~
# BonbonMiel_trimestriel <- function(df_resume_trimestre, list_col){
#   
#   df_camembert_trim <- df_resume_trimestre %>%
#     group_by(classe, Label_Trimestre) %>%
#     summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
#     group_by(Label_Trimestre) %>%
#     arrange(classe) %>%
#     mutate(ylab = cumsum(Debit)-0.5*Debit,
#            classe_num = as.numeric(classe),) %>%
#     as.data.frame()
#   
#   hsize = 2
#   
#   ggplot(df_camembert_trim, aes(x=hsize, y=Debit, fill=classe)) +
#     
#     geom_bar(width = 1, stat = "identity", position = position_stack(reverse = TRUE)) +
#     scale_fill_manual(values = list_col) +
#     
#     ggrepel::geom_text_repel(aes(y = ylab,
#                                  label = paste(str_extract(classe,'[^_]+$'),'\n', round(Debit), '€')),
#                              point.size = NA,
#                              # x = 1.3,
#                              min.segment.length = 0.2,
#                              max.overlaps = 15,
#                              xlim = c(1.5,NA)) +
#     
#     guides(fill = 'none') +
#     facet_grid(.~Label_Trimestre) +
#     coord_polar("y", start=0) +
#     xlim(c(0.2, hsize + 0.5)) +
#     # theme_minimal()
#     theme_void()
#   
#   
#   
#   
# }


#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #
#####                 Graph . BonbonMiel ggiraph              #####
#  ¤¤¤¤¤¤¤¤¤¤                   ¤¤                    ¤¤¤¤¤¤¤¤¤¤  #

# ~~~~{    Unique    }~~~~
BonbonMiel_unique_giraph <- function(df_resume_trimestre, list_col){
  
  df_camembert <- df_resume_trimestre %>%
    group_by(classe) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    ungroup() %>%
    
    arrange(classe) %>%
    mutate(ylab = cumsum(Debit)-0.5*Debit,
           classe_num = as.numeric(classe),
           classe_label = paste(str_extract(classe,'[^_]+$'),'\n', round(Debit), '€')) %>%
    as.data.frame()
  
  hsize = 2
  
  myplot <- ggplot(df_camembert,
                   aes(x=hsize, y=Debit, fill=classe,
                       tooltip=classe_label, data_id = classe)) +
    geom_bar_interactive(#aes(onclick = glue::glue('
      # Shiny.setInputValue("last_click", " ");
      # Shiny.setInputValue("last_click", "{classe}");')),
      width = 1, stat = "identity", position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = list_col) +
    
    # ggrepel::geom_text_repel(aes(y = ylab,
    #                              label = paste(str_extract(classe,'[^_]+$'),'\n', round(Debit), '€')),
    #                          point.size = NA,
    #                          min.segment.length = 0.2,
    #                          max.overlaps = 15,
    #                          # xlim = c(1.5,NA),
    # ) +
    guides(fill = 'none') +
    coord_polar("y", start=0) +
    xlim(c(0.2, hsize + 0.5)) +
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
  
  df_camembert_trim <- df_resume_trimestre %>%
    group_by(classe, Label_Trimestre, trimestre) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    group_by(Label_Trimestre) %>%
    arrange(classe) %>%
    mutate(ylab = cumsum(Debit)-0.5*Debit,
           # classe_num = as.numeric(classe),
           classe_label = paste(str_extract(classe,'[^_]+$'),'\n', round(Debit), '€'),
           classe_trimestre = paste0(classe,'/',trimestre)) %>%
    as.data.frame()
  
  hsize = 2
  
  myplot <- ggplot(df_camembert_trim, 
                   aes(x=hsize, y=Debit, fill=classe, 
                       tooltip=classe_label, data_id = classe_trimestre)) +
    
    geom_bar_interactive(width = 1, stat = "identity", position = position_stack(reverse = TRUE)) +
    scale_fill_manual(values = list_col) +
    
    # ggrepel::geom_text_repel(aes(y = ylab,
    #                              label = paste(str_extract(classe,'[^_]+$'),'\n', round(Debit), '€')),
    #                          point.size = NA,
    #                          # x = 1.3,
    #                          min.segment.length = 0.2,
    #                          max.overlaps = 15,
    #                          xlim = c(1.5,NA)) +
    
    guides(fill = 'none') +
    facet_grid(.~Label_Trimestre) +
    coord_polar("y", start=0) +
    xlim(c(0.2, hsize + 0.5)) +
    # theme_minimal()
    theme_void()
  
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

# # ~~~~{    GGirafe    }~~~~
# 
# Courbe_empile_giraph <- function(df_resume_trimestre, list_col){
#   
#   df_courbe_empile <- df_resume_trimestre %>%
#     group_by(super_classe, classe, trimestre) %>%
#     summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
#     group_by(trimestre) %>%
#     arrange(super_classe, classe) %>%
#     mutate(YMAX = cumsum(Debit),
#            YMIN = YMAX - Debit)  %>%
#     
#     # filter(!is.na(classe))
#     # %>%
#     
#     # dunno why on a besoin de tout ça ...
#     arrange(desc(super_classe), desc(classe)) %>%
#     mutate(super_classe = factor(super_classe, unique(super_classe)),
#            classe = factor(classe, unique(classe)),
#            # classe_label = paste(str_extract(classe,'[^_]+$'),'\n', round(Debit), '€')
#     ) %>%
#     filter(!is.na(classe))
#   
#   
#   # y = Debit,
#   myplot <-
#     ggplot(df_courbe_empile, aes(x=trimestre,  fill = classe,
#                                  tooltip=classe, data_id = classe)) +
#     # geom_area_interactive(position = position_stack(reverse = TRUE)) +
#     geom_ribbon_interactive(aes(ymin = YMIN, ymax = YMAX)) +
#     
#     scale_fill_manual(values = list_col) +
#     guides(fill = 'none')
#   
#   
#   interactive_plot<-girafe(ggobj = myplot,
#                            options = list(
#                              opts_hover(css = "filter: brightness(95%)"),
#                              opts_hover_inv(css = "opacity:0.4;")
#                            )
#   )
#   interactive_plot
#   
#   # htmltools::save_html(interactive_plot, "Courbe_empile.html")
# }


