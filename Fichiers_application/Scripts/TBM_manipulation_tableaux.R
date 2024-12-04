#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Formatage des données bancaires identifié

#      IN : df_identifie[c('Date', 'libelle', 'Debit', 'Compte', 'lib_id', 'lib_nice', 'classe', 'super_classe']
#      OUT: df_resume_trimestre[c('super_classe','classe', 'trimestre', 'Debit', 'Label_Trimestre')]
#           list_col list(classe=hex_color)

# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤








#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#####                   Synthèse par Trimestres                  #####
#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #

f_resume_trimestre <- function(df_identifie){
  
  # print('str(df_identifie)') 
  # print(str(df_identifie))
  # 
  # print('summary(df_identifie)')
  # print(summary(df_identifie))
  
  
  
  
  # ~~~~{    Summarize    }~~~~
  
  # print(df_identifie)
  df_identifie2 <- filter(df_identifie, (super_classe != '' | is.na(super_classe))) %>%
    mutate(classe = str_c(super_classe, '_', classe))
  
  resume_trim <- df_identifie2 %>%
    mutate(trimestre = ceiling(as.numeric(format(Date, '%m'))/3),
           trimestre = as.Date(paste(format(Date, '%Y'),3*trimestre-1, '15', sep='-'))) %>%
    group_by(super_classe, classe, trimestre) %>%
    summarise(Debit = sum(Debit, na.rm = TRUE)) %>%
    ungroup()

  # ~~~~{    Toutes les classes représentées tous les trimestres    }~~~~
  resume_trim <- expand.grid(classe = unique(resume_trim$classe),
                             trimestre = unique(resume_trim$trimestre)) %>%
    mutate(super_classe = str_extract(classe, '^[^_]+'),
           Debit = 0) %>%
    bind_rows(resume_trim) %>%
    arrange(desc(Debit)) %>%
    distinct(classe, trimestre, .keep_all = TRUE)
  
  
  
  
  # ~~~~{    Mise en forme noms trimestres    }~~~~
  
  # '2023-11-15' => 'Oct. Nov. Dec. 2023 \n 7500 €'
  
  resume_trim <- resume_trim %>%
    group_by(trimestre) %>%
    summarize(Total = sum(Debit)) %>%
    arrange(trimestre) %>%
    mutate(Label_Trimestre = paste(format(trimestre -30, '%b'), format(trimestre , '%b'), format(trimestre +30, '%b'), format(trimestre, '%Y'), '\n', Total, '€'),
           Label_Trimestre = factor(Label_Trimestre, unique(Label_Trimestre))) %>%
    select(-Total) %>%
    right_join(resume_trim, by = 'trimestre')
  
  
  
  
  # ~~~~{    Classement des dépenses par super_classe (primaire)    }~~~~
  ordre_resume_sup <-resume_trim %>%
    group_by(super_classe) %>%
    summarize(ordre_sup = -sum(Debit, na.rm = TRUE)/sd(Debit/mean(Debit))) # les dépenses très variables sont pénalisées
  
  # ~~~~{    Classement des dépenses par classe (secondaire)    }~~~~
  
  df_resume_trimestre <-
    resume_trim %>%
    group_by(super_classe, classe) %>%
    summarize(ordre_inf = -sum(Debit, na.rm = TRUE)/sd(Debit/mean(Debit))) %>%
    ungroup() %>%
    left_join(ordre_resume_sup, by = join_by(super_classe)) %>%
    left_join(resume_trim, by = join_by(super_classe, classe)) %>%  # + Debit, trimestres
    arrange(ordre_sup, ordre_inf) %>%
    mutate(super_classe = factor(super_classe, unique(super_classe)),
           classe = str_extract(classe,'[^_]+$'), # on retire la super classe de la classe
           classe = factor(classe, unique(classe))) %>%
    select(super_classe, classe, trimestre, Debit, Label_Trimestre)
  
  # # ~~~~{    Ménage    }~~~~
  # rm(list = c('ordre_resume_sup', 'resume_trim'))
  
}


#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#####                          Couleurs                          #####
#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #


f_couleurs <- function(df_resume_trimestre){
  # ~~~~{    Assignation de couleurs aux super-classes    }~~~~
  df_sup_col <- 
    data.frame(super_classe = unique(df_resume_trimestre$super_classe),
               super_col = qualitative_hcl(length(unique(df_resume_trimestre$super_classe)), 
                                           palette = "Dark 3"))
  
  df_couleur <- df_resume_trimestre %>%
    distinct(super_classe, classe) %>%
    # distinct() %>%
    left_join(df_sup_col, by = join_by(super_classe)) 
  
  
  # ~~~~{    Variations de la couleur par "sous" classe    }~~~~
  
  col_dev <- function(col, N){
    palette <- c()
    for(i in 0.5 *(0:(N-1))/N)
      palette <- c(palette, lighten(col, i))
    
    palette
  }
  
  df_couleur$col <- NA
  # i=unique(ordre_resume_col$super_col)[1]
  for(i in unique(df_couleur$super_col))
    df_couleur[df_couleur$super_col == i, 'col'] <- col_dev(i, nrow(df_couleur[df_couleur$super_col == i,])) 
  
  df_couleur <- as.data.frame(select(df_couleur, classe, col))
  
  
  # ~~~~{    Liste des couleurs pour utilisation dans scale_._manual()    }~~~~
  
  list_col <- list()
  i=1
  for(i in 1:nrow(df_sup_col))
    list_col[[as.character(df_sup_col[i,'super_classe'])]] <- darken(df_sup_col[i,'super_col'])
  
  for(i in 1:nrow(df_couleur))
    list_col[[as.character(df_couleur[i,'classe'])]] <- df_couleur[i,'col']
  
  
  
  # # ~~~~{    test    }~~~~
  # df_resume_trimestre %>%
  #   distinct(super_classe, classe) %>%
  #   ggplot() +
  #   geom_bar(aes(x=classe, fill = classe), y=1) +
  #   geom_bar(aes(x=classe, fill = super_classe, y=0.3),stat = 'identity', color = 'black')+
  #   scale_fill_manual(values = list_col) +
  #   guides(fill='none') +
  #   theme(axis.text.x = element_text(angle = (330), hjust=0))
  
  # # ~~~~{    Ménage    }~~~~
  # rm(list = c('df_couleur', 'col_dev', 'df_sup_col'))
  
  list_col
  
}

















