#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Divers fonctions utilitaires
# 
# A.Jorant - Avril 2021

# R version 4.0.3
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤



loca_dossier <- function(Dossiers, marqueur, Stop = "\\*"){
  
  # Dossiers <- Dossiers[!grepl(Stop, Dossiers)]
  Dossiers <- Dossiers[!str_detect(Dossiers, Stop)]
  
  # Dossiers_valide <- Dossiers[grepl(marqueur, Dossiers, perl = TRUE)]
  Dossiers_valide <- Dossiers[str_detect(Dossiers, marqueur)]
  
  if(!length(Dossiers_valide) & length(Dossiers)){
    # element =Dossiers[1]
    for (element in Dossiers){
      Dossiers_E <- paste0(element,'/', list.files(element),recycle0 = TRUE)
      
      Dossiers_valide <- c(Dossiers_valide, loca_dossier(Dossiers_E, marqueur, Stop))
    }
  }
  
  Dossiers_valide
}

as.numeric2 <- function(X){
  as.numeric(gsub(',', '.', X))
}



deb.Trimestre <- function(Date = Sys.Date(), fin = FALSE) {
  sub <- ifelse(fin, 0, 2)
  as.Date(paste(format(Date, '%Y'),3*ceiling(as.numeric(format(Date, '%m'))/3)-sub, '15', sep='-'))
}


#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
#####             Determine la période pour groupage             #####
#  ¤¤¤¤¤¤¤¤¤¤                     ¤¤                     ¤¤¤¤¤¤¤¤¤¤  #
periodifier <- function(Date, echelle = c('Mois', 'Trimestre', 'Semestre', 'An')[2], format = c('Court', 'Date', 'Long')[1]){
  
  # ~~~~{    Par mois    }~~~~
  if(echelle == 'Mois'){
    if(format == 'Court') return(str_c(format(Date, '%Y'),' ', 'M', format(Date, '%m')))
    
    if(format == 'Date') return(as.Date(str_c(format(Date, '%Y-%m-'), '15')))
    
    if(format == 'Long') return(str_c(format(Date, '%B'), format(Date, '%Y'), sep = ' '))
  }
  
  # ~~~~{    Par Trimestre    }~~~~
  if(echelle == 'Trimestre'){
    N_Trim <- ceiling(as.numeric(format(Date, '%m'))/3)
    if(format == 'Court') return(str_c(format(Date, '%Y'),' ','T', N_Trim ))
    
    Date_centrale_trimestre <- as.Date(str_c(format(Date, '%Y'),3*N_Trim-1, '15', sep='-'))
    if(format == 'Date') return(Date_centrale_trimestre)
    
    Mois <- str_c(format(Date_centrale_trimestre -30, '%b'), 
                  format(Date_centrale_trimestre , '%b'), 
                  format(Date_centrale_trimestre +30, '%b'), 
                  sep = ' ')
    if(format == 'Long') return(str_c(Mois, format(Date, '%Y'), sep = ' '))
  }
  
  # ~~~~{    Par semestre    }~~~~
  if(echelle == 'Semestre'){
    N_Sem <- ceiling(as.numeric(format(Date, '%m'))/6)
    if(format == 'Court') return(str_c(format(Date, '%Y'),' ','S', N_Sem))
    
    Date_centrale_semestre <- as.Date(str_c(format(Date, '%Y'),6*N_Sem-2, '01', sep='-'))
    if(format == 'Date') return(Date_centrale_semestre)
    
    if(format == 'Long') return(str_c(case_match(N_Sem, 1~'janvier à juin', 2~'juiller à décembre'), format(Date, '%Y'), sep = ' '))
  }
  # ~~~~{    Par an    }~~~~
  if(echelle == 'An'){
    if(format == 'Court') return(format(Date, '%Y'))
    
    if(format == 'Date') return(as.Date(str_c(format(Date, str_c('%Y', '-07-01')))))
    
    if(format == 'Long') return(format(Date, '%Y'))
  }
  
  NA
}


# for(i in c('Mois', 'Trimestre', 'Semestre', 'An'))
#   for(j in c('Court', 'Date', 'Long'))
#     print(periodifier(Date = Sys.Date(), echelle=i, format=j))

# CentrePeriode = periodifier(Date = Sys.Date(), echelle = 'Semestre', format = 'Date')
# 
# f_encadrement(CentrePeriode,'Semestre')

de_periodifier <- function(CentrePeriode, echelle = c('Mois', 'Trimestre', 'Semestre', 'An')[2]){
  
  deb <- fin <- NA
  
  # ~~~~{    Par mois    }~~~~
  if(echelle == 'Mois'){
    deb <- as.Date(str_c(format(CentrePeriode, '%Y-%m-'), 01))
    fin <- as.Date(str_c(format(CentrePeriode+30, '%Y-%m-'), 01))-1
  }
  
  # ~~~~{    Par Trimestre    }~~~~
  if(echelle == 'Trimestre'){
    deb <- as.Date(paste0(format(CentrePeriode - 30, '%Y-%m-'), 01))
    fin <- as.Date(paste0(format(CentrePeriode + 60, '%Y-%m-'), 01))-1
  }
  
  # ~~~~{    Par semestre    }~~~~
  if(echelle == 'Semestre'){
    deb <- as.Date(paste0(format(CentrePeriode - 90, '%Y-%m-'), 01))
    fin <- as.Date(paste0(format(CentrePeriode + 120, '%Y-%m-'), 01))-1
  }
  
  # ~~~~{    Par an    }~~~~
  if(echelle == 'An'){
    deb <- as.Date(paste0(format(CentrePeriode , '%Y-'), '01-01'))
    fin <- as.Date(paste0(format(CentrePeriode, '%Y-'), '12-31'))-1
  }
  
  list(deb = deb, fin = fin)
  
}





quel_periode <- function(label){
  if(str_detect(label[[1]], 'à')) return('Semestre')
  if(str_detect(label[[1]], '\\.')) return('Trimestre')
  if(str_detect(label[[1]], '^\\d')) return('An')
  if(str_detect(label[[1]], 'à')) return('Mois')
  NA
}






format_plotmath <- function(Date){
  
  j <- format(Date, '%d') 
  m <- format(Date, '%m') 
  str_c('paste(',str_extract(j, '.'), ',', str_extract(j, '.$'), '/', str_extract(m, '.'), ',', str_extract(m, '.$'),')')
  
}







