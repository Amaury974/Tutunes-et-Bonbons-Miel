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























