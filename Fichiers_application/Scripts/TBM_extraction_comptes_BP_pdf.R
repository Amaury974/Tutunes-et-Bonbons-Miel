#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤
# Objectif : Extraction et pré-formatage du classeur de relevé de 
#            compte Banque Postale en pdf 
# 
# A.Jorant - Nov 2024

# R version 4.4.1
# encoding UTF8
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤


# library(pdftools)
# library(stringr)
# library(dplyr)


# dir_PDF <- "./releve_CCP2006014Y038_20240812.pdf"
# dir_PDF=all_dir[37]
# dir_PDF= "D:/apis_/Documents/R/Analyse des comptes bancaire TBM/Data/Relevés/releve_CCP2006014Y038_20240112.pdf" 

extraction_BP_pdf <- function(dir_PDF){
  # print(dir_PDF)
  
  # for(dir in all_dir)
  PDF_1 <- pdf_text(dir_PDF)
  
  Annee <- str_extract(PDF_1[1], '20\\d{2}') %>% as.numeric()
  
  janvier <- str_detect(PDF_1[1], 'janvier 20\\d{2}\\n\\n') # si janvier, alors le decembre n'est pas de la bonne année
  
  
  
  
  # str_extract(PDF_1[1], '(?<=Votre identifiant . )\\d+')
  num <- str_extract(PDF_1[1], '(?<=CCP n° ).+?(?=   )')
  nom <- str_extract(PDF_1[1], '(?<=(MR )|(MME )).+')
  intitule <- paste('LBP', nom, num)
  # Annee <- str_extract(dir_PDF, '(?<=_)\\d{4}(?=\\d{4})')
  # PDF_1[2]
  
  # PDF_1 %>%
  #   # str_replace_all('\\n','_SAUT_DE_LIGNE_') %>%
  #   str_remove(regex('^[\\s\\S]+Crédit[\\s\\S]{4}')) %>%
  #   paste(collapse = '')%>%
  #   str_extract(regex('(?<=Ancien.solde.au.{0,20}\\n)[\\s\\S]+(?=Total.des.opérations)'))
  
  
  PDF_2 <- PDF_1 %>%
    str_replace_all('\\n','_SAUT_DE_LIGNE_') %>%
    str_remove(regex('^.+Crédit.{4}')) %>%
    str_remove(regex('(_SAUT_DE_LIGNE_)+\\s+Page../.(_SAUT_DE_LIGNE_)+')) %>%
    paste(collapse = '') %>%
    str_extract(regex('(?<=Ancien.solde.au).+?(?=Total.des.opérations)')) %>%
    str_extract(regex('(?<=_SAUT_DE_LIGNE_).+(?:_SAUT_DE_LIGNE_)')) %>%
    str_remove('APRES REDUCTION JEUNES 26-29 ANS DE \\d+,\\d{2}') %>%
    
    # Date dans les libelle
    # str_replace('(?<=\\d{2})/(?=\\d{2}/\\d{2})','-') %>%
    # str_replace('(?<=\\d{2}-\\d{2})/(?=\\d{2})','-') %>%
    str_split('(?=((\\s)|(_SAUT_DE_LIGNE_))\\d{2}/\\d{2}\\s)')
  # str_split('(?=\\s\\d{2}/\\d{2}\\s)')
  
  
  
  PDF_2[[1]][1] <- str_c(' ',PDF_2[[1]][1]) # aajout d'un espace devant la première ligne. Important pour l'etape suivant
  
  # PDF_3 <- sapply(PDF_2[[1]], str_split, pattern ='(?<=\\d{2}/\\d{2})') %>%
  #   as.data.frame()
  PDF_3 <- PDF_2[[1]] %>%
    str_split('(?<=((\\s)|(_SAUT_DE_LIGNE_))\\d{2}/\\d{2}\\s)') %>%
    as.data.frame() %>%
    t() %>%
    as_tibble(.name_repair = 'minimal') # nimporte, de toute façon on les force juste après. uniquement là pour cacher le message d'avis
  
  # print(PDF_3)
  
  names(PDF_3) <- c('Date', 'reste')
  
  PDF_3 <- PDF_3 %>%
    filter(str_trim(Date) != "") %>%
    as.data.frame()
  
  # print('av')
  # print(PDF_3)
  
  
  
  # print('anne')
  # print(Annee)
  
  # format date, de 23/05 à 2023-05-23
    Dates <- PDF_3$Date %>%
    str_remove_all(' ') %>%
    str_remove_all('_SAUT_DE_LIGNE_') 
    
    Dates <- paste0(Dates, '/', if_else(janvier & str_extract(Dates, '\\d{2}$') == '12', Annee-1, Annee)) %>% # problème du relevé de janvier qui comporte les lignes de fin décembre de l'année rprécedente
    as.Date(format = '%d/%m/%Y')
    
    PDF_3$Date <- Dates
  # print('ap')
  
  # print(PDF_3)
  
  # list_split <- str_split(PDF_3$reste, '_SAUT_DE_LIGNE_')
  PDF_3$Montant <- str_extract(PDF_3$reste, '(\\d+\\s)?\\d{1,3},\\d{2}') %>%
    str_remove('\\s') %>%
    as.numeric2()
  
  # PDF_3$Direction <- PDF_3$reste %>%
  #   str_extract('.+(?=\\d,\\d{2})') %>%
  #   str_count('.')
  # 
  # PDF_3$Direction <- ifelse(PDF_3$Direction > 101, 'Credit', 'Debit')
  
  PDF_3$lib <- PDF_3$reste %>%
    str_remove_all('_SAUT_DE_LIGNE_') %>%
    str_remove('\\s+\\d+,\\d{2}') %>%
    str_remove_all('\\s+(?=\\s)')
  
  PDF_3$Direction <- ifelse(str_detect(PDF_3$lib, '(VIREMENT DE)|(VIREMENT INSTANTANE DE)') , 'Credit', 'Debit')
  
  # dir_PDF=all_dir[1]
  
  PDF_3$Montant <- with(PDF_3, if_else(Direction == 'Credit', Montant, -Montant))
  
  # if(nrow(TEST)>0) warning(paste(c('\n', dir_PDF, TEST$lib), collapse = '\n'))
  
  PDF_4 <- PDF_3 %>%
    # filter(Direction == 'Debit') %>%
    select(Date, libelle = lib,  Montant) %>%
    mutate(Compte = intitule)
  
}














