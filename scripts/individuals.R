#####################################################################################################
# Individual modules
#####################################################################################################

library(datasets)
library(dplyr)
library(xlsx)
library(reshape)

#####################################################################################################
# Loading data
#####################################################################################################

  #Information about people inside each household
  Ig_ml_persona<-read.delim(modules[23], header=TRUE, colClasses = "character")
  ml_persona<-merge(viviendas.cali[,c(1,3)], Ig_ml_persona, by="VIVIENDA")
  rm(Ig_ml_persona)
  ml_persona[ml_persona== ""] <- NA
  ml_persona[ml_persona== 98] <- NA
  ml_persona[ml_persona== 99] <- NA
  ml_persona$HOUSEID<-substr(ml_persona$CODIGO_ENIG, 1, 7)
  #write.csv(ml_persona, "../outputs/ml_persona_Cali.csv")
  
  #Information about working-age population
  Ig_ml_pblcion_edad_trbjar<-read.delim(modules[22], header=TRUE, colClasses = "character")
  ml_pblcion_edad_trbjar<-merge(viviendas.cali[,c(1,3)], Ig_ml_pblcion_edad_trbjar, by="VIVIENDA")
  rm(Ig_ml_pblcion_edad_trbjar)
  ml_pblcion_edad_trbjar$HOUSEID<-substr(ml_pblcion_edad_trbjar$CODIGO_ENIG , 1, 7)
  #write.csv(ml_pblcion_edad_trbjar, "../outputs/ml_pblcion_edad_trbjar_Cali.csv")
  
  #Information about employed population 
  Ig_ml_ocupado<-read.delim(modules[21], header=TRUE, colClasses = "character")
  ml_ocupado<-merge(viviendas.cali[,c(1,3)], Ig_ml_ocupado, by="VIVIENDA")
  rm(Ig_ml_ocupado)
  ml_ocupado[ml_ocupado== ""] <- NA
  ml_ocupado[ml_ocupado== 98] <- NA
  ml_ocupado[ml_ocupado== 99] <- NA
  ml_ocupado$HOUSEID<-substr(ml_ocupado$CODIGO_ENIG , 1, 7)
  #write.csv(ml_ocupado, "../outputs/ml_ocupado_Cali.csv")
  
  #Information about unemployed population 
  Ig_ml_desocupado<-read.delim(modules[18], header=TRUE, colClasses = "character")
  ml_desocupado<-merge(viviendas.cali[,c(1,3)], Ig_ml_desocupado, by="VIVIENDA")
  rm(Ig_ml_desocupado)
  #write.csv(ml_desocupado, "../outputs/ml_desocupado_Cali.csv")
  
  
  #Information about inactive population
  Ig_ml_inactivo<-read.delim(modules[20], header=TRUE, colClasses = "character")
  ml_inactivo<-merge(viviendas.cali[,c(1,3)], Ig_ml_inactivo, by="VIVIENDA")
  rm(Ig_ml_inactivo)
  #write.csv(ml_inactivo, "../outputs/ml_inactivo_Cali.csv")

#####################################################################################################
# People countings
#####################################################################################################
  
  #####
  # Total number of people inside each household
  #####
  people.byhousehold<- count(ml_persona, HOUSEID)
  colnames(people.byhousehold)<-c("HOUSEID", "peoplebyhousehold")
  
  #####
  # Number working age people inside each household
  ##### 
  people.workingage<-count(ml_pblcion_edad_trbjar, HOUSEID)
  colnames(people.workingage)<-c("HOUSEID", "peopleworkingage")
  
  #####
  # Number of employed people inside each household
  #####
  people.employed<-count(ml_ocupado, HOUSEID)
  colnames(people.employed)<-c("HOUSEID", "peopleemployed")
  
  #####
  # Dataframe number of people
  #####
  people<-merge(people.byhousehold, people.workingage, by="HOUSEID")
  people<-merge(people, people.employed, by="HOUSEID")
  View(people)
  
#####################################################################################################
# Head of household database
#####################################################################################################
  
  ml_persona.headofhousehold<-ml_persona[ml_persona$P6050== "1",] 


#####################################################################################################
# People: relevant variables
#####################################################################################################
  
  individuals<-ml_persona.headofhousehold[,c("HOUSEID","P6080","P6060","P6220")]
  individuals<-merge(individuals, people, by="HOUSEID")
  # P6080	¿De acuerdo con su cultura, pueblo, o rasgos físicos, ... es o se reconoce como:
    #   individuals$P6080<-recode(individuals$P6080,
    #                             "1"= "Indígena"
    #                             "2"= "Gitano - Rom",
    #                             "3"= "Raizal del Archipiélago de San Andrés y Providencia?",
    #                             "4"= "Palenquero de San Basilio o descendiente?",
    #                             "5"= "Negro(a), mulato(a), afrocolombiano o afrodescendiente?",
    #                             "6"= "Ninguno de los anteriores (mestizo, blanco, etc.)"
    #                             )
  individuals$P6080<-recode(individuals$P6080,
                            "1" = "Indigenous",
                            "2" = "Gipsy - Rom",
                            "3" = "Raizal from the Archipelago of San Andres and Providencia?",
                            "4" = "Palenquero from St. Basil or descendant of them?",
                            "5" = "Black (a), Mulatto (a), Afro-Colombian or African descent?",
                            "6" = "None of the above (white, mestizo, etc.)")
  # P6060	¿Por FALTA de dinero, ... no consumió NINGUNA de las tres COMIDAS BASICAS O PRINCIPALES (desayuno, almuerzo, comida), uno o más días de la semana pasada?	
  individuals$P6060<-recode(individuals$P6060,"1" = "Yes","2" = "No")
  
  # P6220	¿Cuál es el título o diploma de mayor nivel educativo que usted ha recibido?
    #   individuals$P6220<-recode(individuals$P6220,
    #                             "1" ="Ninguno",
    #                             "2"= "Bachiller",
    #                             "3"= "Técnico o tecnológico",
    #                             "4"= "Universitario",
    #                             "5"= "Postgrado",
    #                             "6"= "No sabe, no informa"
    #                             )
  individuals$P6220<-recode(individuals$P6220,
                            "1" = "None",
                            "2" = "High-school",
                            "3" = "Technical or technological",
                            "4" = "Bachellor's/Professional degree",
                            "5" = "Posgraduate",
                            "6" = "Do not know, do not report")
  individuals[individuals== ""] <- NA
  colnames(individuals)<-c("HOUSEID",
                           "According to the head of household culture, community or physical features, he/she is or can be identified as:",
                           "Because of lack of money, the head of household did not consume any of the three BASIC OR MAIN MEAL (breakfast, lunch, dinner), one or more days during last week?",
                           "What is the degree or diploma of higher education the head of household have received?",
                           "Number of people inside the household",
                           "Number working age people inside each household",
                           "Number of employed people inside each household")
  