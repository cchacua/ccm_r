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
# People: relevant variables
#####################################################################################################
  
  
  # Nivel Educativo del jefe del hogar