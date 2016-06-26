#############################################################################################
# 0. Loading Labor market modules
####

#Information about people inside each household
Ig_ml_persona<-read.delim(modules[23], header=TRUE, colClasses = "character")
ml_persona<-merge(viviendas.cali[,c(1,3)], Ig_ml_persona, by="VIVIENDA")
rm(Ig_ml_persona)
ml_persona[ml_persona== ""] <- NA
ml_persona[ml_persona== 98] <- NA
ml_persona[ml_persona== 99] <- NA
#write.csv(ml_persona, "../outputs/ml_persona_Cali.csv")

#Information about working-age population
Ig_ml_pblcion_edad_trbjar<-read.delim(modules[22], header=TRUE, colClasses = "character")
ml_pblcion_edad_trbjar<-merge(viviendas.cali[,c(1,3)], Ig_ml_pblcion_edad_trbjar, by="VIVIENDA")
rm(Ig_ml_pblcion_edad_trbjar)
#write.csv(ml_pblcion_edad_trbjar, "../outputs/ml_pblcion_edad_trbjar_Cali.csv")

#Information about employed population 
Ig_ml_ocupado<-read.delim(modules[21], header=TRUE, colClasses = "character")
ml_ocupado<-merge(viviendas.cali[,c(1,3)], Ig_ml_ocupado, by="VIVIENDA")
rm(Ig_ml_ocupado)
ml_ocupado[ml_ocupado== ""] <- NA
ml_ocupado[ml_ocupado== 98] <- NA
ml_ocupado[ml_ocupado== 99] <- NA
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

#############################################################################################


#############################################################################################
# 1. Estimating total income of the expenditure unit
####

##############
# 1.1. Nominal monetary income of the expenditure unit
###

#1.1.1 Nominal monetary income of the people inside the expenditure unit
source("./scripts/income.1.1.1.R")


##############

##############
# 1.2. Nominal non-monetary income of the expenditure unit
###
##############

##############
# 1.3. Occasional monetary income of the expenditure unit, when used
###
##############

#############################################################################################



