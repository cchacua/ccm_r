#####################################################################################################
#Household modules
#####################################################################################################

#Income - expenses module
Ig_gs_hogar<-read.delim(modules[1], header=TRUE, colClasses = "character")
hogares.cali.ig<-merge(viviendas.cali[,c(1,3)], Ig_gs_hogar, by="VIVIENDA")
rm(Ig_gs_hogar)

#Labor market module
Ig_ml_hogar<-read.delim(modules[19], header=TRUE, colClasses = "character")
hogares.cali.ml<-merge(viviendas.cali[,c(1,3)], Ig_ml_hogar, by="VIVIENDA")
rm(Ig_ml_hogar)

#hogares.cali<-merge(hogares.cali.ig,hogares.cali.ml, by="CODIGO_ENIG")
#rm(hogares.cali)
