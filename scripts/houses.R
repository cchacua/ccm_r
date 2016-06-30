#####################################################################################################
#House modules
#####################################################################################################

#Income - expenses module
Ig_gs_vivienda<-read.delim(modules[2], header=TRUE, colClasses = "character")
#In the whole survey, there are 41.330 houses
#table(Ig_gs_vivienda$DOMINIO)
gs_vivienda<-Ig_gs_vivienda[Ig_gs_vivienda$DOMINIO=="CALI A.M.",]
#There are 1.402 houses in Cali
rm(Ig_gs_vivienda)

#Labor market module
Ig_ml_vivienda<-read.delim(modules[24], header=TRUE, colClasses = "character")

#A merge of both modules
#viviendas.cali<-merge(gs_vivienda, Ig_ml_vivienda, by="VIVIENDA", suffixes = c(" ",".del"))
viviendas.cali<-merge(gs_vivienda, Ig_ml_vivienda[, c("VIVIENDA", setdiff(colnames(Ig_ml_vivienda),colnames(gs_vivienda)))], by="VIVIENDA")
View(viviendas.cali)
rm(Ig_ml_vivienda, gs_vivienda)
#length(unique(viviendas.cali$VIVIENDA))
#length(unique(viviendas.cali$FACTOR_EXPANSION_EC_E1))
#write.csv2(viviendas.cali,"../outputs/viviendas.cali.csv")
#write.csv(viviendas.cali,"../outputs/viviendas.cali.db.csv")

#?write.csv
#library(xlsx)
#write.xlsx(viviendas.cali,"../outputs/viviendas.cali.xlsx")
