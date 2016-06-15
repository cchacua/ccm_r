#####################################################################################################
#Household modules
#####################################################################################################


#Income - expenses module
Ig_gs_hogar<-read.delim(files.ig[1], header=TRUE, colClasses = "character")
hogares.cali.ig<-merge(viviendas.cali[,c(1,3)], Ig_gs_hogar, by="VIVIENDA")
rm(Ig_gs_hogar)

#Labor market module
Ig_ml_hogar<-read.delim(files.ig[19], header=TRUE, colClasses = "character")
hogares.cali.ml<-merge(viviendas.cali[,c(1,3)], Ig_ml_hogar, by="VIVIENDA")
rm(Ig_ml_hogar)


#Food expenses
#Daily food consumption by household unit
Ig_gsdu_gas_dia<-read.delim(files.ig[8], header=TRUE, colClasses = "character")
gsdu_gas_dia<-merge(viviendas.cali[,c(1,3)], Ig_gsdu_gas_dia, by="VIVIENDA")
rm(Ig_gsdu_gas_dia)
gsdu_gas_dia[gsdu_gas_dia== ""] <- NA
colnames(gsdu_gas_dia)

valor.articulo<-data.frame(gsdu_gas_dia$GDU_VALOR_PGDO_ESTMDO_MES_AJST,gsdu_gas_dia$GDU_ARTCLO)
valor.articulo<-na.omit(valor.articulo)
aggregate(as.numeric(as.character(valor.articulo$gsdu_gas_dia.GDU_VALOR_PGDO_ESTMDO_MES)), by=list(Category=valor.articulo$gsdu_gas_dia.GDU_ARTCLO), FUN=sum)

gsdu_gas_dia.articles<-aggregate(as.numeric(as.character(gsdu_gas_dia$GDU_VALOR_PGDO_ESTMDO_MES_AJST)), by=list(Category=gsdu_gas_dia$GDU_ARTCLO), FUN=sum)
item.01120202<-gsdu_gas_dia[gsdu_gas_dia$GDU_ARTCLO=="01110301",]

#Daily food consumption by other people with income inside the household unit
Ig_gsdp_gas_dia<-read.delim(files.ig[4], header=TRUE, colClasses = "character")
gsdp_gas_dia<-merge(viviendas.cali[,c(1,3)], Ig_gsdp_gas_dia, by="VIVIENDA")
rm(Ig_gsdp_gas_dia)
gsdp_gas_dia[gsdp_gas_dia== ""] <- NA
colnames(gsdp_gas_dia)

#Extrapolated food expenses
Ig_gsdu_gasto_alimentos_cap_c<-read.delim(files.ig[9], header=TRUE, colClasses = "character")
