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
View(gsdu_gas_dia)
colnames(gsdu_gas_dia)

gsdu_gas_dia.monthly.articles<-aggregate.artquaval(gsdu_gas_dia$GDU_ARTCLO, gsdu_gas_dia$GDU_CNTDAD_ADQURDA_MES_AJST , gsdu_gas_dia$GDU_VALOR_PGDO_ESTMDO_MES_AJST,  "Product code", "Adjusted monthly quantity", "Adjusted monthly value")


#Daily food consumption by other people with income inside the household unit
Ig_gsdp_gas_dia<-read.delim(files.ig[4], header=TRUE, colClasses = "character")
gsdp_gas_dia<-merge(viviendas.cali[,c(1,3)], Ig_gsdp_gas_dia, by="VIVIENDA")
rm(Ig_gsdp_gas_dia)
gsdp_gas_dia[gsdp_gas_dia== ""] <- NA
colnames(gsdp_gas_dia)
gsdp_gas_dia.monthly.articles<-aggregate.artquaval(gsdp_gas_dia$GDP_ARTCLO, gsdp_gas_dia$GDP_CNTDAD_ADQURDA_MES_AJST, gsdp_gas_dia$GDP_VALOR_PGDO_ESTMDO_MES_AJST, "Product code", "Adjusted monthly quantity", "Adjusted monthly value")



#Extrapolated food expenses of the household unit
Ig_gsdu_gasto_alimentos_cap_c<-read.delim(files.ig[9], header=TRUE, colClasses = "character")
gsdu_gasto_alimentos_cap_c<-merge(viviendas.cali[,c(1,3)], Ig_gsdu_gasto_alimentos_cap_c, by="VIVIENDA")
rm(Ig_gsdu_gasto_alimentos_cap_c)
gsdu_gasto_alimentos_cap_c.monthly.articles<-aggregate.artquaval(gsdu_gasto_alimentos_cap_c$ARTICULO, gsdu_gasto_alimentos_cap_c$CANTIDAD,gsdu_gasto_alimentos_cap_c$VALOR_MENSUAL_ALIMENTO,"Product code", "Adjusted monthly quantity", "Adjusted monthly value")


#Extrapolated food expenses by other people with income inside the household unit