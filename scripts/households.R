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

#############################################################################################
#Food expenses
#############################################################################################
#Daily food consumption by household unit
Ig_gsdu_gas_dia<-read.delim(modules[8], header=TRUE, colClasses = "character")
gsdu_gas_dia<-merge(viviendas.cali[,c(1,3)], Ig_gsdu_gas_dia, by="VIVIENDA")
rm(Ig_gsdu_gas_dia)
gsdu_gas_dia[gsdu_gas_dia== ""] <- NA
#View(gsdu_gas_dia)
colnames(gsdu_gas_dia)
gsdu_gas_dia$class<-substr(gsdu_gas_dia$GDU_ARTCLO, 1, 2)
unique(gsdu_gas_dia$GDU_ARTCLO)
cod.dos.du<-gsdu_gas_dia[gsdu_gas_dia$class=="02",]
unique(cod.dos.du$GDU_ARTCLO)
gsdu_gas_dia.monthly.articles<-aggregate.artquaval(gsdu_gas_dia$GDU_ARTCLO, gsdu_gas_dia$GDU_CNTDAD_ADQURDA_MES_AJST , gsdu_gas_dia$GDU_VALOR_PGDO_ESTMDO_MES_AJST,  "Product code", "Adjusted monthly quantity", "Adjusted monthly value")


#Daily food consumption by other people with income inside the household unit
Ig_gsdp_gas_dia<-read.delim(modules[4], header=TRUE, colClasses = "character")
gsdp_gas_dia<-merge(viviendas.cali[,c(1,3)], Ig_gsdp_gas_dia, by="VIVIENDA")
unique(gsdp_gas_dia$GDP_ARTCLO)
rm(Ig_gsdp_gas_dia)
gsdp_gas_dia[gsdp_gas_dia== ""] <- NA
colnames(gsdp_gas_dia)
gsdp_gas_dia.monthly.articles<-aggregate.artquaval(gsdp_gas_dia$GDP_ARTCLO, gsdp_gas_dia$GDP_CNTDAD_ADQURDA_MES_AJST, gsdp_gas_dia$GDP_VALOR_PGDO_ESTMDO_MES_AJST, "Product code", "Adjusted monthly quantity", "Adjusted monthly value")

#Extrapolated food expenses of the household unit
Ig_gsdu_gasto_alimentos_cap_c<-read.delim(modules[9], header=TRUE, colClasses = "character")
gsdu_gasto_alimentos_cap_c<-merge(viviendas.cali[,c(1,3)], Ig_gsdu_gasto_alimentos_cap_c, by="VIVIENDA")
unique(gsdu_gasto_alimentos_cap_c$ARTICULO)
rm(Ig_gsdu_gasto_alimentos_cap_c)
gsdu_gasto_alimentos_cap_c.monthly.articles<-aggregate.artquaval(gsdu_gasto_alimentos_cap_c$ARTICULO, gsdu_gasto_alimentos_cap_c$CANTIDAD,gsdu_gasto_alimentos_cap_c$VALOR_MENSUAL_ALIMENTO,"Product code", "Adjusted monthly quantity", "Adjusted monthly value")

articles.monthly1<-merge(gsdu_gas_dia.monthly.articles, gsdp_gas_dia.monthly.articles, by="Product code", all=TRUE)
articles.monthly<-merge(articles.monthly1, gsdu_gasto_alimentos_cap_c.monthly.articles, by="Product code", all=TRUE)
articles.monthly$class<-substr(articles.monthly$`Product code`, 1, 2)
articles.monthly<-articles.monthly[articles.monthly$class=="01"  | articles.monthly$class=="02",]
articles.monthly$`Total adjusted monthly quantity`<-rowSums (articles.monthly[,c(2,4,6)], na.rm = TRUE, dims = 1)
articles.monthly$`Total adjusted monthly value`<-rowSums (articles.monthly[,c(3,5,7)], na.rm = TRUE, dims = 1)
articles.monthly$`Value per unit`<-articles.monthly$`Total adjusted monthly value`/articles.monthly$`Total adjusted monthly quantity`

#Units (measure for the quantities)
gsdu_gas_dia.um<-unique(data.frame(GDU_ARTCLO=gsdu_gas_dia$GDU_ARTCLO, GDU_UDM_ESTANDAR=gsdu_gas_dia$GDU_UDM_ESTANDAR))
gsdu_gas_dia.um<-as.data.frame(gsdu_gas_dia.um)
View(gsdu_gas_dia.um)

table(gsdu_gas_dia.um$GDU_UDM_ESTANDAR)
#length(unique(gsdu_gas_dia.um$GDU_UDM_ESTANDAR))
#length(unique(gsdu_gas_dia.um$GDU_ARTCLO))
#nrow(unique(gsdu_gas_dia.um))
#centímetro cúbico (cm3), litro, gramo (gr.), onza, libra, kilogramo, y arroba. 

articles.monthlyu<-merge(articles.monthly, gsdu_gas_dia.um, by.x="Product code", by.y="GDU_ARTCLO", all.x=TRUE)
table(articles.monthlyu$GDU_UDM_ESTANDAR)
write.csv2(articles.monthlyu, "../outputs/articles.monthlyu.csv")
#library(xlsx)
write.xlsx(articles.monthlyu,"../outputs/articles.monthlyu.xlsx")
