#############################################################################################
#Food expenses
#############################################################################################

#############################################################################################
#Load Databases
#############################################################################################

#####
#Daily food consumption by household unit
Ig_gsdu_gas_dia<-read.delim(modules[8], header=TRUE, colClasses = "character")
gsdu_gas_dia<-merge(viviendas.cali[,c(1,3)], Ig_gsdu_gas_dia, by="VIVIENDA")
rm(Ig_gsdu_gas_dia)
gsdu_gas_dia[gsdu_gas_dia== ""] <- NA
gsdu_gas_dia$class<-substr(gsdu_gas_dia$GDU_ARTCLO, 1, 2)
#unique(gsdu_gas_dia$GDU_ARTCLO)
#cod.dos.du<-gsdu_gas_dia[gsdu_gas_dia$class=="02",]
#unique(cod.dos.du$GDU_ARTCLO)
#View(gsdu_gas_dia)
#colnames(gsdu_gas_dia)

#####
#Daily food consumption by other people with income inside the household unit
Ig_gsdp_gas_dia<-read.delim(modules[4], header=TRUE, colClasses = "character")
gsdp_gas_dia<-merge(viviendas.cali[,c(1,3)], Ig_gsdp_gas_dia, by="VIVIENDA")
rm(Ig_gsdp_gas_dia)
gsdp_gas_dia[gsdp_gas_dia== ""] <- NA
#colnames(gsdp_gas_dia)

#####
#Extrapolated food expenses of the household unit
Ig_gsdu_gasto_alimentos_cap_c<-read.delim(modules[9], header=TRUE, colClasses = "character")
gsdu_gasto_alimentos_cap_c<-merge(viviendas.cali[,c(1,3)], Ig_gsdu_gasto_alimentos_cap_c, by="VIVIENDA")
rm(Ig_gsdu_gasto_alimentos_cap_c)
gsdu_gasto_alimentos_cap_c[gsdu_gasto_alimentos_cap_c== ""] <- NA
#View(gsdu_gasto_alimentos_cap_c)
#unique(gsdu_gasto_alimentos_cap_c$ARTICULO)

#####
# Food expenses that are not frequent
Ig_gsmf_compra<-read.delim(modules[11], header=TRUE, colClasses = "character")
gsmf_compra<-merge(viviendas.cali[,c(1,3)], Ig_gsmf_compra, by="VIVIENDA")
rm(Ig_gsmf_compra)
gsmf_compra[gsmf_compra== ""] <- NA

#####
# Non-monetary food expenses that are not frequent
Ig_gsmf_forma_adqui<-read.delim(modules[12], header=TRUE, colClasses = "character")
gsmf_forma_adqui<-merge(viviendas.cali[,c(1,3)], Ig_gsmf_forma_adqui, by="VIVIENDA")
rm(Ig_gsmf_forma_adqui)
gsmf_forma_adqui[gsmf_forma_adqui== ""] <- NA

#############################################################################################
#Aggregate consuption, quantities and units per article
#############################################################################################

gsdu_gas_dia.monthly.articles<-aggregate.artquavalunit(gsdu_gas_dia$GDU_ARTCLO, gsdu_gas_dia$GDU_CNTDAD_ADQURDA_MES_AJST , gsdu_gas_dia$GDU_VALOR_PGDO_ESTMDO_MES_AJST, gsdu_gas_dia$GDU_UDM_ESTANDAR,  "Product code", "Adjusted monthly quantity", "Adjusted monthly value", "Units")
gsdp_gas_dia.monthly.articles<-aggregate.artquaval(gsdp_gas_dia$GDP_ARTCLO, gsdp_gas_dia$GDP_CNTDAD_ADQURDA_MES_AJST, gsdp_gas_dia$GDP_VALOR_PGDO_ESTMDO_MES_AJST, "Product code", "Adjusted monthly quantity", "Adjusted monthly value")
gsdu_gasto_alimentos_cap_c.monthly.articles<-aggregate.artquaval(gsdu_gasto_alimentos_cap_c$ARTICULO, gsdu_gasto_alimentos_cap_c$CANTIDAD,gsdu_gasto_alimentos_cap_c$VALOR_MENSUAL_ALIMENTO,"Product code", "Adjusted monthly quantity", "Adjusted monthly value")
gsmf_compra.monthly.articles<-aggregate.twov(gsmf_compra$GMF_CMPRA_ARTCLO,gsmf_compra$GMF_CMPRA_VLR_PAGO_MES, "Product code", "Monthly value")
gsmf_forma_adqui.monthly.articles<-aggregate.twov(gsmf_forma_adqui$GMF_ADQU_ARTCLO,gsmf_forma_adqui$GMF_ADQU_VLR_PAGO_MES, "Product code", "Monthly value")


monthly.articles.list<-list(gsdu_gas_dia.monthly.articles
                            , gsdp_gas_dia.monthly.articles
                            , gsdu_gasto_alimentos_cap_c.monthly.articles
                            , gsmf_compra.monthly.articles
                            , gsmf_forma_adqui.monthly.articles)
monthly.articles<-Reduce(function(...) merge(..., all=TRUE , by="Product code"), monthly.articles.list)
rm(monthly.articles.list, gsdu_gas_dia.monthly.articles
   , gsdp_gas_dia.monthly.articles
   , gsdu_gasto_alimentos_cap_c.monthly.articles
   , gsmf_compra.monthly.articles
   , gsmf_forma_adqui.monthly.articles)

monthly.articles$class<-substr(monthly.articles$`Product code`, 1, 2)
monthly.articles<-monthly.articles[monthly.articles$class=="01"  | monthly.articles$class=="02",]
View(monthly.articles)
monthly.articles.values<-monthly.articles[,c(1,3,6,8,9,10)]
View(monthly.articles.values)
colnames(monthly.articles.values)<-c("Product code", 
                                    "Value for the household unit database", 
                                    "Value for the other members receiving income data",
                                    "Value for the extrapolated food expenses data",
                                    "Value for the less frequent expenses in cash or credit",
                                    "Value for the less frequent expenses in gift, barter, etc."
                                                                    )
monthly.articles.values$`Total monthly value`<-rowSums (monthly.articles.values[,2:6], na.rm = TRUE, dims = 1)

#library(xlsx)
#write.xlsx(monthly.articles.values,"../outputs/monthly.articles.values.xlsx")


#Average expenses for household that adquires the good