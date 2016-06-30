#############################################################################################
#Food expenses
#############################################################################################

#############################################################################################
#Loading Databases
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
#write.csv(gsdu_gas_dia,"../outputs/gsdu_gas_dia_Cali.csv")


#####
#Daily food consumption by other people with income inside the household unit
Ig_gsdp_gas_dia<-read.delim(modules[4], header=TRUE, colClasses = "character")
gsdp_gas_dia<-merge(viviendas.cali[,c(1,3)], Ig_gsdp_gas_dia, by="VIVIENDA")
rm(Ig_gsdp_gas_dia)
gsdp_gas_dia[gsdp_gas_dia== ""] <- NA
#colnames(gsdp_gas_dia)
#write.csv(gsdp_gas_dia,"../outputs/gsdp_gas_dia_Cali.csv")

#####
#Extrapolated food expenses of the household unit
Ig_gsdu_gasto_alimentos_cap_c<-read.delim(modules[9], header=TRUE, colClasses = "character")
gsdu_gasto_alimentos_cap_c<-merge(viviendas.cali[,c(1,3)], Ig_gsdu_gasto_alimentos_cap_c, by="VIVIENDA")
rm(Ig_gsdu_gasto_alimentos_cap_c)
gsdu_gasto_alimentos_cap_c[gsdu_gasto_alimentos_cap_c== ""] <- NA
#View(gsdu_gasto_alimentos_cap_c)
#unique(gsdu_gasto_alimentos_cap_c$ARTICULO)
#write.csv(gsdu_gasto_alimentos_cap_c,"../outputs/gsdu_gasto_alimentos_cap_c_Cali.csv")

#####
# Food expenses that are not frequent
Ig_gsmf_compra<-read.delim(modules[11], header=TRUE, colClasses = "character")
gsmf_compra<-merge(viviendas.cali[,c(1,3)], Ig_gsmf_compra, by="VIVIENDA")
rm(Ig_gsmf_compra)
gsmf_compra[gsmf_compra== ""] <- NA
#write.csv(gsmf_compra, "../outputs/gsmf_compra_Cali.csv")

#####
# Non-monetary food expenses that are not frequent
Ig_gsmf_forma_adqui<-read.delim(modules[12], header=TRUE, colClasses = "character")
gsmf_forma_adqui<-merge(viviendas.cali[,c(1,3)], Ig_gsmf_forma_adqui, by="VIVIENDA")
rm(Ig_gsmf_forma_adqui)
gsmf_forma_adqui[gsmf_forma_adqui== ""] <- NA
#write.csv(gsmf_forma_adqui, "../outputs/gsmf_forma_adqui_Cali.csv")


#############################################################################################
#Aggregating consumption, quantities and units per article
#############################################################################################

gsdu_gas_dia.monthly.articles<-aggregate.artquavalunit(gsdu_gas_dia$GDU_ARTCLO, gsdu_gas_dia$GDU_CNTDAD_ADQURDA_MES_AJST , gsdu_gas_dia$GDU_VALOR_PGDO_ESTMDO_MES_AJST, gsdu_gas_dia$GDU_UDM_ESTANDAR,  "Product code", "Adjusted monthly quantity", "Adjusted monthly value", "Units")
#gsdu_gas_dia.monthly.articles.values<-summarise(group_by(gsdu_gas_dia, GDU_ARTCLO), sum(as.numeric(as.character(GDU_VALOR_PGDO_ESTMDO_MES_AJST)), na.rm = TRUE))
#View(gsdu_gas_dia.monthly.articles.values)
gsdp_gas_dia.monthly.articles<-aggregate.artquaval(gsdp_gas_dia$GDP_ARTCLO, gsdp_gas_dia$GDP_CNTDAD_ADQURDA_MES_AJST, gsdp_gas_dia$GDP_VALOR_PGDO_ESTMDO_MES_AJST, "Product code", "Adjusted monthly quantity", "Adjusted monthly value")
#gsdp_gas_dia.monthly.articles.values<-summarise(group_by(gsdp_gas_dia, GDP_ARTCLO), sum(as.numeric(as.character(GDP_VALOR_PGDO_ESTMDO_MES_AJST)), na.rm = TRUE))
#View(gsdp_gas_dia.monthly.articles.values)
gsdu_gasto_alimentos_cap_c.monthly.articles<-aggregate.artquaval(gsdu_gasto_alimentos_cap_c$ARTICULO, gsdu_gasto_alimentos_cap_c$CANTIDAD,gsdu_gasto_alimentos_cap_c$VALOR_MENSUAL_ALIMENTO,"Product code", "Adjusted monthly quantity", "Adjusted monthly value")
#gsdu_gasto_alimentos_cap_c.monthly.articles.values<-summarise(group_by(gsdu_gasto_alimentos_cap_c, ARTICULO), sum(as.numeric(as.character(VALOR_MENSUAL_ALIMENTO)), na.rm = TRUE))
#View(gsdu_gasto_alimentos_cap_c.monthly.articles.values)
gsmf_compra.monthly.articles<-aggregate.twov(gsmf_compra$GMF_CMPRA_ARTCLO,gsmf_compra$GMF_CMPRA_VLR_PAGO_MES, "Product code", "Monthly value")
#gsmf_compra.monthly.articles.values<-summarise(group_by(gsmf_compra, GMF_CMPRA_ARTCLO), sum(as.numeric(as.character(GMF_CMPRA_VLR_PAGO_MES)), na.rm = TRUE))
#View(gsmf_compra.monthly.articles.values)
gsmf_forma_adqui.monthly.articles<-aggregate.twov(gsmf_forma_adqui$GMF_ADQU_ARTCLO,gsmf_forma_adqui$GMF_ADQU_VLR_PAGO_MES, "Product code", "Monthly value")
#gsmf_forma_adqui.monthly.articles.values<-summarise(group_by(gsmf_forma_adqui, GMF_ADQU_ARTCLO), sum(as.numeric(as.character(GMF_ADQU_VLR_PAGO_MES)), na.rm = TRUE))
#View(gsmf_forma_adqui.monthly.articles.values)

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

#############################################################################################
#Big table houses vs clases
#############################################################################################

#Load classes names
library(xlsx)
subclass.dane<-read.xlsx(m.labels[1], sheetName="subclass.dane",encoding="UTF-8")

#File with ENIG_CODE, articles, values and quantities
gsdu_gas_dia.bigtable<-data.frame(gsdu_gas_dia$CODIGO_ENIG, 
                                  gsdu_gas_dia$GDU_ARTCLO, 
                                  gsdu_gas_dia$GDU_VALOR_PGDO_ESTMDO_MES_AJST, 
                                  gsdu_gas_dia$GDU_CNTDAD_ADQURDA_MES_AJST , 
                                  gsdu_gas_dia$GDU_UDM_ESTANDAR)
colnames(gsdu_gas_dia.bigtable)<-c("ENIG_CODE", "Product code", "Adjusted monthly value", "Adjusted monthly quantity", "Units")
#gsdu_gas_dia.bigtable.values<-summarise(group_by(gsdu_gas_dia.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
#View(gsdu_gas_dia.bigtable.values)


gsdp_gas_dia.bigtable<-data.frame(gsdp_gas_dia$CODIGO_ENIG, 
                                  gsdp_gas_dia$GDP_ARTCLO, 
                                  gsdp_gas_dia$GDP_VALOR_PGDO_ESTMDO_MES_AJST, 
                                  gsdp_gas_dia$GDP_CNTDAD_ADQURDA_MES_AJST)
colnames(gsdp_gas_dia.bigtable)<-c("ENIG_CODE", "Product code", "Adjusted monthly value", "Adjusted monthly quantity")
#gsdp_gas_dia.bigtable.values<-summarise(group_by(gsdp_gas_dia.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
#View(gsdp_gas_dia.bigtable.values)


gsdu_gasto_alimentos_cap_c.bigtable<-data.frame(paste(gsdu_gasto_alimentos_cap_c$VIVIENDA,gsdu_gasto_alimentos_cap_c$HOGAR, sep=""), 
                                                gsdu_gasto_alimentos_cap_c$ARTICULO,
                                                gsdu_gasto_alimentos_cap_c$VALOR_MENSUAL_ALIMENTO, 
                                                gsdu_gasto_alimentos_cap_c$CANTIDAD)
colnames(gsdu_gasto_alimentos_cap_c.bigtable)<-c("ENIG_CODE", "Product code", "Adjusted monthly value", "Adjusted monthly quantity")
#View(gsdu_gasto_alimentos_cap_c.bigtable)
#gsdu_gasto_alimentos_cap_c.bigtable.values<-summarise(group_by(gsdu_gasto_alimentos_cap_c.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
#View(gsdu_gasto_alimentos_cap_c.bigtable.values)

gsmf_compra.monthly.bigtable<-data.frame(gsmf_compra$CODIGO_ENIG ,
                                         gsmf_compra$GMF_CMPRA_ARTCLO,
                                         gsmf_compra$GMF_CMPRA_VLR_PAGO_MES)
colnames(gsmf_compra.monthly.bigtable)<-c("ENIG_CODE", "Product code", "Adjusted monthly value")
#View(gsmf_compra.monthly.bigtable)
#gsmf_compra.monthly.bigtable.values<-summarise(group_by(gsmf_compra.monthly.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
#View(gsmf_compra.monthly.bigtable.values)

gsmf_forma_adqui.bigtable<-data.frame(gsmf_forma_adqui$CODIGO_ENIG, gsmf_forma_adqui$GMF_ADQU_ARTCLO,gsmf_forma_adqui$GMF_ADQU_VLR_PAGO_MES)
colnames(gsmf_forma_adqui.bigtable)<-c("ENIG_CODE", "Product code", "Adjusted monthly value")
#gsmf_forma_adqui.bigtable.values<-summarise(group_by(gsmf_forma_adqui.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
#View(gsmf_forma_adqui.bigtable.values)


bigtable.list<-list(gsdu_gas_dia.bigtable[,1:3],
                    gsdp_gas_dia.bigtable[,1:3],
                    gsdu_gasto_alimentos_cap_c.bigtable[,1:3],
                    gsmf_compra.monthly.bigtable[,1:3],
                    gsmf_forma_adqui.bigtable[,1:3])


bigtable.all<-Reduce(function(...) rbind(...), bigtable.list)
View(bigtable.all)


bigtable.all$HOUSEID<-substr(bigtable.all$ENIG_CODE, 1, 7)
bigtable.all$`Adjusted monthly value`<-as.numeric(as.character(bigtable.all$`Adjusted monthly value`)) 

#Total expenses by product (incluiding all modules)
bigtable.all.values<-summarise(group_by(bigtable.all, `Product code`), sum(`Adjusted monthly value`, na.rm = TRUE))
View(bigtable.all.values)


#For building the dataset at the level of products (Until here any classification can be used for all the products)
library(datasets)
library(dplyr)
bigtable.outone<-summarise(group_by(bigtable.all, HOUSEID, `Product code`), sum(`Adjusted monthly value`, na.rm = TRUE))
colnames(bigtable.outone)<-c("HOUSEID", "PRODUCTCODE", "VALUE")
#bigtable.out.houseandproduct<-cast(bigtable.outone, HOUSEID ~ PRODUCTCODE)


#From here, only food products are used and the DANE's Classcode is used
bigtable.outone$CLASSCODE<-substr(bigtable.outone$PRODUCTCODE, 1, 6)
bigtable.outone$CLASSTWO<-substr(bigtable.outone$PRODUCTCODE, 1, 2)
bigtable.outone<-bigtable.outone[bigtable.outone$CLASSTWO=="01"  | bigtable.outone$CLASSTWO=="02",]
bigtable.outone<-merge(bigtable.outone, subclass.dane, by.x="CLASSCODE", by.y="subclass.code", all.x = TRUE)
#View(bigtable.outone)

#Total expenses by subclass (incluiding all modules)
#bigtable.outone.subclass<-summarise(group_by(bigtable.outone, CLASSCODE), sum(VALUE, na.rm = TRUE))
#View(bigtable.outone.subclass)

library(reshape)
bigtable.out.houseandclass<-cast(bigtable.outone, HOUSEID ~ CLASSCODE + subclass.name.eng , sum, value="VALUE")
View(bigtable.out.houseandclass)
#bigtable.out.houseandclass.colsums<-as.data.frame(colSums(bigtable.out.houseandclass))
#View(bigtable.out.houseandclass.colsums)
#bigtable.out.houseandclass.colsums

myNumCols <- which(unlist(lapply(bigtable.out.houseandclass, is.numeric)))
bigtable.out.houseandclass[(nrow(bigtable.out.houseandclass) + 1), myNumCols] <- colSums(bigtable.out.houseandclass[, myNumCols], na.rm=TRUE)
bigtable.out.houseandclass[nrow(bigtable.out.houseandclass), 1]<-"Total value of the class"
#One row is deleted, because it corresponds to the row that contains the sums values
bigtable.out.houseandclass[(nrow(bigtable.out.houseandclass) + 1), myNumCols] <- sapply(bigtable.out.houseandclass[, myNumCols], function(x)(sum(x > 0, na.rm=TRUE))-1)
bigtable.out.houseandclass[nrow(bigtable.out.houseandclass), 1]<-"Number of households that consume the product"

View(bigtable.out.houseandclass)
#write.xlsx2(bigtable.out.houseandclass,"../outputs/bigtable.out.houseandclass.xlsx")
#write.csv2(bigtable.out.houseandclass,"../outputs/bigtable.out.houseandclass.csv")



