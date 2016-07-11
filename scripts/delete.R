
#####################################

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


#bigtable.out.houseandclass[(nrow(bigtable.out.houseandclass) + 1), myNumCols] <- 
#sum(df$sex == 'M', na.rm=TRUE)

#View(bigtable.out.houseandclass)
#bigtable.out.houseandclass

#sapply(bigtable.out.houseandclass, max)
#Average expenses for household that adquires the good





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


aggregate.twov= function(v1,v2, title1, title2){
  mini.df<-data.frame(v1,v2)
  mini.df<-na.omit(mini.df)
  out.df<-aggregate(as.numeric(as.character(mini.df[,2])), by=list(Category=mini.df[,1]), FUN=sum)
  colnames(out.df)<-c(title1, title2)
  out.df
}

aggregate.artquaval= function(v1,v2, v3, title1, title2, title3){
  mini.df<-data.frame(v1,v2,v3)
  mini.df<-na.omit(mini.df)
  out.df1<-aggregate(as.numeric(as.character(mini.df[,2])), by=list(Category=mini.df[,1]), FUN=sum)
  colnames(out.df1)<-c(title1, title2)
  out.df2<-aggregate(as.numeric(as.character(mini.df[,3])), by=list(Category=mini.df[,1]), FUN=sum)
  colnames(out.df2)<-c(title1, title3)
  out.df<-merge(out.df1,out.df2, by=title1)
  out.df
}

aggregate.artquavalunit= function(v1,v2, v3, v4, title1, title2, title3, titleunit){
  mini.df<-data.frame(v1,v2,v3, v4)
  mini.df<-na.omit(mini.df)
  out.df1<-aggregate(as.numeric(as.character(mini.df[,2])), by=list(Category=mini.df[,1]), FUN=sum)
  colnames(out.df1)<-c(title1, title2)
  out.df2<-aggregate(as.numeric(as.character(mini.df[,3])), by=list(Category=mini.df[,1]), FUN=sum)
  colnames(out.df2)<-c(title1, title3)
  out.df<-merge(out.df1,out.df2, by=title1)
  out.df3<-unique(mini.df[,c(1,4)])
  print(nrow(out.df3))
  colnames(out.df3)<-c(title1, titleunit)
  out.df<-merge(out.df,out.df3, by=title1)
  out.df
}



#For counting na
length(na.omit(households[,12]))
nrow(na.omit(houses))


#Código viejo horas extras

t.2<-allindividuals[, c("CODIGO_ENIG", "P6510S1", "P6510S2")]
t.2$horasextras<-ifelse(t.2$P6510S2=="2", t.2$P6510S1, NA)
sum(as.numeric(as.character(t.2$horasextras)), na.rm=TRUE)
t2.1<-as.data.frame(table(t.2$horasextras))
sum(t2.1$Freq)
i1.1<-merge(i1.1, t.2[,c("horasextras", "CODIGO_ENIG")], by="CODIGO_ENIG", all=TRUE) 
rm(t.2)


#Para crear un df con carácterés
i1.1$total<- as.data.frame(sapply(i1.1[, 3:ncol(i1.1)], function(x) (as.numeric(as.character(x)))))

#Para agregar por hogares
i1.2.indiv<-allindividuals[, c("CODIGO_ENIG", "HOUSEID")]
i1.2.indiv<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6190S1", output.df=i1.2.indiv)
i1.2.indiv.P6190S1<-summarise(group_by(i1.2.indiv, HOUSEID), sum(P6190S1, na.rm = TRUE))


i1.2.indiv<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6200S1", output.df=i1.2.indiv)
i1.2.indiv.P6200S1<-summarise(group_by(i1.2.indiv, HOUSEID), sum(P6200S1, na.rm = TRUE))
rm(i1.2.indiv, i1.2.indiv.P6200S1, i1.2.indiv.P6190S1)


# Algo que no va en los ingresos, pero que podría servir a futuro
# P7080	¿Cuál fue el total de los ingresos recibidos o ganados por concepto de los trabajos que tuvo durante los últimos doce meses?
#/12
#i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7080", output.df=i1.1)


########### Older Food expenses
# 
# 
# #File with ENIG_CODE, articles, values and quantities
# gsdu_gas_dia.bigtable<-data.frame(gsdu_gas_dia$CODIGO_ENIG, 
#                                   gsdu_gas_dia$GDU_ARTCLO, 
#                                   gsdu_gas_dia$GDU_VALOR_PGDO_ESTMDO_MES_AJST, 
#                                   gsdu_gas_dia$GDU_CNTDAD_ADQURDA_MES_AJST , 
#                                   gsdu_gas_dia$GDU_UDM_ESTANDAR)
# colnames(gsdu_gas_dia.bigtable)<-c("ENIG_CODE", "Product code", "Adjusted monthly value", "Adjusted monthly quantity", "Units")
# gsdu_gas_dia.bigtable.values<-summarise(group_by(gsdu_gas_dia.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
# #View(gsdu_gas_dia.bigtable.values)
# 
# 
# gsdp_gas_dia.bigtable<-data.frame(gsdp_gas_dia$CODIGO_ENIG, 
#                                   gsdp_gas_dia$GDP_ARTCLO, 
#                                   gsdp_gas_dia$GDP_VALOR_PGDO_ESTMDO_MES_AJST, 
#                                   gsdp_gas_dia$GDP_CNTDAD_ADQURDA_MES_AJST)
# colnames(gsdp_gas_dia.bigtable)<-c("ENIG_CODE", "Product code", "Adjusted monthly value", "Adjusted monthly quantity")
# #gsdp_gas_dia.bigtable.values<-summarise(group_by(gsdp_gas_dia.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
# #View(gsdp_gas_dia.bigtable.values)
# 
# 
# gsdu_gasto_alimentos_cap_c.bigtable<-data.frame(paste(gsdu_gasto_alimentos_cap_c$VIVIENDA,gsdu_gasto_alimentos_cap_c$HOGAR, sep=""), 
#                                                 gsdu_gasto_alimentos_cap_c$ARTICULO,
#                                                 gsdu_gasto_alimentos_cap_c$VALOR_MENSUAL_ALIMENTO, 
#                                                 gsdu_gasto_alimentos_cap_c$CANTIDAD)
# colnames(gsdu_gasto_alimentos_cap_c.bigtable)<-c("ENIG_CODE", "Product code", "Adjusted monthly value", "Adjusted monthly quantity")
# #View(gsdu_gasto_alimentos_cap_c.bigtable)
# #gsdu_gasto_alimentos_cap_c.bigtable.values<-summarise(group_by(gsdu_gasto_alimentos_cap_c.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
# #View(gsdu_gasto_alimentos_cap_c.bigtable.values)
# 
# gsmf_compra.monthly.bigtable<-data.frame(gsmf_compra$CODIGO_ENIG ,
#                                          gsmf_compra$GMF_CMPRA_ARTCLO,
#                                          gsmf_compra$GMF_CMPRA_VLR_PAGO_MES)
# colnames(gsmf_compra.monthly.bigtable)<-c("ENIG_CODE", "Product code", "Adjusted monthly value")
# #View(gsmf_compra.monthly.bigtable)
# #gsmf_compra.monthly.bigtable.values<-summarise(group_by(gsmf_compra.monthly.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
# #View(gsmf_compra.monthly.bigtable.values)
# 
# gsmf_forma_adqui.bigtable<-data.frame(gsmf_forma_adqui$CODIGO_ENIG, gsmf_forma_adqui$GMF_ADQU_ARTCLO,gsmf_forma_adqui$GMF_ADQU_VLR_PAGO_MES)
# colnames(gsmf_forma_adqui.bigtable)<-c("ENIG_CODE", "Product code", "Adjusted monthly value")
# #gsmf_forma_adqui.bigtable.values<-summarise(group_by(gsmf_forma_adqui.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
# #View(gsmf_forma_adqui.bigtable.values)
# 
# 
# bigtable.list<-list(gsdu_gas_dia.bigtable[,1:3],
#                     gsdp_gas_dia.bigtable[,1:3],
#                     gsdu_gasto_alimentos_cap_c.bigtable[,1:3],
#                     gsmf_compra.monthly.bigtable[,1:3],
#                     gsmf_forma_adqui.bigtable[,1:3])
# 
# 
# bigtable.all<-Reduce(function(...) rbind(...), bigtable.list)
# #View(bigtable.all)
# 
# 
# bigtable.all$HOUSEID<-substr(bigtable.all$ENIG_CODE, 1, 7)
# #length(unique(bigtable.all$HOUSEID))
# bigtable.all$`Adjusted monthly value`<-as.numeric(as.character(bigtable.all$`Adjusted monthly value`)) 
# 
# #Total expenses by product (incluiding all modules, without households' information)
# bigtable.all.values<-summarise(group_by(bigtable.all, `Product code`), sum(`Adjusted monthly value`, na.rm = TRUE))
# #View(bigtable.all.values)
# 
# 
# #For building the dataset at the level of products (Until here any classification can be used for all the products)
# bigtable.outone<-summarise(group_by(bigtable.all, HOUSEID, `Product code`), sum(`Adjusted monthly value`, na.rm = TRUE))
# colnames(bigtable.outone)<-c("HOUSEID", "PRODUCTCODE", "VALUE")
# #bigtable.out.houseandproduct<-cast(bigtable.outone, HOUSEID ~ PRODUCTCODE)
# #length(unique(bigtable.outone$HOUSEID))


#For including the sums, count and average as additionals rows
# #bigtable.out.houseandclass.colsums
# myNumCols <- which(unlist(lapply(bigtable.out.houseandclass, is.numeric)))
# bigtable.out.houseandclass[(nrow(bigtable.out.houseandclass) + 1), myNumCols] <- colSums(bigtable.out.houseandclass[, myNumCols], na.rm=TRUE)
# bigtable.out.houseandclass[nrow(bigtable.out.houseandclass), 1]<-"Total monthly value of the class"
# #One row is deleted, because it corresponds to the row that contains the sums values
# bigtable.out.houseandclass[(nrow(bigtable.out.houseandclass) + 1), myNumCols] <- sapply(bigtable.out.houseandclass[, myNumCols], function(x)(sum(x > 0, na.rm=TRUE))-1)
# bigtable.out.houseandclass[nrow(bigtable.out.houseandclass), 1]<-"Number of households that consume the product"
# #For estimating the average values of the households that consumed the product
# bigtable.out.houseandclass[(nrow(bigtable.out.houseandclass) + 1), myNumCols] <- sapply(bigtable.out.houseandclass[(nrow(bigtable.out.houseandclass)-1):nrow(bigtable.out.houseandclass), myNumCols], function(x)(x[1]/x[2]))
# bigtable.out.houseandclass[nrow(bigtable.out.houseandclass), 1]<-"Average monthly consumption for the households that consumed the product"
#View(bigtable.out.houseandclass)
#write.xlsx2(bigtable.out.houseandclass,"../outputs/bigtable.out.houseandclass.xlsx")
#write.csv2(bigtable.out.houseandclass,"../outputs/bigtable.out.houseandclass.csv")

# bigtable.out.houseandclass.aggregated<-bigtable.out.houseandclass[(nrow(bigtable.out.houseandclass) -2):nrow(bigtable.out.houseandclass),]
# #write.xlsx2(bigtable.out.houseandclass.aggregated,"../outputs/bigtable.out.houseandclass.aggregated.xlsx")
# #View(bigtable.out.houseandclass.aggregated)

# Aggregated table of consumption by class
bigtable.out.houseandclass.onlyclass<-cast(bigtable.outone, HOUSEID ~ CLASSCODE, sum, value="VALUE")
bigtable.out.houseandclass.onlyclass.tvalue<-as.data.frame(colSums(bigtable.out.houseandclass.onlyclass))
colnames(bigtable.out.houseandclass.onlyclass.tvalue)<-c("Total monthly consumption")
bigtable.out.houseandclass.onlyclass.tvalue<-merge(bigtable.out.houseandclass.onlyclass.tvalue, subclass.dane, by.x=0, by.y="subclass.code")
bigtable.out.houseandclass.onlyclass.myNumCols <- which(unlist(lapply(bigtable.out.houseandclass.onlyclass, is.numeric)))
bigtable.out.houseandclass.onlyclass.number<- as.data.frame(sapply(bigtable.out.houseandclass.onlyclass[, bigtable.out.houseandclass.onlyclass.myNumCols], function(x)(sum(x > 0, na.rm=TRUE))))
colnames(bigtable.out.houseandclass.onlyclass.number)<-c("Number of households that consume the product")
bigtable.out.conbyclass<-merge(bigtable.out.houseandclass.onlyclass.tvalue, bigtable.out.houseandclass.onlyclass.number, by.x="Row.names", by.y=0)
bigtable.out.conbyclass$`Average monthly consumption for the households that consumed the product`<-bigtable.out.conbyclass$`Total monthly consumption`/bigtable.out.conbyclass$`Number of households that consume the product`
bigtable.out.conbyclass.names<-colnames(bigtable.out.conbyclass)
bigtable.out.conbyclass.names[1]<-"Class code"
colnames(bigtable.out.conbyclass)<-bigtable.out.conbyclass.names
View(bigtable.out.conbyclass)
write.xlsx2(bigtable.out.conbyclass,"../outputs/Consumption by class.xlsx")
rm(bigtable.out.houseandclass.onlyclass.tvalue, bigtable.out.houseandclass.onlyclass.myNumCols, bigtable.out.houseandclass.onlyclass.number, bigtable.out.conbyclass.names)



###############################################################################################
# Table consumption of food products
#####
bigtable.out.conbyclass<-agg.consu.sum(df.src=data.frame(id.v=bigtable.outone$HOUSEID, levelcode.v=bigtable.outone$CLASSCODE, value.v=bigtable.outone$VALUE), 
                                       levelcode.labels=subclass.dane, 
                                       levelcode.labels.by="subclass.code",
                                       saveto="../outputs/Consumption by class .xlsx")

bigtable.outproduct<-summarise(group_by(bigtable.all, `Product code`), sum(`Adjusted monthly value`, na.rm = TRUE))
colnames(bigtable.outproduct)<-c("PRODUCTCODE", "VALUE")
View(bigtable.outproduct)
#For assigning labels from the products.name file to the products table
consbyproduct<-merge(bigtable.outproduct, products.dane, by.x="PRODUCTCODE", by.y="products.code", all.x = TRUE)
consbyproduct<-consbyproduct[,c("CLASSCODE", 
                                "subclass.name.sp",
                                "subclass.name.eng",
                                "subclass.longname.eng",
                                "PRODUCTCODE",
                                "products.name.es","VALUE")]

colnames(consbyproduct)<-c("Subclass code", 
                           "Subclass name (Spanish)",
                           "Subclass name (English)",
                           "Subclass description (English)",
                           "Product code",
                           "Product name (Spanish)","Total consumption value of the selected items")
View(consbyproduct)
write.xlsx2(consbyproduct,"../outputs/consbyproduct.xlsx")

###############################################################################################


#Total expenses by subclass (incluiding all modules, without households' information)
#bigtable.outone.subclass<-summarise(group_by(bigtable.outone, CLASSCODE), sum(VALUE, na.rm = TRUE))
#View(bigtable.outone.subclass)

#For assigning labels from the products.name file to the products table
bigtable.outone.products<-merge(bigtable.outone, products.dane, by.x="PRODUCTCODE", by.y="products.code", all.x = TRUE)
bigtable.outone.products<-bigtable.outone.products[,c("HOUSEID", 
                                                      "CLASSCODE", 
                                                      "subclass.name.sp",
                                                      "subclass.name.eng",
                                                      "subclass.longname.eng",
                                                      "PRODUCTCODE",
                                                      "products.name.es","VALUE")]

colnames(bigtable.outone.products)<-c("HOUSEID", 
                                      "Subclass code", 
                                      "Subclass name (Spanish)",
                                      "Subclass name (English)",
                                      "Subclass description (English)",
                                      "Product code",
                                      "Product name (Spanish)","Total consumption value of the selected items")
#View(bigtable.outone.products)
write.xlsx2(bigtable.outone.products,"../outputs/bigtable.outone.products.xlsx")


#############################################################################################
#Big table with added modules
#############################################################################################
bigtable.out.houseandclass.raw$VIVIENDA<-substr(bigtable.out.houseandclass.raw$HOUSEID, 1, 5)
bigtable.out.houseandclass.addedmod<-merge(bigtable.out.houseandclass.raw, houses, by="VIVIENDA", all.x=TRUE)
bigtable.out.houseandclass.addedmod<-merge(bigtable.out.houseandclass.addedmod, households, by="HOUSEID")
bigtable.out.houseandclass.addedmod<-merge(bigtable.out.houseandclass.addedmod, individuals, by="HOUSEID",all.x=TRUE)
bigtable.out.houseandclass.addedmod[bigtable.out.houseandclass.addedmod== ""] <- NA

bigtable.out.houseandclass<-cast(bigtable.outone.wclass, HOUSEID ~ CLASSCODE + subclass.name.eng , sum, value="VALUE")
bigtable.out.houseandclass.raw<-bigtable.out.houseandclass
#write.xlsx2(bigtable.out.houseandclass.raw,"../outputs/bigtable.out.houseandclass.raw.xlsx")

houseandclass.addedmod<-add.rlvnt.houshld.inf(table.df=bigtable.out.houseandclass.raw, 
                                              houseid.v=bigtable.out.houseandclass.raw$HOUSEID, 
                                              houses.df=houses, 
                                              households.df=households, 
                                              individuals.df=individuals)


#   
#     conbyproduct3<-agg.consu.sum3(df.src=data.frame(id.v=bigtable.outone$HOUSEID, levelcode.v=bigtable.outone$PRODUCTCODE, value.v=bigtable.outone$VALUE), 
#                                   levelcode.labels=classsubandpro,
#                                   levelcode.labels.by="products.code",
#                                   saveto="../outputs/Consumption by product 2.xlsx", individuals)
#     
#     View(conbyproduct3)



agg.consu.sum3<-function(df.src, levelcode.labels, levelcode.labels.by, saveto="../outputs/name.xlsx", individuals.df){
  #df.src=data.frame(id.v, levelcode.v, value.v)  
  ptable<-cast(df.src, id.v ~ levelcode.v, sum, value="value.v")
  
  ptable.myNumCols <- which(unlist(lapply(ptable, is.numeric)))
  # Sums of values 
  ptable.tvalue<-as.data.frame(colSums(ptable[,ptable.myNumCols]))
  print(colnames(ptable.tvalue))
  # Times where value>0 in each column
  
  ptable.number<- as.data.frame(sapply(ptable[, ptable.myNumCols], function(x)(sum(x > 0, na.rm=TRUE))))
  ptable.sd<- as.data.frame(sapply(ptable[, ptable.myNumCols], function(x)(sd(x > 0, na.rm=TRUE))))
  
  ptable.conbylevel<-merge(ptable.tvalue, ptable.number, by.x=0, by.y=0, all=TRUE)
  #ptable.conbylevel<-merge(ptable.conbylevel, ptable.sd, by.x=1, by.y=0, all=TRUE)
  ptable.conbylevel$AVERAGE<-ptable.conbylevel[,2]/ptable.conbylevel[,3]
  
  colnames(ptable.conbylevel)<-c("Code", "Total consumption", "Numer of consumer households", "Average consumption")
  # Labels for the levelcode variable
  ptable.conbylevel1<-merge(ptable.conbylevel[,1:2], levelcode.labels, by.x=1, by.y=levelcode.labels.by, all.x=TRUE)
  ptable.conbylevel<-merge(ptable.conbylevel1, ptable.conbylevel, by="Code", all=TRUE)
  print(colnames(ptable.conbylevel))
  ptable.conbylevel<-ptable.conbylevel[,c(1,3:ncol(ptable.conbylevel))]
  ptable.conbylevel<-merge(ptable.conbylevel, ptable.sd, by.x="Code", by.y=0, all=TRUE)
  
  # Estimations for individuals
  numberofindi<-merge(ptable, individuals.df, by="id.v",by.y="HOUSEID", all.x=TRUE)
  ptable$vector<-numberofindi$`Number of people inside the household`
  ptable[, ptable.myNumCols]<-sweep(ptable[, ptable.myNumCols],1,ptable$vector,`/`)
  print("Hace bien la división")
  ptable.tvalue.indi<-as.data.frame(colSums(ptable[,ptable.myNumCols]))
  print(colnames(ptable.tvalue.indi))
  ptable.number.indi<- as.data.frame(sapply(ptable[, ptable.myNumCols], function(x)(sum(x > 0, na.rm=TRUE))))
  ptable.sd.indi<- as.data.frame(sapply(ptable[, ptable.myNumCols], function(x)(sd(x > 0, na.rm=TRUE))))
  ptable.conbylevel.indi<-merge(ptable.tvalue.indi, ptable.number.indi, by.x=0, by.y=0, all=TRUE)
  ptable.conbylevel<-merge(ptable.conbylevel, ptable.conbylevel.indi, by.x="Code", by.y=1, all=TRUE)
  ptable.conbylevel<-merge(ptable.conbylevel, ptable.sd.indi, by.x="Code", by.y=0, all=TRUE)
  #ptable.meanindi<- as.data.frame(sapply(ptable[, ptable.myNumCols], function(x)(mean(x > 0, na.rm=TRUE))))
  #print(ptable.meanindi)
  
  write.xlsx2(ptable.conbylevel, saveto)
  return(ptable.conbylevel)
}

agg.consu.sum4<-function(df.src, levelcode.labels, levelcode.labels.by, saveto="../outputs/name.xlsx"){
  #df.src=data.frame(id.v, levelcode.v, value.v)  
  ptable<-cast(df.src, id.v ~ levelcode.v, sum, value="value.v")
  
  # Sums of values 
  ptable.tvalue<-as.data.frame(colSums(ptable))
  # Times where value>0 in each column
  ptable.myNumCols <- which(unlist(lapply(ptable, is.numeric)))
  ptable.number<- as.data.frame(sapply(ptable[, ptable.myNumCols], function(x)(sum(x > 0, na.rm=TRUE))))
  ptable.conbylevel<-merge(ptable.tvalue, ptable.number, by.x=0, by.y=0, all=TRUE)
  ptable.conbylevel$AVERAGE<-ptable.conbylevel[,2]/ptable.conbylevel[,3]
  colnames(ptable.conbylevel)<-c("Code", "Total consumption", "Numer of consumer households", "Average consumption")
  # Labels for the levelcode variable
  ptable.conbylevel1<-merge(ptable.conbylevel[,1:2], levelcode.labels, by.x=1, by.y=levelcode.labels.by, all.x=TRUE)
  ptable.conbylevel<-merge(ptable.conbylevel1, ptable.conbylevel, by="Code", all=TRUE)
  ptable.conbylevel<-ptable.conbylevel[,c(1,3:ncol(ptable.conbylevel))]
  write.xlsx2(ptable.conbylevel, saveto)
  return(ptable.conbylevel)
}
