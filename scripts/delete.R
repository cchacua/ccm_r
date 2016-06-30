
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
