
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
