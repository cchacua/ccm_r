#############################################################################################
#Food expenses
#############################################################################################
library(datasets)
library(dplyr)
library(xlsx)
library(reshape)
library(reshape2)
#library(stringi)
#library(googlesheets)

#############################################################################################
# Load labels
#############################################################################################
  
  # Load classes names  
    class.dane<-read.xlsx(m.labels[3], sheetName="class.dane",encoding="UTF-8")
  # Load Subclasses names
    subclass.dane<-read.xlsx(m.labels[1], sheetName="subclass.dane",encoding="UTF-8")
    #subclass.dane.key<-gs_key("11cq-icYs_BcpJ0yBBiRllN5KnkPJxdGky1PmhUYIvrA")
    # A command/browser prompt will appear for giving access to the Google Spreadsheet
    #subclass.dane<-gs_read_csv(subclass.dane.key)
    #rm(subclass.dane.key<-gs_key)
    
  # Load products names
  products.dane<-read.xlsx(m.labels[2], sheetName="products.dane",encoding="UTF-8")
  
  # Subclass and products
  products.dane$CLASSCODE<-substr(products.dane$products.code, 1, 6)
  subandpro<-merge(subclass.dane, products.dane, by.x="subclass.code", by.y="CLASSCODE", all=TRUE)
  
  # Class, subclass and products
  subandpro$class<-substr(subandpro$subclass.code, 1, 4)
  classsubandpro<-merge(class.dane, subandpro, by.x="class.code", by.y="class", all=TRUE)

  
  # NOVA class
  class.nova<-read.xlsx(m.labels[4], sheetName="class.nova",encoding="UTF-8")
###############################################################################################
# Outone: Table products by household
#####
  
  #For building the dataset at the level of products (Until here any classification can be used for all the products)
  bigtable.outone<-summarise(group_by(bigtable.all, HOUSEID, `Product code`), sum(`Adjusted monthly value`, na.rm = TRUE))
  colnames(bigtable.outone)<-c("HOUSEID", "PRODUCTCODE", "VALUE")
  #bigtable.out.houseandproduct<-cast(bigtable.outone, HOUSEID ~ PRODUCTCODE)
  #length(unique(bigtable.outone$HOUSEID))

  bigtable.outone$CLASSCODE<-substr(bigtable.outone$PRODUCTCODE, 1, 6)
  #From here, only food products are used
  bigtable.outone$CLASSTWO<-substr(bigtable.outone$PRODUCTCODE, 1, 2)
  bigtable.outone<-bigtable.outone[bigtable.outone$CLASSTWO=="01"  | bigtable.outone$CLASSTWO=="02",]
  #bigtable.outone.cigarrettes<-bigtable.outone[substr(bigtable.outone$PRODUCTCODE,1,4)=="0220" |substr(bigtable.outone$PRODUCTCODE,1,4)=="0230" ,]
  bigtable.outone[substr(bigtable.outone$PRODUCTCODE,1,4)=="0220" |substr(bigtable.outone$PRODUCTCODE,1,4)=="0230" ,]<-NA
  bigtable.outone<-na.omit(bigtable.outone)
  
###############################################################################################  

# Preliminar House and product for estimating outliers
  
  bigtable.outone.wpro<-merge(bigtable.outone, products.dane, by.x="PRODUCTCODE", by.y="products.code", all.x = TRUE)
  houseandproduct.addedmod.indi<-bigtable.full.individuals(df.src=data.frame(id.v=bigtable.outone.wpro$HOUSEID, levelcode.v=bigtable.outone.wpro$PRODUCTCODE, levelname.v=bigtable.outone.wpro$products.name.es, value.v=bigtable.outone.wpro$VALUE),
                                                           houses.df=houses, 
                                                           households.df=households, 
                                                           individuals.df=individuals)
   View(houseandproduct.addedmod.indi[,c(1:4,240:ncol(houseandproduct.addedmod.indi))])
###############################################################################################
# Outliers
#####  
  
  # This dataframe contains consumption by product at the level of people
  # http://r-statistics.co/Outlier-Treatment-With-R.html
  outliers<-as.data.frame(sapply(houseandproduct.addedmod.indi[,3:240], remove_outliers4))
  outliers$HOUSEID<-houseandproduct.addedmod.indi$id.v
  colnames(outliers)
  outliers.total<-outliers[,c(ncol(outliers), 1:ncol(outliers)-1)]
  colnames(outliers.total)
  outliers.total$TOTAL<-rowSums(outliers.total[,2:ncol(outliers.total)])
  #View(outliers.total[,c(1,200:ncol(outliers.total))])
  outliers.total[outliers.total$TOTAL>400000,]<-NA
  outliers.total<-na.omit(outliers.total)
  outliers.total[outliers.total$TOTAL==0,]<-NA
  outliers.total<-na.omit(outliers.total)
  outliers<-outliers.total[,1:ncol(outliers.total)-1]
  #outliers.total.sup400<-outliers.total[outliers.total$TOTAL>400000,]
  #View(outliers.total.sup400[,210:ncol(outliers.total.sup400)])
  
  #outliersdel<-na.omit(outliers)
  #nrow(outliers)/nrow(houseandproduct.addedmod.indi)

  bigtable.outone2<-melt(outliers)
  bigtable.outone2$PRODUCTCODE<-substr(bigtable.outone2$variable, 1,8)
  bigtable.outone2$CLASSCODE<-substr(bigtable.outone2$PRODUCTCODE, 1, 6)
  #From here, only food products are used
  bigtable.outone2$CLASSTWO<-substr(bigtable.outone2$PRODUCTCODE, 1, 2)
  bigtable.outone2<-bigtable.outone2[,c(1,4,3,5,6)]
  colnames(bigtable.outone2)<-colnames(bigtable.outone)
  bigtable.outone.indiv<-bigtable.outone2
  
  
  bigtable.outone3<-melt(outliers)
  bigtable.outone3$PRODUCTCODE<-substr(bigtable.outone3$variable, 1,8)
  bigtable.outone3$CLASSCODE<-substr(bigtable.outone3$PRODUCTCODE, 1, 6)
  #From here, only food products are used
  bigtable.outone3$CLASSTWO<-substr(bigtable.outone3$PRODUCTCODE, 1, 2)
  bigtable.outone3<-bigtable.outone3[,c(1,4,3,5,6)]
  colnames(bigtable.outone3)<-colnames(bigtable.outone)
  View(bigtable.outone3)
  bigtable.outone3<-merge(bigtable.outone3, individuals[,c(1,5)], by="HOUSEID", all.x=TRUE)
  bigtable.outone3$VALUE<-bigtable.outone3$VALUE*bigtable.outone3$`Number of people inside the household`
  bigtable.outone<-bigtable.outone3[,c(1:5)]

  View(bigtable.outone)
  
###############################################################################################    
  
  
  
#############################################################################################
# Product big table with added modules
#############################################################################################
  #####
  # House and DANE product table
  #   
  bigtable.outone.wpro<-merge(bigtable.outone, products.dane, by.x="PRODUCTCODE", by.y="products.code", all.x = TRUE)
  houseandproduct.addedmod<-bigtable.full(df.src=data.frame(id.v=bigtable.outone.wpro$HOUSEID, levelcode.v=bigtable.outone.wpro$PRODUCTCODE, levelname.v=bigtable.outone.wpro$products.name.es, value.v=bigtable.outone.wpro$VALUE),
                                          houses.df=houses, 
                                          households.df=households, 
                                          individuals.df=individuals)
  View(houseandproduct.addedmod)
  write.xlsx2(houseandproduct.addedmod,"../outputs/Food consumption by product at household level.xlsx")
  
  houseandproduct.addedmod.indi<-bigtable.full.individuals(df.src=data.frame(id.v=bigtable.outone.wpro$HOUSEID, levelcode.v=bigtable.outone.wpro$PRODUCTCODE, levelname.v=bigtable.outone.wpro$products.name.es, value.v=bigtable.outone.wpro$VALUE),
                                                           houses.df=houses, 
                                                           households.df=households, 
                                                           individuals.df=individuals)
  View(houseandproduct.addedmod.indi)
  write.xlsx2(houseandproduct.addedmod.indi,"../outputs/Average individual food consumption by product at household level.xlsx")
  colnames(houseandproduct.addedmod.indi)
  
  
  
#############################################################################################
  
  
#############################################################################################
# Subclass big table with added modules
#############################################################################################
  #####
  # House and DANE class table
  #   
  bigtable.outone.wclass<-merge(bigtable.outone, subclass.dane, by.x="CLASSCODE", by.y="subclass.code", all.x = TRUE)
  
  houseandclass.addedmod<-bigtable.full(df.src=data.frame(id.v=bigtable.outone.wclass$HOUSEID, levelcode.v=bigtable.outone.wclass$CLASSCODE, levelname.v=bigtable.outone.wclass$subclass.name.eng, value.v=bigtable.outone.wclass$VALUE),
                                        houses.df=houses, 
                                        households.df=households, 
                                        individuals.df=individuals)
  #View(houseandclass.addedmod)
  write.xlsx2(houseandclass.addedmod,"../outputs/Food consumption by subclass at household level.xlsx")
  houseandclass.addedmod.indi<-bigtable.full.individuals(df.src=data.frame(id.v=bigtable.outone.wclass$HOUSEID, levelcode.v=bigtable.outone.wclass$CLASSCODE, levelname.v=bigtable.outone.wclass$subclass.name.eng, value.v=bigtable.outone.wclass$VALUE),
                                                         houses.df=houses, 
                                                         households.df=households, 
                                                         individuals.df=individuals)
  
  
  write.xlsx2(houseandclass.addedmod.indi,"../outputs/Average individual food consumption by subclass at household level.xlsx")
  #View(houseandclass.addedmod.indi)
#############################################################################################
  
#############################################################################################
# Class big table with added modules
#############################################################################################
  #####
  # House and DANE class table
  # 
  bigtable.outone.wdclass<-bigtable.outone
  bigtable.outone.wdclass$CLASSFOUR<-substr(bigtable.outone.wdclass$CLASSCODE,1,4)
  bigtable.outone.wdclass<-merge(bigtable.outone.wdclass, class.dane, by.x="CLASSFOUR", by.y="class.code", all.x = TRUE)
  
  houseandclass.14.addedmod<-bigtable.full(df.src=data.frame(id.v=bigtable.outone.wdclass$HOUSEID, levelcode.v=bigtable.outone.wdclass$CLASSFOUR, levelname.v=bigtable.outone.wdclass$class.name.en, value.v=bigtable.outone.wdclass$VALUE),
                                        houses.df=houses, 
                                        households.df=households, 
                                        individuals.df=individuals)
  #View(houseandclass.14.addedmod)
  write.xlsx2(houseandclass.14.addedmod,"../outputs/Food consumption by Class at household level.xlsx")
  houseandclass.14.addedmod.indi<-bigtable.full.individuals(df.src=data.frame(id.v=bigtable.outone.wdclass$HOUSEID, levelcode.v=bigtable.outone.wdclass$CLASSFOUR, levelname.v=bigtable.outone.wdclass$class.name.en, value.v=bigtable.outone.wdclass$VALUE),
                                                            houses.df=houses, 
                                                            households.df=households, 
                                                            individuals.df=individuals)
  
  
  write.xlsx2(houseandclass.14.addedmod.indi,"../outputs/Average individual food consumption by Class at household level.xlsx")
  #View(houseandclass.14.addedmod.indi)
#############################################################################################
  
  
#############################################################################################
# NOVA Class big table with added modules
#############################################################################################
#####
  # House and NOVA class table

  bigtable.outone.wnovaclass<-merge(bigtable.outone, class.nova, by.x="PRODUCTCODE", by.y="product.code", all.x = TRUE)
  View(bigtable.outone.wnovaclass)
  #unique(bigtable.outone.wnovaclass$PRODUCTCODE[is.na(bigtable.outone.wnovaclass$class.code)])
  houseandclass.nova.addedmod<-bigtable.full(df.src=data.frame(id.v=bigtable.outone.wnovaclass$HOUSEID, levelcode.v=bigtable.outone.wnovaclass$class.code, levelname.v=bigtable.outone.wnovaclass$class.name.en, value.v=bigtable.outone.wnovaclass$VALUE),
                                             houses.df=houses, 
                                             households.df=households, 
                                             individuals.df=individuals)
  # View(houseandclass.nova.addedmod)
  write.xlsx2(houseandclass.nova.addedmod,"../outputs/Food consumption by NOVA Class at household level.xlsx")
  houseandclass.nova.addedmod.indi<-bigtable.full.individuals(df.src=data.frame(id.v=bigtable.outone.wnovaclass$HOUSEID, levelcode.v=bigtable.outone.wnovaclass$class.code, levelname.v=bigtable.outone.wnovaclass$class.name.en, value.v=bigtable.outone.wnovaclass$VALUE),
                                                              houses.df=houses, 
                                                              households.df=households, 
                                                              individuals.df=individuals)
  
  
  write.xlsx2(houseandclass.nova.addedmod.indi,"../outputs/Average individual food consumption by NOVA Class at household level.xlsx")
#############################################################################################
  
      
###############################################################################################
# Table consumption of food products
#####
  conbyproduct<-agg.consu.sum(df.src=data.frame(id.v=bigtable.outone$HOUSEID, levelcode.v=bigtable.outone$PRODUCTCODE, value.v=bigtable.outone$VALUE), 
                                         levelcode.labels=classsubandpro,
                                         levelcode.labels.by="products.code",
                                         saveto="../outputs/Consumption by product .xlsx")
  
  View(conbyproduct)
  
  #write.xlsx(alpha.pre.item, file="Alpha items.xlsx", sheetName=paste(nombredimension, "pre"), append=TRUE)
  
    #     conbyproduct.households<-agg.consu.sum2(df.src=data.frame(id.v=bigtable.outone$HOUSEID, levelcode.v=bigtable.outone$PRODUCTCODE, value.v=bigtable.outone$VALUE), 
    #                                 levelcode.labels=classsubandpro,
    #                                 levelcode.labels.by="products.code",
    #                                 saveto="../outputs/Consumption by product of households.xlsx")
    
    #View(conbyproduct.households)
    
    
    conbyproduct.indiv<-agg.consu.sum2(df.src=data.frame(id.v=bigtable.outone.indiv$HOUSEID, levelcode.v=bigtable.outone.indiv$PRODUCTCODE, value.v=bigtable.outone.indiv$VALUE), 
                                  levelcode.labels=classsubandpro,
                                  levelcode.labels.by="products.code",
                                  saveto="../outputs/Consumption by product of households.xlsx")
    
    #View(conbyproduct.indiv)
    #colnames(conbyproduct.indiv)
    
    conbyproduct<-merge(conbyproduct.households,conbyproduct.indiv[,c(1,(ncol(conbyproduct.indiv)-1):ncol(conbyproduct.indiv))], by="Code")
    conbyproduct$PROPGASTO<-(conbyproduct$`Total consumption.y`/sum(conbyproduct$`Total consumption.y`))*100
    colnames(conbyproduct)<-c("Product code",
                              "Class code",
                              "Class name (Spanish)",
                              "Class name (English)",
                              "Class name (French)",
                              "Subclass code",
                              "Subclass name (Spanish)",
                              "Subclass name (English)",
                              "Subclass description (English)",
                              "Product name (Spanish)",
                              "Total household monthly consumption",
                              "Number of consumer households",
                              "Average household monthly consumption",
                              "Standard deviation",
                              "Average individual monthly consumption",
                              "Standard deviation",
                              "Budget share")
    View(conbyproduct)
    write.xlsx2(conbyproduct, "../outputs/Consumption by product of households and individuals.xlsx")

    #sum(conbyproduct$PROPGASTO)
###############################################################################################

###############################################################################################
# Table consumption of class
#####    
  # Aggregated table of consumption by class
  conbyclass<-agg.consu.sum(df.src=data.frame(id.v=bigtable.outone$HOUSEID, levelcode.v=bigtable.outone$CLASSCODE, value.v=bigtable.outone$VALUE), 
                                         levelcode.labels=subclass.dane, 
                                         levelcode.labels.by="subclass.code",
                                         saveto="../outputs/Consumption by class .xlsx")
  View(conbyclass)
  
  
###############################################################################################
 

  