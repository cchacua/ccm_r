#############################################################################################
#Food expenses
#############################################################################################
library(datasets)
library(dplyr)
library(xlsx)
library(reshape)
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
  View(bigtable.outone)
  
###############################################################################################  

###############################################################################################
# Table consumption of food products
#####
  conbyproduct<-agg.consu.sum(df.src=data.frame(id.v=bigtable.outone$HOUSEID, levelcode.v=bigtable.outone$PRODUCTCODE, value.v=bigtable.outone$VALUE), 
                                         levelcode.labels=classsubandpro,
                                         levelcode.labels.by="products.code",
                                         saveto="../outputs/Consumption by product .xlsx")
  View(conbyproduct)
   
###############################################################################################

###############################################################################################
# Table consumption of food products
#####    
  # Aggregated table of consumption by class
  conbyclass<-agg.consu.sum(df.src=data.frame(id.v=bigtable.outone$HOUSEID, levelcode.v=bigtable.outone$CLASSCODE, value.v=bigtable.outone$VALUE), 
                                         levelcode.labels=subclass.dane, 
                                         levelcode.labels.by="subclass.code",
                                         saveto="../outputs/Consumption by class .xlsx")
  #View(conbyclass)
###############################################################################################
  
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
  write.xlsx2(houseandclass.addedmod,"../outputs/houseandclass.addedmod.xlsx")
  houseandclass.addedmod.indi<-bigtable.full.individuals(df.src=data.frame(id.v=bigtable.outone.wclass$HOUSEID, levelcode.v=bigtable.outone.wclass$CLASSCODE, levelname.v=bigtable.outone.wclass$subclass.name.eng, value.v=bigtable.outone.wclass$VALUE),
                                                         houses.df=houses, 
                                                         households.df=households, 
                                                         individuals.df=individuals)
  write.xlsx2(houseandclass.addedmod.indi,"../outputs/houseandclass.addedmod.indi.xlsx")
  #View(houseandclass.addedmod.indi)
#############################################################################################
  
  
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
  
  write.xlsx2(houseandproduct.addedmod,"../outputs/houseandproduct.addedmod.xlsx")
  
  houseandproduct.addedmod.indi<-bigtable.full.individuals(df.src=data.frame(id.v=bigtable.outone.wpro$HOUSEID, levelcode.v=bigtable.outone.wpro$PRODUCTCODE, levelname.v=bigtable.outone.wpro$products.name.es, value.v=bigtable.outone.wpro$VALUE),
                                          houses.df=houses, 
                                          households.df=households, 
                                          individuals.df=individuals)
  View(houseandproduct.addedmod.indi)
  write.xlsx2(houseandproduct.addedmod.indi,"../outputs/houseandproduct.addedmod.indi.xlsx")

#############################################################################################
  
  