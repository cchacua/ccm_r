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

  #Load classes names
    subclass.dane<-read.xlsx(m.labels[1], sheetName="subclass.dane",encoding="UTF-8")
    #subclass.dane.key<-gs_key("11cq-icYs_BcpJ0yBBiRllN5KnkPJxdGky1PmhUYIvrA")
    # A command/browser prompt will appear for giving access to the Google Spreadsheet
    #subclass.dane<-gs_read_csv(subclass.dane.key)
    #rm(subclass.dane.key<-gs_key)
    
  #Load products names
  products.dane<-read.xlsx(m.labels[2], sheetName="products.dane",encoding="UTF-8")
  
  #Subclass and products
  products.dane$CLASSCODE<-substr(products.dane$products.code, 1, 6)
  subandpro<-merge(subclass.dane, products.dane, by.x="subclass.code", by.y="CLASSCODE", all=TRUE)

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
                                         levelcode.labels=subandpro,
                                         levelcode.labels.by="products.code",
                                         saveto="../outputs/Consumption by product .xlsx")
  #View(bigtable.out.conbyproduct)
   
###############################################################################################

###############################################################################################
# Table consumption of food products
#####    
  # Aggregated table of consumption by class
  conbyclass<-agg.consu.sum(df.src=data.frame(id.v=bigtable.outone$HOUSEID, levelcode.v=bigtable.outone$CLASSCODE, value.v=bigtable.outone$VALUE), 
                                         levelcode.labels=subclass.dane, 
                                         levelcode.labels.by="subclass.code",
                                         saveto="../outputs/Consumption by class .xlsx")
  #View(bigtable.out.conbyclass)
###############################################################################################
  
   
#####
# House and DANE class table
#   
bigtable.outone.wclass<-merge(bigtable.outone, subclass.dane, by.x="CLASSCODE", by.y="subclass.code", all.x = TRUE)
bigtable.out.houseandclass<-cast(bigtable.outone.wclass, HOUSEID ~ CLASSCODE + subclass.name.eng , sum, value="VALUE")
bigtable.out.houseandclass.raw<-bigtable.out.houseandclass
#write.xlsx2(bigtable.out.houseandclass.raw,"../outputs/bigtable.out.houseandclass.raw.xlsx")

#############################################################################################
#Big table with added modules
#############################################################################################
bigtable.out.houseandclass.raw$VIVIENDA<-substr(bigtable.out.houseandclass.raw$HOUSEID, 1, 5)

bigtable.out.houseandclass.addedmod<-merge(bigtable.out.houseandclass.raw, houses, by="VIVIENDA", all.x=TRUE)
bigtable.out.houseandclass.addedmod<-merge(bigtable.out.houseandclass.addedmod, households, by="HOUSEID")
bigtable.out.houseandclass.addedmod<-merge(bigtable.out.houseandclass.addedmod, individuals, by="HOUSEID",all.x=TRUE)
bigtable.out.houseandclass.addedmod[bigtable.out.houseandclass.addedmod== ""] <- NA
write.xlsx2(bigtable.out.houseandclass.addedmod,"../outputs/bigtable.out.houseandclass.addedmod.xlsx")

# View(bigtable.out.houseandclass.addedmod)
