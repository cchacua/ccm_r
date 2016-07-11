#############################################################################################
# Food Consumption and Income Modules
#############################################################################################

#############################################################################################
# Food and income by DANE class
#####
  
  foodandinc<-merge(houseandclass.14.addedmod.indi, totalincome, by.x="id.v", by.y="HOUSEID")
  colnames(foodandinc)
  
  
  #Boxplot graphs
  # Example: graph.boxplot(y.v="0111_Bread and cereals")
  sapply(colnames(foodandinc[,3:16]), graph.boxplot)
  #c("0111_Bread and cereals", "Quintile")
  foodandinc.table<-as.data.frame(describeBy(foodandinc[,c(3:16, 47)], group="Quintile", mat=TRUE, digits=2))
  foodandinc.table<-foodandinc.table[1:(nrow(foodandinc.table)-5),]
  write.xlsx2(foodandinc.table,"../outputs/Descriptive Food Class statistics by group.xlsx")
  

#############################################################################################
# Food and income by NOVA class
#####
  
  foodandinc.nova<-merge(houseandclass.nova.addedmod.indi, totalincome, by.x="id.v", by.y="HOUSEID")
  colnames(foodandinc.nova)
  
  
  #Boxplot graphs
  sapply(colnames(foodandinc.nova[,3:7]), graph.boxplot.nova)
  #c("0111_Bread and cereals", "Quintile")
  foodandinc.nova.table<-as.data.frame(describeBy(foodandinc.nova[,c(3:7, 38)], group="Quintile", mat=TRUE, digits=2))
  foodandinc.nova.table<-foodandinc.nova.table[1:(nrow(foodandinc.nova.table)-5),]
  write.xlsx2(foodandinc.nova.table,"../outputs/Descriptive Food NOVA Class statistics by group.xlsx")
  
