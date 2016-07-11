
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

merge.setdiff<- function(df1, df2, by.data="CODIGO_ENIG", all.data=TRUE){
  merge(df1, df2[, c(by.data, setdiff(colnames(df2),colnames(df1)))], by=by.data, all=all.data)
}

add.cond.var<-function(basedf, id.v, value.v, condition.v, output.df, name.string){
  temp<-basedf[, c(id.v, value.v, condition.v)]
  temp<-as.data.frame(temp)
  temp[,2]<-as.numeric(as.character(temp[,2]))
  temp[,2]<-ifelse(temp[,2]==98, NA, temp[,2])
  temp[,2]<-ifelse(temp[,2]==99, NA, temp[,2])
  temp$new<-ifelse(temp[,3]=="2", temp[,2], NA)
  colnames(temp)<-c(id.v, value.v, condition.v, name.string)
  output.df<-merge(output.df, temp[,c(name.string,id.v )], by=id.v, all=TRUE) 
  return(output.df)
}

add.nor.var<-function(basedf, id.v, value.v, output.df){
  temp<-basedf[, c(id.v, value.v)]
  temp<-as.data.frame(temp)
  temp[,2]<-as.numeric(as.character(temp[,2]))
  temp[,2]<-ifelse(temp[,2]==98, NA, temp[,2])
  temp[,2]<-ifelse(temp[,2]==99, NA, temp[,2])
  colnames(temp)<-c(id.v, value.v)
  output.df<-merge(output.df, temp[,c(value.v,id.v )], by=id.v, all=TRUE) 
  return(output.df)
}

add.nor.var.yearly<-function(basedf, id.v, value.v, output.df){
  temp<-basedf[, c(id.v, value.v)]
  temp<-as.data.frame(temp)
  temp[,2]<-as.numeric(as.character(temp[,2]))
  temp[,2]<-ifelse(temp[,2]==98, NA, temp[,2])
  temp[,2]<-ifelse(temp[,2]==99, NA, temp[,2])
  temp[,2]<-temp[,2]/12
  colnames(temp)<-c(id.v, value.v)
  output.df<-merge(output.df, temp[,c(value.v,id.v )], by=id.v, all=TRUE) 
  return(output.df)
}

add.nor.var.nmes<-function(basedf, id.v, value.v, output.df, nmes){
  temp<-basedf[, c(id.v, value.v, nmes)]
  temp<-as.data.frame(temp)
  temp[,2]<-as.numeric(as.character(temp[,2]))
  temp[,3]<-as.numeric(as.character(temp[,3]))
  temp[,2]<-ifelse(temp[,2]==98, NA, temp[,2])
  temp[,2]<-ifelse(temp[,2]==99, NA, temp[,2])
  temp[,2]<-ifelse(temp[,3]!=0, temp[,2]/temp[,3], NA)
  colnames(temp)<-c(id.v, value.v)
  output.df<-merge(output.df, temp[,c(value.v,id.v )], by=id.v, all=TRUE) 
  return(output.df)
}  


add.nor.var.yearly.households<-function(basedf, id.v, value.v, output.df){
  temp<-basedf[, c(id.v, value.v)]
  temp<-as.data.frame(temp)
  temp[,2]<-as.numeric(as.character(temp[,2]))
  temp[,2]<-ifelse(temp[,2]==98, NA, temp[,2])
  temp[,2]<-ifelse(temp[,2]==99, NA, temp[,2])
  temp[,2]<-temp[,2]/12
  colnames(temp)<-c(id.v, "value.name")
  temp$HOUSEID<-substr(temp[,1], 1, 7)
  output.temp<-summarise(group_by(temp, HOUSEID), sum(value.name, na.rm = TRUE))
  colnames(output.temp)<-c("HOUSEID", value.v)
  output.df<-merge(output.df, output.temp, by="HOUSEID", all=TRUE) 
  return(output.df)
}

add.nor.var.households<-function(basedf, id.v, value.v, output.df){
  temp<-basedf[, c(id.v, value.v)]
  temp<-as.data.frame(temp)
  temp[,2]<-as.numeric(as.character(temp[,2]))
  temp[,2]<-ifelse(temp[,2]==98, NA, temp[,2])
  temp[,2]<-ifelse(temp[,2]==99, NA, temp[,2])
  colnames(temp)<-c(id.v, "value.name")
  temp$HOUSEID<-substr(temp[,1], 1, 7)
  output.temp<-summarise(group_by(temp, HOUSEID), sum(value.name, na.rm = TRUE))
  colnames(output.temp)<-c("HOUSEID", value.v)
  output.df<-merge(output.df, output.temp, by="HOUSEID", all=TRUE) 
  return(output.df)
}

add.nor.var.ali.mes<-function(basedf, id.v, value.v, valuepag.v, output.df, name.v){
  temp<-basedf[, c(id.v, value.v, valuepag.v)]
  temp<-as.data.frame(temp)
  temp[,2]<-as.numeric(as.character(temp[,2]))
  temp[,2]<-ifelse(temp[,2]==98, NA, temp[,2])
  temp[,2]<-ifelse(temp[,2]==99, NA, temp[,2])
  
  temp[,3]<-as.numeric(as.character(temp[,3]))
  temp[,3]<-ifelse(temp[,3]==98, NA, temp[,3])
  temp[,3]<-ifelse(temp[,3]==99, NA, temp[,3])
  
  temp[,2]<-(temp[,2]-temp[,3])*30
  colnames(temp)<-c(id.v, name.v, valuepag.v)
  output.df<-merge(output.df, temp[,c(name.v,id.v )], by=id.v, all=TRUE) 
  return(output.df)
}

add.nor.var.ali.mes.households<-function(basedf, id.v, value.v, valuepag.v, output.df, name.v){
  temp<-basedf[, c(id.v, value.v, valuepag.v)]
  temp<-as.data.frame(temp)
  temp[,2]<-as.numeric(as.character(temp[,2]))
  temp[,2]<-ifelse(temp[,2]==98, NA, temp[,2])
  temp[,2]<-ifelse(temp[,2]==99, NA, temp[,2])
  
  temp[,3]<-as.numeric(as.character(temp[,3]))
  temp[,3]<-ifelse(temp[,3]==98, NA, temp[,3])
  temp[,3]<-ifelse(temp[,3]==99, NA, temp[,3])
  
  temp[,2]<-(temp[,2]-temp[,3])*30
  temp<-temp[,1:2]
  colnames(temp)<-c(id.v, "value.name")
  temp$HOUSEID<-substr(temp[,1], 1, 7)
  output.temp<-summarise(group_by(temp, HOUSEID), sum(value.name, na.rm = TRUE))
  colnames(output.temp)<-c("HOUSEID", name.v)
  output.df<-merge(output.df, output.temp, by="HOUSEID", all=TRUE) 
  return(output.df)
}


agg.consu.sum<-function(df.src, levelcode.labels, levelcode.labels.by, saveto="../outputs/name.xlsx"){
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

agg.consu.sum2<-function(df.src, levelcode.labels, levelcode.labels.by, saveto="../outputs/name.xlsx"){
  #df.src=data.frame(id.v, levelcode.v, value.v)  
  ptable<-cast(df.src, id.v ~ levelcode.v, sum, value="value.v")
  
  # Sums of values 
  ptable.tvalue<-as.data.frame(colSums(ptable))
  # Times where value>0 in each column
  ptable.myNumCols <- which(unlist(lapply(ptable, is.numeric)))
  ptable.number<- as.data.frame(sapply(ptable[, ptable.myNumCols], function(x)(sum(x > 0, na.rm=TRUE))))
  ptable.sd<- as.data.frame(sapply(ptable[, ptable.myNumCols], function(x)(sd(x, na.rm=TRUE))))
  #x[x>0]
  ptable.conbylevel<-merge(ptable.tvalue, ptable.number, by.x=0, by.y=0, all=TRUE)
  ptable.conbylevel$AVERAGE<-ptable.conbylevel[,2]/ptable.conbylevel[,3]
  
  colnames(ptable.conbylevel)<-c("Code", "Total consumption", "Numer of consumer households", "Average consumption")
  # Labels for the levelcode variable
  ptable.conbylevel1<-merge(ptable.conbylevel[,1:2], levelcode.labels, by.x=1, by.y=levelcode.labels.by, all.x=TRUE)
  ptable.conbylevel<-merge(ptable.conbylevel1, ptable.conbylevel, by="Code", all=TRUE)
  print(colnames(ptable.conbylevel))
  ptable.conbylevel<-ptable.conbylevel[,c(1,3:ncol(ptable.conbylevel))]
  ptable.conbylevel<-merge(ptable.conbylevel, ptable.sd, by.x="Code", by.y=0, all=TRUE)
  
  write.xlsx2(ptable.conbylevel, saveto)
  return(ptable.conbylevel)
}


add.rlvnt.houshld.inf<-function(table.df, houseid.v, houses.df, households.df, individuals.df){
  print("All modules should contain the VIVIENDA and HOUSEID variables")
  table.df$`Total food consumption`<-rowSums(table.df[,2:ncol(table.df)])
  table.df$VIVIENDA<-substr(houseid.v, 1, 5)
  o<-merge(table.df, houses.df, by="VIVIENDA", all.x=TRUE)
  o<-merge(o, households.df, by="HOUSEID")
  o<-merge(o, individuals.df, by="HOUSEID",all.x=TRUE)
  o[o== ""] <- NA
  return(o)
}


bigtable.full<-function(df.src, houses.df, households.df, individuals.df){
  #df.src=data.frame(id.v, levelcode.v, levelname.v, value.v)
  print("All modules, but df.src, should contain the VIVIENDA and HOUSEID variables")
  table.df<-cast(df.src, id.v ~ levelcode.v + levelname.v, sum, value="value.v")
  #print(colnames(table.df))
  table.df$`Total food consumption`<-rowSums(table.df[,2:ncol(table.df)])
  table.df$VIVIENDA<-substr(table.df$id.v, 1, 5)
  o<-merge(table.df, houses.df, by="VIVIENDA", all.x=TRUE)
  o<-merge(o, households.df, by.x="id.v", by.y="HOUSEID", all.x=TRUE)
  o<-merge(o, individuals.df, by="id.v",by.y="HOUSEID", all.x=TRUE)
  o[o== ""] <- NA
  return(o)
} 


bigtable.full.individuals<-function(df.src, houses.df, households.df, individuals.df){
  #df.src=data.frame(id.v, levelcode.v, levelname.v, value.v)
  print("All modules, but df.src, should contain the VIVIENDA and HOUSEID variables")
  table.df<-cast(df.src, id.v ~ levelcode.v + levelname.v, sum, value="value.v")
  #print(colnames(table.df))
  table.df$`Total food consumption`<-rowSums(table.df[,2:ncol(table.df)])
  myNumCols <- which(unlist(lapply(table.df, is.numeric)))
  numberofindi<-merge(table.df, individuals.df, by="id.v",by.y="HOUSEID", all.x=TRUE)
  table.df$vector<-numberofindi$`Number of people inside the household`
  table.df[, myNumCols]<-sweep(table.df[, myNumCols],1,table.df$vector,`/`)
    #sapply(table.df[, myNumCols], function(x)(x/2))
  table.df$VIVIENDA<-substr(table.df$id.v, 1, 5)
  o<-merge(table.df, houses.df, by="VIVIENDA", all.x=TRUE)
  o<-merge(o, households.df, by.x="id.v", by.y="HOUSEID", all.x=TRUE)
  o<-merge(o, individuals.df, by="id.v",by.y="HOUSEID", all.x=TRUE)
  o[o== ""] <- NA
  return(o)
} 

bigtable.full.individuals<-function(df.src, houses.df, households.df, individuals.df){
  #df.src=data.frame(id.v, levelcode.v, levelname.v, value.v)
  print("All modules, but df.src, should contain the VIVIENDA and HOUSEID variables")
  table.df<-cast(df.src, id.v ~ levelcode.v + levelname.v, sum, value="value.v")
  #print(colnames(table.df))
  table.df$`Total food consumption`<-rowSums(table.df[,2:ncol(table.df)])
  myNumCols <- which(unlist(lapply(table.df, is.numeric)))
  numberofindi<-merge(table.df, individuals.df, by="id.v",by.y="HOUSEID", all.x=TRUE)
  table.df$vector<-numberofindi$`Number of people inside the household`
  table.df[, myNumCols]<-sweep(table.df[, myNumCols],1,table.df$vector,`/`)
  #sapply(table.df[, myNumCols], function(x)(x/2))
  table.df$VIVIENDA<-substr(table.df$id.v, 1, 5)
  o<-merge(table.df, houses.df, by="VIVIENDA", all.x=TRUE)
  o<-merge(o, households.df, by.x="id.v", by.y="HOUSEID", all.x=TRUE)
  o<-merge(o, individuals.df, by="id.v",by.y="HOUSEID", all.x=TRUE)
  o[o== ""] <- NA
  return(o)
} 


bigtable.outliers.individuals<-function(df.src, houses.df, households.df, individuals.df){
  #df.src=data.frame(id.v, levelcode.v, levelname.v, value.v)
  print("All modules, but df.src, should contain the VIVIENDA and HOUSEID variables")
  table.df<-cast(df.src, id.v ~ levelcode.v + levelname.v, sum, value="value.v")
  #print(colnames(table.df))
  #table.df$`Total food consumption`<-rowSums(table.df[,2:ncol(table.df)])
  myNumCols <- which(unlist(lapply(table.df, is.numeric)))
  numberofindi<-merge(table.df, individuals.df, by="id.v",by.y="HOUSEID", all.x=TRUE)
  table.df$vector<-numberofindi$`Number of people inside the household`
  table.df[, myNumCols]<-sweep(table.df[, myNumCols],1,table.df$vector,`/`)
  table.df[table.df== 0]<-NA
  table.df[, myNumCols]<-sapply(table.df[, myNumCols], function(x){
    qnt <- quantile(x, probs=c(.50), na.rm = T)
    print(qnt)
    H <- 6* IQR(x, na.rm = T)
    print(H)
    x<-ifelse(x > (qnt + H), 0, x)
    print(length(x[x > (qnt + H)]))
  })

  table.df[is.na(table.df)]<-0.0001
  table.df[table.df== 0]<-NA
  table.df[table.df== 0.0001]<-0
  o<-na.omit(table.df)
  #sapply(table.df[, myNumCols], function(x)(x/2))
  #   table.df$VIVIENDA<-substr(table.df$id.v, 1, 5)
  #   o<-merge(table.df, houses.df, by="VIVIENDA", all.x=TRUE)
  #   o<-merge(o, households.df, by.x="id.v", by.y="HOUSEID", all.x=TRUE)
  #   o<-merge(o, individuals.df, by="id.v",by.y="HOUSEID", all.x=TRUE)
  # o[o== ""] <- NA
  return(o)
} 


bigtable.outlierssimple.individuals<-function(df.src, houses.df, households.df, individuals.df){
  #df.src=data.frame(id.v, levelcode.v, levelname.v, value.v)
  print("All modules, but df.src, should contain the VIVIENDA and HOUSEID variables")
  table.df<-cast(df.src, id.v ~ levelcode.v + levelname.v, sum, value="value.v")
  #print(colnames(table.df))
  #table.df$`Total food consumption`<-rowSums(table.df[,2:ncol(table.df)])
  myNumCols <- which(unlist(lapply(table.df, is.numeric)))
  numberofindi<-merge(table.df, individuals.df, by="id.v",by.y="HOUSEID", all.x=TRUE)
  table.df$vector<-numberofindi$`Number of people inside the household`
  table.df[, myNumCols]<-sweep(table.df[, myNumCols],1,table.df$vector,`/`)
  table.df[, myNumCols]<-sapply(table.df[, myNumCols], function(x){
    qnt <- quantile(x, probs=c(.50), na.rm = T)
    print(qnt)
    H <- 6* IQR(x, na.rm = T)
    print(H)
    x<-ifelse(x > (qnt + H), NA, x)
    print(length(x[x > (qnt + H)]))
  })
  o<-table.df
  #sapply(table.df[, myNumCols], function(x)(x/2))
  #   table.df$VIVIENDA<-substr(table.df$id.v, 1, 5)
  #   o<-merge(table.df, houses.df, by="VIVIENDA", all.x=TRUE)
  #   o<-merge(o, households.df, by.x="id.v", by.y="HOUSEID", all.x=TRUE)
  #   o<-merge(o, individuals.df, by="id.v",by.y="HOUSEID", all.x=TRUE)
  # o[o== ""] <- NA
  return(o)
} 

bigtable.outliersreplace.individuals<-function(df.src, houses.df, households.df, individuals.df){
  #df.src=data.frame(id.v, levelcode.v, levelname.v, value.v)
  print("All modules, but df.src, should contain the VIVIENDA and HOUSEID variables")
  table.df<-cast(df.src, id.v ~ levelcode.v + levelname.v, sum, value="value.v")
  myNumCols <- which(unlist(lapply(table.df, is.numeric)))
  numberofindi<-merge(table.df, individuals.df, by="id.v",by.y="HOUSEID", all.x=TRUE)
  table.df$vector<-numberofindi$`Number of people inside the household`
  table.df[, myNumCols]<-sweep(table.df[, myNumCols],1,table.df$vector,`/`)
  table.df[table.df== 0]<-NA
  #http://r-statistics.co/Outlier-Treatment-With-R.html
  table.df[, myNumCols]<-sapply(table.df[, myNumCols], function(x){
    qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
    caps <- quantile(x, probs=c(.05, .95), na.rm = T)
    H <- 1.5 * IQR(x, na.rm = T)
    print("Limit: ")
    print(qnt[2] + H)
    print("Replaced value: ")
    print(caps[2])
    #x[x < (qnt[1] - H)] <- caps[1]
    x[x > (qnt[2] + H)] <- caps[2]
    print("# of replaced values: ")
    print(length(x[x > (qnt[2] + H)]))
  })
  
  return(table.df)
} 

outliers.delete<-function(x){
  qnt <- quantile(x, probs=c(.50), na.rm = T)
  print(qnt)
  H <- 6* IQR(x, na.rm = T)
  print(H)
  x<-ifelse(x > (qnt + H), 0, x)
  print(length(x[x > (qnt + H)]))
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  #y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

remove_outliers2 <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .50), na.rm = na.rm, ...)
  H <- 10 * IQR(x, na.rm = na.rm)
  y <- x
  #y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

remove_outliers3 <- function(x, na.rm = TRUE, ...) {
  x[x==0]<-NA
  qnt <- quantile(x, probs=c(.25, .50), na.rm = na.rm, ...)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 20 * IQR(x, na.rm = na.rm)
  y <- x
  #y[x < (qnt[1] - H)] <- NA
  #y[x > (qnt[2] + H)] <- caps[2]
  y[x > (qnt[2] + H)] <- 0.001
  y[is.na(y)]<-0
  y[y==0.001]<-NA
  y
}

remove_outliers4 <- function(x, na.rm = TRUE, ...) {
  x[x==0]<-NA
  qnt <- quantile(x, probs=c(.25, .50), na.rm = na.rm, ...)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 30 * IQR(x, na.rm = na.rm)
  y <- x
  #y[x < (qnt[1] - H)] <- NA
  #y[x > (qnt[2] + H)] <- caps[2]
  y<-ifelse( y > (qnt[2] + H),ifelse(y<3000, y,caps[2]), y)
  y[is.na(y)]<-0
  y[y==0.001]<-NA
  y
}

graph.boxplot<-function(y.v, df.src=foodandinc){
  df<-df.src[, c(y.v, "Quintile number", "Quintile")]
  y.name<-substr(y.v,1,4)
  colnames(df)<-c("CLASS", "Quintile number", "Quintile")
  bplot<- ggplot(df, aes(x=`Quintile number`, y=CLASS, fill=Quintile)) + 
    geom_boxplot()+labs(x = "Quintile number", y=y.name)
  bplot
  #ggsave("0111_Bread and cereals.png", scale=0.9)
  ggsave(file=paste("../outputs/", y.v, "_boxplot.png"), scale=0.9)
}

graph.boxplot.nova<-function(y.v, df.src=foodandinc.nova){
  df<-df.src[, c(y.v, "Quintile number", "Quintile")]
  y.name<-substr(y.v,1,1)
  colnames(df)<-c("CLASS", "Quintile number", "Quintile")
  bplot<- ggplot(df, aes(x=`Quintile number`, y=CLASS, fill=Quintile)) + 
    geom_boxplot()+labs(x = "Quintile number", y=y.name)
  bplot
  #ggsave("0111_Bread and cereals.png", scale=0.9)
  ggsave(file=paste("../outputs/", y.v, "_boxplot.png"), scale=0.9)
}
