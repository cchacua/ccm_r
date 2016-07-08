
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
    