
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