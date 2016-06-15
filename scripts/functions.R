aggregate.twov= function(v1,v2, title1, title2){
  mini.df<-data.frame(v1,v2)
  mini.df<-na.omit(mini.df)
  out.df<-aggregate(as.numeric(as.character(mini.df[,2])), by=list(Category=mini.df[,1]), FUN=sum)
  colnames(out.df)<-c(title1, title2)
  out.df
}

aggregate.artquaval= function(v1,v2, v3, title1, title2, title3){
  mini.df<-data.frame(v1,v2, v3)
  mini.df<-na.omit(mini.df)
  out.df1<-aggregate(as.numeric(as.character(mini.df[,2])), by=list(Category=mini.df[,1]), FUN=sum)
  colnames(out.df1)<-c(title1, title2)
  out.df2<-aggregate(as.numeric(as.character(mini.df[,3])), by=list(Category=mini.df[,1]), FUN=sum)
  colnames(out.df2)<-c(title1, title3)
  out.df<-merge(out.df1,out.df2, by=title1)
  out.df
}
