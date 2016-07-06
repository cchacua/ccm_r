#####################################################################################################
# Pre-processed data sources
#####################################################################################################

hogares.preproc<-read.csv(m.preproc[1], header=TRUE, colClasses = "character")
hogares.cali.preproc<-merge(viviendas.cali[,c(1,3)], hogares.preproc, by="VIVIENDA")
rm(hogares.preproc)
#View(hogares.cali.preproc)
#hogares.cali.preproc

hogares.cali.preproc.new<-hogares.cali.preproc[,c("Iden_hog","ICMUG")]
hogares.cali.preproc.new$ICMUG<-as.numeric(as.character(hogares.cali.preproc.new$ICMUG))
hogares.cali.preproc.new<-merge(i1.2, hogares.cali.preproc.new, by.x="HOUSEID", by.y="Iden_hog", all=TRUE)
hogares.cali.preproc.new$equal<-hogares.cali.preproc.new$total==hogares.cali.preproc.new$ICMUG
