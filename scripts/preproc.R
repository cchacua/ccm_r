#####################################################################################################
# Pre-processed data sources
#####################################################################################################

hogares.preproc<-read.csv(m.preproc[1], header=TRUE, colClasses = "character")
#View(hogares.preproc)
rm(hogares.preproc)
hogares.cali.preproc<-merge(viviendas.cali[,c(1,3)], hogares.cali.preproc, by="VIVIENDA")
View(hogares.cali.preproc)
hogares.cali.preproc