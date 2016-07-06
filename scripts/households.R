#####################################################################################################
# Household modules
#####################################################################################################
library(datasets)
library(dplyr)
library(xlsx)
library(reshape)

#Income - expenses module
Ig_gs_hogar<-read.delim(modules[1], header=TRUE, colClasses = "character")
hogares.cali.ig<-merge(viviendas.cali[,c(1,3)], Ig_gs_hogar, by="VIVIENDA")
rm(Ig_gs_hogar)

#Labor market module
Ig_ml_hogar<-read.delim(modules[19], header=TRUE, colClasses = "character")
hogares.cali.ml<-merge(viviendas.cali[,c(1,3)], Ig_ml_hogar, by="VIVIENDA")
hogares.cali.ml$HOUSEID<-substr(households$CODIGO_ENIG, 1, 7)
#nrow(unique(data.frame(hogares.cali.ml$VIVIENDA,hogares.cali.ml$FACTOR_EXPANSION_EC_E1)))
rm(Ig_ml_hogar)

#hogares.cali<-merge(hogares.cali.ig,hogares.cali.ml, by="CODIGO_ENIG")
#rm(hogares.cali)

#####################################################################################################
# Households: relevant variables
#####################################################################################################

  households1<-hogares.cali.ig[,c("CODIGO_ENIG","GDU_GASTO_DSGRGDO_ALMNTO","GDU_GASTO_FSTA_ENCSTA")]
  households2<-hogares.cali.ml[,c("CODIGO_ENIG", "P5000", "P5010", "P5070", "P5080","P5090","P5240","P5250","P5230")]
  households<-merge(households1,households2, by="CODIGO_ENIG", all=TRUE)
  rm(households1, households2)
  households$HOUSEID<-substr(households$CODIGO_ENIG, 1, 7)
  # GDU_GASTO_DSGRGDO_ALMNTO	2 Urbano - ¿ Se logró obtener el gasto desagregado para todos los alimentos anteriores?
  households$GDU_GASTO_DSGRGDO_ALMNTO<-recode(households$GDU_GASTO_DSGRGDO_ALMNTO,"1"="Yes", "2"="No")
  # GDU_GASTO_FSTA_ENCSTA	2 Urbano - ¿ Durante los siete días en que se registró la información de este formulario ¿el hogar tuvo algún gasto en alimentos debido a fiestas, reuniones o invitados?
  households$GDU_GASTO_FSTA_ENCSTA<-recode(households$GDU_GASTO_FSTA_ENCSTA,"1"="Yes", "2"="No")
  # P5000	Incluyendo sala-comedor ¿de cuántos cuartos en total dispone este hogar?
  households$P5000<-as.numeric(as.character(households$P5000))
  # P5010	¿En cuántos de esos cuartos duermen las personas de este hogar?
  households$P5010<-as.numeric(as.character(households$P5010))
  # P5070	¿En que sitio de la vivienda preparan los alimentos las personas de este hogar?
    #   households$P5070<-recode(households$P5070,
    #                                             "1"= "En un cuarto usado sólo para cocinar",
    #                                             "2"="En un cuarto usado también para dormir",
    #                                             "3"="En una sala-comedor con lavaplatos",
    #                                             "4"="En una sala-comedor sin lavaplatos",
    #                                             "5"="En un patio, corredor, enramada, al aire libre",
    #                                             "6"="En ninguna parte, no preparan alimentos")
  households$P5070<-recode(households$P5070,
                                            "1" = "In a room used only for cooking",
                                            "2" = "In a room also used to sleep",
                                            "3" = "In a dining room with dishwasher",
                                            "4" = "In a living room without dishwasher",
                                            "5" = "In a courtyard, corridor, bower, outdoor",
                                            "6" = "Nowhere, do not prepare food")
  # P5080	¿Con qué energía o combustible cocinan principalmente en este hogar?
      #   households$P5080<-recode(households$P5080,
      #                            "1" ="Electricidad",
      #                            "2" ="Petróleo, gasolina, kerosene, alcohol",
      #                            "3" ="Gas natural conectado a red pública",
      #                            "4" ="Gas propano en cilindro o pipeta",
      #                            "5" ="Leña, madera o carbón de leña",
      #                            "6" ="Carbón mineral",
      #                            "7" ="Materiales de desecho")
  households$P5080<-recode(households$P5080,
                                            "1" = "Electricity",
                                            "2" = "Oil, gasoline, kerosene, alcohol",
                                            "3" = "Natural gas connected to the public network",
                                            "4" = "Propane cylinder or pipette",
                                            "5" = "Firewood, wood or charcoal",
                                            "6" = "Coals",
                                            "7" = "Waste materials")
  # P5090	La vivienda ocupada por este hogar es:
    #   households$P5090<-recode(households$P5090,
    #                                             "1"="Propia, totalmente pagada",
    #                                             "2"="Propia, la están pagando",
    #                                             "3" = "En arriendo o subarriendo",
    #                                             "4"= "En usufructo",
    #                                             "5"= "Posesión sin título (ocupante de hecho) o propiedad colectiva",
    #                                             "6"= "Otra")
  households$P5090<-recode(households$P5090,
                                              "1" = "Own home, fully paid",
                                              "2" = "Own home, still paying",
                                              "3" = "Rented or sublet",
                                              "4" = "Taken in usufruct",
                                              "5" = "Possession Untitled or collective ownership",
                                              "6" = "Another")
  # P5240	Usted considera que los ingresos mensuales de su hogar:
    #   households$P5240<-recode(households$P5240,
    #                            "1"= "Son más que suficientes para cubrir los gastos básicos del hogar",
    #                            "2"= "Son suficientes para cubrir los gastos básicos del hogar",
    #                            "3"= "No alcanzan para cubrir los gastos básicos del hogar")
  households$P5240<-recode(households$P5240,
                                            "1" = "is more than enough to cover basic household expenses",
                                            "2" = "is sufficient to cover basic household expenses",
                                            "3" = "is not enough to cover basic household expenses")
  # P5250	¿Cuál considera que debería ser el ingreso mínimo mensual que requiere este hogar para satisfacer adecuadamente sus necesidades?
  households$P5250<-as.numeric(as.character(households$P5250))
  households$P5250[households$P5250== 98] <- NA
  households$P5250[households$P5250== 99] <- NA
  # P5230	¿Usted se considera pobre?
  #households$P5230<-recode(households$P5230,"1"="Sí", "2"="No")
  households$P5230<-recode(households$P5230,"1"="Yes", "2"="No")
  
  households[households== ""] <- NA
  households<-households[,2:ncol(households)]
  colnames(households)<-c("Were disaggregated food expenses expenses recorded?",
                          "During the collection period, were there food expenditures associated to parties, meetings or similar?",
                          "Including living room, how many rooms does this household have?",
                          "How of those rooms are used to sleep by the household's members?",
                          "Where do household people prepare food?",
                          "Which is the mainly source of heat for cooking in this household?",
                          "The property occupied by this household is:",
                          "You think that your monthly household income:",
                          "What do you think should be the minimum monthly income required by this household to adequately meet its needs?",
                          "Do you consider yourself poor?",
                          "HOUSEID")
  
  households.myNumCols<- which(unlist(lapply(households, is.numeric)))
  households.myCharCols<- which(unlist(lapply(households, is.character)))
  households[, households.myCharCols][is.na(households[, households.myCharCols])] <- "Not available"
  #In numeric variables, NA values are replaced with 0
  households[, households.myNumCols][is.na(households[, households.myNumCols])] <- 0
  