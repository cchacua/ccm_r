#####################################################################################################
# Household modules
#####################################################################################################

#Income - expenses module
Ig_gs_hogar<-read.delim(modules[1], header=TRUE, colClasses = "character")
hogares.cali.ig<-merge(viviendas.cali[,c(1,3)], Ig_gs_hogar, by="VIVIENDA")
rm(Ig_gs_hogar)

#Labor market module
Ig_ml_hogar<-read.delim(modules[19], header=TRUE, colClasses = "character")
hogares.cali.ml<-merge(viviendas.cali[,c(1,3)], Ig_ml_hogar, by="VIVIENDA")
#nrow(unique(data.frame(hogares.cali.ml$VIVIENDA,hogares.cali.ml$FACTOR_EXPANSION_EC_E1)))
rm(Ig_ml_hogar)

#hogares.cali<-merge(hogares.cali.ig,hogares.cali.ml, by="CODIGO_ENIG")
#rm(hogares.cali)

#####################################################################################################
# Households: relevant variables
#####################################################################################################

  households1<-hogares.cali.ig[,c("CODIGO_ENIG","GDU_GASTO_DSGRGDO_ALMNTO","GDU_GASTO_FSTA_ENCSTA", "GMF_NMRO_ORDEN_PRSNA")]
  households2<-hogares.cali.ml[,c("CODIGO_ENIG", "P5000", "P5010", "P5070", "P5080","P5090","P5240","P5250","P5230")]
  households<-merge(households1,households2, by="CODIGO_ENIG")
  rm(households1, households2)
  households$HOUSEID<-substr(households$CODIGO_ENIG, 1, 7)
  # P5000	Incluyendo sala-comedor ¿de cuántos cuartos en total dispone este hogar?
  # P5010	¿En cuántos de esos cuartos duermen las personas de este hogar?
  # P5070	¿En que sitio de la vivienda preparan los alimentos las personas de este hogar?
  households$P5070<-recode(households$P5070,
                                            "1"= "En un cuarto usado sólo para cocinar",
                                            "2"="En un cuarto usado también para dormir",
                                            "3"="En una sala-comedor con lavaplatos",
                                            "4"="En una sala-comedor sin lavaplatos",
                                            "5"="En un patio, corredor, enramada, al aire libre",
                                            "6"="En ninguna parte, no preparan alimentos")
  # P5080	¿Con qué energía o combustible cocinan principalmente en este hogar?
  households$P5080<-recode(households$P5080,
                           "1" ="Electricidad",
                           "2" ="Petróleo, gasolina, kerosene, alcohol",
                           "3" ="Gas natural conectado a red pública",
                           "4" ="Gas propano en cilindro o pipeta",
                           "5" ="Leña, madera o carbón de leña",
                           "6" ="Carbón mineral",
                           "7" ="Materiales de desecho")
  # P5090	La vivienda ocupada por este hogar es:
  households$P5090<-recode(households$P5090,
                                            "1"="Propia, totalmente pagada",
                                            "2"="Propia, la están pagando",
                                            "3" = "En arriendo o subarriendo",
                                            "4"= "En usufructo",
                                            "5"= "Posesión sin título (ocupante de hecho) o propiedad colectiva",
                                            "6"= "Otra")
  # P5240	Usted considera que los ingresos mensuales de su hogar:
  households$P5240<-recode(households$P5240,
                           "1"= "Son más que suficientes para cubrir los gastos básicos del hogar",
                           "2"= "Son suficientes para cubrir los gastos básicos del hogar",
                           "3"= "No alcanzan para cubrir los gastos básicos del hogar")
  
  # P5250	¿Cuál considera que debería ser el ingreso mínimo mensual que requiere este hogar para satisfacer adecuadamente sus necesidades?
  # P5230	¿Usted se considera pobre?
  #households$P5230<-recode(households$P5230,"1"="Sí", "2"="No")
  households$P5230<-recode(households$P5230,"1"="Yes", "2"="No")
  
  households[households== ""] <- NA
  