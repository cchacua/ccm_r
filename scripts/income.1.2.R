#############################################################################################
# 1.2 Nominal monetary income of the expenditure unit
####
library(dplyr)


# 1.2.1 Ingreso corriente monetario de cada persona perteneciente a la unidad de gasto (UG) 
  i1.2<-summarise(group_by(i1.1, HOUSEID), sum(total, na.rm = TRUE))

#ML_HOGAR
# 1.2.2 Subsidios de vivienda en dinero que recibe el hogar 
  # P5160S1A1 En dinero	¿En los últimos doce meses, algún miembro del hogar recibió subsidio del gobierno o de otra institución para la compra o construcción de vivienda, casalote o lote?
  # /12
  i1.2<-add.nor.var.yearly(basedf=hogares.cali.ml, id.v="HOUSEID", value.v="P5160S1A1", output.df=i1.2)
  
# 1.2.3 Otros subsidios del gobierno en dinero 
  # P5190S1A1	Este o estos subsidios los reciben: En dinero? - Cuánto recibieron en los últimos doce meses?
  # /12
  i1.2<-add.nor.var.yearly(basedf=hogares.cali.ml, id.v="HOUSEID", value.v="P5190S1A1", output.df=i1.2)

#ML_PERSONA
# 1.2.4 Becas para estudiar que reciben las personas de la UG 
  # P6190S1	¿Durante los últimos doce meses, ... recibió beca u subsidio de alguna institución para estudiar? - Cuánto recibió?
  # /12
  # Aggregate for the number of people
  i1.2<-add.nor.var.yearly.households(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6190S1", output.df=i1.2)

  # 1.2.5 Préstamos para educación que reciben las personas de la UG
  # P6200S1	¿Durante los últimos doce meses, recibió ... prestamos de alguna institución, para estudiar? - Cuánto recibió?
  # /12
  # Aggregate for the number of people
  i1.2<-add.nor.var.yearly.households(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6200S1", output.df=i1.2)

  i1.2$total<-rowSums(i1.2[,2:ncol(i1.2)], na.rm=TRUE)
  