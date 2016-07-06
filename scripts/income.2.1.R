#############################################################################################
# 2.1 Nominal non-monetary income of the people inside the expenditure unit
####

  i2.1<-allindividuals[, c("CODIGO_ENIG", "HOUSEID")]

# 2.1.1 Alimentos como salario en especie
  # P6590S1	Además Del Salario En Dinero, ¿El Mes Pasado Recibió Alimentos Como Parte De Pago Por Su Trabajo? - ¿En Cuánto Estima Lo Que Recibió El Mes Pasado?(Si Recibió Pero No Sabe Estimar El Monto, Escriba 98)
  i2.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6590S1", output.df=i2.1)
# 2.1.2 Vivienda como salario en especie
  # P6600S1	Además Del Salario En Dinero, ¿El Mes Pasado Recibió Vivienda Como Parte De Pago Por Su Trabajo? - ¿En Cuánto Estima Lo Que Recibió El Mes Pasado? (Si Recibió Pero No Sabe Estimar El Monto, Escriba 98)
  i2.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6600S1", output.df=i2.1)
  
# 2.1.3 Transporte como salario en especie
  # P6610S1	¿Normalmente ... Utiliza Transporte De La Empresa Para Desplazarse A Su Trabajo (Bus O Automóvil)? - ¿En Cuánto Estima Lo Que Recibió El Mes Pasado?(Si Recibió Pero No Sabe Estimar El Monto, Escriba 98)
  i2.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6610S1", output.df=i2.1)
  
# 2.1.4 Otros ingresos en especie 
  # P6620S1	Además Del Salario En Dinero, ¿El Mes Pasado ... Recibió Otros Ingresos En Especie Por Su Trabajo (Electrodomésticos, Ropa, Productos Diferentes A Alimentos O Bonos Tipo Sodexho)? - ¿En Cuánto Estima Lo Que Recibió El Mes Pasado? (Si Recibió Pero No Sabe Estimar El Monto, Escriba 98)
  i2.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6620S1", output.df=i2.1)

# TOTAL VALUES  
  i2.1$total<-rowSums(i2.1[,3:ncol(i2.1)], na.rm=TRUE)    
  