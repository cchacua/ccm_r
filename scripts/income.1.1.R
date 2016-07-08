#############################################################################################
# 1.1 Nominal monetary income of the people inside the expenditure unit
####

# The dataframe is initialized only with the information of the individuals from Cali

  i1.1<-allindividuals[, c("CODIGO_ENIG", "HOUSEID")]

# 1.1.1 Sueldos y salarios
  # P6500 Antes de descuentos ¿cuánto ganó ... el mes pasado en este empleo?
  i1.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6500", output.df=i1.1)

# 1.1.2 Horas extras
  #El Mes Pasado Recibió Ingresos Por Concepto De Horas Extras? - ¿Cuánto Recibió? (P6510S1)                     
  #El Mes Pasado Recibió Ingresos Por Concepto De Horas Extras? - ¿Incluyó Este Valor En Los Ingresos Del Mes Pasado que me Declaró Anteriormente? (P6510S2) 1. Sí, 2. No
  #Sólo se colocan los ingresos por horas extras, si respondió que nó los había incluido
  i1.1<-add.cond.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6510S1", condition.v="P6510S2", output.df=i1.1, name.string="horasextras")
  
# 1.1.3 Auxilios y subsidios laborales
  # P6585S1A1	Cual(Es) De Los Siguientes Subsidios Recibió ... El Mes Pasado: - Auxilio O Subsidio De Alimentación? - ¿Cuánto? (Si Recibió Pero No Sabe El Monto, Escriba 98)
  # P6585S1A2	Cual(Es) De Los Siguientes Subsidios Recibió ... El Mes Pasado: - Auxilio O Subsidio De Alimentación? - ¿Incluyo Este Valor En Los Ingresos Del Mes Pasado Que Me Declaró Anteriormente?
  i1.1<-add.cond.var(allindividuals, "CODIGO_ENIG", value.v="P6585S1A1", condition.v="P6585S1A2", output.df=i1.1, name.string="sub.alimentacion")
  
  # P6585S2A1	Cual(Es) De Los Siguientes Subsidios Recibió ... El Mes Pasado: - Auxilio O Subsidio De Transporte? - ¿Cuánto? (Si Recibió Pero No Sabe El Monto, Escriba 98)
  # P6585S2A2	Cual(Es) De Los Siguientes Subsidios Recibió ... El Mes Pasado: - Auxilio O Subsidio De Transporte? - ¿Incluyo Este Valor En Los Ingresos Del Mes Pasado Que Me Declaró Anteriormente?
  i1.1<-add.cond.var(allindividuals, "CODIGO_ENIG", value.v="P6585S2A1", condition.v="P6585S2A2", output.df=i1.1, name.string="sub.transporte")
  
  # P6585S3A1	Cual(Es) De Los Siguientes Subsidios Recibió ... El Mes Pasado: - Subsidio Familiar? - ¿Cuánto? (Si Recibió Pero No Sabe El Monto, Escriba 98)
  # P6585S3A2	Cual(Es) De Los Siguientes Subsidios Recibió ... El Mes Pasado: - Subsidio Familiar? - ¿Incluyo Este Valor En Los Ingresos Del Mes Pasado Que Me Declaró Anteriormente?
  i1.1<-add.cond.var(allindividuals, "CODIGO_ENIG", value.v="P6585S3A1", condition.v="P6585S3A2", output.df=i1.1, name.string="sub.familiar")
  
  # P6585S4A1	Cual(Es) De Los Siguientes Subsidios Recibió ... El Mes Pasado: - Subsidio Educativo? - ¿Cuánto Recibió? (Si Recibió Pero No Sabe El Monto, Escriba 98)
  i1.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6585S4A1", output.df=i1.1)
  
# 1.1.4 Gastos de representación
  # P6530S1	¿El Mes Pasado Recibió Ingresos Por Gastos De Representación? - ¿Cuánto Recibió? (Si Recibió Pero No Sabe El Monto, Escriba 98)
  i1.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6530S1", output.df=i1.1)
  
# 1.1.5 Primas y bonificaciones mensuales
  # P6540S1	El Mes Pasado Recibió ... Prima Técnica? - ¿Cuánto Recibió El Mes Pasado?
  # P6540S2	El Mes Pasado Recibió ... Prima Técnica? - ¿Incluyó Este Valor En Los Ingresos Del Mes Pasado Que Me Declaró Anteriormente ?
  i1.1<-add.cond.var(allindividuals, "CODIGO_ENIG", value.v="P6540S1", condition.v="P6540S2", output.df=i1.1, name.string="prima.tecnica")
  
  # P6550S1	El Mes Pasado Recibió ... Prima De Antigüedad? - ¿Cuánto Recibió El Mes Pasado?
  # P6550S2	El Mes Pasado Recibió ... Prima De Antigüedad? - ¿Incluyó Este Valor En Los Ingresos Del Mes Pasado Que Me Declaró Anteriormente ?
  i1.1<-add.cond.var(allindividuals, "CODIGO_ENIG", value.v="P6550S1", condition.v="P6550S2", output.df=i1.1, name.string="prima.antiguedad")
  
  # P6560S1	El Mes Pasado Recibió ... Prima De Clima? - ¿Cuánto Recibió El Mes Pasado?
  # P6560S2	El Mes Pasado Recibió ... Prima De Clima? - ¿Incluyó Este Valor En Los Ingresos Del Mes Pasado Que Me Declaró Anteriormente ?
  i1.1<-add.cond.var(allindividuals, "CODIGO_ENIG", value.v="P6560S1", condition.v="P6560S2", output.df=i1.1, name.string="prima.clima")
  
  # P6570S1	El Mes Pasado Recibió ... Otras Primas (Orden Público, Etc.)? - ¿Cuantó Recibió El Mes Pasado?
  # P6570S2	El Mes Pasado Recibió ... Otras Primas (Orden Público, Etc.)? - ¿Incluyó Este Valor En Los Ingresos Del Mes Pasado Que Me Declaró Anteriormente ?
  i1.1<-add.cond.var(allindividuals, "CODIGO_ENIG", value.v="P6570S1", condition.v="P6570S2", output.df=i1.1, name.string="prima.otras")
  
  # P6580S1	El Mes Pasado Recibió ... Bonificaciones Mensuales? - ¿Cuánto Recibió El Mes Pasado?
  # P6580S2	El Mes Pasado Recibió ... Bonificaciones Mensuales? - ¿Incluyó Este Valor En Los Ingresos Del Mes Pasado Que Me Declaró Anteriormente ?
  i1.1<-add.cond.var(allindividuals, "CODIGO_ENIG", value.v="P6580S1", condition.v="P6580S2", output.df=i1.1, name.string="bonif.mensual")
  
# 1.1.6 Primas y bonificaciones anuales
  # /12 for all items in this section
  # P6630S1A1	En Los Últimos 12 Meses Recibió: - Prima De Servicios? - ¿Cuánto Recibió? (Si Recibió Pero No Sabe El Monto, Escriba 98)
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6630S1A1", output.df=i1.1)
  
  # P6630S2A1	En Los Últimos 12 Meses Recibió: - Prima De Navidad? - ¿Cuánto Recibió? (Si Recibió Pero No Sabe El Monto, Escriba 98)
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6630S2A1", output.df=i1.1)
  
  # P6630S3A1	En Los Últimos 12 Meses Recibió: - Prima De Vacaciones? - ¿Cuánto Recibió? (Si Recibió Pero No Sabe El Monto, Escriba 98)
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6630S3A1", output.df=i1.1)
  
  # P6630S4A1	En Los Últimos 12 Meses Recibió: - Viáticos Permanentes? - ¿Cuánto Recibió? (Si Recibió Pero No Sabe El Monto, Escriba 98)
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6630S4A1", output.df=i1.1)
  
  # P6630S5A1	En Los Últimos 12 Meses Recibió: - Bonificaciones Anuales? - ¿Cuánto Recibió? (Si Recibió Pero No Sabe El Monto, Escriba 98)
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6630S5A1", output.df=i1.1)
  
# 1.1.7 Ganancia Neta-Ingreso empleo independiente áreas urbanas, para hogares en cabeceras (clase 1)  
  # P6750	¿Cuál fue la ganancia neta o los honorarios netos de ... esa actividad, negocio, profesión o finca, el mes pasado?
  # P6760	¿A cuántos meses corresponde lo que recibió?
  # //P6760
  i1.1<-add.nor.var.nmes(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6750", output.df=i1.1, nmes="P6760")
  #View(allindividuals[,c("CODIGO_ENIG","P6750","P6760")])
  

# 1.1.8 Ganancia Neta-Ingreso empleo independiente áreas rurales, para hogares en clases 2 y 3.  Para este caso, se toma el mayor valor entre el mes pasado y los últimos doce meses. 
  # For Cali, there is not information about its rural areas.
  # P550	¿Cuál fue la ganancia neta del negocio o de la cosecha durante los últimos 12 meses?
  # /12
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P550", output.df=i1.1)
  
  
# 1.1.9 Ingreso por segundo trabajo
  # P7070	¿Cuánto recibio o ganó ... el mes pasado en ese segundo trabajo?
  i1.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7070", output.df=i1.1)
  
# 1.1.10 Ingresos de trabajo para desocupados 
  # P7422S1	¿Recibió o Ganó El Mes Pasado Ingresos Por Concepto De Trabajo? - ¿Cuánto?
  i1.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7422S1", output.df=i1.1)
  
  # P7424	¿Durante los últimos 12 meses, ganó o recibió ingresos por concepto de trabajo?
  #/12
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7424", output.df=i1.1)
  
  
# 1.1.11 Ingresos de trabajo para inactivos
  # P7472S1	¿Recibió o Ganó El Mes Pasado Ingresos Por Concepto De Trabajo? - ¿Cuánto?
  i1.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7472S1", output.df=i1.1)
  
  # P7490	¿Durante los últimos doce meses ganó o recibió ingresos por concepto de trabajo?
  #/12
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7490", output.df=i1.1)
  
  
# 1.1.12 Arriendos y pensiones
  # P7500S1A1	El mes pasado recibió ... pagos por concepto de: - Arriendos de casas, apartamentos, fincas, lotes, vehículos, equipos, etc. - Valor mes pasado
  i1.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7500S1A1", output.df=i1.1)
  
  # P7500S2A1	El mes pasado recibió ... pagos por concepto de: - Pensiones o jubilaciones por vejez, invalidez o sustitución pensional - Valor mes pasado
  i1.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7500S2A1", output.df=i1.1)
  
  # P7500S3A1	El mes pasado recibió ... pagos por concepto de: - Pensión alimenticia por paternidad, divorcio o separación - Valor mes pasado
  i1.1<-add.nor.var(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7500S3A1", output.df=i1.1)
  
# 1.1.13 Ayudas, intereses y cesantías 
  # /12 for all items in this section
  # P7510S1A1	Durante Los Últimos Doce Meses Recibió: - Dinero De Otros Hogares o Personas Residentes En El País? - Valor
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7510S1A1", output.df=i1.1)
  
  # P7510S2A1	Durante Los Últimos Doce Meses Recibió: - Dinero De Otros Hogares o Personas Residentes Fuera Del País? - Valor
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7510S2A1", output.df=i1.1)
  
  # P7510S3A1	Durante Los Últimos Doce Meses Recibió: - Ayudas En Dinero De Instituciones En El País? - Valor
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7510S3A1", output.df=i1.1)
  
  # P7510S4A1	Durante Los Últimos Doce Meses Recibió: - Ayudas En Dinero De Instituciones Fuera Del País? - Valor
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7510S4A1", output.df=i1.1)
  
  # P7510S5A1	Durante Los Últimos Doce Meses Recibió: - Dinero Por Intereses o Dividendos De Cdt'S, Depositos De Ahorro, Utilidades, Ganancias Por Inversiones? - Valor
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7510S5A1", output.df=i1.1)
  
  # P7510S6A1	Durante Los Últimos Doce Meses Recibió: - Ingresos Por Concepto De Cesantías y/o Intereses A La Cesatías - Valor
  i1.1<-add.nor.var.yearly(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P7510S6A1", output.df=i1.1)
  
  
  
# TOTAL VALUES  
  i1.1$total<-rowSums(i1.1[,3:ncol(i1.1)], na.rm=TRUE)
    
