#############################################################################################
#1.1.1 Nominal monetary income of the people inside the expenditure unit
####

# 1.1.1.1 Sueldos y salarios
#Antes de descuentos ¿cuánto ganó ... el mes pasado en este empleo?
i.1.1.1.1<-data.frame(P6500=ml_ocupado$P6500
                     )
nrow(i.1.1.1.1) 
nrow(i.1.1.1.1)-nrow(na.omit(i.1.1.1.1))  
  
# 1.1.1.2 Horas extras
  i.1.1.1.2<-data.frame(P6510S1=as.numeric(ml_ocupado$P6510S1), P6510S2=ml_ocupado$P6510S2, CODIGO_ENIG=ml_ocupado$CODIGO_ENIG)
  #El Mes Pasado Recibió Ingresos Por Concepto De Horas Extras? - ¿Cuánto Recibió? (P6510S1)                     
  #El Mes Pasado Recibió Ingresos Por Concepto De Horas Extras? - ¿Incluyó Este Valor En Los Ingresos Del Mes Pasado que me Declaró Anteriormente? (P6510S2) 1. Sí, 2. No
  i.1.1.1.2$horasextras<-ifelse(i.1.1.1.2$P6510S2=="2", i.1.1.1.2$P6510S1, NA)
  #Sólo se colocan los ingresos por horas extras, si respondió que nó los había incluido
  i.1.1.1.2$val<-i.1.1.1.2$horasextras

  
# 1.1.1.3 Auxilios y subsidios laborales

# 1.1.1.4 Gastos de representación

# 1.1.1.5 Primas y bonificaciones mensuales

# 1.1.1.6 Primas y bonificaciones anuales

# 1.1.1.7 Ganancia Neta-Ingreso empleo independiente áreas urbanas, para hogares en cabeceras (clase 1)  
# 1.1.1.8 Ganancia Neta-Ingreso empleo independiente áreas rurales, para hogares en clases 2 y 3.  Para este caso, se toma el mayor valor entre el mes pasado y los últimos doce meses. 
# 1.1.1.9 Ingreso por segundo trabajo
# 1.1.1.10 Ingresos de trabajo para desocupados 
# 1.1.1.11 Ingresos de trabajo para inactivos
# 1.1.1.12 Arriendos y pensiones 
# 1.1.1.13 Ayudas, intereses y cesantías 

