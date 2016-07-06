#############################################################################################
# 2.2 Nominal non-monetary income of the expenditure unit
####

# 2.2.1 Suma del ingreso corriente no monetario de cada persona de la UG
  i2.2<-summarise(group_by(i2.1, HOUSEID), sum(total, na.rm = TRUE))
# 2.2.2 Alquiler imputado del servicio de la vivienda ocupada por su propietario 
  # P5130	Si tuviera que pagar el arriendo por esta vivienda, ¿cuánto estima que tendria que pagar mensualmente?
  i2.2<-add.nor.var(basedf=hogares.cali.ml, id.v="HOUSEID", value.v="P5130", output.df=i2.2)
  
# 2.2.3	Subsidio de vivienda en especie que recibe el hogar
  #	¿En los últimos doce meses, algún miembro del hogar recibió subsidio del gobierno o de otra institución para la compra o construcción de vivienda, casalote o lote?
  # P5160S2A1	Cuánto Especie?
  #/12
  i2.2<-add.nor.var.yearly(basedf=hogares.cali.ml, id.v="HOUSEID", value.v="P5160S2A1", output.df=i2.2)
  
# 2.2.4	Diferencia entre valor estimado de alimentos que reciben los niños menores de 3 años de la UG y el valor mensual pagado por ellos
  # P5180S2	¿Estas personas, reciben en el plantel educativo alimentos (desayunos, medias nueves, almuerzos, etc.), en forma gratuita o por un pago simbólico? - Si lo tuviera que comprar en otra parte cuánto pagaría al día por lo que recibe?
  # - P5180S1	¿Estas personas, reciben en el plantel educativo alimentos (desayunos, medias nueves, almuerzos, etc.), en forma gratuita o por un pago simbólico? - Cuánto paga por día?
  #*30
  i2.2<-add.nor.var.ali.mes(basedf=hogares.cali.ml, id.v="HOUSEID", value.v="P5180S2", valuepag.v="P5180S1", output.df=i2.2, name.v="alimenorestres")

# 2.2.5	Diferencia entre valor estimado de alimentos que reciben las personas de la UG que estudian y el valor mensual pagado por ellos
  # P6180S2	¿Recibe en el plantel educativo alimentos (desayunos, medias nueves, almuerzos, etc.) en forma gratuita o por un pago simbólico? - Si lo tuviera que comprar en otra parte cuanto pagaría al día por lo que recibe?
  # P6180S1	¿Recibe en el plantel educativo alimentos (desayunos, medias nueves, almuerzos, etc.) en forma gratuita o por un pago simbólico? - Cuánto paga por día?
  # *30
  i2.2<-add.nor.var.ali.mes.households(basedf=allindividuals, id.v="CODIGO_ENIG", value.v="P6180S2", valuepag.v="P6180S1", output.df=i2.2, name.v="alimentosestu")

# 2.2.6	Otros subsidios del gobierno en especie que recibe el hogar
  #	P5190S2A1	Este o estos subsidios los reciben: En especie? - En cuánto estima lo que recibieron en los últimos doce meses?
  #/12
  i2.2<-add.nor.var.yearly(basedf=hogares.cali.ml, id.v="HOUSEID", value.v="P5190S2A1", output.df=i2.2)
  
# 2.2.7	Valor estimado de bienes y servicios adquiridos por el hogar por autoconsumo, autosuministro y pago en especie 
