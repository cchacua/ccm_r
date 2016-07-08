#############################################################################################
# Consumption - expenses
#############################################################################################
library(datasets)
library(dplyr)
library(xlsx)
library(reshape)

#############################################################################################
#Loading Databases
#############################################################################################

#####
#Daily consumption by household unit
Ig_gsdu_gas_dia<-read.delim(modules[8], header=TRUE, colClasses = "character")
gsdu_gas_dia<-merge(viviendas.cali[,c(1,3)], Ig_gsdu_gas_dia, by="VIVIENDA")
rm(Ig_gsdu_gas_dia)
gsdu_gas_dia[gsdu_gas_dia== ""] <- NA
gsdu_gas_dia$class<-substr(gsdu_gas_dia$GDU_ARTCLO, 1, 2)
#unique(gsdu_gas_dia$GDU_ARTCLO)
#cod.dos.du<-gsdu_gas_dia[gsdu_gas_dia$class=="02",]
#unique(cod.dos.du$GDU_ARTCLO)
#View(gsdu_gas_dia)
#colnames(gsdu_gas_dia)
#write.csv(gsdu_gas_dia,"../outputs/gsdu_gas_dia_Cali.csv")
gsdu_gas_dia$GDU_LUGAR_CMPRA<-recode(gsdu_gas_dia$GDU_LUGAR_CMPRA,
                                     '01'='Almacenes o supermercados de cadena y tiendas por departamento',
                                     '02'='Hipermercados',
                                     '03'='Cooperativas, Fondos de Empleados y Comisariatos',
                                     '04'='Supermercado de Cajas de Compensación',
                                     '05'='Supermercados de barrio',
                                     '06'='Tiendas de barrio',
                                     '07'='Misceláneas de barrio y cacharrerías',
                                     '08'='Cigarrerías, salsamentarias y delicatessen',
                                     '09'='Graneros',
                                     '10'='Plazas de mercado y galerías',
                                     '11'='Central mayorista de abastecimiento',
                                     '12'='Mercados móviles',
                                     '13'='Vendedores ambulantes o ventas callejeras',
                                     '14'='Sanandresitos',
                                     '15'='Bodegas o fábricas',
                                     '16'='Establecimiento especializado en la venta del artículo o la prestación del servicio adquirido',
                                     '17'='Farmacias y droguerías',
                                     '18'='Restaurantes',
                                     '19'='Cafeterías y establecimientos de comidas rápidas',
                                     '20'='Persona particular',
                                     '21'='Ferias especializadas: artesanal, del hogar, del libro, de computadores, etc.',
                                     '22'='A través de Internet',
                                     '23'='Televentas y ventas por catálogo',
                                     '24'='Otro',
                                     '99'='No sabe, no informa')

gsdu_gas_dia$GDU_FORMA_ADQSCION<-recode(gsdu_gas_dia$GDU_FORMA_ADQSCION,
                                        '01'='Compra',
                                        '02'='Traidos de la finca o producidos por el hogar',
                                        '03'='Tomados de un negocio del hogar',
                                        '04'='Recibidos como pago por trabajo',
                                        '05'='Regalo',
                                        '06'='Intercambio o trueque',
                                        '07'='Otra',
                                        '08'='No sabe, no informa')

#####
#Daily consumption by other people with income inside the household unit
Ig_gsdp_gas_dia<-read.delim(modules[4], header=TRUE, colClasses = "character")
gsdp_gas_dia<-merge(viviendas.cali[,c(1,3)], Ig_gsdp_gas_dia, by="VIVIENDA")
rm(Ig_gsdp_gas_dia)
gsdp_gas_dia[gsdp_gas_dia== ""] <- NA
#colnames(gsdp_gas_dia)
#write.csv(gsdp_gas_dia,"../outputs/gsdp_gas_dia_Cali.csv")

gsdp_gas_dia$GDP_LUGAR_CMPRA<-recode(gsdp_gas_dia$GDP_LUGAR_CMPRA,
                                     '01'='Almacenes o supermercados de cadena y tiendas por departamento',
                                     '02'='Hipermercados',
                                     '03'='Cooperativas, Fondos de Empleados y Comisariatos',
                                     '04'='Supermercado de Cajas de Compensación',
                                     '05'='Supermercados de barrio',
                                     '06'='Tiendas de barrio',
                                     '07'='Misceláneas de barrio y cacharrerías',
                                     '08'='Cigarrerías, salsamentarias y delicatessen',
                                     '09'='Graneros',
                                     '10'='Plazas de mercado y galerías',
                                     '11'='Central mayorista de abastecimiento',
                                     '12'='Mercados móviles',
                                     '13'='Vendedores ambulantes o ventas callejeras',
                                     '14'='Sanandresitos',
                                     '15'='Bodegas o fábricas',
                                     '16'='Establecimiento especializado en la venta del artículo o la prestación del servicio adquirido',
                                     '17'='Farmacias y droguerías',
                                     '18'='Restaurantes',
                                     '19'='Cafeterías y establecimientos de comidas rápidas',
                                     '20'='Persona particular',
                                     '21'='Ferias especializadas: artesanal, del hogar, del libro, de computadores, etc.',
                                     '22'='A través de Internet',
                                     '23'='Televentas y ventas por catálogo',
                                     '24'='Otro',
                                     '99'='No sabe, no informa')

gsdp_gas_dia$GDP_FORMA_ADQSCION<-recode(gsdp_gas_dia$GDP_FORMA_ADQSCION,
                                        '01'='Compra',
                                        '02'='Traidos de la finca o producidos por el hogar',
                                        '03'='Tomados de un negocio del hogar',
                                        '04'='Recibidos como pago por trabajo',
                                        '05'='Regalo',
                                        '06'='Intercambio o trueque',
                                        '07'='Otra',
                                        '08'='No sabe, no informa')

#####
#Extrapolated expenses of the household unit
Ig_gsdu_gasto_alimentos_cap_c<-read.delim(modules[9], header=TRUE, colClasses = "character")
gsdu_gasto_alimentos_cap_c<-merge(viviendas.cali[,c(1,3)], Ig_gsdu_gasto_alimentos_cap_c, by="VIVIENDA")
rm(Ig_gsdu_gasto_alimentos_cap_c)
gsdu_gasto_alimentos_cap_c[gsdu_gasto_alimentos_cap_c== ""] <- NA
#View(gsdu_gasto_alimentos_cap_c)
#unique(gsdu_gasto_alimentos_cap_c$ARTICULO)
#write.csv(gsdu_gasto_alimentos_cap_c,"../outputs/gsdu_gasto_alimentos_cap_c_Cali.csv")

#####
# Expenses that are not frequent
Ig_gsmf_compra<-read.delim(modules[11], header=TRUE, colClasses = "character")
gsmf_compra<-merge(viviendas.cali[,c(1,3)], Ig_gsmf_compra, by="VIVIENDA")
rm(Ig_gsmf_compra)
gsmf_compra[gsmf_compra== ""] <- NA
#write.csv(gsmf_compra, "../outputs/gsmf_compra_Cali.csv")
gsmf_compra$GMF_CMPRA_LUGAR<-recode(gsmf_compra$GMF_CMPRA_LUGAR,
                                    '01'='Almacenes o supermercados de cadena y tiendas por departamento',
                                    '02'='Hipermercados',
                                    '03'='Cooperativas, Fondos de Empleados y Comisariatos',
                                    '04'='Supermercado de Cajas de Compensación',
                                    '05'='Supermercados de barrio',
                                    '06'='Tiendas de barrio',
                                    '07'='Misceláneas de barrio y cacharrerías',
                                    '08'='Cigarrerías, salsamentarias y delicatessen',
                                    '09'='Graneros',
                                    '10'='Plazas de mercado y galerías',
                                    '11'='Central mayorista de abastecimiento',
                                    '12'='Mercados móviles',
                                    '13'='Vendedores ambulantes o ventas callejeras',
                                    '14'='Sanandresitos',
                                    '15'='Bodegas o fábricas',
                                    '16'='Establecimiento especializado en la venta del artículo o la prestación del servicio adquirido',
                                    '17'='Farmacias y droguerías',
                                    '18'='Restaurantes',
                                    '19'='Cafeterías y establecimientos de comidas rápidas',
                                    '20'='Persona particular',
                                    '21'='Ferias especializadas: artesanal, del hogar, del libro, de computadores, etc.',
                                    '22'='A través de Internet',
                                    '23'='Televentas y ventas por catálogo',
                                    '24'='Otro',
                                    '99'='No sabe, no informa')
table(gsmf_compra$GMF_CMPRA_LUGAR)
gsmf_compra$GMF_FORMA_ADQSCION<-"Compra"

#####
# Non-monetary expenses that are not frequent
Ig_gsmf_forma_adqui<-read.delim(modules[12], header=TRUE, colClasses = "character")
gsmf_forma_adqui<-merge(viviendas.cali[,c(1,3)], Ig_gsmf_forma_adqui, by="VIVIENDA")
rm(Ig_gsmf_forma_adqui)
gsmf_forma_adqui[gsmf_forma_adqui== ""] <- NA
#write.csv(gsmf_forma_adqui, "../outputs/gsmf_forma_adqui_Cali.csv")

gsmf_forma_adqui$GMF_LUGAR<-"No aplica"

gsmf_forma_adqui$GMF_ADQU_FORMA<-recode(gsmf_forma_adqui$GMF_ADQU_FORMA,
                                        '5'='Pago por trabajo',
                                        '6'='Regalo',
                                        '7'='Intercambio',
                                        '3'='Producido por el hogar',
                                        '4'='Tomado de un negocio propio',
                                        '8'='Otra forma')

#############################################################################################

#############################################################################################
# File with ENIG_CODE, product code, values, place of purchase, way of purchase and quantities
#############################################################################################
    
    gsdu_gas_dia.bigtable<-data.frame(gsdu_gas_dia$CODIGO_ENIG, 
                                      gsdu_gas_dia$GDU_ARTCLO, 
                                      gsdu_gas_dia$GDU_VALOR_PGDO_ESTMDO_MES_AJST,
                                      gsdu_gas_dia$GDU_LUGAR_CMPRA,
                                      gsdu_gas_dia$GDU_FORMA_ADQSCION,
                                      gsdu_gas_dia$GDU_CNTDAD_ADQURDA_MES_AJST , 
                                      gsdu_gas_dia$GDU_UDM_ESTANDAR)
    colnames(gsdu_gas_dia.bigtable)<-c("ENIG_CODE", 
                                       "Product code", 
                                       "Adjusted monthly value", 
                                       "Place of purchase", 
                                       "Way of purchase", 
                                       "Adjusted monthly quantity", 
                                       "Units")
    gsdu_gas_dia.bigtable.values<-summarise(group_by(gsdu_gas_dia.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
    #View(gsdu_gas_dia.bigtable.values)
    
    gsdp_gas_dia.bigtable<-data.frame(gsdp_gas_dia$CODIGO_ENIG, 
                                      gsdp_gas_dia$GDP_ARTCLO, 
                                      gsdp_gas_dia$GDP_VALOR_PGDO_ESTMDO_MES_AJST,
                                      gsdp_gas_dia$GDP_LUGAR_CMPRA,
                                      gsdp_gas_dia$GDP_FORMA_ADQSCION,
                                      gsdp_gas_dia$GDP_CNTDAD_ADQURDA_MES_AJST)
    colnames(gsdp_gas_dia.bigtable)<-c("ENIG_CODE", 
                                       "Product code", 
                                       "Adjusted monthly value", 
                                       "Place of purchase", 
                                       "Way of purchase",
                                       "Adjusted monthly quantity")
    #gsdp_gas_dia.bigtable.values<-summarise(group_by(gsdp_gas_dia.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
    #View(gsdp_gas_dia.bigtable.values)
    
    
    gsdu_gasto_alimentos_cap_c.bigtable<-data.frame(paste(gsdu_gasto_alimentos_cap_c$VIVIENDA,gsdu_gasto_alimentos_cap_c$HOGAR, sep=""), 
                                                    gsdu_gasto_alimentos_cap_c$ARTICULO,
                                                    gsdu_gasto_alimentos_cap_c$VALOR_MENSUAL_ALIMENTO,
                                                    gsdu_gasto_alimentos_cap_c$CANTIDAD)
    colnames(gsdu_gasto_alimentos_cap_c.bigtable)<-c("ENIG_CODE", 
                                                     "Product code", 
                                                     "Adjusted monthly value",
                                                     "Adjusted monthly quantity")
    #View(gsdu_gasto_alimentos_cap_c.bigtable)
    #gsdu_gasto_alimentos_cap_c.bigtable.values<-summarise(group_by(gsdu_gasto_alimentos_cap_c.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
    #View(gsdu_gasto_alimentos_cap_c.bigtable.values)
    
    gsmf_compra.monthly.bigtable<-data.frame(gsmf_compra$CODIGO_ENIG ,
                                             gsmf_compra$GMF_CMPRA_ARTCLO,
                                             gsmf_compra$GMF_CMPRA_VLR_PAGO_MES,
                                             gsmf_compra$GMF_CMPRA_LUGAR,
                                             gsmf_compra$GMF_FORMA_ADQSCION)
    colnames(gsmf_compra.monthly.bigtable)<-c("ENIG_CODE", 
                                              "Product code", 
                                              "Adjusted monthly value", 
                                              "Place of purchase",
                                              "Way of purchase")
    #View(gsmf_compra.monthly.bigtable)
    #gsmf_compra.monthly.bigtable.values<-summarise(group_by(gsmf_compra.monthly.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
    #View(gsmf_compra.monthly.bigtable.values)
    
    gsmf_forma_adqui.bigtable<-data.frame(gsmf_forma_adqui$CODIGO_ENIG, 
                                          gsmf_forma_adqui$GMF_ADQU_ARTCLO,
                                          gsmf_forma_adqui$GMF_ADQU_VLR_PAGO_MES,
                                          gsmf_forma_adqui$GMF_LUGAR,
                                          gsmf_forma_adqui$GMF_ADQU_FORMA)
    colnames(gsmf_forma_adqui.bigtable)<-c("ENIG_CODE", 
                                           "Product code", 
                                           "Adjusted monthly value",
                                           "Place of purchase",
                                           "Way of purchase")
    #gsmf_forma_adqui.bigtable.values<-summarise(group_by(gsmf_forma_adqui.bigtable, `Product code`), sum(as.numeric(as.character(`Adjusted monthly value`)), na.rm = TRUE))
    #View(gsmf_forma_adqui.bigtable.values)
    
    #####
    # File with ENIG_CODE, Product code and value : bigtable.all
    #####
    bigtable.list<-list(gsdu_gas_dia.bigtable[,1:3],
                        gsdp_gas_dia.bigtable[,1:3],
                        gsdu_gasto_alimentos_cap_c.bigtable[,1:3],
                        gsmf_compra.monthly.bigtable[,1:3],
                        gsmf_forma_adqui.bigtable[,1:3])
    
    
    bigtable.all<-Reduce(function(...) rbind(...), bigtable.list)
    #View(bigtable.all)
    
    
    bigtable.all$HOUSEID<-substr(bigtable.all$ENIG_CODE, 1, 7)
    #length(unique(bigtable.all$HOUSEID))
    bigtable.all$`Adjusted monthly value`<-as.numeric(as.character(bigtable.all$`Adjusted monthly value`)) 
    
    #####

#############################################################################################


#
#bigtable.all.housevalues<-summarise(group_by(bigtable.all, HOUSEID), sum(`Adjusted monthly value`, na.rm = TRUE))






