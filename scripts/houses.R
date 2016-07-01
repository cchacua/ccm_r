#####################################################################################################
#House modules
#####################################################################################################

#Income - expenses module
Ig_gs_vivienda<-read.delim(modules[2], header=TRUE, colClasses = "character")
#In the whole survey, there are 41.330 houses
#table(Ig_gs_vivienda$DOMINIO)
gs_vivienda<-Ig_gs_vivienda[Ig_gs_vivienda$DOMINIO=="CALI A.M.",]
#There are 1.402 houses in Cali
rm(Ig_gs_vivienda)

#Labor market module
Ig_ml_vivienda<-read.delim(modules[24], header=TRUE, colClasses = "character")

#A merge of both modules
#viviendas.cali<-merge(gs_vivienda, Ig_ml_vivienda, by="VIVIENDA", suffixes = c(" ",".del"))
viviendas.cali<-merge(gs_vivienda, Ig_ml_vivienda[, c("VIVIENDA", setdiff(colnames(Ig_ml_vivienda),colnames(gs_vivienda)))], by="VIVIENDA")
View(viviendas.cali)
rm(Ig_ml_vivienda, gs_vivienda)
#length(unique(viviendas.cali$VIVIENDA))
#length(unique(viviendas.cali$FACTOR_EXPANSION_EC_E1))
#write.csv2(viviendas.cali,"../outputs/viviendas.cali.csv")
#write.csv(viviendas.cali,"../outputs/viviendas.cali.db.csv")

#?write.csv
#library(xlsx)
#write.xlsx(viviendas.cali,"../outputs/viviendas.cali.xlsx")

#####################################################################################################
# Houses: relevant variables
#####################################################################################################

  houses<-viviendas.cali[,c("VIVIENDA","ESTRATO","P4000", "P4010", "P4020")]
  houses$P4000<-recode(houses$P4000,
                                    '1'='Casa',
                                    '2'='Apartamento',
                                    '3'='Cuarto(s) en inquilinato',
                                    '4'='Cuarto(s) en otro tipo de estructura',
                                    '5'='Vivienda indígena',
                                    '6'='Otra vivienda (carpa, vagón, embarcación, cueva, refugio natural, etc.)')
  
  houses$P4010<-recode(houses$P4010,
                                    '1'='Ladrillo, bloque, material prefabricado, piedra',
                                    '2'='Madera pulida',
                                    '3'='Adobe o tapia pisada',
                                    '4'='Bahareque',
                                    '5'='Madera burda, tabla, tablón',
                                    '6'='Guadua',
                                    '7'='Caña, esterilla, otro tipo de material vegetal',
                                    '8'='Zinc, tela, cartón, latas, desechos, plástico',
                                    '9'='Sin paredes')
  
  houses$P4020<-recode(houses$P4020,
                                     '1'='Tierra, arena',
                                     '2'='Cemento, gravilla',
                                     '3'='Madera burda, tabla, tablón, otro vegetal',
                                     '4'='Baldosín, ladrillo, vinisol, otros materiales sintéticos',
                                     '5'='Mármol',
                                     '6'='Madera pulida',
                                     '7'='Alfombra o tapete de pared a pared')
  


  
