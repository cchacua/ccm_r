#####################################################################################################
#House modules
#####################################################################################################
library(datasets)
library(dplyr)
library(xlsx)
library(reshape)

#Income - expenses module
Ig_gs_vivienda<-read.delim(modules[2], header=TRUE, colClasses = "character")
#In the whole survey, there are 41.330 houses
#table(Ig_gs_vivienda$DOMINIO)
gs_vivienda<-Ig_gs_vivienda[Ig_gs_vivienda$DOMINIO=="CALI A.M.",]
#There are 1.402 houses in Cali
rm(Ig_gs_vivienda)

#Labor market module
Ig_ml_vivienda<-read.delim(modules[24], header=TRUE, colClasses = "character")
#View(Ig_ml_vivienda)
#A merge of both modules
#viviendas.cali<-merge(gs_vivienda, Ig_ml_vivienda, by="VIVIENDA", suffixes = c(" ",".del"))
viviendas.cali<-merge(gs_vivienda, Ig_ml_vivienda[, c("VIVIENDA", setdiff(colnames(Ig_ml_vivienda),colnames(gs_vivienda)))], by="VIVIENDA")
#View(viviendas.cali)
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

  # P4000: Tipo de vivienda
    #   houses$P4000<-recode(houses$P4000,
    #                                     '1'='Casa',
    #                                     '2'='Apartamento',
    #                                     '3'='Cuarto(s) en inquilinato',
    #                                     '4'='Cuarto(s) en otro tipo de estructura',
    #                                     '5'='Vivienda indígena',
    #                                     '6'='Otra vivienda (carpa, vagón, embarcación, cueva, refugio natural, etc.)')
  houses$P4000<-recode(houses$P4000,
                       '1'='House',
                       '2' = 'Apartment',
                       '3' = 'Room(s) in inquilinato',
                       '4' = 'Room(s) in other structure',
                       '5' = 'Indigenous Housing',
                       '6' = 'Another house (tent, car, boat, cave, natural shelter, etc.)')

  # P4010: ¿Cuál es el material predominante de las paredes exteriores de la vivienda?
    #   houses$P4010<-recode(houses$P4010,
    #                                     '1'='Ladrillo, bloque, material prefabricado, piedra',
    #                                     '2'='Madera pulida',
    #                                     '3'='Adobe o tapia pisada',
    #                                     '4'='Bahareque',
    #                                     '5'='Madera burda, tabla, tablón',
    #                                     '6'='Guadua',
    #                                     '7'='Caña, esterilla, otro tipo de material vegetal',
    #                                     '8'='Zinc, tela, cartón, latas, desechos, plástico',
    #                                     '9'='Sin paredes')
  houses$P4010<-recode(houses$P4010,
                                  '1' = 'Bricks, prefabricated material, stone',
                                  '2' = 'Polished wood',
                                  '3' = '"Adobe" or rammed earth',
                                  '4' = 'Bahareque',
                                  '5' = 'Rough wood, board, plank',
                                  '6' = 'Guadua',
                                  '7' = 'Reed, mat, other plant material',
                                  '8' = 'Zinc, fabric, cardboard, cans, debris, plastic',
                                  '9' = 'without walls')
  # P4020: ¿Cuál es el material predominante de los pisos de la vivienda?
    # houses$P4020<-recode(houses$P4020,
    #                                          '1'='Tierra, arena',
    #                                          '2'='Cemento, gravilla',
    #                                          '3'='Madera burda, tabla, tablón, otro vegetal',
    #                                          '4'='Baldosín, ladrillo, vinisol, otros materiales sintéticos',
    #                                          '5'='Mármol',
    #                                          '6'='Madera pulida',
    #                                          '7'='Alfombra o tapete de pared a pared')
  houses$P4020<-recode(houses$P4020,
                                    '1' = 'Ground, sand',
                                    '2' = 'Concrete, gravel',
                                    '3' = 'Rough wooden table, plank, other vegetable',
                                    '4' = 'Ceramic tile, brick, vinisol, other synthetic materials',
                                    '5' = 'Marble',
                                    '6' = 'Polished wood',
                                    '7' = 'carpet or rug from wall to wall')
  
  

  houses[houses== ""] <- NA
  
  colnames(houses)<-c("VIVIENDA", 
                      "Stratum", 
                      "Type of house", 
                      "What is the predominant material of the outer walls of the house?",
                      "What is the main material of the floors of the house?")
  
  houses.myCharCols<- which(unlist(lapply(houses, is.character)))
  houses[, houses.myCharCols][is.na(houses[, houses.myCharCols])] <- "Not available"
  