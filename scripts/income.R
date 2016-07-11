#############################################################################################
# Estimating total income of the expenditure unit
####
library(Hmisc)

##############
# 1. Nominal monetary income of the expenditure unit
###

# 1.1 Nominal monetary income of the people inside the expenditure unit
source("./scripts/income.1.1.R")

# 1.2 Nominal monetary income of the expenditure unit
source("./scripts/income.1.2.R")


##############

##############
# 2. Nominal non-monetary income of the expenditure unit
###

# 2.1 Nominal non-monetary income of the people inside the expenditure unit
source("./scripts/income.2.1.R")

# 2.2 Nominal non-monetary income of the expenditure unit
source("./scripts/income.2.2.R")



##############

##############
# 3. Occasional monetary income of the expenditure unit, when used
###

library(dplyr)

  # 3.1 Nominal Occasional monetary income of the people inside the expenditure unit that were used in the household expenses
      # P7514S1	¿Utilizó ... Durante Los Ultimos Doce Meses, Parte De Esos Ingresos Ocasionales En Los Gastos Del Hogar? - Valor ($) :
      i3.1<-ml_persona[, c("CODIGO_ENIG", "HOUSEID")]
      i3.1<-merge(i3.1, ml_pblcion_edad_trbjar[,c("P7514S1", "CODIGO_ENIG")], by="CODIGO_ENIG", all=TRUE)
      i3.1$P7514S1<-as.numeric(as.character(i3.1$P7514S1))
      i3.1[i3.1== 99] <- NA
      i3.1[i3.1== 98] <- NA
      #Dividing by 12 because of the DANE's suggestion
        # Ya que para agregar la información los diferentes rubros deben referirse a un mismo período de tiempo, todos los ingresos 
        # deben equivaler a ingresos mensuales y, por tanto, los ingresos captados con periodicidad anual se deben dividir entre doce 
        # para que representen ingresos de un solo mes. 
      i3.1$P7514S1<-i3.1$P7514S1/12
  # 3.2 Nominal Occasional monetary  income of the expenditure unit
      i3.2<-summarise(group_by(i3.1, HOUSEID), sum(`P7514S1`, na.rm = TRUE))
      colnames(i3.2)<-c("HOUSEID", "Nominal Occasional monetary income")
  rm(i3.1)
##############

##############
# Total income of the expenditure unit
###

 totalincome<- merge(i1.2[,c("HOUSEID", "total")], i2.2[,c("HOUSEID", "total")], by="HOUSEID", all=TRUE)
 totalincome<- merge(totalincome, i3.2, by="HOUSEID", all=TRUE)
 totalincome$total<-rowSums(totalincome[,2:4])
 colnames(totalincome)<-c("HOUSEID", "Nominal monetary income", "Nominal non-monetary income", "Nominal Occasional monetary income", "Total income" )
 
 totalincome<-merge(totalincome, houseandproduct.addedmod[,1:2], by.x="HOUSEID", by.y="id.v")

 totalincome$`Empirical Cumulative Income Distribution`<-ecdf(totalincome$`Total income`)(totalincome$`Total income`)
 totalincome$`Quintile`<-cut2(totalincome$`Total income`, g=5)
 totalincome$`Quintile number`<-as.numeric(totalincome$`Quintile`)
  #  quantile(totalincome$`Total income`, probs = c(0, 0.25, 0.5, 0.75, 1))
  #  quantile(totalincome$`Total income`, probs = seq(0, 1, by= 0.1))
  #  quantile(totalincome$`Total income`, prob = seq(0, 1, length = 11), type = 5)
  #  quantile(totalincome$`Total income`, probs = c(0, 0.25, 0.5, 0.75, 1))
 
 View(totalincome)
 
 write.xlsx2(totalincome,"../outputs/Total income.xlsx")
 
##############

 
#############################################################################################

 
 
#############################################################################################
# 4. Estimating nominal monetary available income
####
 
 # 4.1 Estimating nominal monetary available personal income
 source("./scripts/income.4.1.R")
 
 # 4.2 Estimating nominal monetary available income of the expenditure unit
 source("./scripts/income.4.2.R")
 
 
