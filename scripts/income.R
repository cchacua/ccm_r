#############################################################################################
# Estimating total income of the expenditure unit
####

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

# 3.1 Nominal Occasional monetary income of the people inside the expenditure unit that were used in the household expenses

# 3.2 Nominal non-monetary income of the expenditure unit

##############

##############
# Total income of the expenditure unit
###

 totalincome<- merge(i1, i2, by="HOUSEID")
 totalincome<- merge(totalincome, i3, by="HOUSEID")
 totalincome$total<-rowSums(totalincome[,2:4])

##############

#############################################################################################



