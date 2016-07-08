source("./scripts/functions.R")


modules<-list.files(path="../data/txt", full.names=TRUE)
modules

m.labels<-list.files(path="../data/labels", full.names=TRUE)
m.labels

source("./scripts/houses.R")
source("./scripts/households.R")
source("./scripts/individuals.R")
source("./scripts/consumption.R")
source("./scripts/food.R")
source("./scripts/income.R")
