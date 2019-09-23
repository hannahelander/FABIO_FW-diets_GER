
# Script takes FABIO-files as input
# it calculates Status Quo of German Food Consumption Footprints
# By using FABIO-file "2013_Y.rds" together with dietary alternatives (developed by Hanna Helander), it will calculate scenarios for different diets
# Through adding data on food waste and loss, it will calculate scenarios for food waste reduction

###################################################
# Footprints Status Quo - german Consumption
###################################################
library(tidyr)


# Defining matrices
agg <- function(x){
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)}

path <- "C:/Users/hhelander/Documents/FABIO_files/FABIO_files/"

countries <- read.csv2("fabio_countries.csv")
waste_shares <- read.csv2("fabio_waste_shares.csv")
waste_shares[is.na(waste_shares)] <- 0

L <- readRDS(paste0(path,"2013_L_mass.rds"))
Y <- readRDS(paste0(path,"2013_Y.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
X <- readRDS(paste0(path,"2013_X.rds"))

# prepare extension
e <- E$Biomass / X
e[!is.finite(e)] <- 0

MP <- e * L



