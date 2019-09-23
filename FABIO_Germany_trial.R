
# Script takes FABIO-files as input
# it calculates Status Quo of German Food Consumption Footprints (=supply - food waste)
# By using FABIO-file "2013_Y.rds" together with dietary alternatives, it will calculate scenarios for different diets
# It will calculate scenarios for food waste reduction


library(tidyr)
###################################################
# Read data 
###################################################

# Defining matrices                         # why this?
agg <- function(x){
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)}

path <- "C:/Hanna/Germany_foodWaste_diets/input/"

countries <- read.csv2("fabio_countries.csv")
waste_shares <- read.csv2("fabio_waste_shares.csv")
waste_shares[is.na(waste_shares)] <- 0

L <- readRDS(paste0(path,"2013_L_mass.rds"))
Y <- readRDS(paste0(path,"2013_Y.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
X <- readRDS(paste0(path,"2013_X.rds"))


########################################
####      LAND USE FOOTPRINTS      #####
########################################

# prepare extension for Landuse
e <- E$Landuse / X                    # 
e[!is.finite(e)] <- 0

MP <- e * L

# view data structure:
Colnames(Y)

#######################################
# Land footprints Status Quo 
#######################################

# Total footprint
FP_tot <- t(t(MP) * Y[,"DEU_Food"])           # why transpose twice?

# Sum                                     # how to sum?

######################################
# Land footprint Food waste reduction 
######################################
# Total footprints minus 100% food waste reduction

# 1) Calculate food waste (in function generating output "Y_waste")
# 2) How do I calculate the reduction the best? a) using the Y matrix and create a matrix for "Y minus Food_waste", or b) calculate the respective footprints and then substract?


# read waste data
FP_fw <- t(t(MP) * Y_waste)

# e.g.Food waste France...
FP_france <- t(t(MP) * Y_waste)
sum(FP_france) / 67000
sum(Y[,"FRA_Food"]) / 67000


