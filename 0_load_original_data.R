# ------------------------------
# Load data for Project 
# ------------------------------

library(tidyr)
library(dplyr)

path <- "C:/Hanna/Germany_foodWaste_diets/input/"

# load classification data
countries <- read.csv2(paste0(path,"fabio_countries.csv"))
products <- read.csv2(paste0(path, "items.csv"))

# load food waste data
waste_shares <- read.csv2(paste0(path,"fabio_waste_shares.csv"))  # change source!
waste_shares[is.na(waste_shares)] <- 0                            # set NA to 0


# load FABIO output data
L <- readRDS(paste0(path,"2013_L_mass.rds"))
Y <- readRDS(paste0(path,"2013_Y.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
X <- readRDS(paste0(path,"2013_X.rds"))

# prepare FABIO extension for Biomass
e <- E$Biomass / X
e[!is.finite(e)] <- 0
MP <- e * L


rm(L) # to save RAM
rm(E) 

# POPULATION 2013
population <- 80645605 #2013 Source: World bank
