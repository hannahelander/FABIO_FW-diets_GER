# ------------------------------
# Load data for Project 
# ------------------------------

library(tidyr)
library(dplyr)

path <- "C:/Hanna/FABIO_FW-diets_GER/input/"

# load classification data
countries <- read.csv2(paste0(path,"fabio_countries.csv"))
products <- read.csv2(paste0(path, "items.csv"))

# POPULATION 2013
population <- 80645605 #2013 Source: World bank


# load FABIO output data
L <- readRDS(paste0(path,"2013_L_mass.rds"))
Y <- readRDS(paste0(path,"2013_Y.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
X <- readRDS(paste0(path,"2013_X.rds"))




