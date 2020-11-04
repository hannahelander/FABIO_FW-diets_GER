# ------------------------------
# Load data for Project 
# ------------------------------

library(tidyr)
library(dplyr)

path <- "C:/Hanna/FABIO_FW-diets_GER/input/"

# load classification data
countries <- read.csv(paste0(path,"regions.csv"))
products <- read.csv(paste0(path, "itemsmb.csv"))

# POPULATION 2013
population <- 80645605 #2013 Source: World bank 

# load FABIO output data
L <- readRDS(paste0(path,"2013_L_mass.rds"))
Y <- readRDS(paste0(path,"2013_Y.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
X <- readRDS(paste0(path,"2013_X.rds"))

load(paste0(path,"E_ghg_2013.RData"))

