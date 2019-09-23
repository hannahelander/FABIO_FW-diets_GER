########################
# Consumption Footprints of German Food consumption using FABIO
# - Status Quo (Consumption and "consumed food")
# - Diet scenarios
# - Food waste reduction scenarios
# 
library(tidyr)
library(dplyr)
path <- "C:/Hanna/Germany_foodWaste_diets/input/"
#############################################################
# Footprints Status Quo - german consumption (="bought food")
#############################################################




########################################
####      BIOMASS FOOTPRINTS      #####
########################################

# prepare extension for Biomass
e <- E$Biomass / X                    # 
e[!is.finite(e)] <- 0

MP <- e * L


#########################################################
# Biomass footprints along the Supply Chain - Status Quo 
#########################################################

# Total footprint
FP_tot <- t(t(MP) * Y[,"DEU_Food"])           # why transpose twice?


# per capita footprint
population <- 82442336
FP_capita <- sum(FP_tot) / population             # gives ~3 ... which unit? (water content not relevant?, only kg?)





# For Plant-based products--------------------

waste <- read.csv2(file = "data/waste_data_frame")




######################################
# Mapping the system in KG
######################################

# when to use Y and when to use X ?? Y= final demand, X = total output

# 
substract <- function(x){
  x <- 
  return(x)}


Y_DEU_Food <- Y[,"DEU_Food"]
waste_harvest <- Y_DEU_Food * waste$harvest_production  # why not remove the waste here? - because its not included in the original data!? 

waste_storage <- Y_DEU_Food * waste$storage_transport   # get the absolut Number for waste from Storage
Y_DEU_procIn <- Y_DEU_Food - waste_storage              # get the amount of food that "continues" to processing  

waste_processing <- Y_DEU_procIn * waste$processing     # get the amount of food waste from Processing



######################################
# Scenarios for Food waste reduction 
######################################
# 1) Halfing food waste in Households






# 1) Calculate food waste (in function generating output "Y_waste")
# 2) How do I calculate the reduction the best? a) using the Y matrix and create a matrix for "Y minus Food_waste", or b) calculate the respective footprints and then substract?


# read waste data
FP_fw <- t(t(MP) * Y_waste)
sum(FP_fw) / population





#results <- 
