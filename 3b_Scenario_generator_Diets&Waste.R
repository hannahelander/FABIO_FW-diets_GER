#################################
# This script reads data for diet references (developed in excel)
# SQ is taken from script: 2b_quantities and converted.
# notes: eaten is used to refer to national data but the step after consumption ("eaten")
# diet refer to gram per person and day

# Step 1: read Y_eaten SQ from file, read diet scenarios from file 
# Step 2: create Y-vectors for Diet Scenarios
# Step 3: create Y-vectors for different FW-scenarios (whole Germany per year)

#########################################################
### ----------------- Functions --------------- ###
#########################################################
# Function to add the consumer waste to eaten (step 3):

add.consumer.waste <- function(Yeaten){
  Yreal <- Yeaten/(100-waste$final_consumption)*100
  Yreal <- Yreal/(100-waste$distribution)*100
  Yreal <- Yreal/(100-waste$processing)*100
  Yreal <- Yreal/(100-waste$storage_transport)*100
  return(Yreal)
}

###############################################################
### Step 1. ------- Read Y_Eaten SQ from file --------------###
###############################################################
# Eaten are diets upscaled to national level and given per year
# Y_eaten SQ is generated based on SQ-data, assuming different FW levels (see......)


### -----   Y vectors on eaten food SQ ------- ##
Y_SQ_eaten <- read.csv2(file = "data/Y_SQ_eaten.csv")
Y_SQ_eaten <- Y_SQ_eaten[,2]
sum(Y_SQ_eaten) # 60519278

Y_SQ_diet <- Y_SQ_eaten * 1000000 / population / 365 # generates SQ diet as Y-vector 

#####----READ DIETS FROM FILE-----------########

Diets <- read.csv(file ="input/Diets_final.csv", sep = ";") # file include SQ and all other diets
prod_cat <- read.csv("input/product_characteristics.csv",na.strings = "NaN", colClasses = c("factor",rep("numeric",10)), sep=";" )
Diets[,1] <- as.character(Diets[, 1])

sum(Diets$DGErec_g) # 2189.092

################################################################################
### Step 2   ------   construct Y-vectors for Diet Scenarios    ---------    ###
################################################################################
# A: Y-vectors for diets (per person and day)
# B: Y-vectors for eaten food (Germany 2013)

#####  ----  Step 2A: Y vectors for diets (=per person and day)   ----  #####

# construct Y vector for DGE rec (2796 kcal)
Y_DGErec_diet <- Y_SQ_diet
for (i in 1:nrow(Diets)){
  Y_DGErec_diet[which(index$item_code %in% Diets$Item_code[i])] <- Diets$DGErec_g[i]*
    (Y_SQ_diet[which(index$item_code %in% Diets$Item_code[i])]/
       sum(Y_SQ_diet[which(index$item_code %in% Diets$Item_code[i])]))
}
Y_DGErec_diet[is.na(Y_DGErec_diet)] <- 0 # delete NaNs
sum(Y_DGErec_diet)    # gives 2189.092 =D (old value: 2198.913) 


# construct Y-vector for eat lancet (2796 kcal)
Y_lancet_diet <- Y_SQ_diet
for (i in 1:nrow(Diets)){
  Y_lancet_diet[which(index$item_code %in% Diets$Item_code[i])] <- Diets$EAT_g[i]*
    (Y_SQ_diet[which(index$item_code %in% Diets$Item_code[i])]/
       sum(Y_SQ_diet[which(index$item_code %in% Diets$Item_code[i])]))
}
Y_lancet_diet[is.na(Y_lancet_diet)] <- 0
sum(Y_lancet_diet)    # 1691.963!! (old 1691.59)



# construct Y-vector for EAT_veg (2796 kcal)
Y_EATveg_diet <- Y_SQ_diet
for (i in 1:nrow(Diets)) {
  Y_EATveg_diet[which(index$item_code %in% Diets$Item_code[i])] <- Diets$EATveg_g[i]*
    (Y_SQ_diet[which(index$item_code %in% Diets$Item_code[i])]/
       sum(Y_SQ_diet[which(index$item_code %in% Diets$Item_code[i])]))
}
Y_EATveg_diet[is.na(Y_EATveg_diet)] <- 0
sum(Y_EATveg_diet)    # 1690.706 (1690.353)


# Save as diets
write.csv2(Y_SQ_diet, file = "data/diets/Y_SQ_diet.csv") 
write.csv2(Y_DGErec_diet, file = "data/diets/Y_DGE_diet.csv") 
write.csv2(Y_lancet_diet, file = "data/diets/Y_lancet_diet.csv") 
write.csv2(Y_EATveg_diet, file = "data/diets/Y_EATveg_diet.csv") 



######-- Step 2B: Y-vectors for eaten food (whole Germany per year) -- #########

# Convert to a national Y-matrix (= Eaten)
Y_DGE_eaten <- Y_DGErec_diet / 1000000 * population * 365         #Converting unit (from "eaten food grams/person/day" to "national tonnes/year") 
Y_lancet_eaten <-Y_lancet_diet / 1000000 * population * 365
Y_EATveg_eaten <- Y_EATveg_diet / 1000000 * population * 365


#################################################################################################
######   Step 3: creating Y-vectors for different FW- scenarios (whole Germany per year) -- #####
#################################################################################################
# A: Only Diet Scenarios
# B: Minimum and maximum for Diet senarios (sensitivy analysis)
# c: FW-reduction scenarios for each dietary scenario
# D: Minimum and maximum for Diet senarios (sensitivy analysis)


### Step 3A ------- Y-vectors Consumption, dietary scenarios ----- ####


# add waste to reflect consumption step (for FOOTPRINT and supply-chain calculations):
waste <- read.csv2(file = "data/waste_data.csv") # also possible to choose minimum or maximum levels for the uncertainty analysis
Y_DGE <- add.consumer.waste(Y_DGE_eaten)
Y_lancet <- add.consumer.waste(Y_lancet_eaten)
Y_EATveg <- add.consumer.waste(Y_EATveg_eaten)

sum(Y_DGE)   # control    -with new food data is Y_DGE slightly bigger -> realistic due to increased food waste)
sum(Y[,"DEU_Food"])

# save as Y-matrices for footprint calculation (Y for consumption step)
write.csv2(Y_DGE, file = "data/Y_DGE.csv")
write.csv2(Y_lancet, file = "data/Y_lancet.csv") 
write.csv2(Y_EATveg, file = "data/Y_EATveg.csv") 



### Step 3B ----- Y-vectors Consumption, dietary scenarios MAX and MIN ----- ####

# maximum waste :
waste <- read.csv2(file = "data/waste_data_maximum.csv") # choose FW-data!
Y_DGE_mW <- add.consumer.waste(Y_DGE_eaten)
Y_lancet_mW <- add.consumer.waste(Y_lancet_eaten)
Y_EATveg_mW <- add.consumer.waste(Y_EATveg_eaten)

sum(Y_DGE_mW)   # control

# save as Y-matrices for footprint calculation (Y for consumption step)
write.csv2(Y_DGE_mW, file = "data/Y_DGE_maxW.csv")
write.csv2(Y_lancet_mW, file = "data/Y_lancet_maxW.csv") 
write.csv2(Y_EATveg_mW, file = "data/Y_EATveg_maxW.csv")

# minimum waste:
waste <- read.csv2(file = "data/waste_data_minimum.csv") # choose FW-data!
Y_DGE_mW <- add.consumer.waste(Y_DGE_eaten)
Y_lancet_mW <- add.consumer.waste(Y_lancet_eaten)
Y_EATveg_mW <- add.consumer.waste(Y_EATveg_eaten)

sum(Y_DGE_mW)   # 73735536 control

# save as Y-matrices for footprint calculation (Y for consumption step)
write.csv2(Y_DGE_mW, file = "data/Y_DGE_minW.csv")
write.csv2(Y_lancet_mW, file = "data/Y_lancet_minW.csv") 
write.csv2(Y_EATveg_mW, file = "data/Y_EATveg_minW.csv")



##### Step 3 -------- Scenarios for Halfing food waste   --------- #######


# Add food waste to Y_eaten according to scenarios of halfing food waste

waste <- read.csv2(file = "data/waste_data_halfingFW.csv") # files were generated in "1b_prep_food_waste_data.R"

Y_SQ50 <- add.consumer.waste(Y_SQ_eaten)
Y_DGE50 <- add.consumer.waste(Y_DGE_eaten)
Y_lancet50 <- add.consumer.waste(Y_lancet_eaten)
Y_EATveg50 <- add.consumer.waste(Y_EATveg_eaten)

sum(Y_SQ50)
sum(Y_DGE)
sum(Y_DGE50)

write.csv2(Y_SQ50, file = "data/Y_SQ_50.csv")
write.csv2(Y_DGE50, file = "data/Y_DGE_50.csv")
write.csv2(Y_lancet50, file = "data/Y_lancet_50.csv") 
write.csv2(Y_EATveg50, file = "data/Y_EATveg_50.csv") 

# ---- Uncertainty analysis for halfing food waste ----

# Choose scenario (MIN/MAX)
waste <- read.csv2(file = "data/waste_data_halfingFW_MIN.csv")
#waste <- read.csv2(file = "data/waste_data_halfingFW_MAX.csv")

Y_SQ50_mW <- add.consumer.waste(Y_SQ_eaten)
Y_DGE50_mW <- add.consumer.waste(Y_DGE_eaten)
Y_lancet50_mW <- add.consumer.waste(Y_lancet_eaten)
Y_EATveg50_mW <- add.consumer.waste(Y_EATveg_eaten)

sum(Y_DGE50)
sum(Y_DGE50_mW)

write.csv2(Y_SQ50_mW, file = "data/Y_SQ_50_MIN.csv")
write.csv2(Y_DGE50_mW, file = "data/Y_DGE_50_MIN.csv")
write.csv2(Y_lancet50_mW, file = "data/Y_lancet_50_MIN.csv") 
write.csv2(Y_EATveg50_mW, file = "data/Y_EATveg_50_MIN.csv") 


####### For visualization of diets #######
#Y_lancet <- Y_SQ_eaten[match(index$product, Diets$product)]
#Y_lancet <- cbind(as.character(Diets[, 1]), Diets[,5])

