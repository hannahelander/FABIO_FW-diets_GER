#################################
# This script reads data for diet references as Y-vectors
# Structure them for visualisation 
# SQ is taken from script: 2b_quantities and converted.

# notes: eaten is used to refer to national data but the step after consumption ("eaten")
# diet refer to gram per person and day

### Y vectors on eaten food ##
Y_SQ_eaten <- read.csv2(file = "data/Y_SQ_eaten.csv")
Y_SQ_eaten <- Y_SQ_eaten[,2]
Y_SQ_eaten[index$DGE_group == "excluded"] <- 0  # need to adjust some inconsistency in data
Y_SQ_diet <- Y_SQ_eaten * 1000000 / population / 365 #Converting unit (from "national tonnes/year" to "eaten food grams/person/day")

sum(Y_SQ_eaten) # = 62117393
sum(Y_SQ_diet) # 2110.278

### read data from file for the other diets
Diets <- read.csv2(file ="input/Diets.csv" )
Diets[,1] <- as.character(Diets[, 1])



# Function to add the consumer waste to eaten (step 3):
add.consumer.waste <- function(Yeaten){
  Yreal <- Yeaten/(100-waste$final_consumption)*100
  Yreal <- Yreal/(100-waste$distribution)*100
  Yreal <- Yreal/(100-waste$processing)*100
  Yreal <- Yreal/(100-waste$storage_transport)*100
  return(Yreal)
}


############# Contain errors!! ###########################
# construct Y-vector for DGE rec (2796 kcal)
Y_DGErec_diet <- Y_SQ_diet
for (i in 1:nrow(Diets)){
  #Sum_i <- sum(Y_SQ_diet[which(index$product %in% Diets$product[i])])
  Y_DGErec_diet[which(index$item_code %in% Diets$Item_code[i])] <- Diets$DGErec_g[i]*(Y_SQ_diet[which(index$item_code %in% Diets$Item_code[i])]/sum(Y_SQ_diet[which(index$item_code %in% Diets$Item_code[i])]))
  
  #cat(paste0("for i=",i, " sum is ", (sum(Y_DGErec_diet[which(index$product %in% Diets$product[i])]))), "\n")
}
Y_DGErec_diet[is.na(Y_DGErec_diet)] <- 0 # delete NaNs
sum(Y_DGErec_diet)    # gives 1927.2 =D



# construct Y-vector for eat lancet (2796 kcal)
Y_lancet_diet <- Y_SQ_diet
for (i in 1:nrow(Diets)){
  Y_lancet_diet[which(index$item_code %in% Diets$Item_code[i])] <- Diets$EAT_g[i]*(Y_SQ_diet[which(index$item_code %in% Diets$Item_code[i])]/sum(Y_SQ_diet[which(index$item_code %in% Diets$Item_code[i])]))
}
Y_lancet_diet[is.na(Y_lancet_diet)] <- 0
sum(Y_lancet_diet)    # gives 1707.6 !!



# construct Y-vector for plant-based (2796 kcal)
Y_plantbased_diet <- Y_SQ_diet
for (i in 1:nrow(Diets)) {
  Y_plantbased_diet[which(index$item_code %in% Diets$Item_code[i])] <- Diets$Plant_g[i]*(Y_SQ_diet[which(index$item_code %in% Diets$Item_code[i])]/sum(Y_SQ_diet[which(index$item_code %in% Diets$Item_code[i])]))
}
Y_plantbased_diet[is.na(Y_plantbased_diet)] <- 0
sum(Y_plantbased_diet)    # 


# Save as diets
write.csv2(Y_SQ_diet, file = "data/diets/Y_SQ_diet.csv") 
write.csv2(Y_DGErec_diet, file = "data/diets/Y_DGE_diet.csv") 
write.csv2(Y_lancet_diet, file = "data/diets/Y_lancet_diet.csv") 
write.csv2(Y_plantbased_diet, file = "data/diets/Y_plantbased_diet.csv") 


# Convert to a national Y-matrix
Y_DGE_eaten <- Y_DGErec_diet   / 1000000 * population * 365         #Converting unit (from "eaten food grams/person/day" to "national tonnes/year") 
Y_lancet_eaten <-Y_lancet_diet / 1000000 * population * 365
Y_plantbased_eaten <- Y_plantbased_diet / 1000000 * population * 365


# add waste to reflect consumption step (for FOOTPRINT and supply-chain calculations):
waste <- read.csv2(file = "data/waste_data_frame.csv")
Y_DGE <- add.consumer.waste(Y_DGE_eaten)
Y_lancet <- add.consumer.waste(Y_lancet_eaten)
Y_plantbased <- add.consumer.waste(Y_plantbased_eaten)


sum(Y_DGE)   # control    -   shows that sum Y_DGE is slightly bigger -> realistic due to increased food waste
sum(Y[,"DEU_Food"])

# save as Y-matrices for footprint calculation (Y for consumption step)
write.csv2(Y_DGE, file = "data/Y_DGE.csv")
write.csv2(Y_lancet, file = "data/Y_lancet.csv") 
write.csv2(Y_plantbased, file = "data/Y_plantbased.csv") 






####### For visualization of diets #######
Y_lancet <- Y_SQ_eaten[match(index$product, Diets$product)]
Y_lancet <- cbind(as.character(Diets[, 1]), Diets[,5])

