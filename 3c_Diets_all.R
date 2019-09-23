#################################
# This script reads data for diet references as Y-vectors
# Structure them for visualisation 
# SQ is taken from script: 2b_quantities and converted.

# notes: eaten is used to refer to national data but the step after consumption ("eaten")
# diet refer to gram per person and day

###### SQ original data #######
Y_SQ_eaten <- read.csv2(file = "data/9.9.2019/Y_SQ_eaten.csv")
Y_SQ_eaten <- Y_eaten[,2]
Y_SQ_eaten[index$DGE_group == "excluded"] <- 0  # need to adjust some inconsistency in data
Y_SQ_diet <- Y_SQ_eaten * 1000000 / population / 365 #Converting unit (from "national tonnes/year" to "eaten food grams/person/day")


# read data from file for the other diets
Diets <- read.csv2(file ="input/Diets.csv" )
Diets[,1] <- as.character(Diets[, 1])


############# DOESN'T WORK!! ###########################
# construct Y-vector for DGE rec (2796 kcal)
Y_DGErec_diet <- Y_SQ_diet

  
for (i in 1:nrow(Diets)){
  Y_DGErec_diet[which(index$product %in% Diets$product[i])] <- Diets$DGErec_g[i]
}



# # construct Y-vector for eat lancet (2796 kcal)
# Y_lancet_diet <- Y_SQ_diet
# for (i in 1:length(Y_lancet_diet)) {
#   Y_lancet_diet[i] <- Diets[match(index$product[i], Diets$product), 6]} # warning due to lack of replacement (should one add an if-loop?)
# 
# # construct Y-vector for plant-based (2796 kcal)
# Y_plantbased_diet <- Y_SQ_diet
# for (i in 1:length(Y_plantbased_diet)) {
#   Y_plantbased_diet[i] <- Diets[match(index$product, Diets$product), 7]} # warning due to lack of replacement (should one add an if-loop?)




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
Y_lancet <- Y_lancet_eaten / (1 - waste$final_consumption/100)
Y_plantbased <- Y_plantbased_eaten / (1 - waste$final_consumption/100)

# save as Y-matrices for footprint calculation (Y for consumption step)
write.csv2(Y_DGE, file = "data/Y_DGE.csv")
write.csv2(Y_lancet, file = "data/Y_lancet.csv") 
write.csv2(Y_plantbased, file = "data/Y_plantbased.csv") 



####### For visualization of diets #######
Y_lancet <- Y_SQ_eaten[match(index$product, Diets$product)]
Y_lancet <- cbind(as.character(Diets[, 1]), Diets[,5])

