#################################
# This script reads data for diet references as Y-vectors
# Structure them for visualisation 

# read data
Diets <- read.csv2(file ="input/Diets.csv" )
Diets[,1] <- as.character(Diets[, 1])

# get Y-vector of SQ eaten
Y_eaten <- read.csv2(file = "data/9.9.2019/Y_SQ_eaten.csv")
Y_SQ_eaten <- Y_eaten[,2]
Y_SQ_eaten[index$DGE_group == "excluded"] <- 0  # need to adjust some inconsistency in data

# get Y-vector for DGE rec

# construct Y-vector for eat lancet
Y_lancet <- Y_SQ_eaten
for (i in 1:length(Y_lancet)) {
  Y_lancet[i] <- Diets[match(index$product, Diets$product), 6]} # warning due to lack of replacement (should one add an if-loop?)

# construct Y-vector for plant-based
Y_plantbased <- Y_SQ_eaten
for (i in 1:length(Y_plantbased)) {
  Y_plantbased[i] <- Diets[match(index$product, Diets$product), 7]} # warning due to lack of replacement (should one add an if-loop?)


# Save as diets
write.csv2(Y_lancet, file = "data/diets/Y_lancet_eaten.csv") 
write.csv2(Y_plantbased, file = "data/diets/Y_plantbased_eaten.csv") 

# Convert to Y-matrix reflecting consumption step for footprint and supply-chain calculations:
Y_lancet_ <- Y_lancet (1- FOOTPRINT!!! )

####### For visualization of diets #######


Y_lancet <- Y_SQ_eaten[match(index$product, Diets$product)]
Y_lancet <- cbind(as.character(Diets[, 1]), Diets[,5])

