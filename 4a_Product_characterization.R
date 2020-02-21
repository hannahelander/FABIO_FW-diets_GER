#############

# A) This script will calculate the average footprint per kg of German consumption per product group.
# B) it makes the supply chain analysis in quantities to get FW per kg, which is later converted to FW per kcal in excel.


############# ----- Functions -------##########

step.calculator2 <- function(waste_step, Y_Q){                    
  YQ_waste <- Y_Q * (waste_step / 100)
  YQ_cont <- Y_Q - YQ_waste              
  YQ_step <- c(sum(YQ_cont), sum(YQ_waste))
  output <- list(YQ_step, YQ_cont)                                # gives a vector with only 2 elements
  return(output)
}


## Load index and waste data
index <- read.csv2("data/index_data_frame.csv")
waste <- read.csv2(file = "data/waste_data.csv")

####### create Y-vectors for each product groups (using com_groups and diet_groups)########

Y_cereals <- Y_SQ
Y_cereals[index$com_group != "Cereals"] <- 0
Y_roots <- Y_SQ
Y_roots[index$com_group != "Roots and tubers"] <- 0
Y_veg    <- Y_SQ
Y_veg[index$diet_group != "Vegetables, pulses, spices"] <- 0
Y_fruits <- Y_SQ 
Y_fruits[index$diet_group != "Fruits"] <- 0
Y_beans  <- Y_SQ
Y_beans[index$diet_group != "Oil crops and nuts"] <- 0
Y_oils   <- Y_SQ
Y_oils[index$diet_group != "Vegetable oils"] <- 0
Y_milk <- Y_SQ
Y_milk[index$com_group != "Milk"] <- 0
Y_eggs   <- Y_SQ
Y_eggs[index$diet_group != "Eggs"] <- 0
Y_fish   <- Y_SQ
Y_fish[index$diet_group != "Fish"] <- 0
Y_meat <- Y_SQ
Y_meat[index$diet_group != "Meat"] <- 0
Y_sugalc <- Y_SQ
Y_sugalc[!index$diet_group %in% c("Sugar, sweeteners", "Alcohol")] <- 0

product_Y_list <- list(Y_cereals, Y_roots, Y_veg, Y_fruits, Y_beans, Y_oils, Y_milk, Y_eggs, Y_fish, Y_meat, Y_sugalc)

######
# Create data frame
prod_character <- data.frame(product_group = c("Cereals", "Potatoes & roots", "Vegetables", "Fruits", 
                                               "Pulses, beans & nuts",  "Vegetable oils",  "Milk & products", 
                                               "Eggs", "Fish", "Meat", "Sugar & Alcohol"),
                            Kg_ger  = rep(NA, length(product_Y_list)), 
                            Biomass = rep(NA, length(product_Y_list)), 
                            Land    = rep(NA, length(product_Y_list)),
                            Water   = rep(NA, length(product_Y_list)))

############## Quantities ################

for (i in 1:length(product_Y_list)){
  prod_character$Kg_ger[i] <- sum(product_Y_list[[i]])
}

############## Footprints #################

## Biomass
L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))

e <- E$Biomass / X
e[!is.finite(e)] <- 0
MP <- e * L                           
rm(L)
rm(E)

for (i in 1:length(product_Y_list)){
  Y_tot <- product_Y_list[[i]]  
  FP_tot <- t(t(MP) * Y_tot)
  prod_character$Biomass[i] <- sum(FP_tot)/prod_character$Kg_ger[i]
}


## Landuse
L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))

e <- E$Landuse / X
e[!is.finite(e)] <- 0
MP <- e * L                           
rm(L)
rm(E)

for (i in 1:length(product_Y_list)){
  Y_tot <- product_Y_list[[i]]  
  FP_tot <- t(t(MP) * Y_tot)
  prod_character$Land[i] <- sum(FP_tot)/prod_character$Kg_ger[i]
}


## Water
L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))

e <- E$Blue_water / X
e[!is.finite(e)] <- 0
MP <- e * L                           
rm(L)
rm(E)

for (i in 1:length(product_Y_list)){
  Y_tot <- product_Y_list[[i]]
  FP_tot <- t(t(MP) * Y_tot)
  prod_character$Water[i] <- sum(FP_tot)/prod_character$Kg_ger[i]
}


write.csv2(prod_character, file = "output/product_characterization.csv")



################### B) Supply chain quantities ########################

###### create a df using the same product categories as in Diets (based on index$com_group & products #########
supply_chain <- data.frame(chain_type = c("Cereals", "Cereals", "Potatoes & roots",  "Potatoes & roots", "Vegetables", "Vegetables", "Fruits", "Fruits",
                                         "Pulses, beans & nuts",  "Pulses, beans & nuts", "Vegetable oils", "Vegetable oils", "Milk & products", 
                                         "Milk & products", "Eggs", "Eggs", "Fish", "Fish", "Meat",  "Meat", "Sugar & Alcohol", "Sugar & Alcohol"),
                          flow = c("cont", "waste", "cont", "waste", "cont", "waste", "cont", "waste", "cont", "waste", "cont", "waste", "cont", 
                                 "waste", "cont", "waste", "cont", "waste", "cont", "waste", "cont", "waste"), 
                          storage_transport = rep(NA, length(product_Y_list)*2),
                          processing   = rep(NA, length(product_Y_list)*2),
                          distribution = rep(NA, length(product_Y_list)*2),
                          consumption  = rep(NA, length(product_Y_list)*2))


for (i in 1:length(product_Y_list)){
  Y_tot <- product_Y_list[[i]]  # choose scenario consistent with waste scenario
  Output_storage    <- step.calculator2(waste$storage_transport, Y_tot)  # Storage
  supply_chain$storage_transport[((2*i)-1):(2*i)]  <- Output_storage[[1]] 
  Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]]) # Processing
  supply_chain$processing[((2*i)-1):(2*i)]         <- Output_processing[[1]]
  rm(Output_storage)
  Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]]) # Distribution
  supply_chain$distribution[((2*i)-1):(2*i)]      <- Output_distribution[[1]]
  rm(Output_processing)
  Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]]) # Consumption
  supply_chain$consumption[((2*i)-1):(2*i)]        <- Output_consumption[[1]]
  rm(Output_distribution)
  rm(Output_consumption)
}



########### Write to File #############
write.csv2(supply_chain, file = "output/product_characteristics_supply_chain_quantities.csv")


