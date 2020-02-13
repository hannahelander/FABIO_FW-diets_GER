#############

# A) This script will calculate the average footprint per kg of German consumption per product group.
# B) it makes the supply chain analysis in quantities to get FW per kg, which is later converted to FW per kcal in excel.


#############
#### Function to calculate flows per step  #####
step.calculator3 <- function(waste_step, FP){                      # waste_step is a column in the data-frame "waste", eg. waste$harvest_production?
  FP_waste <- t(t(FP) * (waste_step / 100))
  FP_cont <- FP  - FP_waste              
  FP_step <- c(sum(FP_cont), sum(FP_waste), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) # nrow of matrix
  output <- list(FP_step, FP_cont)                                # gives one vector with 4 elements & a new footprint-matrix (or Y-matrix)
  return(output)
}

step.calculator2 <- function(waste_step, FP){                      # waste_step is column in waste, eg. waste$harvest_production?
  FP_waste <- t(t(FP) * (waste_step / 100))
  FP_cont <- FP  - FP_waste              
  FP_step <- c(sum(FP_cont), sum(FP_waste))
  output <- list(FP_step, FP_cont)                                # gives a vector with only 2 elements
  return(output)
}


############## A: Footprints #################

e <- E$Biomass / X
e[!is.finite(e)] <- 0
MP <- e * L                           # L needed

rm(L)
rm(E)

## Load index and waste data
index <- read.csv2("data/index_data_frame.csv")


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

######
# Create data frame


### Calculate footprints and add to data frame
Y_tot <- Y_EATveg  # choose scenario consistent with waste scenario


# Total footprint -
FP_tot <- t(t(MP) * Y_tot)





##### B) Supply chain quantities ##############

###### create a df using the same product categories as in Diets (based on index$com_group & products #########
supply_chain_FP <- data.frame(chain_type = c("Cereals", "Cereals", "Potatoes & roots",  "Potatoes & roots", "Vegetables", "Vegetables", "Fruits", "Fruits",
                                             "Pulses, beans & nuts",  "Pulses, beans & nuts", "Vegetable oils", "Vegetable oils", "Milk & products", 
                                             "Milk & products", "Eggs", "Eggs", "Fish", "Fish", "Meat",  "Meat", "Sugar & Alcohol", "Sugar & Alcohol"),
                              flow = c("cont", "waste", "cont", "waste", "cont", "waste", "cont", "waste", "cont", "waste", "cont", "waste", "cont", 
                                       "waste", "cont", "waste", "cont", "waste", "cont", "waste", "cont", "waste"))
#for (i in 1:ncol(Y_vectors)){
#Y_prod <- Y_vectors[i]

###### NEW PRODUCT (1) ##########
FP_cereals <- t(t(MP) * Y_cereals)  

# Storage
Output_storage    <- step.calculator3(waste$storage_transport, FP_cereals)      # Last argument takes either Y_cereals (for mass calculations) or FP_cereals
supply_chain_FP$storage_transport  <- Output_storage[[1]]                     # add values to table
rm(Y_cereals)                                                                 # remove big data to save space
rm(FP_cereals)

# Processing
Output_processing       <- step.calculator3(waste$processing, Output_storage[[2]])
supply_chain_FP$processing         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator3(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator3(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)
#eaten_food_plants <- Output_consumption[[2]]   ### save the Y matrix that represent only "eaten food"
rm(Output_consumption)

###### NEW PRODUCT (2) ##########
FP_roots <- t(t(MP) * Y_roots) 

# Storage
Output_storage    <- step.calculator2(waste$storage_transport, FP_roots)
supply_chain_FP$storage_transport[3:4]  <- Output_storage[[1]]
rm(Y_roots)
rm(FP_roots)

Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])  # Processing
supply_chain_FP$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)

Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])   # Distribution
supply_chain_FP$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)

Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])  # Consumption
supply_chain_FP$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


###### NEW PRODUCT (3) ########## 
FP_veg <- t(t(MP) * Y_veg) 

Output_storage    <- step.calculator2(waste$storage_transport, FP_veg)
supply_chain_FP$storage_transport[5:6]  <- Output_storage[[1]]
rm(Y_veg)
rm(FP_veg)

Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_FP$processing[5:6]         <- Output_processing[[1]]
rm(Output_storage)

Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution[5:6]       <- Output_distribution[[1]]
rm(Output_processing)

Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption[5:6]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


###### NEW PRODUCT (4) ##########
FP_fruits <- t(t(MP) * Y_fruits) 


Output_storage    <- step.calculator2(waste$storage_transport, FP_fruits)
supply_chain_FP$storage_transport[7:8]  <- Output_storage[[1]]
rm(Y_fruits)
rm(FP_fruits)

Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_FP$processing[7:8]         <- Output_processing[[1]]
rm(Output_storage)

Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution[7:8]       <- Output_distribution[[1]]
rm(Output_processing)

Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption[7:8]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


###### NEW PRODUCT (5) ##########
FP_beans  <- t(t(MP) * Y_beans) 


Output_storage    <- step.calculator2(waste$storage_transport, FP_beans)  # Storage
supply_chain_FP$storage_transport[9:10]  <- Output_storage[[1]]
rm(Y_beans)
rm(FP_beans)

Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])  # Processing
supply_chain_FP$processing[9:10]         <- Output_processing[[1]]
rm(Output_storage)

Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])  # Distribution
supply_chain_FP$distribution[9:10]       <- Output_distribution[[1]]
rm(Output_processing)

Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])  # Consumption
supply_chain_FP$Consumption[9:10]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)
#

###### NEW PRODUCT (6) ##########
FP_oils  <- t(t(MP) * Y_oils) 


Output_storage    <- step.calculator2(waste$storage_transport, FP_oils)   # Storage
supply_chain_FP$storage_transport[11:12]  <- Output_storage[[1]]
rm(Y_oils)
rm(FP_oils)

Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])   # Processing
supply_chain_FP$processing[11:12]         <- Output_processing[[1]]
rm(Output_storage)

Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])   # Distribution
supply_chain_FP$distribution[11:12]       <- Output_distribution[[1]]
rm(Output_processing)

Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])   # Consumption
supply_chain_FP$Consumption[11:12]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)

###### NEW PRODUCT (7) ##########
FP_milk   <- t(t(MP) * Y_milk) 


Output_storage    <- step.calculator2(waste$storage_transport, FP_milk)   # Storage
supply_chain_FP$storage_transport[13:14]  <- Output_storage[[1]]
rm(Y_milk)
rm(FP_milk)

Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]]) # Processing
supply_chain_FP$processing[13:14]         <- Output_processing[[1]]
rm(Output_storage)

Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])  # Distribution
supply_chain_FP$distribution[13:14]       <- Output_distribution[[1]]
rm(Output_processing)

Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]]) # Consumption
supply_chain_FP$Consumption[13:14]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)



###### NEW PRODUCT (8) ##########
FP_eggs  <- t(t(MP) * Y_eggs) 



Output_storage    <- step.calculator2(waste$storage_transport, FP_eggs)  # Storage
supply_chain_FP$storage_transport[15:16]  <- Output_storage[[1]]
rm(Y_eggs)
rm(FP_eggs)

Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])   # Processing
supply_chain_FP$processing[15:16]         <- Output_processing[[1]]
rm(Output_storage)

Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])  # Distribution
supply_chain_FP$distribution[15:16]       <- Output_distribution[[1]]
rm(Output_processing)

Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])  # Consumption
supply_chain_FP$Consumption[15:16]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)

###### NEW PRODUCT (9) ##########
FP_fish  <- t(t(MP) * Y_fish) 

Output_storage    <- step.calculator2(waste$storage_transport, FP_fish)  # Storage
supply_chain_FP$storage_transport[17:18]  <- Output_storage[[1]]
rm(Y_fish)
rm(FP_fish)

Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])   # Processing
supply_chain_FP$processing[17:18]         <- Output_processing[[1]]
rm(Output_storage)

Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])   # Distribution
supply_chain_FP$distribution[17:18]       <- Output_distribution[[1]]
rm(Output_processing)

Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])  # Consumption
supply_chain_FP$Consumption[17:18]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)

###### NEW PRODUCT (10) ##########
FP_meat  <- t(t(MP) * Y_meat) 


Output_storage    <- step.calculator2(waste$storage_transport, FP_meat)   # Storage
supply_chain_FP$storage_transport[19:20]  <- Output_storage[[1]]
rm(Y_meat)

Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])   # Processing
supply_chain_FP$processing[19:20]         <- Output_processing[[1]]
rm(Output_storage)

Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])  # Distribution
supply_chain_FP$distribution[19:20]       <- Output_distribution[[1]]
rm(Output_processing)

Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])  # Consumption
supply_chain_FP$Consumption[19:20]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)

###### NEW PRODUCT (11) ##########
FP_sugalc  <- t(t(MP) * Y_sugalc) 


Output_storage    <- step.calculator2(waste$storage_transport, FP_sugalc) # Storage
supply_chain_FP$storage_transport[21:22]  <- Output_storage[[1]]
rm(Y_sugalc)
rm(FP_sugalc)

Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]]) # Processing
supply_chain_FP$processing[21:22]         <- Output_processing[[1]]
rm(Output_storage)

Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]]) # Distribution
supply_chain_FP$distribution[21:22]       <- Output_distribution[[1]]
rm(Output_processing)

Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])  # Consumption
supply_chain_FP$Consumption[21:22]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


########### Write to File #############
write.csv2(supply_chain_FP, file = "output/product_characteristics_supply_chain_quantities.csv")     # write to file in output-folder! 


