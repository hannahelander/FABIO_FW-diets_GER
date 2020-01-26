######################################################
# ---------------------------------------------------
# Quantities
# ----------------------------------------------------
# needed functions: step.calculator() and step.calculator2()

#### load data (if needed):
#Y     <- readRDS(paste0(path,"2013_Y.rds"))
#waste <- read.csv2(file = "data/waste_data_frame.csv")
# index <- read.csv2(file = "data/index_data_frame.csv")


#########################################################
# along the Supply Chain - Status Quo 
#########################################################

# Total amount of food consumed in Germany
 
# Load Y-matrices
Y_SQ <- Y[ ,"DEU_Food"]                           # Status Quo
Y_DGE <- read.csv2(file = "data/Y_DGE.csv")      # DGE recommendation scenario (adapted to calories demand)
Y_DGE <- Y_DGE[,2]
Y_DGE <- as.numeric(Y_DGE)
Y_lancet <- read.csv2(file = "data/Y_lancet.csv")
Y_lancet <- Y_lancet[,2] 
Y_EATveg <- read.csv2(file = "data/Y_EATveg.csv")
Y_EATveg <- Y_EATveg[,2] 

# Load Y-matrices for FWL minimum and maximum
Y_DGE_minW <- read.csv2(file = "data/Y_DGE_minimumWaste.csv")      # DGE recommendation scenario (adapted to calories demand)
Y_DGE_minW <- Y_DGE_minW[,2]
Y_lancet_minW <- read.csv2(file = "data/Y_lancet_minimumWaste.csv")
Y_lancet_minW <- Y_lancet_minW[,2] 
Y_EATveg_minW <- read.csv2(file = "data/Y_EATveg_minimumWaste.csv")
Y_EATveg_minW <- Y_EATveg_minW[,2] 

Y_DGE_maxW <- read.csv2(file = "data/Y_DGE_maximumWaste.csv")      # DGE recommendation scenario (adapted to calories demand)
Y_DGE_maxW <- Y_DGE_maxW[,2]
Y_lancet_maxW <- read.csv2(file = "data/Y_lancet_maximumWaste.csv")
Y_lancet_maxW <- Y_lancet_maxW[,2] 
Y_EATveg_maxW <- read.csv2(file = "data/Y_EATveg_maximumWaste.csv")
Y_EATveg_maxW <- Y_EATveg_maxW[,2] 


# per capita footprint
population <- 80645605 #2013, Source: World bank 
Y_capita <- sum(Y_tot) / population             # gives ~1 ton ( ~ 2,69 kg per capita/day including what is thrown away)

sum(Y_SQ)
### Choose scenario
Y_tot <- Y_SQ
waste <- read.csv2(file = "data/waste_data.csv") # must be consistent with scenario (note that for SQ the diet depend on wastage levels)

# create output matrix
supply_chain_Y <- data.frame(chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))




############# For Plant-based products ##############
# Create a Y-matrix where all animal-products are 0
Y_plant <- Y_tot
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0

### Calculate Footprints of flows and fill in Table: 
## Production
#Y_prod_waste <- sum(Y_plant * waste$harvest_production) / 100               # waste shares are given as percentage (8% = 0,08)
#supply_chain_Y$harvest_production <- c(sum(Y_plant), Y_prod_waste, NA, NA) # column has 4 rows # FAO production data reflect 'cont' flow

# Storage
Output_storage    <- step.calculator(waste$storage_transport, Y_plant)      # calculate
supply_chain_Y$storage_transport  <- Output_storage[[1]]                    # add values to table
rm(Y_plant)                                                                 # remove big data to save space

# Processing
Output_processing       <- step.calculator(waste$processing, Output_storage[[2]])
supply_chain_Y$processing         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]])
supply_chain_Y$distribution       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]])
supply_chain_Y$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)

### save the Y matrix that represent only "eaten food":
eaten_food_plants <- Output_consumption[[2]]

rm(Output_consumption)


######## For animal based products (Livestock) ###########
# Create Y matrix that only includes animal-based products (Livestock)

Y_lvst <- Y_tot
Y_lvst[index$product_group %in% c("Crop products", "Primary crops")] <- 0  # Set Y to 0 for all plant-based products
Y_lvst <- Y_meat

######## Calculate Footprints of flows and fill in Table: 
# Production 
#Y_prod_waste <- sum(Y_lvst * waste$harvest_production) / 100               # waste shares are given as percentage (8% = 0,08)
#supply_chain_Y$harvest_production[3:4] <- c(sum(Y_lvst), Y_prod_waste) # column has 4 rows # FAO production data reflect 'cont' flow

# Storage and transport
Output_storage    <- step.calculator2(waste$storage_transport, Y_lvst)
supply_chain_Y$storage_transport[3:4]  <- Output_storage[[1]]
rm(Y_lvst)

# Processing
Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_Y$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_Y$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_Y$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)



write.csv2(supply_chain_Y, file = "output/supply_chain_mass_MEAT_MILK_SQ.csv")




############ save the Y matrix that represent only "eaten food":#############
eaten_food_lvst <- Output_consumption[[2]]

rm(Output_consumption)


Y_SQ_eaten <- eaten_food_plants + eaten_food_lvst
write.csv2(Y_SQ_eaten, file = "data/Y_SQ_eaten_maxW.csv") # for minimum Waste (SQ eaten depends on waste levels)




#write.csv2(Y_eaten_food, file = "data/14.8.2019/Y_eaten_food.csv")     # write to file in output-folder! 
#write.csv2(Y_SQ_eaten, file = "data/9.9.2019/Y_SQ_eaten.csv")     # write to file in output-folder! 


supply_chain_Y$Consumption/80000
sum(eaten_food_lvst[index$product=="Milk - Excluding Butter"])/80000
sum(eaten_food_lvst[index$product=="Fish, Seafood"])/80000

