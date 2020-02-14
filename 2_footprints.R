######################################################
# ---------------------------------------------------
# Footprints
# ----------------------------------------------------
# Calculate Footprints along the supply chain. 
##########################################################

# --------------- LOAD DATA ----------------- #
## Prepare extension and define footprint (if needed)
e <- E$Blue_water / X
e[!is.finite(e)] <- 0
MP <- e * L                           # L needed

rm(L)
rm(E)

## Load index and waste data
index <- read.csv2("data/index_data_frame.csv")




# --------------- RUN FUNCTIONS ------------------ #
#### Function to calculate flows per step  #####
step.calculator <- function(waste_step, FP){                      # waste_step is a column in the data-frame "waste", eg. waste$harvest_production?
  FP_waste <- t(t(FP) * (waste_step / 100))
  FP_cont <- FP  - FP_waste              
  FP_step <- c(sum(FP_cont), sum(FP_waste), NA, NA)
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

#########################################################
# Footprints along the Supply Chain 
#########################################################

# Load Y-matrices
Y_SQ <- Y[ ,"DEU_Food"]                           # Status Quo

Y_DGE <- read.csv2(file = "data/Y_DGE.csv")      # DGE recommendation scenario (adapted to calories demand)
Y_DGE <- Y_DGE[,2]
Y_lancet <- read.csv2(file = "data/Y_lancet.csv")
Y_lancet <- Y_lancet[,2] 
Y_EATveg <- read.csv2(file = "data/Y_EATveg.csv")
Y_EATveg <- Y_EATveg[,2] 
# Load Y-matrices for FWL minimum and maximum
Y_DGE_minW <- read.csv2(file = "data/Y_DGE_minW.csv")      # DGE recommendation scenario (adapted to calories demand)
Y_DGE_minW <- Y_DGE_minW[,2]
Y_lancet_minW <- read.csv2(file = "data/Y_lancet_minW.csv")
Y_lancet_minW <- Y_lancet_minW[,2] 
Y_EATveg_minW <- read.csv2(file = "data/Y_EATveg_minW.csv")
Y_EATveg_minW <- Y_EATveg_minW[,2] 

Y_DGE_maxW <- read.csv2(file = "data/Y_DGE_maxW.csv")      # DGE recommendation scenario (adapted to calories demand)
Y_DGE_maxW <- Y_DGE_maxW[,2]
Y_lancet_maxW <- read.csv2(file = "data/Y_lancet_maxW.csv")
Y_lancet_maxW <- Y_lancet_maxW[,2] 
Y_EATveg_maxW <- read.csv2(file = "data/Y_EATveg_maxW.csv")
Y_EATveg_maxW <- Y_EATveg_maxW[,2] 

Y_SQ50 <- read.csv2(file = "data/Y_SQ_50.csv")      # SQ-eaten (diet) + 50% FW reduction
Y_SQ50 <- Y_SQ50[,2]

Y_DGE50 <- read.csv2(file = "data/Y_DGE_50.csv")      # DGE + 50% FW reduction
Y_DGE50 <- Y_DGE50[,2]
Y_DGE50_MIN <- read.csv2(file = "data/Y_DGE_50_MIN.csv")
Y_DGE50_MIN <- Y_DGE50_MIN[,2]
Y_DGE50_MAX <- read.csv2(file = "data/Y_DGE_50_MAX.csv")
Y_DGE50_MAX <- Y_DGE50_MAX[,2]

Y_lancet50 <- read.csv2(file = "data/Y_lancet_50.csv")      # Lancet with 50% FW reduction 
Y_lancet50 <- Y_lancet50[,2]
Y_lancet50_MIN <- read.csv2(file = "data/Y_lancet_50_MIN.csv")  # uncertainty analysis (MIN)
Y_lancet50_MIN <- Y_lancet50_MIN[,2]
Y_lancet50_MAX <- read.csv2(file = "data/Y_lancet_50_MAX.csv")  # uncertainty analysis (MAX)
Y_lancet50_MAX <- Y_lancet50_MAX[,2]


Y_EATveg50 <- read.csv2(file = "data/Y_EATveg_50.csv")      # Vegetarian scenario with 50% FW reduction 
Y_EATveg50 <- Y_EATveg50[,2]
Y_EATveg50_MIN <- read.csv2(file = "data/Y_EATveg_50_MIN.csv")  # uncertainty analysis (MIN)
Y_EATveg50_MIN <- Y_EATveg50_MIN[,2]
Y_EATveg50_MAX <- read.csv2(file = "data/Y_EATveg_50_MAX.csv")  #  # uncertainty analysis (MAX)
Y_EATveg50_MAX <- Y_EATveg50_MAX[,2]



sum(Y_DGE50_MIN)
sum(Y_DGE)

sum(Y_DGE50)

sum(Y_DGE50_MAX)
sum(Y_SQ)
sum(Y_lancet_maxW)
sum(Y_EATveg)






##### SCENARIO CHOISE ###########
waste <- read.csv2(file = "data/waste_data_halfingFW.csv") # need to be consistent with diet scenario! 
Y_tot <- Y_SQ50  # choose scenario consistent with waste scenario
  
  
# Total footprint -
FP_tot <- t(t(MP) * Y_tot)           

########## create output matrix ###########################
supply_chain_FP <- data.frame(chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))


##############################################
# For Plant-based products--------------------

# Create a Y-matrix where all animal-products are 0
Y_plant <- Y_tot 
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0

FP_plant <- t(t(MP) * Y_plant)        # Total footprint of all plant-based products
#FP_plant_capita <- sum(FP_plant) / population


# Storage
Output_storage    <- step.calculator(waste$storage_transport, FP_plant)      # calculate
supply_chain_FP$storage_transport  <- Output_storage[[1]]                    # add values to table
rm(FP_plant)                                                                # remove big data to save space

# Processing
Output_processing       <- step.calculator(waste$processing, Output_storage[[2]])
supply_chain_FP$processing         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)



##############################################################
# For animal based products (Livestock) ----------------------
##############################################################

# Create Y matrix that only includes animal-based products (Livestock)
Y_lvst <- Y_tot
Y_lvst[index$product_group %in% c("Crop products", "Primary crops")] <- 0  # Set Y to 0 for all plant-based products


FP_lvst <- t(t(MP) * Y_lvst)        # Total footprint of all animal-based products
#supply_chain_FP$harvest_production[3:4] <- c(sum(FP_lvst), FP_prod_waste) # column has 4 rows # FAO production data reflect 'cont' flow

# Storage and transport
Output_storage    <- step.calculator2(waste$storage_transport, FP_lvst)
supply_chain_FP$storage_transport[3:4]  <- Output_storage[[1]]
rm(FP_lvst)

# Processing
Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_FP$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


########### Write to File #############
write.csv2(supply_chain_FP, file = "output/supply_chain_WATER_SQ50.csv")     # write to file in output-folder! 





L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))

e <- E$Landuse / X
e[!is.finite(e)] <- 0
MP <- e * L                           # L needed

rm(L)
rm(E)



#############################

##### SCENARIO CHOISE ###########
waste <- read.csv2(file = "data/waste_data_halfingFW.csv") # need to be consistent with diet scenario! 
Y_tot <- Y_SQ50  # choose scenario consistent with waste scenario


# Total footprint -
FP_tot <- t(t(MP) * Y_tot)           

########## create output matrix ###########################
supply_chain_FP <- data.frame(chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))


##############################################
# For Plant-based products--------------------

# Create a Y-matrix where all animal-products are 0
Y_plant <- Y_tot 
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0

FP_plant <- t(t(MP) * Y_plant)        # Total footprint of all plant-based products
#FP_plant_capita <- sum(FP_plant) / population


# Storage
Output_storage    <- step.calculator(waste$storage_transport, FP_plant)      # calculate
supply_chain_FP$storage_transport  <- Output_storage[[1]]                    # add values to table
rm(FP_plant)                                                                # remove big data to save space

# Processing
Output_processing       <- step.calculator(waste$processing, Output_storage[[2]])
supply_chain_FP$processing         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)



##############################################################
# For animal based products (Livestock) ----------------------
##############################################################

# Create Y matrix that only includes animal-based products (Livestock)
Y_lvst <- Y_tot
Y_lvst[index$product_group %in% c("Crop products", "Primary crops")] <- 0  # Set Y to 0 for all plant-based products


FP_lvst <- t(t(MP) * Y_lvst)        # Total footprint of all animal-based products
#supply_chain_FP$harvest_production[3:4] <- c(sum(FP_lvst), FP_prod_waste) # column has 4 rows # FAO production data reflect 'cont' flow

# Storage and transport
Output_storage    <- step.calculator2(waste$storage_transport, FP_lvst)
supply_chain_FP$storage_transport[3:4]  <- Output_storage[[1]]
rm(FP_lvst)

# Processing
Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_FP$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


########### Write to File #############
write.csv2(supply_chain_FP, file = "output/supply_chain_LAND_SQ50.csv")     # write to file in output-folder! 




L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))

e <- E$Biomass / X
e[!is.finite(e)] <- 0
MP <- e * L                           # L needed

rm(L)
rm(E)







##### SCENARIO CHOISE ###########
waste <- read.csv2(file = "data/waste_data_halfingFW.csv") # need to be consistent with diet scenario! 
Y_tot <- Y_SQ50  # choose scenario consistent with waste scenario


# Total footprint -
FP_tot <- t(t(MP) * Y_tot)           

########## create output matrix ###########################
supply_chain_FP <- data.frame(chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))


##############################################
# For Plant-based products--------------------

# Create a Y-matrix where all animal-products are 0
Y_plant <- Y_tot 
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0

FP_plant <- t(t(MP) * Y_plant)        # Total footprint of all plant-based products
#FP_plant_capita <- sum(FP_plant) / population


# Storage
Output_storage    <- step.calculator(waste$storage_transport, FP_plant)      # calculate
supply_chain_FP$storage_transport  <- Output_storage[[1]]                    # add values to table
rm(FP_plant)                                                                # remove big data to save space

# Processing
Output_processing       <- step.calculator(waste$processing, Output_storage[[2]])
supply_chain_FP$processing         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)



##############################################################
# For animal based products (Livestock) ----------------------
##############################################################

# Create Y matrix that only includes animal-based products (Livestock)
Y_lvst <- Y_tot
Y_lvst[index$product_group %in% c("Crop products", "Primary crops")] <- 0  # Set Y to 0 for all plant-based products


FP_lvst <- t(t(MP) * Y_lvst)        # Total footprint of all animal-based products
#supply_chain_FP$harvest_production[3:4] <- c(sum(FP_lvst), FP_prod_waste) # column has 4 rows # FAO production data reflect 'cont' flow

# Storage and transport
Output_storage    <- step.calculator2(waste$storage_transport, FP_lvst)
supply_chain_FP$storage_transport[3:4]  <- Output_storage[[1]]
rm(FP_lvst)

# Processing
Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_FP$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


########### Write to File #############
write.csv2(supply_chain_FP, file = "output/supply_chain_BIOMASS_SQ50.csv")     # write to file in output-folder! 


###############################################################################################################







##### SCENARIO CHOISE ###########
waste <- read.csv2(file = "data/waste_data_halfingFW.csv") # need to be consistent with diet scenario! 
Y_tot <- Y_lancet50  # choose scenario consistent with waste scenario


# Total footprint -
FP_tot <- t(t(MP) * Y_tot)           



########## create output matrix ###########################
supply_chain_FP <- data.frame(chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))


##############################################
# For Plant-based products--------------------
###############################################

# Create a Y-matrix where all animal-products are 0
Y_plant <- Y_tot 
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0

FP_plant <- t(t(MP) * Y_plant)        # Total footprint of all plant-based products

# Storage
Output_storage    <- step.calculator(waste$storage_transport, FP_plant)      # calculate
supply_chain_FP$storage_transport  <- Output_storage[[1]]                    # add values to table
rm(FP_plant)                                                                # remove big data to save space

# Processing
Output_processing       <- step.calculator(waste$processing, Output_storage[[2]])
supply_chain_FP$processing         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


##############################################################
# For animal based products (Livestock) ----------------------
##############################################################

# Create Y matrix that only includes animal-based products (Livestock)
Y_lvst <- Y_tot
Y_lvst[index$product_group %in% c("Crop products", "Primary crops")] <- 0  # Set Y to 0 for all plant-based products

FP_lvst <- t(t(MP) * Y_lvst)        # Total footprint of all animal-based products

# Storage and transport
Output_storage    <- step.calculator2(waste$storage_transport, FP_lvst)
supply_chain_FP$storage_transport[3:4]  <- Output_storage[[1]]
rm(FP_lvst)

# Processing
Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_FP$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


########### Write to File #############
write.csv2(supply_chain_FP, file = "output/supply_chain_WATER_lancet50.csv")    

#
#
#
#
#
#
#####################################################################
#
#
#
#
#
#
#
#
#
#
#


##### SCENARIO CHOISE ###########
waste <- read.csv2(file = "data/waste_data_maximum.csv") # need to be consistent with diet scenario! 
Y_tot <- Y_lancet_maxW # choose scenario consistent with waste scenario


# Total footprint -
FP_tot <- t(t(MP) * Y_tot)           


# per capita footprint
#FP_capita <- sum(FP_tot) / population             # gives ~3 tonnes for Biomass and SQ and 1.8248 for DGErec
#sum(FP_tot)                                       # 159 *10^6 for DGErec Biomass

########## create output matrix ###########################
supply_chain_FP <- data.frame(chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))


##############################################
# For Plant-based products--------------------
###############################################

# Create a Y-matrix where all animal-products are 0
Y_plant <- Y_tot 
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0

FP_plant <- t(t(MP) * Y_plant)        # Total footprint of all plant-based products
#FP_plant_capita <- sum(FP_plant) / population

# Production
#FP_prod_waste <- sum(FP_plant * waste$harvest_production) / 100               # waste shares are given as percentage (8% = 0,08)
#supply_chain_FP$harvest_production <- c(sum(FP_plant), FP_prod_waste, NA, NA) # column has 4 rows # FAO production data reflect 'cont' flow

# Storage
Output_storage    <- step.calculator(waste$storage_transport, FP_plant)      # calculate
supply_chain_FP$storage_transport  <- Output_storage[[1]]                    # add values to table
rm(FP_plant)                                                                # remove big data to save space
 
# Processing
Output_processing       <- step.calculator(waste$processing, Output_storage[[2]])
supply_chain_FP$processing         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)



##############################################################
# For animal based products (Livestock) ----------------------
##############################################################

# Create Y matrix that only includes animal-based products (Livestock)
Y_lvst <- Y_tot
Y_lvst[index$product_group %in% c("Crop products", "Primary crops")] <- 0  # Set Y to 0 for all plant-based products

FP_lvst <- t(t(MP) * Y_lvst)        # Total footprint of all animal-based products
#FP_lvst_capita <- sum(FP_lvst) / population       # For control (FP per capta == FP_lvst_capita + FP_plant_capita)


# Storage and transport
Output_storage    <- step.calculator2(waste$storage_transport, FP_lvst)
supply_chain_FP$storage_transport[3:4]  <- Output_storage[[1]]
rm(FP_lvst)

# Processing
Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_FP$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


########### Write to File #############
write.csv2(supply_chain_FP, file = "output/supply_chain_WATER_lancet_MAX.csv")     # write to file in output-folder! 


#
################################################################################################################



##### SCENARIO CHOISE ###########
waste <- read.csv2(file = "data/waste_data_minimum.csv") # need to be consistent with diet scenario! 
Y_tot <- Y_lancet_minW  # choose scenario consistent with waste scenario


# Total footprint -
FP_tot <- t(t(MP) * Y_tot)           

########## create output matrix ###########################
supply_chain_FP <- data.frame(chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))


##############################################
# For Plant-based products--------------------

# Create a Y-matrix where all animal-products are 0
Y_plant <- Y_tot 
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0

FP_plant <- t(t(MP) * Y_plant)        # Total footprint of all plant-based products
#FP_plant_capita <- sum(FP_plant) / population


# Storage
Output_storage    <- step.calculator(waste$storage_transport, FP_plant)      # calculate
supply_chain_FP$storage_transport  <- Output_storage[[1]]                    # add values to table
rm(FP_plant)                                                                # remove big data to save space

# Processing
Output_processing       <- step.calculator(waste$processing, Output_storage[[2]])
supply_chain_FP$processing         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)



##############################################################
# For animal based products (Livestock) ----------------------
##############################################################

# Create Y matrix that only includes animal-based products (Livestock)
Y_lvst <- Y_tot
Y_lvst[index$product_group %in% c("Crop products", "Primary crops")] <- 0  # Set Y to 0 for all plant-based products


FP_lvst <- t(t(MP) * Y_lvst)        # Total footprint of all animal-based products
#supply_chain_FP$harvest_production[3:4] <- c(sum(FP_lvst), FP_prod_waste) # column has 4 rows # FAO production data reflect 'cont' flow

# Storage and transport
Output_storage    <- step.calculator2(waste$storage_transport, FP_lvst)
supply_chain_FP$storage_transport[3:4]  <- Output_storage[[1]]
rm(FP_lvst)

# Processing
Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_FP$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


########### Write to File #############
write.csv2(supply_chain_FP, file = "output/supply_chain_WATER_lancet_MIN.csv")     # write to file in output-folder! 


##################################################################################################








##### SCENARIO CHOISE ###########
waste <- read.csv2(file = "data/waste_data_halfingFW_MAX.csv") # need to be consistent with diet scenario! 
Y_tot <- Y_lancet50_MAX  # choose scenario consistent with waste scenario


# Total footprint -
FP_tot <- t(t(MP) * Y_tot)           

########## create output matrix ###########################
supply_chain_FP <- data.frame(chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))


##############################################
# For Plant-based products--------------------

# Create a Y-matrix where all animal-products are 0
Y_plant <- Y_tot 
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0

FP_plant <- t(t(MP) * Y_plant)        # Total footprint of all plant-based products
#FP_plant_capita <- sum(FP_plant) / population


# Storage
Output_storage    <- step.calculator(waste$storage_transport, FP_plant)      # calculate
supply_chain_FP$storage_transport  <- Output_storage[[1]]                    # add values to table
rm(FP_plant)                                                                # remove big data to save space

# Processing
Output_processing       <- step.calculator(waste$processing, Output_storage[[2]])
supply_chain_FP$processing         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


##############################################################
# For animal based products (Livestock) ----------------------
##############################################################

# Create Y matrix that only includes animal-based products (Livestock)
Y_lvst <- Y_tot
Y_lvst[index$product_group %in% c("Crop products", "Primary crops")] <- 0  # Set Y to 0 for all plant-based products


FP_lvst <- t(t(MP) * Y_lvst)        # Total footprint of all animal-based products


# Storage and transport
Output_storage    <- step.calculator2(waste$storage_transport, FP_lvst)
supply_chain_FP$storage_transport[3:4]  <- Output_storage[[1]]
rm(FP_lvst)

# Processing
Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_FP$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


########### Write to File #############
write.csv2(supply_chain_FP, file = "output/supply_chain_WATER_lancet50_MAX.csv")     # write to file in output-folder! 












###############################################################################################################







##### SCENARIO CHOISE ###########
waste <- read.csv2(file = "data/waste_data_halfingFW_MIN.csv") # need to be consistent with diet scenario! 
Y_tot <- Y_lancet50_MIN  # choose scenario consistent with waste scenario


# Total footprint -
FP_tot <- t(t(MP) * Y_tot)           


# per capita footprint
#FP_capita <- sum(FP_tot) / population             # gives ~3 tonnes for Biomass and SQ and 1.8248 for DGErec
#sum(FP_tot)                                       # 159 *10^6 for DGErec Biomass

########## create output matrix ###########################
supply_chain_FP <- data.frame(chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))


##############################################
# For Plant-based products--------------------
###############################################

# Create a Y-matrix where all animal-products are 0
Y_plant <- Y_tot 
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0

FP_plant <- t(t(MP) * Y_plant)        # Total footprint of all plant-based products


# Storage
Output_storage    <- step.calculator(waste$storage_transport, FP_plant)      # calculate
supply_chain_FP$storage_transport  <- Output_storage[[1]]                    # add values to table
rm(FP_plant)                                                                # remove big data to save space

# Processing
Output_processing       <- step.calculator(waste$processing, Output_storage[[2]])
supply_chain_FP$processing         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)



##############################################################
# For animal based products (Livestock) ----------------------
##############################################################

# Create Y matrix that only includes animal-based products (Livestock)
Y_lvst <- Y_tot
Y_lvst[index$product_group %in% c("Crop products", "Primary crops")] <- 0  # Set Y to 0 for all plant-based products


FP_lvst <- t(t(MP) * Y_lvst)        # Total footprint of all animal-based products

# Storage and transport
Output_storage    <- step.calculator2(waste$storage_transport, FP_lvst)
supply_chain_FP$storage_transport[3:4]  <- Output_storage[[1]]
rm(FP_lvst)

# Processing
Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]])
supply_chain_FP$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)

# Distribution
Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]])
supply_chain_FP$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)

# Consumption
Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]])
supply_chain_FP$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)


########### Write to File #############
write.csv2(supply_chain_FP, file = "output/supply_chain_WATER_lancet50_MIN.csv")     # write to file in output-folder! 







###############################################################################################################

#
#
#
#
#
#
#
#
#
#
#
#

#










#######################################################
###------ Continent-specific footprints ----------#####
#######################################################

#FP_Continent <- data.frame(continent = unique(FP$continent), 
                           SQ = rep(NA, length(unique(FP$continent))), 
                           DGE = rep(NA, length(unique(FP$continent))), 
                           lancet = rep(NA, length(unique(FP$continent))), 
                           plantbased = rep(NA, length(unique(FP$continent))),
                           EATveg = rep(NA, length(unique(FP$continent))))


#Y_tot <- Y_EATveg  # choose scenario
#FP_tot <- t(t(MP) * Y_tot)  

#FP <- data.frame(country = index$country, 
#                 continent = index$continent,
#                 value = rowSums(FP_tot))
#FP <- aggregate(value ~country + continent, FP, sum) # footprint pro ursprungsland 

# for EACH FP (footprint and scenario), fill in the data table!
#for (i in 1:nrow(FP_Continent)){
#  FP_Continent$SQ[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
#}
# 
#for (i in 1:nrow(FP_Continent)){
#  FP_Continent$DGE[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
#}
 
# for (i in 1:nrow(FP_Continent)){
#   FP_Continent$lancet[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
# }
#for (i in 1:nrow(FP_Continent)){
# FP_Continent$plantbased[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
#}

#for (i in 1:nrow(FP_Continent)){
#  FP_Continent$EATveg[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
#}


#write.csv2(FP_Continent, file = "output/FP_Landuse_continents.csv")

########## notes ################

# FP_plant <- sum(colSums(MP) * Y_plant) # 

# create result matrix
# NrOfProducts <- 130
#FP__chain <- data.frame(Scenario = rep("SQ", NrOfProducts),
#                        products = rep(index$product, each = NrOfProducts),
#                        product_group = rep(index$product_group, each = NrOfProducts))


### Create function!?  ####
#step.calculator <- function(waste_step, FP){                      # waste_step is column in waste, eg. waste$harvest_production?
#  FP_step <- c(sum(FP * waste_step),               
#               sum(FP - (FP * waste_step)), NA, NA)
#  FP_cont <- FP  - (FP* waste_step)                  
#  return(list(FP_step,FP_cont))
#}

