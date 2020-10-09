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

# --------------- RUN FUNCTIONS ------------------ #
#### Function to calculate flows per step  #####
step.calculator <- function(waste_step, Y_Q){                      # waste_step is a column in the data-frame "waste", eg. waste$harvest_production?
  YQ_waste <- t(t(Y_Q)* (waste_step / 100))
  YQ_cont <- Y_Q  - YQ_waste              
  YQ_step <- c(sum(YQ_cont), sum(YQ_waste), NA, NA)
  output <- list(YQ_step, YQ_cont)                                # gives one vector with 4 elements & a new footprint-matrix (or Y-matrix)
  return(output)
}


step.calculator2 <- function(waste_step, Y_Q){                      # waste_step is column in waste, eg. waste$harvest_production?
  YQ_waste <- t(t(Y_Q) * (waste_step / 100))
  YQ_cont <- Y_Q  - YQ_waste              
  YQ_step <- c(sum(YQ_cont), sum(YQ_waste))
  output <- list(YQ_step, YQ_cont)                                # gives a vector with only 2 elements
  return(output)
}

#########################################################
# along the Supply Chain - Status Quo 
#########################################################

# Total amount of food consumed in Germany
 
# Load Y-matrix
Y_SQ <- Y[ ,"DEU_Food"]                           # Status Quo
sum(Y_SQ)


# per capita footprint
population <- 80645605 #2013, Source: World bank 
sum(Y_tot) / population             # gives 0.9822955, ~1 ton ( ~ 2,69 kg per capita/day including what is thrown away)


### For SQ
Y_tot <- Y_SQ
waste <- read.csv2(file = "data/waste_data.csv") 

# create output matrix
supply_chain_Y <- data.frame(chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))



############# For Plant-based products ##############
# Create a Y-matrix where all animal-products are 0
Y_plant <- Y_tot
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0


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

#
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


### save the Y matrix that represent only "eaten food":
eaten_food_lvst <- Output_consumption[[2]]

write.table(supply_chain_Y, file = "output/supply_chain_mass_SQ.csv", dec = ".", sep = ";", row.names = FALSE) 


# save Y_SQ_eaten for development of diets
Y_SQ_eaten <- eaten_food_plants + eaten_food_lvst
Y_SQ_eaten[index$DGE_group == "excluded"] <- 0 # removing products not eaten by humans
sum(Y_SQ_eaten)     # 60519278
write.csv2(Y_SQ_eaten, file = "data/Y_SQ_eaten.csv") # for minimum Waste (SQ eaten depends on waste levels)



