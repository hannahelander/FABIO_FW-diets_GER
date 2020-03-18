######################################################
# ---------------------------------------------------
# Footprints
# ----------------------------------------------------
# Calculate Footprints along the supply chain. 
##########################################################

L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
## Prepare extension and define footprint (if needed)
e <- E$Biomass / X
e[!is.finite(e)] <- 0
MP <- e * L                           # L needed

rm(L)
rm(E)

gc()
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
Y_SQ50_MAX <- read.csv2(file = "data/Y_SQ_50_MAX.csv")
Y_SQ50_MAX <- Y_SQ50_MAX[,2]
Y_SQ50_MIN <- read.csv2(file = "data/Y_SQ_50_MIN.csv")
Y_SQ50_MIN <- Y_SQ50_MIN[,2]

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


sum(Y_DGE)
sum(Y_DGE50_MAX)
sum(Y_SQ)
sum(Y_lancet_maxW)
sum(Y_EATveg)

Y_list <- list(Y_SQ, Y_DGE, Y_lancet, Y_EATveg,
               Y_SQ, Y_DGE_maxW, Y_lancet_maxW, Y_EATveg_maxW, 
               Y_SQ, Y_DGE_minW, Y_lancet_minW, Y_EATveg_minW, 
               Y_SQ50, Y_DGE50, Y_lancet50, Y_EATveg50,
               Y_SQ50_MAX ,  Y_DGE50_MAX, Y_lancet50_MAX, Y_EATveg50_MAX,
               Y_SQ50_MIN, Y_DGE50_MIN, Y_lancet50_MIN, Y_EATveg50_MIN)


# load waste data
waste_dat <- read.csv2(file = "data/waste_data.csv")
wasteMAX <- read.csv2(file = "data/waste_data_maximum.csv")
wasteMIN <- read.csv2(file = "data/waste_data_minimum.csv")
waste50  <- read.csv2(file = "data/waste_data_halfingFW.csv")
waste50MAX <- read.csv2(file = "data/waste_data_halfingFW_MAX.csv")
waste50MIN <- read.csv2(file = "data/waste_data_halfingFW_MIN.csv")

waste_all <- list(waste_dat, wasteMAX, wasteMIN, waste50, waste50MAX, waste50MIN)

Scenario_names <- c("Y_SQ", "Y_DGE", "Y_lancet", "Y_EATveg",
                    "Y_SQ_max", "Y_DGE_max", "Y_lancet_max", "Y_EATveg_max", 
                    "Y_SQ_min", "Y_DGE_min", "Y_lancet_min", "Y_EATveg_min", 
                    "Y_SQ50", "Y_DGE50", "Y_lancet50", "Y_EATveg50",
                    "Y_SQ50_max" ,  "Y_DGE50_max", "Y_lancet50_max", "Y_EATveg50_max",
                    "Y_SQ50_min", "Y_DGE50_min", "Y_lancet50_min", "Y_EATveg50_min")


######### LOOP FOR ALL SCENARIOS ####################

for (i in 1:length(Y_list)){
  Y_tot <- Y_list[[i]]
  if (i <= 4){
    waste <- waste_all[[1]]
  }
  if (i >= 5 & i <= 8){
    waste <- waste_all[[2]]
  }
  if (i >=9 & i <= 12){
    waste <- waste_all[[3]]
  }
  if (i >= 13 & i <= 16){
  waste <- waste_all[[4]]
  }
  if (i >= 17 & i <= 20){
    waste <- waste_all[[5]]
  }
  if (i >= 21 & i <= 24){
    waste <- waste_all[[6]]
  }
  
  FP_tot <- t(t(MP) * Y_tot)   # Total footprint   

# create output matrix 
supply_chain_FP <- data.frame(chain_type = c("plant_based", "plant_based", "animal_based", "animal_based"),
                              flow = c("cont", "waste", "cont", "waste"))

# For Plant-based products
Y_plant <- Y_tot # Create a Y-matrix where all animal-products are 0
Y_plant[index$product_group %in% c("Livestock products", "Fish")] <- 0  

FP_plant <- t(t(MP) * Y_plant)        # Total footprint of all plant-based products

Output_storage    <- step.calculator(waste$storage_transport, FP_plant)     # Storage # calculate
supply_chain_FP$storage_transport  <- Output_storage[[1]]                    # add values to table
rm(FP_plant)                                                                # remove big data to save space
gc()

Output_processing       <- step.calculator(waste$processing, Output_storage[[2]]) # Processing
supply_chain_FP$processing         <- Output_processing[[1]]
rm(Output_storage)
gc()

Output_distribution     <- step.calculator(waste$distribution, Output_processing[[2]]) # Distribution
supply_chain_FP$distribution       <- Output_distribution[[1]]
rm(Output_processing)
gc()

Output_consumption  <- step.calculator(waste$final_consumption, Output_distribution[[2]]) # Consumption
supply_chain_FP$Consumption        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)
gc()

###### For animal based products (Livestock) #########
Y_lvst <- Y_tot # Create Y matrix that only includes animal-based products (Livestock)
Y_lvst[index$product_group %in% c("Crop products", "Primary crops")] <- 0  # Set Y to 0 for all plant-based products

FP_lvst <- t(t(MP) * Y_lvst)        # Total footprint of all animal-based products

Output_storage    <- step.calculator2(waste$storage_transport, FP_lvst) # Storage and transport
supply_chain_FP$storage_transport[3:4]  <- Output_storage[[1]]
rm(FP_lvst)
gc()

Output_processing       <- step.calculator2(waste$processing, Output_storage[[2]]) # Processing
supply_chain_FP$processing[3:4]         <- Output_processing[[1]]
rm(Output_storage)
gc()

Output_distribution     <- step.calculator2(waste$distribution, Output_processing[[2]]) # Distribution
supply_chain_FP$distribution[3:4]       <- Output_distribution[[1]]
rm(Output_processing)
gc()

Output_consumption  <- step.calculator2(waste$final_consumption, Output_distribution[[2]]) # Consumption
supply_chain_FP$Consumption[3:4]        <- Output_consumption[[1]]
rm(Output_distribution)
rm(Output_consumption)
gc()

##Write to File 
write.table(supply_chain_FP, file = paste0("output/biomass/", Scenario_names[i],".csv"), dec = ".", sep = ";") 
  print(i)
  
}
  





#######################################################
###------ Continent-specific footprints ----------#####
#######################################################

#FP_Continent <- data.frame(continent = unique(FP$continent), 
#                           SQ = rep(NA, length(unique(FP$continent))), 
#                           DGE = rep(NA, length(unique(FP$continent))), 
#                           lancet = rep(NA, length(unique(FP$continent))), 
#                           plantbased = rep(NA, length(unique(FP$continent))),
#                           EATveg = rep(NA, length(unique(FP$continent))))


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

