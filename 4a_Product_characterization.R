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

# create Y-vectors for each product groups (using com_groups and diet_groups)
product_Y_list.creator <- function(Y_tot){  
  Y_cereals <- Y_tot
  Y_cereals[index$com_group != "Cereals"] <- 0
  Y_roots <- Y_tot
  Y_roots[index$com_group != "Roots and tubers"] <- 0
  Y_veg    <- Y_tot
  Y_veg[index$diet_group != "Vegetables, pulses, spices"] <- 0
  Y_fruits <- Y_tot 
  Y_fruits[index$diet_group != "Fruits"] <- 0
  Y_beans  <- Y_tot
  Y_beans[index$diet_group != "Oil crops and nuts"] <- 0
  Y_oils   <- Y_tot
  Y_oils[index$diet_group != "Vegetable oils"] <- 0
  Y_milk <- Y_tot
  Y_milk[index$com_group != "Milk"] <- 0
  Y_eggs   <- Y_SQ
  Y_eggs[index$diet_group != "Eggs"] <- 0
  Y_fish   <- Y_tot
  Y_fish[index$diet_group != "Fish"] <- 0
  Y_meat <- Y_tot
  Y_meat[index$diet_group != "Meat"] <- 0
  Y_sugalc <- Y_tot
  Y_sugalc[!index$diet_group %in% c("Sugar, sweeteners", "Alcohol")] <- 0
  product_Y_list <- list(Y_cereals, Y_roots, Y_veg, Y_fruits, Y_beans, Y_oils, Y_milk, Y_eggs, Y_fish, Y_meat, Y_sugalc)
  return(product_Y_list)
}


## Load index and waste data
index <- read.csv2("data/index_data_frame.csv")
waste <- read.csv2(file = "data/waste_data.csv")

#nutr_data <- read.csv(file = "output/Diets_SQ_DGErec_v3.csv")
#nutr_data <- nutr_data[,2:8]


######


# Create data frame
prod_character <- data.frame(product_group = c("Cereals", "Potatoes & roots", "Vegetables", "Fruits", 
                                               "Pulses, beans & nuts",  "Vegetable oils",  "Milk & products", 
                                               "Eggs", "Fish", "Meat", "Sugar & Alcohol"),
                            Y_Kg_ger = rep(NA, length(product_Ylist_SQ)),
                            Kg_eaten = rep(NA, length(product_Ylist_SQ)),
                            Biomass  = rep(NA, length(product_Ylist_SQ)), 
                            Land     = rep(NA, length(product_Ylist_SQ)),
                            Water    = rep(NA, length(product_Ylist_SQ)),
                            kcal     = rep(NA, length(product_Ylist_SQ)),
                            prot     = rep(NA, length(product_Ylist_SQ)))

############## Total Quantities ################

Y_SQ <- Y[ ,"DEU_Food"]   
product_Ylist_SQ <- product_Y_list.creator(Y_SQ) # create Y-vectors for each product group

for (i in 1:length(product_Ylist_SQ)){ 
  prod_character$Y_Kg_ger[i] <- sum(product_Ylist_SQ[[i]])
}


############## Eaten Quantities ################

Y_SQ_eaten <- read.csv2(file = "data/Y_SQ_eaten.csv")
Y_SQ_eaten <- Y_SQ_eaten[,2]
sum(Y_SQ_eaten) # 60519278
product_Ylist_eaten <- product_Y_list.creator(Y_SQ_eaten) # create Y_eaten-vectors for each product group

for (i in 1:length(product_Ylist_eaten)){ 
  prod_character$Kg_eaten[i] <- sum(product_Ylist_eaten[[i]])
}

######### Eaten nutrients ##############

# load nutritional data and save is as per gram specifications
items <- read.csv2(file = "data/Items_nutr_.csv", stringsAsFactors = FALSE)
items$Kcal <- as.numeric(items$Kcal) /100
items$G_prot <- as.numeric(items$G_prot) /100
items$G_fat <- as.numeric(items$G_fat) /100

#for each product_Ylist_eaten[i]: calculate kcal, sum and add to data-frame! (behöver loop i loop)
for (i in 1:length(product_Ylist_eaten)){ 

# data frame für jeder produkt-gruppe machen 
nutr_SQ_product <- data.frame(kcal   = rep(NA, nrow(items)),
                              prot   = rep(NA, nrow(items)))

for (j in 1:nrow(items)) { # j is each product-item within a product group (e.g. oat, wheat)
  nutr_SQ_product$kcal[j] <- items$Kcal[j]   * sum(product_Ylist_eaten[[i]][index$item_code==items$Item.Code[j]])
  nutr_SQ_product$prot[j] <- items$G_prot[j] * sum(product_Ylist_eaten[[i]][index$item_code==items$Item.Code[j]])
                                           }

  prod_character$kcal[i] <- sum(nutr_SQ_product$kcal, na.rm=T) / prod_character$Kg_eaten[i]
  prod_character$prot[i] <- sum(nutr_SQ_product$prot, na.rm=T) / prod_character$Kg_eaten[i]
}


############## Footprints per unit of EATEN food #################

## Biomass
L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))

e <- E$Biomass / X
e[!is.finite(e)] <- 0
MP <- e * L                           
rm(L)
rm(E)

for (i in 1:length(product_Ylist_SQ)){
  Y_tot <- product_Ylist_SQ[[i]]  
  FP_tot <- t(t(MP) * Y_tot)
  prod_character$Biomass[i] <- sum(FP_tot)/prod_character$Kg_eaten[i]
}

## Landuse
L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))

e <- E$Landuse / X
e[!is.finite(e)] <- 0
MP <- e * L                           
rm(L)
rm(E)

for (i in 1:length(product_Ylist_SQ)){
  Y_tot <- product_Ylist_SQ[[i]]  
  FP_tot <- t(t(MP) * Y_tot)
  prod_character$Land[i] <- sum(FP_tot)/prod_character$Kg_eaten[i]
}


## Water
L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))

e <- E$Blue_water / X
e[!is.finite(e)] <- 0
MP <- e * L                           
rm(L)
rm(E)

for (i in 1:length(product_Ylist_SQ)){
  Y_tot <- product_Ylist_SQ[[i]]
  FP_tot <- t(t(MP) * Y_tot)
  prod_character$Water[i] <- sum(FP_tot)/prod_character$Kg_eaten[i]
}


write.table(prod_character, file = "output/product_characterization.csv", sep = ";", dec = ".", row.names() = FALSE)



################### B) Supply chain quantities ########################
# this part map the supply chain to later sum up the amount of waste 
# and waste types associated whith each diet. The resulting data is used 
# in script 5d_visualization_scenarios_FW.R

Y_DGE <- read.csv2(file = "data/Y_DGE.csv")      
Y_DGE <- Y_DGE[,2]
Y_DGE <- as.numeric(Y_DGE)
Y_lancet <- read.csv2(file = "data/Y_lancet.csv")
Y_lancet <- Y_lancet[,2] 
Y_EATveg <- read.csv2(file = "data/Y_EATveg.csv")
Y_EATveg <- Y_EATveg[,2] 

product_Ylist_DGE <- product_Y_list.creator(Y_DGE)
product_Ylist_lancet <- product_Y_list.creator(Y_lancet)
product_Ylist_VEG <- product_Y_list.creator(Y_EATveg)

###### create a df using the same product categories as in Diets (based on index$com_group & products #########
supply_chain <- data.frame(chain_type = c("Cereals", "Cereals", "Potatoes & roots",  "Potatoes & roots", "Vegetables", "Vegetables", "Fruits", "Fruits",
                                         "Pulses, beans & nuts",  "Pulses, beans & nuts", "Vegetable oils", "Vegetable oils", "Milk & products", 
                                         "Milk & products", "Eggs", "Eggs", "Fish", "Fish", "Meat",  "Meat", "Sugar & Alcohol", "Sugar & Alcohol"),
                          flow = c("cont", "waste", "cont", "waste", "cont", "waste", "cont", "waste", "cont", "waste", "cont", "waste", "cont", 
                                 "waste", "cont", "waste", "cont", "waste", "cont", "waste", "cont", "waste"), 
                          storage_transport = rep(NA, length(product_Ylist_SQ)*2),
                          processing   = rep(NA, length(product_Ylist_SQ)*2),
                          distribution = rep(NA, length(product_Ylist_SQ)*2),
                          consumption  = rep(NA, length(product_Ylist_SQ)*2))


for (i in 1:length(product_Ylist_SQ)){
  
  Y_tot <- product_Ylist_SQ[[i]]  # choose scenario consistent with waste scenario
  
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
#write.table(supply_chain, file = "output/product_FW_supply_chain_SQ.csv", sep = ";", dec = ".", row.numbers = FALSE)
#write.table(supply_chain, file = "output/product_FW_supply_chain_DGE.csv", sep = ";", dec = ".", row.numbers = FALSE)
#write.table(supply_chain, file = "output/product_FW_supply_chain_lancet.csv", sep = ";", dec = ".", row.numbers = FALSE)
write.table(supply_chain, file = "output/product_FW_supply_chain_VEG.csv", sep = ";", dec = ".", row.numbers = FALSE)

########## For SQ: add to prod_character #########

for (i in 1:nrow(supply_chain)){
  if ((i %% 2) == 0){
    prod_character$FW_shares[i/2] <- (supply_chain$distribution[i] + supply_chain$consumption[i]) / prod_character$Y_Kg_ger[i/2]
  }
  else{
    print(i)
  }
}

write.table(prod_character, file = "output/product_characterization.csv", sep = ";", dec = ".", row.names= FALSE)
