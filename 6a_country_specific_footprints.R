###########################################################################
###----------------------- Spatial footprints ------------------------#####
###########################################################################
library(plyr)
library(tidyr)
# GHG is given in Gg
# Biomass is given in Ton?
# Land use is given in hectars
# Water is given in m3


path <- "C:/Hanna/FABIO_FW-diets_GER/input/"

######### Country-specific footprints #####################

# FOOTPRINT FUNCTION ------------------------------------------------------------------
footprint1 <- function(indicator, Y_vector){ # indicator is e.g. E?Biomass, Y_vector= e.g. Y_SQ or Y_roots
  e <- as.vector(indicator) / X
  e[!is.finite(e)] <- 0
  e[e<0] <- 0
  rm(indicator)
  
  # calculate multipliers
  L <- readRDS(paste0(path,"2013_L_mass.rds"))
  MP <- e * L
  rm(L)
  FP <- t(t(MP) * Y_vector)
  rm(MP)
  gc()
  
  rownames(FP) <- colnames(FP) <- paste0(rep(countries$iso3c, each=130), "_", rep(products$Item, 192)) 
  
  FP <- as.matrix(FP)
  FP <- reshape2::melt(FP)
  
  results <- FP %>%
    mutate(year = "2013",  type = "food", value = round(value, 4)) %>% 
    filter(value != 0) %>% 
    separate(Var1, sep = "_", into = c("source_iso", "source_item")) %>% 
    separate(Var2, sep = "_", into = c("final_iso", "final_item"))

return(results)
}

################# Main code ###############################

#load Data
Y_SQ <- Y[ ,"DEU_Food"]                       # might need to check if all products are what I consider food
Y_Scen1 <- Y_lancet <- read.csv2(file = "data/Y_lancet.csv")
Y_Scen1<- Y_lancet[,2] 
E <- readRDS(paste0(path,"2013_E.rds"))
#E_wsf <- read.csv(paste0(path,"E_wsf.csv"), sep = ";", dec = ".")
load(paste0(path,"E_ghg_2013.RData"))

population <- 80645605 #2013 Source: World bank 

# List indicators
ind_list <- list("land"= E$Landuse, 
                 "biomass" = E$Biomass,
                 "blue_water" = E$Green_water,
                 "green_water" = E$Blue_water,
                 #"water_cons" = as.numeric(E_wsf$cons_water),
                 #"WSF1" = as.numeric(E_wsf$WSF),
                 "GHG" = colSums(E_ghg[ , 2:ncol(E_ghg)]))
rm(E)
rm(E_ghg)
#rm(E_wsf)
gc()


###### ------- LOOP OVER INDICATORS FOR Status Quo (SQ) -----#######

for (j in names(ind_list)){
  FP_results <- footprint1(ind_list[[j]], Y_Scen1) 
  data = aggregate(FP_results$value,by=list(FP_results$source_iso),FUN=sum)
  #if (j=="land"){
 #   FP_output <- data
  #} else {
    FP_output <- merge(FP_output, data, by= intersect("Group.1", "Group.1"), all.x=T, all.y=T, sort = FALSE)
  #}
  names(FP_output)[names(FP_output) == 'x'] <- paste0("Scen1.",j) # change according to scenario!
  rm(data)
}

colSums(FP_output)
# # --- PREVIOUS CODE in tidyverse ----
# FP <- footprint1(ind, Y_SQ)
# data <- FP %>% group_by(source_iso) %>% summarise(water_scarcity = sum(value))
# FP_output <- merge(FP_output, data, by = intersect("source_iso", "source_iso"), all.x=T, all.y=T, sort = FALSE

# ---- Calculate Water scarcity footprints --- #
WSF.Calculator <- function(blue_water, green_water, out_inc, recharge_rate){
  bw_ep <- blue_water * (1-recharge_rate)
  ep_min <- green_water - out_inc + bw_ep
  ep_max <- green_water - out_inc + blue_water
  return(ep_min, ep_max )
}

recharge_rate <- 0.65

RWC <- products[, c(2,6)]

#This makes no sense! I cannot add a "rate" to the E-matrix. In that case I kind of need to multiply it with the total production?
E_WF <- merge(E_WF, RWC, by.x="Item.Code", by.y="item_code", # add moisture to E_WF
              all.x=T, all.y=T, sort = FALSE)

recharge_rate <- 0        # if reharge_rate = 0, max and min is the same
E_WF$bw_ep <- E_WF$Blue_water * (1 - recharge_rate)  #bw_ep
E_WF$ep_min <- E_WF$Green_water - E_WF$rwc + bw_ep     # ep_min ### RECONSIDER
E_WF$ep_max <- E_WF$Green_water - E_WF$rwc + E_WF$Blue_water   #ep_max ### RECONSIDER

E_WF$WF_min <- E_WF$rwc + E_WF$ep_min
E_WF$WF_max <- E_WF$rwc + E_WF$ep_max

# - Add AWARE factors for background map 
E_wsf <- read.csv(paste0(path,"E_wsf.csv"), sep = ";", dec = ".")
E_wsf$iso3 <- countries$iso3c[match(E_wsf$Country, countries$country)]  # add iso using "countries" 
CF_aware <- subset(E_wsf, select = c(iso3, Agg_CF_irri))
CF_aware <- CF_aware[!duplicated(CF_aware), ]  
FP_output <- merge(FP_output, CF_aware, by.x = "Group.1", by.y = "iso3", all.x=T, all.y=F, sort = FALSE)

## - WSF version 2: 
#FP_output$WSF2 <- FP_output$water_cons* FP_output$Agg_CF_irri

# - Add full country names and continents 
FP_output <- merge(FP_output, countries, by.x = "Group.1", by.y = "iso3c" , all.x=T, all.y=F, sort = FALSE)
#FP_output<- FP_output[ , -c(7) ]

# Calculate per capita
FP_output$GHG <- FP_output$GHG/population*1000000 # for GHG: per capita in kg


### PRINT TO FILE 
write.table(FP_output, file = "output/spatial_FP/footprints_SQ_Scen1.csv", dec = ".", sep = ";",row.names = FALSE)  


###### ------- LOOP OVER INDICATORS FOR DIETARY CHANGE (Scen1) -------- ####

for (j in names(ind_list[2:5])){
  FP_results <- footprint1(ind_list[[j]], Y_Scen1) 
  data = aggregate(FP_results$value,by=list(FP_results$source_iso),FUN=sum)
  if (i=="land"){
    FP_output <- data
  } else {
    FP_output <- merge(FP_output, data, by= intersect("Group.1", "Group.1"), all.x=T, all.y=T, sort = FALSE)
  }
  names(FP_output)[names(FP_output) == 'x'] <- j
  rm(data)
}

### PRINT TO FILE ######
write.table(FP_output, file = "output/spatial_FP/footprints_Scen1.csv", dec = ".", sep = ";",row.names = FALSE)  


##################### Country and Product-group specific footprint ####################

# modify Y-matrix to set all the not included product-groups to zero
# create Y-vectors for each product groups (using com_groups and diet_groups)
index <- read.csv2("data/index_data_frame.csv")


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
  Y_eggs   <- Y_tot
  Y_eggs[index$diet_group != "Eggs"] <- 0
  Y_fish   <- Y_tot
  Y_fish[index$diet_group != "Fish"] <- 0
  Y_meat <- Y_tot
  Y_meat[index$diet_group != "Meat"] <- 0
  Y_sugalc <- Y_tot
  Y_sugalc[!index$diet_group %in% c("Sugar, sweeteners", "Alcohol")] <- 0
  product_Y_list <- list("Cereals" = Y_cereals, "Roots"= Y_roots, "Veg"=Y_veg, "Fruit"=Y_fruits, "Beans"=Y_beans, 
                         "Oils"=Y_oils, "Milk"=Y_milk, "Eggs"=Y_eggs, "Fish"=Y_fish, "Meat"= Y_meat, "SugAlc"= Y_sugalc)
  return(product_Y_list)
}

product_Y_list_SQ <- product_Y_list.creator(Y_SQ)
product_Y_list_Scen1 <- product_Y_list.creator(Y_Scen1)


#product_Y_list_Scen1[[i]]
#i="Eggs"

j = "land"

# ---------- Loop over indicators ------ 
for (j in names(ind_list)){

  # ------ Loop over products -------
for (i in names(product_Y_list_Scen1[3:11])){    # Define Y-lists
  if (i=="Fish") { next                                             # FP_results are empty for "Fish" as there is no data
  }else{
    FP_results <- footprint1(ind_list[[j]], product_Y_list_Scen1[[i]])
    data = aggregate(FP_results$value,by=list(FP_results$source_iso),FUN=sum) 
    print(i)
  }
  if (i=="Cereals"){
    output <- data
  } else {
    output <- merge(output, data, by= intersect("Group.1", "Group.1"), all.x=T, all.y=T, sort = FALSE)
  }
  names(output)[names(output) == 'x'] <- i
}
#assign(paste0("output_",j), output)

# PRINT TO FILE 
write.table(output, file = paste0("output/spatial_FP/footprints_products_",j,"_Scen1_10.11.csv"), dec = ".", sep = ";",row.names = FALSE)  
}


bb<- output[rev(order(b$x, na.last = FALSE)), ]
bb[1:10, ]

#output_land <- output_land[ , -c(10) ]


#####  ----------- Quantities ------------ #####
product_Y_list <- product_Y_list_Scen1


for (i in names(product_Y_list)){  
  data = aggregate(product_Y_list[[i]], by=list(index$country), FUN=sum)
  if (i=="Cereals"){
    output_mass <- data
  } else {
    output_mass <- merge(output_mass, data, by= intersect("Group.1", "Group.1"), all.x=T, all.y=T, sort = FALSE)
  }
  names(output_mass)[names(output_mass) == 'x'] <- i
}

### PRINT TO FILE ######
write.table(output_mass, file = "output/spatial_FP/products_mass_Scen1.csv", dec = ".", sep = ";",row.names = FALSE)  


# ##############################################################################
# # - OLD CODE -----------------------------------------
# ##############################################################################
# # load index
# 
# # create output df
# FP_country <- data.frame(Country_ID = unique(index$country),
#                         Country = rep(NA, length(unique(FP$country))),
#                          SQ_biomass = rep(NA, length(unique(FP$country))),
#                          DGE_biomass = rep(NA, length(unique(FP$country))),
#                          lancet_biomass = rep(NA, length(unique(FP$country))),
#                          EATveg_biomass = rep(NA, length(unique(FP$country))),
#                          SQ_landuse = rep(NA, length(unique(FP$country))),
#                          DGE_landuse = rep(NA, length(unique(FP$country))),
#                          lancet_landuse = rep(NA, length(unique(FP$country))),
#                          EATveg_landuse = rep(NA, length(unique(FP$country))), 
#                          SQ_water = rep(NA, length(unique(FP$country))),
#                          DGE_water = rep(NA, length(unique(FP$country))),
#                          lancet_water = rep(NA, length(unique(FP$country))),
#                          EATveg_water = rep(NA, length(unique(FP$country)))
#                          )
# FP_country$Country <- countries$Country[match(FP_country$Country_ID, countries$ISO)] # add column with complete name of country
# 
# Y_list <- list(Y_SQ, Y_DGE, Y_lancet, Y_EATveg)
# 
# ##### BIOMASS ######
# L <- readRDS(paste0(path,"2013_L_mass.rds"))
# E <- readRDS(paste0(path,"2013_E.rds"))
# e <- E$Biomass / X
# e[!is.finite(e)] <- 0
# MP <- e * L                           # L needed
# rm(L)
# rm(E)
# 
# for (i in 1:length(Y_list)){
#   FP_tot <- t(t(MP) * Y_list[[i]])
#   FP <- data.frame(country = index$country,
#                  value = rowSums(FP_tot))
#   FP_country[,(i+2)] <- aggregate(value ~country, FP, sum)[,2] # footprint per country of origin (+2 because the scenarios for biomass start in column 3)
# }
# 
# 
# 
# ##### LANDUSE ######
# L <- readRDS(paste0(path,"2013_L_mass.rds"))
# E <- readRDS(paste0(path,"2013_E.rds"))
# e <- E$Landuse / X
# e[!is.finite(e)] <- 0
# MP <- e * L                           # L needed
# rm(L)
# rm(E)
# 
# for (i in 1:length(Y_list)){
#   FP_tot <- t(t(MP) * Y_list[[i]])
#   FP <- data.frame(country = index$country,
#                    value = rowSums(FP_tot))
#   FP_country[,(i+6)] <- aggregate(value ~country, FP, sum)[,2] # footprint per country of origin (+6 because the scenarios for landuse start in column 7)
# }
# 
# 
# ##### WATER ######
# L <- readRDS(paste0(path,"2013_L_mass.rds"))
# E <- readRDS(paste0(path,"2013_E.rds"))
# e <- E$Blue_water / X
# e[!is.finite(e)] <- 0
# MP <- e * L                           # L needed
# rm(L)
# rm(E)
# 
# for (i in 1:length(Y_list)){
#   FP_tot <- t(t(MP) * Y_list[[i]])
#   FP <- data.frame(country = index$country,
#                    value = rowSums(FP_tot))
#   FP_country[,(i+10)] <- aggregate(value ~country, FP, sum)[,2] # footprint per country of origin
# }
# 
# write.csv2(FP_country, file = "output/FP_countries.csv")
# 
# 
# #######################################################
# ###------ Continent-specific footprints ----------#####
# #######################################################
# 
# FP_Continent <- data.frame(continent = unique(FP$continent),
#                           SQ = rep(NA, length(unique(FP$continent))),
#                           DGE = rep(NA, length(unique(FP$continent))),
#                           lancet = rep(NA, length(unique(FP$continent))),
#                           plantbased = rep(NA, length(unique(FP$continent))),
#                           EATveg = rep(NA, length(unique(FP$continent))))
# 
# 
# Y_tot <- Y_EATveg  # choose scenario
# FP_tot <- t(t(MP) * Y_tot)
# 
# FP <- data.frame(country = index$country,
#                 continent = index$continent,
#                 value = rowSums(FP_tot))
# FP <- aggregate(value ~country + continent, FP, sum) # footprint pro ursprungsland
# 
# # for EACH FP (footprint and scenario), fill in the data table!
# for (i in 1:nrow(FP_Continent)){
#  FP_Continent$SQ[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
# }
# 
# for (i in 1:nrow(FP_Continent)){
#  FP_Continent$DGE[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
# }
# 
# for (i in 1:nrow(FP_Continent)){
#   FP_Continent$lancet[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
# }
# for (i in 1:nrow(FP_Continent)){
# FP_Continent$plantbased[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
# }
# 
# for (i in 1:nrow(FP_Continent)){
#  FP_Continent$EATveg[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
# }
# 
# 
# write.csv2(FP_Continent, file = "output/FP_Landuse_continents.csv")


########## notes ################

# FP_plant <- sum(colSums(MP) * Y_plant) #
# 
# create result matrix
# NrOfProducts <- 130
# FP__chain <- data.frame(Scenario = rep("SQ", NrOfProducts),
#                         products = rep(index$product, each = NrOfProducts),
#                         product_group = rep(index$product_group, each = NrOfProducts))



# footprint2 <- function(product_group, var = integer(), Y_vector){
#   e <- as.vector(E[,var]) / X
#   e[!is.finite(e)] <- 0
#   
#   # calculate multipliers
#   L <- readRDS(paste0(path,"2013_L_mass.rds"))
#   MP <- e * L
#   rm(L)
#   gc()
#   FP <- t(t(MP) * Y)
#   
#   rownames(FP) <- colnames(FP) <- paste0(rep(regions$iso3c, each=130), "_", rep(items$item, 192))
#   
#   FP <- as.matrix(FP)
#   FP <- reshape2::melt(FP)
#   
#   results <- FP %>%
#     mutate(year = 2013, emissions = E_var[var], type = "food", value = round(value, 4)) %>% 
#     filter(value != 0) %>% 
#     separate(Var1, sep = "_", into = c("source_iso", "source_item")) %>% 
#     separate(Var2, sep = "_", into = c("final_iso", "final_item"))
#   return(results)
# }