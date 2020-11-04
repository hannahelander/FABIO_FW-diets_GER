###########################################################################
###----------------------- Spatial footprints ------------------------#####
###########################################################################
library(plyr)

path <- "C:/Hanna/FABIO_FW-diets_GER/input/"


######### Country-specific footprints #####################

# FOOTPRINT FUNCTION ------------------------------------------------------------------
footprint1 <- function(indicator, Y_vector){ # indicator is e.g. E?Biomass, Y_vector= e.g. Y_SQ or Y_roots
  e <- as.vector(indicator) / X
  e[!is.finite(e)] <- 0
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

Y_SQ <- Y[ ,"DEU_Food"]                       # might need to check if all products are what I consider food
E <- readRDS(paste0(path,"2013_E.rds"))

#### ---------- Cropland use ---------------
ind <- E$Landuse
rm(E)
gc()

FP_results <- footprint1(ind, Y_SQ) 
data <- FP_results %>% group_by(source_iso) %>% summarise(land = sum(value))
FP_output <- data
#sum(data$land) # 17613993
#length(data$source_iso) #179/181 for land, 157 for water, 181 for biomass
rm(data)

# -------- water scarcity footprint (WSF)---------
E_wsf <- read.csv(paste0(path,"E_wsf.csv"), sep = ";", dec = ".")
ind <- as.numeric(E_wsf$WSF)    
rm(E_wsf)
gc()

FP <- footprint1(ind, Y_SQ)
data <- FP %>% group_by(source_iso) %>% summarise(water_scarcity = sum(value))
FP_output <- merge(FP_output, data, by = intersect("source_iso", "source_iso"), all.x=T, all.y=T, sort = FALSE)

# ------- consumptive water ---------
E_wsf <- read.csv(paste0(path,"E_wsf.csv"), sep = ";", dec = ".")
ind <- as.numeric(E_wsf$cons_water)    
rm(E_wsf)
gc()

FP <- footprint1(ind, Y_SQ)
data <- FP %>% group_by(source_iso) %>% summarise(water_cons = sum(value))
FP_output <- merge(FP_output, data, by = intersect("source_iso", "source_iso"), all.x=T, all.y=T, sort = FALSE)

## WSF version 2:
FP_output$WSF2 <- FP_output$Agg_CF_irri*FP_output$water_cons
  
# ---------- biomass ---------------------
E <- readRDS(paste0(path,"2013_E.rds"))
ind <- E$Biomass
rm(E)

FP <- footprint1(ind, Y_SQ)
data <- FP %>% group_by(source_iso) %>% summarise(biomass = sum(value))
FP_output <- merge(FP_output, data, by = intersect("source_iso", "source_iso"), all.x=T, all.y=T, sort = FALSE)


# ------- GHG emissions ------------------
load(paste0(path,"E_ghg_2013.RData"))
ind <- colSums(E_ghg[ , 2:ncol(E_ghg)])
rm(E_ghg)
gc()

FP <- footprint1(ind, Y_SQ)
data <- FP %>% group_by(source_iso) %>% summarise(GHG = sum(value))
FP_output <- merge(FP_output, data, by = intersect("source_iso", "source_iso"), all.x=T, all.y=T, sort = FALSE)



# --------- Add AWARE factors for background map ----------
E_wsf$iso3 <- countries$iso3c[match(E_wsf$Country, countries$country)] # lägg till iso med hjälp av countries-listan 
CF_aware <- subset(E_wsf, select = c(iso3, Agg_CF_irri))
CF_aware <- CF_aware[!duplicated(CF_aware), ]  
FP_output <- merge(FP_output, CF_aware, by.x = "source_iso", by.y = "iso3", all.x=T, all.y=F, sort = FALSE)


# -------- Add full country names and continents ----------
FP_output2 <- merge(FP_output, countries, by.x = "source_iso", by.y = "iso3c" , all.x=T, all.y=F, sort = FALSE)
FP_output<- FP_output2[ , -c(7) ]


### PRINT TO FILE ######
write.table(FP_output, file = "output/spatial_FP/footprints_wsf_aware_wsf2.csv", dec = ".", sep = ";",row.names = FALSE)  

#FP_output <- read.csv("output/spatial_FP/footprints_wsf.csv", dec = ".", sep = ";")


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
  Y_eggs   <- Y_SQ
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

product_Y_list <- product_Y_list.creator(Y_SQ)


#####  ------- Cropland ------------ #####
# This section has an alternative syntax, but does the same, see e.g. biomass below

E <- readRDS(paste0(path,"2013_E.rds"))
ind <- E$Landuse
rm(E)
gc()

for (i in names(product_Y_list[10:11])){  # bygg in en hoppa-över-funktion för saknad data (FP_results är tom för Fish", ger felmeddelande)
  FP_results <- footprint1(ind, product_Y_list[[i]])
  data = aggregate(FP_results$value,by=list(FP_results$source_iso),FUN=sum) # do same thing as "data <- .." above
  if (i=="Cereals"){
    output_land <- data
  } else {
    output_land <- merge(output_land, data, by= intersect("Group.1", "Group.1"), all.x=T, all.y=T, sort = FALSE)
  }
  names(output_land)[names(output_land) == 'x'] <- i
}

### PRINT TO FILE ######
write.table(output_land, file = "output/spatial_FP/footprints_products_land.csv", dec = ".", sep = ";",row.names = FALSE)  



# ### --------- Water ------------------###
# E <- readRDS(paste0(path,"2013_E.rds"))
# ind <- E$Blue_water
# rm(E)
# gc()
# 
# FP <- footprint1(ind, Y_SQ)
# data <- FP %>% group_by(source_iso) %>% summarise(water = sum(value))
# output <- merge(output, data, by = intersect("source_iso", "source_iso"), all.x=T, all.y=T, sort = FALSE)


###----------- biomass ----------------###
E <- readRDS(paste0(path,"2013_E.rds"))
ind <- E$Biomass
rm(E)


for (i in names(product_Y_list[1:8])){  # bygg in en hoppa-över-funktion för saknad data (FP_results är tom för Fish", ger felmeddelande)
  FP_results <- footprint1(ind, product_Y_list[[i]])
  data = aggregate(FP_results$value,by=list(FP_results$source_iso),FUN=sum) # do same thing as "data <- .." above
  if (i=="Cereals"){
    output_biomass <- data
  } else {
    output_biomass <- merge(output_biomass, data, by= intersect("Group.1", "Group.1"), all.x=T, all.y=T, sort = FALSE)
  }
  names(output_biomass)[names(output_biomass) == 'x'] <- i
}

for (i in names(product_Y_list[10:11])){  # bygg in en hoppa-över-funktion för saknad data (FP_results är tom för Fish", ger felmeddelande)
  FP_results <- footprint1(ind, product_Y_list[[i]])
  data = aggregate(FP_results$value,by=list(FP_results$source_iso),FUN=sum) # do same thing as "data <- .." above
  if (i=="Cereals"){
    output_biomass <- data
  } else {
    output_biomass <- merge(output_biomass, data, by= intersect("Group.1", "Group.1"), all.x=T, all.y=T, sort = FALSE)
  }
  names(output_biomass)[names(output_biomass) == 'x'] <- i
}

### PRINT TO FILE ######
write.table(output_biomass, file = "output/spatial_FP/footprints_products_biomass.csv", dec = ".", sep = ";",row.names = FALSE)  



###---------------- GHG emissions ------------###

load(paste0(path,"E_ghg_2013.RData"))
ind <- colSums(E_ghg[ , 2:ncol(E_ghg)])
rm(E_ghg)
gc()


for (i in names(product_Y_list[1:8])){  # bygg in en hoppa-över-funktion för saknad data (FP_results är tom för Fish", ger felmeddelande)
  FP_results <- footprint1(ind, product_Y_list[[i]])
  data = aggregate(FP_results$value,by=list(FP_results$source_iso),FUN=sum) # do same thing as "data <- .." above
  if (i=="Cereals"){
    output_ghg <- data
  } else {
    output_ghg <- merge(output_ghg, data, by= intersect("Group.1", "Group.1"), all.x=T, all.y=T, sort = FALSE)
  }  
  names(output_ghg)[names(output_ghg) == 'x'] <- i

}

for (i in names(product_Y_list[10:11])){  # bygg in en hoppa-över-funktion för saknad data (FP_results är tom för Fish", ger felmeddelande)
  FP_results <- footprint1(ind, product_Y_list[[i]])
  data = aggregate(FP_results$value,by=list(FP_results$source_iso),FUN=sum) # do same thing as "data <- .." above
  if (i=="Cereals"){
    output_ghg <- data
  } else {
    output_ghg <- merge(output_ghg, data, by= intersect("Group.1", "Group.1"), all.x=T, all.y=T, sort = FALSE)
  }  
  names(output_ghg)[names(output_ghg) == 'x'] <- i
}

### PRINT TO FILE ######
write.table(output_ghg, file = "output/spatial_FP/footprints_products_ghg.csv", dec = ".", sep = ";",row.names = FALSE)  



#####  ------- Quantities ------------ #####

for (i in names(product_Y_list[1:11])){  # bygg in en hoppa-över-funktion för saknad data (FP_results är tom för Fish", ger felmeddelande)
  data = aggregate(product_Y_list[[i]], by=list(index$country), FUN=sum)
  if (i=="Cereals"){
    output_mass <- data
  } else {
    output_mass <- merge(output_mass, data, by= intersect("Group.1", "Group.1"), all.x=T, all.y=T, sort = FALSE)
  }
  names(output_mass)[names(output_mass) == 'x'] <- i
}

### PRINT TO FILE ######
write.table(output_mass, file = "output/spatial_FP/footprints_products_mass.csv", dec = ".", sep = ";",row.names = FALSE)  


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