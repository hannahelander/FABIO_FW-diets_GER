###########################################################################
###----------------------- Spatial footprints ------------------------#####
###########################################################################



######### Country-specific footprints #####################

# FOOTPRINT FUNCTION ------------------------------------------------------------------
footprint1 <- function(indicator, Y_vector){ # indicator is e.g. E?Biomass, Y_vector= e.g. Y_SQ or Y_roots
  e <- as.vector(indicator) / X
  e[!is.finite(e)] <- 0
  
  # calculate multipliers
  L <- readRDS(paste0(path,"2013_L_mass.rds"))
  MP <- e * L
  rm(L)
  gc()
  FP <- t(t(MP) * Y_vector)

  rownames(FP) <- colnames(FP) <- paste0(rep(countries$iso3c, each=130), "_", rep(products$Item, 192)) # statt soll meine INDEX- matrix benutzt werden ->
  
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

# land
ind <- E$Landuse
rm(E)

FP_results <- footprint1(ind, Y_SQ) 
data <- FP_results %>% group_by(source_iso) %>% summarise(land = sum(value))
FP_output <- data
sum(data$land) # 17613993
length(data$source_iso) #179/181 for land, 157 for water, 181 for biomass

# water
E <- readRDS(paste0(path,"2013_E.rds"))
ind <- E$Blue_water
rm(E)

FP <- footprint1(ind, Y_SQ)
data <- FP %>% group_by(source_iso) %>% summarise(water = sum(value))
output <- merge(output, data, by = intersect("source_iso", "source_iso"), all.x=T, all.y=T, sort = FALSE)
  
#biomass
E <- readRDS(paste0(path,"2013_E.rds"))
ind <- E$Biomass
rm(E)

FP <- footprint1(ind, Y_SQ)
data <- FP %>% group_by(source_iso) %>% summarise(biomass = sum(value))
output2 <- merge(output, data, by = intersect("source_iso", "source_iso"), all.x=T, all.y=T, sort = FALSE)
  
#GHG emissions
#footprint1(`2013_E_ghg`[,1], SQ_Y) # rename data-frame and check how to call on this!


### PRINT TO FILE ######
write.table(output2, file = "output/spatial_FP/footprints2.csv", dec = ".", sep = ";",row.names = FALSE)  




############## Country and Product-group specific footprint ##############

# modify Y-matrix to set all the not included product-groups to zero
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


footprint <- function(product_group =  , var = integer()){
  e <- as.vector(E[,var]) / X
  e[!is.finite(e)] <- 0
  
  # calculate multipliers
  MP <- e * L
  # calculate footprints
  # FP <- MP %*% diag(Y)
  FP <- t(t(MP) * Y)
  
  rownames(FP) <- colnames(FP) <- paste0(rep(regions$iso3c, each=130), "_", rep(items$item, 192))
  
  FP <- as.matrix(FP)
  FP <- reshape2::melt(FP)
  
  results <- FP %>%
    mutate(year = 2013, emissions = E_var[var], type = "food", value = round(value, 4)) %>% 
    filter(value != 0) %>% 
    separate(Var1, sep = "_", into = c("source_iso", "source_item")) %>% 
    separate(Var2, sep = "_", into = c("final_iso", "final_item"))
  return(results)
}


#### Planera hur jag vill svara datan och g?r en data.frame - > l?gg till data fr?n varje loop till data-framen.
for (i in 1:length(product_Y_list)){
  data <- footprint(product_Y_list[i]
                    )
}

##############################################################################
# - OLD CODE -----------------------------------------
##############################################################################
# load index

# create output df
FP_country <- data.frame(Country_ID = unique(index$country),
                        Country = rep(NA, length(unique(FP$country))),
                         SQ_biomass = rep(NA, length(unique(FP$country))),
                         DGE_biomass = rep(NA, length(unique(FP$country))),
                         lancet_biomass = rep(NA, length(unique(FP$country))),
                         EATveg_biomass = rep(NA, length(unique(FP$country))),
                         SQ_landuse = rep(NA, length(unique(FP$country))),
                         DGE_landuse = rep(NA, length(unique(FP$country))),
                         lancet_landuse = rep(NA, length(unique(FP$country))),
                         EATveg_landuse = rep(NA, length(unique(FP$country))), 
                         SQ_water = rep(NA, length(unique(FP$country))),
                         DGE_water = rep(NA, length(unique(FP$country))),
                         lancet_water = rep(NA, length(unique(FP$country))),
                         EATveg_water = rep(NA, length(unique(FP$country)))
                         )
FP_country$Country <- countries$Country[match(FP_country$Country_ID, countries$ISO)] # add column with complete name of country

Y_list <- list(Y_SQ, Y_DGE, Y_lancet, Y_EATveg)

##### BIOMASS ######
L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
e <- E$Biomass / X
e[!is.finite(e)] <- 0
MP <- e * L                           # L needed
rm(L)
rm(E)

for (i in 1:length(Y_list)){
  FP_tot <- t(t(MP) * Y_list[[i]])
  FP <- data.frame(country = index$country,
                 value = rowSums(FP_tot))
  FP_country[,(i+2)] <- aggregate(value ~country, FP, sum)[,2] # footprint per country of origin (+2 because the scenarios for biomass start in column 3)
}



##### LANDUSE ######
L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
e <- E$Landuse / X
e[!is.finite(e)] <- 0
MP <- e * L                           # L needed
rm(L)
rm(E)

for (i in 1:length(Y_list)){
  FP_tot <- t(t(MP) * Y_list[[i]])
  FP <- data.frame(country = index$country,
                   value = rowSums(FP_tot))
  FP_country[,(i+6)] <- aggregate(value ~country, FP, sum)[,2] # footprint per country of origin (+6 because the scenarios for landuse start in column 7)
}


##### WATER ######
L <- readRDS(paste0(path,"2013_L_mass.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
e <- E$Blue_water / X
e[!is.finite(e)] <- 0
MP <- e * L                           # L needed
rm(L)
rm(E)

for (i in 1:length(Y_list)){
  FP_tot <- t(t(MP) * Y_list[[i]])
  FP <- data.frame(country = index$country,
                   value = rowSums(FP_tot))
  FP_country[,(i+10)] <- aggregate(value ~country, FP, sum)[,2] # footprint per country of origin
}

write.csv2(FP_country, file = "output/FP_countries.csv")


#######################################################
###------ Continent-specific footprints ----------#####
#######################################################

FP_Continent <- data.frame(continent = unique(FP$continent),
                          SQ = rep(NA, length(unique(FP$continent))),
                          DGE = rep(NA, length(unique(FP$continent))),
                          lancet = rep(NA, length(unique(FP$continent))),
                          plantbased = rep(NA, length(unique(FP$continent))),
                          EATveg = rep(NA, length(unique(FP$continent))))


Y_tot <- Y_EATveg  # choose scenario
FP_tot <- t(t(MP) * Y_tot)

FP <- data.frame(country = index$country,
                continent = index$continent,
                value = rowSums(FP_tot))
FP <- aggregate(value ~country + continent, FP, sum) # footprint pro ursprungsland

# for EACH FP (footprint and scenario), fill in the data table!
for (i in 1:nrow(FP_Continent)){
 FP_Continent$SQ[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
}

for (i in 1:nrow(FP_Continent)){
 FP_Continent$DGE[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
}

for (i in 1:nrow(FP_Continent)){
  FP_Continent$lancet[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
}
for (i in 1:nrow(FP_Continent)){
FP_Continent$plantbased[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
}

for (i in 1:nrow(FP_Continent)){
 FP_Continent$EATveg[i] <- sum(FP$value[FP$continent==FP_Continent$continent[i]])
}


write.csv2(FP_Continent, file = "output/FP_Landuse_continents.csv")


########## notes ################

# FP_plant <- sum(colSums(MP) * Y_plant) #
# 
# create result matrix
# NrOfProducts <- 130
# FP__chain <- data.frame(Scenario = rep("SQ", NrOfProducts),
#                         products = rep(index$product, each = NrOfProducts),
#                         product_group = rep(index$product_group, each = NrOfProducts))



