#############################
# Create different scenarios by changing consumer behaviour in relation to Diets 
# 1) the script take the resulting diet after having removed all waste streams
# 2) it then modifies the diet according to different scenarios (eg. DGE_rec), for this part, new categorization is needed
# 3) it then adds the food wastage in each step to be able to calculate the footprint including embedded wastestreams

######## 1) remove waste from consumption to estimate "real diets"
# read Y-matrices of eaten food (Germany):

#Y_eaten <- Y_eaten_food # or read from data/14.8.2019/Y_eaten_food
Y_SQ_eaten <- read.csv2(file = "data/9.9.2019/Y_SQ_eaten.csv")
Y_eaten <- Y_SQ_eaten[,2]
Y_eaten[index$DGE_group == "excluded"] <- 0  # need to adjust some inconsistency in data

sum(Y_eaten)

# load index!

# POPULATION 2013
population <- 80645605 #2013 Source: World bank
sum(Y_eaten)/population                      # gives ~ 0.77 tonnes (~2.11 kg /capita / day)

############### Functions ###############
# FUNCTION
add.percentage <- function(diet){  # take a data frame as input and return it with a sum-column and a row for percentage
  diet$sum <- rowSums(diet)
  for (i in 1:length(diet)){
    diet[2,i] <- diet[1,i]/diet$sum[1]
  }
  diet$sum[2] <- sum(diet[2,1:length(diet)-1])
  return(diet)
}


# Function to add the consumer waste to diets (step 3):
add.consumer.waste <- function(Yeaten){
  Yreal <- Yeaten/(100-waste$final_consumption)*100
  Yreal <- Yreal/(100-waste$distribution)*100
  Yreal <- Yreal/(100-waste$processing)*100
  Yreal <- Yreal/(100-waste$storage_transport)*100
  return(Yreal)
}



########### Scenario 1 - DGE recommended Diet ##########
# Using index$DGE_group (see 1a_generate_index.R for details)
#Y_eaten <- Y[, "DEU_Food"]

# CHECK OIL CROPS AND NUTS
empty_cal <- 0.1 
SQ_DGEgroups <- data.frame(cereals_potatoes = c(sum(Y_eaten[index$DGE_group == "Cereals and potatoes"]) / population, NA, 0.3*(1-empty_cal)), #order of columns must stay the same)
                              excluded      = c(0, NA, 0),
                              vegetables_legumes = c(sum(Y_eaten[index$DGE_group == "vegetables incl. legumes"]) / population, NA, 0.26*(1-empty_cal)),
                              fruits        = c(sum(Y_eaten[index$DGE_group == "Fruits"]) / population, NA, 0.17*(1-empty_cal)), 
                              alcohol_sugar = c(sum(Y_eaten[index$DGE_group == "Alcohol, sugar and honey"]) /population, NA, empty_cal),                             
                              veg_oils      = c(sum(Y_eaten[index$DGE_group == "Vegetable oils"]) / population, NA, 0.02*(1-empty_cal)),
                              #milk         = c(0.069715, NA, 0.18* (1-empty_cal)),                           # data from NEOMIT
                              milk          = c(sum(Y_eaten[index$DGE_group == "Milk"]) / population, NA, 0.18*(1-empty_cal)),
                              meat_egg_fish = c(sum(Y_eaten[index$DGE_group == "Meat, sausages, fish, eggs"]) / population, NA, 0.07*(1-empty_cal)),
                              row.names = c("SQ_capita", "SQ_percentage", "DGE_rec"))


#library(dplyr)

#shares <- index %>%
#  group_by(DGE_group) %>%
#  summarise(sum = sum(Y_eaten)) %>%
#  mutate(DGE_group_share = sum/sum(Y_eaten))

SQ_DGEgroups <- add.percentage(SQ_DGEgroups)# add values to SQ_percentage
SQ_DGEgroups <- data.frame(t(SQ_DGEgroups))[1:8,]
SQ_DGEgroups$DGE_group <- as.character(unique(index$DGE_group)) # add DGE-groups as column


###### NEW Y-MATRIX for DGE Recommendations : EATEN ############## 
# create new Y-matrix for DGE recommendations:
Y_DGE_rec <- Y_eaten / SQ_DGEgroups$SQ_percentage[match(index$DGE_group,SQ_DGEgroups$DGE_group)] * 
  SQ_DGEgroups$DGE_rec[match(index$DGE_group, SQ_DGEgroups$DGE_group)]                          
Y_DGE_rec[!is.finite(Y_DGE_rec)] <- 0

sum(Y_DGE_rec)       
sum(Y_eaten)

write.csv2(Y_eaten, file = "data/Y_SQ_diet.csv")        # correct and saved 10.9.2019
write.csv2(Y_DGE_rec, file = "data/Y_DGErec_diet.csv") # correct and saved 10.9.2019


##### FOR FOOTPRINT CALCULATIONS: ############
###### Y- MATRICES FOR CONSUMPTION : Add food waste for each step (create the corresponding hypotetical Y-vector)##########
Y_DGE <- add.consumer.waste(Y_DGE_rec) # CREATES MORE WASTE THAN THE SQ SCENARIO
sum(Y_DGE)            #shows that sum Y_DGE is slightly bigger -> realistic due to increased food waste
sum(Y[,"DEU_Food"])

# write the corresponding Y vector to file (for supply chain analysis)
write.csv2(Y_DGE, file = "data/Y_DGE.csv")



######### SQ-diets with more detailed grouping ##########
# use index$diet_group (see 1a_generate_index.R for more details)

# Create data-frame for scenarios, splitted in diet-groups (and sub-groups)
#SQ_Eaten <- data.frame(cereals_potatoes = c(sum(Y_eaten[index$diet_group == "Cereals and potatoes"]) / population, NA), 
#                    vegetables = c(sum(Y_eaten[index$diet_group == "Vegetables, pulses, spices"]) / population, NA),
#                    fruits     = c(sum(Y_eaten[index$diet_group == "Fruits"]) / population, NA),
#                    oil_crops_nuts = c(sum(Y_eaten[index$diet_group == "Oil crops and nuts"]) / population, NA),
#                    veg_oils   = c(sum(Y_eaten[index$diet_group == "Vegetable oils"]) / population, NA),
#                    Oil_cakes  = c(sum(Y_eaten[index$diet_group == "Oil cakes"]) / population, NA),
#                    fibre_crops = c(sum(Y_eaten[index$diet_group == "Fibre crops"]) / population, NA),
#                    sugar      = c(sum(Y_eaten[index$diet_group == "Sugar, sweeteners"]) / population, NA),
#                    sugar_crops = c(sum(Y_eaten[index$diet_group == "Sugar crops"]) / population, NA),
#                    alcohol    = c(sum(Y_eaten[index$diet_group == "Alcohol"]) / population, NA),
#                    coffee_tea = c(sum(Y_eaten[index$diet_group == "Coffee, tea, cocoa"]) / population, NA),
#                    meat       = c(sum(Y_eaten[index$diet_group == "Meat"]) / population, NA),
#                    milk       = c(sum(Y_eaten[index$diet_group == "Milk"]) / population, NA),
#                    eggs       = c(sum(Y_eaten[index$diet_group == "Eggs"]) / population, NA),
#                    fish       = c(sum(Y_eaten[index$diet_group == "Fish"]) / population, NA), 
#                    live_animals = c(sum(Y_eaten[index$diet_group == "Live animals"]) / population, NA), 
#                    fodder = c(sum(Y_eaten[index$diet_group == "Fodder crops, grazing"]) / population, NA),
#                    honey = c(sum(Y_eaten[index$diet_group == "Honey"]) / population, NA), 
#                    row.names = c("SQ_capita", "SQ_percentage"))
# ignoring 'Wood', 'Hides, skines, wool', 'Tobacco, rubber', 'Ethanol'

#################

# DIETS EXCLUDING DRINKS:
#alcohol <- Diets$alcohol[1]
#coffee_tea <- Diets$coffee_tea[1]
#Diets_food = Diets[, which(!Diets[1,] == alcohol & !Diets[1,] == coffee_tea)]

#Diets <- add.percentage(Diets)
#Diets_food <- add.percentage(Diets_food)


# MODIFYING DATA TO SUIT THE PURPOSE 
SQ_Diets = Eaten[, which(!Eaten[1,] == 0)]          #removing categories == 0
SQ_Diets$milk[1] <- 0.069715                        # Replacing Milk-equivalents in tonnes of milk products (source: NEMONIT)

SQ_Diets$fodder[1] <- 0 # OBS!! There are some very small negative values for fodder, that we needt to set to 0




###### FUNKTIONS NOT IN USE ##########


# Function that recalculates the amounts of each product according to DGE_rec in % (The total amount of kg remains the same)#
#diet.converter <- function(Yvector){
#  for (i in 1:length(DGE_table)){
#    Yvector[which(index$DGE_group == DGE_table[1,i])] <- Yvector[which(index$DGE_group == DGE_table[1,i])] /    # takes the existing value, divides it with existing % of product group, multiplies it with desired percentage
#      (DGE_table["SQ_percentage", i] * DGE_table["DGE_rec",i])
#  }
#  return(Yvector)
#}

# Function to group Yvector into DGE-groups: 
DGE.grouper <- function(Yvector){
  DGE_groups <- data.frame(cereals_potatoes = c(sum(Yvector[index$DGE_group == "Cereals and potatoes"]) / population, NA, 0.3), #order of columns must stay the same)
                           excluded      = c(0, NA, 0),
                           vegetables_legumes = c(sum(Yvector[index$DGE_group == "vegetables incl. legumes"]) / population, NA, 0.26),
                           fruits        = c(sum(Yvector[index$DGE_group == "Fruits"]) / population, NA, 0.17), 
                           sugar_honey   = c(sum(Yvector[index$DGE_group == "Sugar & honey"]) / population, NA, 0),
                           veg_oils      = c(sum(Yvector[index$DGE_group == "Vegetable oils"]) / population, NA, 0.02),
                           milk          = c(sum(Yvector[index$DGE_group == "Milk"]) / population, NA, 0.18),                                                      # data from NEOMIT
                           meat_egg_fish = c(sum(Yvector[index$DGE_group == "Meat, sausages, fish, eggs"]) / population, NA, 0.07),
                           row.names = c("capita", "percentage", "DGE_rec"))
  DGE_groups <- add.percentage(DGE_groups)
  return(DGE_groups)
}
