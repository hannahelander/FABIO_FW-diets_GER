#############################
# 
# the script take the resulting eaten food" after having removed all waste streams and modifies the 
# "eaten food" according to DGE recommendations, for this part, new categorization is needed.
# It then adds the food wastage in each step to be able to calculate the footprint including embedded wastestreams

########
#Y_eaten <- Y_eaten_food # or read from data/14.8.2019/Y_eaten_food
Y_SQ_eaten <- read.csv2(file = "data/Y_SQ_eaten.csv")
Y_SQ_eaten <- Y_SQ_eaten[,2]
Y_SQ_eaten[index$DGE_group == "excluded"] <- 0  # need to adjust some inconsistency in data

sum(Y_SQ_eaten) # 62117393

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


########### Scenario 1 - DGE recommended Diet ##########
# Using index$DGE_group (see 1a_generate_index.R for details)
# Including alcohol and suger, but limit to 10%

empty_cal <- 0.1
Milk_exclButter <- 7297058.899 # from BMEL (Milcherzeugnis zusammen minus buttermilcherzeugnis)

# covert milk (primary product equivalents) in Y_eaten to consumed weight
conv_rate <- sum(Y_SQ_eaten[index$DGE_group == "Milk"])/Milk_exclButter

Y_eaten <- Y_SQ_eaten # "final" weight of milk products (weight of milk products)
Y_eaten[index$DGE_group == "Milk"] <- Y_SQ_eaten[index$DGE_group == "Milk"] / conv_rate


SQ_DGEgroups <- data.frame(cereals_potatoes = c(sum(Y_eaten[index$DGE_group == "Cereals and potatoes"]) / population, NA, 0.3*(1-empty_cal)), #order of columns must stay the same)
                              excluded      = c(0, NA, 0),
                              vegetables_legumes = c(sum(Y_eaten[index$DGE_group == "vegetables incl. legumes"]) / population, NA, 0.26*(1-empty_cal)),
                              fruits        = c(sum(Y_eaten[index$DGE_group == "Fruits"]) / population, NA, 0.17*(1-empty_cal)),
                              alcohol_sugar = c(sum(Y_eaten[index$DGE_group == "Alcohol, sugar and honey"]) / population, NA, empty_cal),
                              veg_oils      = c(sum(Y_eaten[index$DGE_group == "Vegetable oils"]) / population, NA, 0.02*(1-empty_cal)),
                              #milk         = c(Milk_exclButter / population, NA, 0.18* (1-empty_cal)),                           # data from NEOMIT
                              milk          = c(sum(Y_eaten[index$DGE_group == "Milk"]) / population, NA, 0.18*(1-empty_cal)),
                              meat_egg_fish = c(sum(Y_eaten[index$DGE_group == "Meat, sausages, fish, eggs"]) / population, NA, 0.07*(1-empty_cal)),
                              row.names = c("SQ_capita", "SQ_percentage", "DGE_rec"))



SQ_DGEgroups <- add.percentage(SQ_DGEgroups)# add values to SQ_percentage
SQ_DGEgroups <- data.frame(t(SQ_DGEgroups))[1:8,]
SQ_DGEgroups$DGE_group <- as.character(unique(index$DGE_group)) # add DGE-groups as column


###### NEW Y-MATRIX for DGE Recommendations : EATEN ############## 

# create new Y-matrix for DGE recommendations:
Y_DGErec_MP <- Y_eaten_MP / SQ_DGEgroups$SQ_percentage[match(index$DGE_group,SQ_DGEgroups$DGE_group)] * 
  SQ_DGEgroups$DGE_rec[match(index$DGE_group, SQ_DGEgroups$DGE_group)]                          
Y_DGErec_MP[!is.finite(Y_DGErec_MP)] <- 0



# convert Y_DGErec to Primary product 
Y_DGErec <- Y_DGErec_MP
Y_DGErec[index$DGE_group == "Milk"] <- Y_DGE_rec[index$DGE_group == "Milk"] * conv_rate
sum(Y_DGErec[index$DGE_group == "Milk"])
sum(Y_eaten[index$DGE_group == "Milk"])

sum(Y_DGErec)       
sum(Y_SQ_eaten)

      
write.csv2(Y_DGErec, file = "data/Y_DGErec_eaten.csv")   


