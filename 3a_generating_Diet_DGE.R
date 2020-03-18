################
# Part 1: Modifies Y-eaten to create a "DGE nutrition circle -Diet". 
# The script takes the "eaten food" (after having removed all waste streams) and modifies the 
# "eaten food" according to DGE recommendations, for this part, new categorization is needed.
# PART 2: Create and exoport DGE Diet (before kcal standardization) 
# It converts Y-vector to a data-frame of product names and nutrient values
################

############### Functions ###############
# FUNCTION calculating percentage of different categories (for the DGE categories)
add.percentage <- function(diet){  # take a data frame as input and return it with a sum-column and a row for percentage
  diet$sum <- rowSums(diet)
  for (i in 1:length(diet)){
    diet[2,i] <- diet[1,i]/diet$sum[1]
  }
  diet$sum[2] <- sum(diet[2,1:length(diet)-1])
  return(diet)
}


######## Load Data ########

index <- read.csv2(file = "data/index_data_frame.csv")
population <- 80645605 #2013 Source: World bank


#Y_eaten <- Y_eaten_food # or read from data/14.8.2019/Y_eaten_food
Y_SQ_eaten <- read.csv2(file = "data/Y_SQ_eaten.csv") # this is generated in Y_quantities
Y_SQ_eaten <- Y_SQ_eaten[,2]

sum(Y_SQ_eaten) # 60519278 (instead of the "old" 62117393)


########### Create Y-eaten according to DGE nutrition circle (before scaling to kcal) ##########

# Using index$DGE_group (see 1a_generate_index.R for details)
# Including alcohol and suger, but limit to 10%

empty_cal <- 0.1

# covert milk (primary product equivalents) in Y_eaten to consumed weight
Milk_exclButter <- 7297058.899 # from BMEL (Milcherzeugnis zusammen minus buttermilcherzeugnis)
conv_rate <- sum(Y_SQ_eaten[index$product == "Milk - Excluding Butter"])/Milk_exclButter
Y_eaten <- Y_SQ_eaten # "final" weight of milk products (weight of milk products)
Y_eaten[index$product == "Milk - Excluding Butter"] <- Y_SQ_eaten[index$product == "Milk - Excluding Butter"] / conv_rate

#sum(Y_eaten[index$DGE_group=="Milk"])


SQ_DGEgroups <- data.frame(cereals_potatoes = c(sum(Y_eaten[index$DGE_group == "Cereals and potatoes"]) / population, NA, 0.3*(1-empty_cal)), #order of columns must stay the same)
                           excluded      = c(0, NA, 0),
                           vegetables_legumes = c(sum(Y_eaten[index$DGE_group == "vegetables incl. legumes"]) / population, NA, 0.26*(1-empty_cal)),
                           fruits        = c(sum(Y_eaten[index$DGE_group == "Fruits"]) / population, NA, 0.17*(1-empty_cal)),
                           alcohol_sugar = c(sum(Y_eaten[index$DGE_group == "Alcohol, sugar and honey"]) / population, NA, empty_cal),
                           veg_oils      = c(sum(Y_eaten[index$DGE_group == "Vegetable oils"]) / population, NA, 0.02*(1-empty_cal)),
                           milk          = c(sum(Y_eaten[index$DGE_group == "Milk"]) / population, NA, 0.18*(1-empty_cal)),
                           meat_egg_fish = c(sum(Y_eaten[index$DGE_group == "Meat, sausages, fish, eggs"]) / population, NA, 0.07*(1-empty_cal)),
                           row.names = c("SQ_capita", "SQ_percentage", "DGE_rec"))



SQ_DGEgroups <- add.percentage(SQ_DGEgroups)# add values to SQ_percentage
SQ_DGEgroups <- data.frame(t(SQ_DGEgroups))[1:8,]
SQ_DGEgroups$DGE_group <- as.character(unique(index$DGE_group)) # add DGE-groups as column



###### NEW Y-MATRIX for DGE Recommendations : EATEN ############## 
Y_DGErec_MP <- Y_eaten / SQ_DGEgroups$SQ_percentage[match(index$DGE_group,SQ_DGEgroups$DGE_group)] * 
  SQ_DGEgroups$DGE_rec[match(index$DGE_group, SQ_DGEgroups$DGE_group)]                          
Y_DGErec_MP[!is.finite(Y_DGErec_MP)] <- 0

sum(Y_DGErec_MP) * 1000000 /(population *365) # 1684.191 (actual weight of eaten food unscaled (therefore same as SQ))
sum(Y_eaten) * 1000000 /(population *365)

# convert Y_DGErec back to Primary product (->DGE eaten BEFORE scaling)
Y_DGErec_eaten <- Y_DGErec_MP
Y_DGErec_eaten[index$product == "Milk - Excluding Butter"] <- Y_DGErec_MP[index$product == "Milk - Excluding Butter"] * conv_rate # DGE eaten BEFORE scaling


#####
#Y_SQ_eaten <- read.csv2(file = "data/Y_SQ_eaten_maxW.csv") # used for scenarios of minimum oor maximum FWL-levels, from 2b_quantities (uncertainty analysis) 
#Y_SQ_eaten <- Y_SQ_eaten[,2]

###### PART 2 - Creating diets for output to Excel ##########

# Converting unit (from tonnes/year) to: eaten food grams/person/day
Y_SQ_diet <- Y_SQ_eaten * 1000000 / population / 365
Y_DGErec_diet <- Y_DGErec_eaten * 1000000 / population / 365

sum(Y_SQ_diet)       # 2055.986 in primary products
sum(Y_DGErec_diet)   # 2092.349 in primary products


# load nutritional data and save is as per gram specifications
items <- read.csv2(file = "data/Items_nutr_.csv", stringsAsFactors = FALSE)
items$Kcal <- as.numeric(items$Kcal) /100
items$G_prot <- as.numeric(items$G_prot) /100
items$G_fat <- as.numeric(items$G_fat) /100

# structure in Data frame
Diets_df <- data.frame(product   = items$Item,
                       Item_code = items$Item.Code,
                       DGEgroup = rep(NA, nrow(items)),
                       SQ_g      = rep(NA, nrow(items)),
                       SQ_kcal   = rep(NA, nrow(items)),
                       SQ_prot   = rep(NA, nrow(items)), 
                       SQ_fat    = rep(NA, nrow(items)),  
                       DGErec_g    = rep(NA, nrow(items)),
                       DGErec_kcal = rep(NA, nrow(items)),
                       DGErec_prot = rep(NA, nrow(items)),
                       DGErec_fat  = rep(NA, nrow(items)),
                       kcal_data = rep(NA, nrow(items)),
                       prot_data = rep(NA, nrow(items)),
                       fat_data = rep(NA, nrow(items))
)


for (i in 1:nrow(items)) {
  Diets_df$DGEgroup[i]  <- as.character(index$DGE_group[match(items$Item.Code[i],index$item_code)])
  Diets_df$SQ_g[i]       <- sum(Y_SQ_diet[index$item_code==items$Item.Code[i]])
  Diets_df$SQ_kcal[i] <- items$Kcal[i]   * sum(Y_SQ_diet[index$item_code==items$Item.Code[i]])
  Diets_df$SQ_prot[i] <- items$G_prot[i] * sum(Y_SQ_diet[index$item_code==items$Item.Code[i]])
  Diets_df$SQ_fat[i]  <- items$G_fat[i]  * sum(Y_SQ_diet[index$item_code==items$Item.Code[i]])
  Diets_df$DGErec_g[i]  <- sum(Y_DGErec_diet[index$item_code==items$Item.Code[i]])
  Diets_df$DGErec_kcal[i] <- items$Kcal[i]   * sum(Y_DGErec_diet[index$item_code==items$Item.Code[i]])
  Diets_df$DGErec_prot[i] <- items$G_prot[i] * sum(Y_DGErec_diet[index$item_code==items$Item.Code[i]])
  Diets_df$DGErec_fat[i] <- items$G_fat[i]   * sum(Y_DGErec_diet[index$item_code==items$Item.Code[i]])
  Diets_df$kcal_data[i] <- as.numeric(items$Kcal[i])
  Diets_df$prot_data[i] <- as.numeric(items$G_prot[i])
  Diets_df$fat_data[i]  <- as.numeric(items$G_fat[i])
}

# MAIN FILE:
Diets_df <- rbind(Diets_df, data.frame(product ="Sum", Item_code = "NA", dietgroup="NA" , t(sapply(Diets_df[,4:11], sum, na.rm = TRUE)), 
                                       kcal_data = NA, prot_data = NA, fat_data = NA))



# Main file without "excluded":
Diets_df <- Diets_df[!(Diets_df$dietgroup == "excluded"), ]
write.csv(Diets_df, file = "output/Diets_SQ_DGErec_v3.csv")





############ Preparation for visualization ############

# #prepare For visualization
# Diets_summary <- data.frame(cereals_potatoes = c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "Cereals and potatoes"]),
#                                                  sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "Cereals and potatoes"]), 
#                                                  sum(Diets_df$SQ_prot[Diets_df$dietgroup == "Cereals and potatoes"]), 
#                                                  sum(Diets_df$SQ_fat[Diets_df$dietgroup == "Cereals and potatoes"]),
#                                                  sum(Diets_df$DGErec_g[Diets_df$dietgroup == "Cereals and potatoes"]),
#                                                  sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "Cereals and potatoes"]), 
#                                                  sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "Cereals and potatoes"]), 
#                                                  sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "Cereals and potatoes"])), #order of columns must stay the same)
#                                excluded      = c(0, 0, 0, 0, 0, 0, 0, 0),
#                                veg_legumes   = c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "vegetables incl. legumes"]),
#                                                  sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "vegetables incl. legumes"]),
#                                                  sum(Diets_df$SQ_prot[Diets_df$dietgroup == "vegetables incl. legumes"]),
#                                                  sum(Diets_df$SQ_fat[Diets_df$dietgroup == "vegetables incl. legumes"]),
#                                                  sum(Diets_df$DGErec_g[Diets_df$dietgroup == "vegetables incl. legumes"]), 
#                                                  sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "vegetables incl. legumes"]), 
#                                                  sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "vegetables incl. legumes"]),
#                                                  sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "vegetables incl. legumes"])),
#                                fruits        = c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "Fruits"]), 
#                                                  sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "Fruits"]),
#                                                  sum(Diets_df$SQ_prot[Diets_df$dietgroup == "Fruits"]),
#                                                  sum(Diets_df$SQ_fat[Diets_df$dietgroup == "Fruits"]),
#                                                  sum(Diets_df$DGErec_g[Diets_df$dietgroup == "Fruits"]),
#                                                  sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "Fruits"]), 
#                                                  sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "Fruits"]),
#                                                  sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "Fruits"])), 
#                                alcohol_sugar = c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "Alcohol, sugar and honey"]), 
#                                                  sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "Alcohol, sugar and honey"]),
#                                                  sum(Diets_df$SQ_prot[Diets_df$dietgroup == "Alcohol, sugar and honey"]),
#                                                  sum(Diets_df$SQ_fat[Diets_df$dietgroup == "Alcohol, sugar and honey"]),
#                                                  sum(Diets_df$DGErec_g[Diets_df$dietgroup == "Alcohol, sugar and honey"]), 
#                                                  sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "Alcohol, sugar and honey"]),
#                                                  sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "Alcohol, sugar and honey"]),
#                                                  sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "Alcohol, sugar and honey"])),
#                                veg_oils      = c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "Vegetable oils"]), 
#                                                  sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "Vegetable oils"]),
#                                                  sum(Diets_df$SQ_prot[Diets_df$dietgroup == "Vegetable oils"]), 
#                                                  sum(Diets_df$SQ_fat[Diets_df$dietgroup == "Vegetable oils"]), 
#                                                  sum(Diets_df$DGErec_g[Diets_df$dietgroup == "Vegetable oils"]), 
#                                                  sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "Vegetable oils"]),
#                                                  sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "Vegetable oils"]), 
#                                                  sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "Vegetable oils"])),
#                                milk          = c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "Milk"]), 
#                                                  sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "Milk"]),
#                                                  sum(Diets_df$SQ_prot[Diets_df$dietgroup == "Milk"]),
#                                                  sum(Diets_df$SQ_fat[Diets_df$dietgroup == "Milk"]),
#                                                  sum(Diets_df$DGErec_g[Diets_df$dietgroup == "Milk"]), 
#                                                  sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "Milk"]),
#                                                  sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "Milk"]),
#                                                  sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "Milk"])),
#                                meat_fish_eggs= c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE), 
#                                                  sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE),
#                                                  sum(Diets_df$SQ_prot[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE),
#                                                  sum(Diets_df$SQ_fat[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE), 
#                                                  sum(Diets_df$DGErec_g[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE), 
#                                                  sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE),
#                                                  sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE),
#                                                  sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE)),
#                                row.names = c("SQ_g", "SQ_kcal", "SQ_prot", "SQ_fat", "DGErec_g", "DGErec_kcal", "DGErec_prot", "DGErec_fat")
# )
# 
# Diets_summary$Sum <- rowSums(Diets_summary)
# Diets_summary <- data.frame(t(Diets_summary))

#write.csv2(Diets_summary, file = "Diet_output")

#sums <- colSums(Diets_df[,3:ncol(Diets_df)])
#rbind(Diets_df, data.frame(product ='Sum', dietgroup=NA, income = sum(x$income)))
#Diets_df$Sum <- sum(Diets_df$SQ_g)
