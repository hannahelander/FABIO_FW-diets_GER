####### 3_Y_modifier_nutr.R
####### This script include nutrient calculations for diets


#DGErec_diet <- read.csv2("data/DGE_rec.csv")
Y_DGErec <- read.csv2("data/Y_DGE.csv")  # Correspond to the amount of food bought in Supermarkets (FAO terminology: Consumption)
Y_DGErec <- Y_DGErec[,2]

Y_SQ_diet <- read.csv2(file = "data/SQ_diet.csv") # the Y matrix for eaten!TO BE USED!
Y_SQ_diet <- Y_SQ_diet[,2]

Y_DGErec_diet <- read.csv2("data/Y_DGE_rec_diet.csv") # the Y matrix for eaten!TO BE USED!
Y_DGErec_diet <- Y_DGErec_diet[,2]

Y_scenario <- Y_DGErec_diet # choose what to calculate
#Y_scenario <- Y_


# Read file with data for food items
items <- read.csv2(file = "data/Items_nutr_.csv", stringsAsFactors = FALSE)
items$Kcal <- as.numeric(items$Kcal)
items$G_prot <- as.numeric(items$G_prot)
items$G_fat <- as.numeric(items$G_fat)

#### Create data-frame to store diets in terms of eaten food and consumed nutrients etc.
Diets_matrix <- data.frame(item = items$Item,
                           Kcal_eaten = rep(NA, nrow(items)),
                           Prot_eaten = rep(NA, nrow(items)),
                           Fats_eaten = rep(NA, nrow(items)),
                           DGE_group = rep(NA, nrow(items))
)

for (i in 1:nrow(items)) {
  Diets_matrix$Kcal_eaten[i] <- items$Kcal[i]   * sum(Y_scenario[index$item_code==items$Item.Code[i]])
  Diets_matrix$Prot_eaten[i] <- items$G_prot[i] * sum(Y_scenario[index$item_code==items$Item.Code[i]])
  Diets_matrix$Fats_eaten[i] <- items$G_fat[i]  * sum(Y_scenario[index$item_code==items$Item.Code[i]])
  Diets_matrix$DGE_group[i]  <- as.character(index$DGE_group[match(items$Item.Code[i],index$item_code)])

}

population <- 80645605 /10000 #2013 Source: World bank #converting tonnes to 100 grams
day <- 365
#prepare data for visualization
Food_nutr_capita <- data.frame(cereals_potatoes = c(sum(Diets_matrix$Kcal_eaten[Diets_matrix$DGE_group == "Cereals and potatoes"]) / population /day, 
                                                    sum(Diets_matrix$Prot_eaten[Diets_matrix$DGE_group == "Cereals and potatoes"]) / population /day, 
                                                    sum(Diets_matrix$Fats_eaten[Diets_matrix$DGE_group == "Cereals and potatoes"]) / population /day), #order of columns must stay the same)
                              excluded      = c(0, 0, 0),
                              veg_legumes   = c(sum(Diets_matrix$Kcal_eaten[Diets_matrix$DGE_group == "vegetables incl. legumes"]) / population /day, 
                                                sum(Diets_matrix$Prot_eaten[Diets_matrix$DGE_group == "vegetables incl. legumes"]) / population /day,
                                                sum(Diets_matrix$Fats_eaten[Diets_matrix$DGE_group == "vegetables incl. legumes"]) / population /day),
                              fruits        = c(sum(Diets_matrix$Kcal_eaten[Diets_matrix$DGE_group == "Fruits"]) / population /day, 
                                                sum(Diets_matrix$Prot_eaten[Diets_matrix$DGE_group == "Fruits"]) / population /day,
                                                sum(Diets_matrix$Fats_eaten[Diets_matrix$DGE_group == "Fruits"]) / population /day) , 
                              alcohol_sugar = c(sum(Diets_matrix$Kcal_eaten[Diets_matrix$DGE_group == "Alcohol, sugar and honey"]) / population /day, 
                                                sum(Diets_matrix$Prot_eaten[Diets_matrix$DGE_group == "Alcohol, sugar and honey"]) / population /day,
                                                sum(Diets_matrix$Fats_eaten[Diets_matrix$DGE_group == "Alcohol, sugar and honey"]) / population /day),
                              veg_oils      = c(sum(Diets_matrix$Kcal_eaten[Diets_matrix$DGE_group == "Vegetable oils"]) / population /day, 
                                                sum(Diets_matrix$Prot_eaten[Diets_matrix$DGE_group == "Vegetable oils"]) / population /day, 
                                                sum(Diets_matrix$Fats_eaten[Diets_matrix$DGE_group == "Vegetable oils"]) / population /day),
                              milk          = c(sum(Diets_matrix$Kcal_eaten[Diets_matrix$DGE_group == "Milk"]) / population /day, 
                                                sum(Diets_matrix$Prot_eaten[Diets_matrix$DGE_group == "Milk"]) / population /day,
                                                sum(Diets_matrix$Fats_eaten[Diets_matrix$DGE_group == "Milk"]) / population /day),
                              meat_fish_eggs= c(sum(Diets_matrix$Kcal_eaten[Diets_matrix$DGE_group == "Meat, sausages, fish, eggs"], na.rm = TRUE) / population /day, 
                                                sum(Diets_matrix$Prot_eaten[Diets_matrix$DGE_group == "Meat, sausages, fish, eggs"], na.rm = TRUE) / population /day,
                                                sum(Diets_matrix$Fats_eaten[Diets_matrix$DGE_group == "Meat, sausages, fish, eggs"], na.rm = TRUE) / population /day),
                              row.names = c("Kcal", "Prot", "Fats")
                              )

Food_nutr_capita$Sum <- rowSums(Food_nutr_capita)
Food_nutr_capita <- data.frame(t(Food_nutr_capita))
#Food_nutr_capita <- rbind(Food_nutr_capita, c(sum(Food_nutr_capita[,"Kcal"]), sum(Food_nutr_capita[,"Prot"]), sum(Food_nutr_capita[,"Fats"])))

# write to file:
#write.csv2(Food_nutr_capita, file = "output/SQ_Food_nutr_capita.csv")
#write.csv2(Food_nutr_capita, file = "output/DGErec_Food_nutr_capita_2_IgnoringFoodWaste.csv")
write.csv2(Food_nutr_capita, file = "output/DGErec_diet_Food_nutr_capita2.csv")

# Calculate total Kcal/person
#Kcal_pp_day <- sum(Diets_matrix$Kcal_eaten) / population / 365



##### Old trials ######
#items$Kcal[which(index$item_code == 2511)] # gives a whole vector with right number + NAs

#sum(Y_eaten[which(index$item_code == 2516)]) * as.numeric(items$Kcal[index$item_code ==  2516]) /population
#Y_eaten[index$item_code == 2516] 
#Kcal_pp_day <- Y_eaten[match(index$item_code, items$Item.Code)] * as.numeric(items$Kcal[match(index$item_code, items$Item.Code)]) /population /356
