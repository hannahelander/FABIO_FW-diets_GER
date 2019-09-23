###
# Load data

index <- read.csv2(file = "data/14.8.2019/index_data_frame.csv")

Y_SQ_diet <- read.csv2(file = "data/Y_SQ_diet.csv") # the Y matrix for eaten!TO BE USED!
Y_SQ_diet <- Y_SQ_diet[,2]

Y_DGErec_diet <- read.csv2("data/Y_DGErec_diet.csv") # the Y matrix for eaten!TO BE USED!
Y_DGErec_diet <- Y_DGErec_diet[,2]

#sum(Y_SQ_diet)
#sum(Y_DGErec_diet)

# Converting unit (from tonnes/year) to: eaten food grams/person/day
Y_SQ_diet <- Y_SQ_diet * 1000000 / population / 365
Y_DGErec_diet <- Y_DGErec_diet * 1000000 / population / 365

# load nutritional data and save is as per gram specifications
items <- read.csv2(file = "data/Items_nutr_.csv", stringsAsFactors = FALSE)
items$Kcal <- as.numeric(items$Kcal) /100
items$G_prot <- as.numeric(items$G_prot) /100
items$G_fat <- as.numeric(items$G_fat) /100

# structure in Data frame

Diets_df <- data.frame(product   = items$Item,
                       dietgroup = rep(NA, nrow(items)),
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
  Diets_df$dietgroup[i]  <- as.character(index$DGE_group[match(items$Item.Code[i],index$item_code)])
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
Diets_df <- rbind(Diets_df, data.frame(product ="Sum", dietgroup="NA" , t(sapply(Diets_df[,3:10], sum, na.rm = TRUE)), 
                                       kcal_data = NA, prot_data = NA, fat_data = NA))
#write.csv2(Diets_df, file = "output/Diets_master.csv")

# Main file without "excluded":
Diets_df <- Diets_df[!(Diets_df$dietgroup == "excluded"), ]
write.csv2(Diets_df, file = "output/Diets_analysis.csv")


#prepare For visualization
Diets_summary <- data.frame(cereals_potatoes = c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "Cereals and potatoes"]),
                                                 sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "Cereals and potatoes"]), 
                                                 sum(Diets_df$SQ_prot[Diets_df$dietgroup == "Cereals and potatoes"]), 
                                                 sum(Diets_df$SQ_fat[Diets_df$dietgroup == "Cereals and potatoes"]),
                                                 sum(Diets_df$DGErec_g[Diets_df$dietgroup == "Cereals and potatoes"]),
                                                 sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "Cereals and potatoes"]), 
                                                 sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "Cereals and potatoes"]), 
                                                 sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "Cereals and potatoes"])), #order of columns must stay the same)
                               excluded      = c(0, 0, 0, 0, 0, 0, 0, 0),
                               veg_legumes   = c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "vegetables incl. legumes"]),
                                                 sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "vegetables incl. legumes"]),
                                                 sum(Diets_df$SQ_prot[Diets_df$dietgroup == "vegetables incl. legumes"]),
                                                 sum(Diets_df$SQ_fat[Diets_df$dietgroup == "vegetables incl. legumes"]),
                                                 sum(Diets_df$DGErec_g[Diets_df$dietgroup == "vegetables incl. legumes"]), 
                                                 sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "vegetables incl. legumes"]), 
                                                 sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "vegetables incl. legumes"]),
                                                 sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "vegetables incl. legumes"])),
                               fruits        = c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "Fruits"]), 
                                                 sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "Fruits"]),
                                                 sum(Diets_df$SQ_prot[Diets_df$dietgroup == "Fruits"]),
                                                 sum(Diets_df$SQ_fat[Diets_df$dietgroup == "Fruits"]),
                                                 sum(Diets_df$DGErec_g[Diets_df$dietgroup == "Fruits"]),
                                                 sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "Fruits"]), 
                                                 sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "Fruits"]),
                                                 sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "Fruits"])), 
                               alcohol_sugar = c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "Alcohol, sugar and honey"]), 
                                                 sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "Alcohol, sugar and honey"]),
                                                 sum(Diets_df$SQ_prot[Diets_df$dietgroup == "Alcohol, sugar and honey"]),
                                                 sum(Diets_df$SQ_fat[Diets_df$dietgroup == "Alcohol, sugar and honey"]),
                                                 sum(Diets_df$DGErec_g[Diets_df$dietgroup == "Alcohol, sugar and honey"]), 
                                                 sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "Alcohol, sugar and honey"]),
                                                 sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "Alcohol, sugar and honey"]),
                                                 sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "Alcohol, sugar and honey"])),
                               veg_oils      = c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "Vegetable oils"]), 
                                                 sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "Vegetable oils"]),
                                                 sum(Diets_df$SQ_prot[Diets_df$dietgroup == "Vegetable oils"]), 
                                                 sum(Diets_df$SQ_fat[Diets_df$dietgroup == "Vegetable oils"]), 
                                                 sum(Diets_df$DGErec_g[Diets_df$dietgroup == "Vegetable oils"]), 
                                                 sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "Vegetable oils"]),
                                                 sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "Vegetable oils"]), 
                                                 sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "Vegetable oils"])),
                               milk          = c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "Milk"]), 
                                                 sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "Milk"]),
                                                 sum(Diets_df$SQ_prot[Diets_df$dietgroup == "Milk"]),
                                                 sum(Diets_df$SQ_fat[Diets_df$dietgroup == "Milk"]),
                                                 sum(Diets_df$DGErec_g[Diets_df$dietgroup == "Milk"]), 
                                                 sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "Milk"]),
                                                 sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "Milk"]),
                                                 sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "Milk"])),
                               meat_fish_eggs= c(sum(Diets_df$SQ_g[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE), 
                                                 sum(Diets_df$SQ_kcal[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE),
                                                 sum(Diets_df$SQ_prot[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE),
                                                 sum(Diets_df$SQ_fat[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE), 
                                                 sum(Diets_df$DGErec_g[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE), 
                                                 sum(Diets_df$DGErec_kcal[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE),
                                                 sum(Diets_df$DGErec_prot[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE),
                                                 sum(Diets_df$DGErec_fat[Diets_df$dietgroup == "Meat, sausages, fish, eggs"], na.rm = TRUE)),
                               row.names = c("SQ_g", "SQ_kcal", "SQ_prot", "SQ_fat", "DGErec_g", "DGErec_kcal", "DGErec_prot", "DGErec_fat")
)

Diets_summary$Sum <- rowSums(Diets_summary)
Diets_summary <- data.frame(t(Diets_summary))

#sums <- colSums(Diets_df[,3:ncol(Diets_df)])
#rbind(Diets_df, data.frame(product ='Sum', dietgroup=NA, income = sum(x$income)))
#Diets_df$Sum <- sum(Diets_df$SQ_g)
