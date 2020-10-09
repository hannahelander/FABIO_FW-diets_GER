##########################################################
###   -----  Sensitivity analysis for SQ   -------- #####
##########################################################

### Y vectors on eaten food for minimum-/maximum waste scenarios SQ
Y_SQ_eaten_minW <- read.csv2(file = "data/Y_SQ_eaten_minW.csv")
Y_SQ_eaten_minW <- Y_SQ_eaten_minW[,2]
Y_SQ_eaten_minW[index$DGE_group == "excluded"] <- 0  # removing products not eaten by humans
Y_SQ_diet_minW <- Y_SQ_eaten_minW * 1000000 / population / 365 #Converting unit (from "national tons/y" to "eaten food g/p/day")

sum(Y_SQ_eaten_minW) # = 69231969
sum(Y_SQ_diet_minW) # 2351.977


Y_SQ_eaten_maxW <- read.csv2(file = "data/Y_SQ_eaten_maxW.csv")
Y_SQ_eaten_maxW <- Y_SQ_eaten_maxW[,2]
Y_SQ_eaten_maxW[index$DGE_group == "excluded"] <- 0  # removing products not eaten by humans
Y_SQ_diet_maxW <- Y_SQ_eaten_maxW * 1000000 / population / 365 #Converting unit (from "national tons/y" to "eaten food g/p/day")

sum(Y_SQ_eaten_maxW) # 52264577
sum(Y_SQ_diet_maxW) # 1775.554
############################################################
####---------Calculation of nutrients -----------------#####
############################################################

### This is using Y_S_diet and Y_DGE_diet, I need to replace these with Y_SQ_diet_minW and Y_SQ_diet_maxW

# load nutritional data and save is as per gram specifications
items <- read.csv2(file = "data/Items_nutr_.csv", stringsAsFactors = FALSE)
items$Kcal <- as.numeric(items$Kcal) /100
items$G_prot <- as.numeric(items$G_prot) /100
items$G_fat <- as.numeric(items$G_fat) /100

# structure in Data frame
Diets_df <- data.frame(product   = items$Item,
                       Item_code = items$Item.Code,
                       DGEgroup = rep(NA, nrow(items)),
                       SQ_minW_g      = rep(NA, nrow(items)),
                       SQ_minW_kcal   = rep(NA, nrow(items)),
                       SQ_minW_prot   = rep(NA, nrow(items)), 
                       SQ_minW_fat    = rep(NA, nrow(items)),  
                       SQ_maxW_g    = rep(NA, nrow(items)),
                       SQ_maxW_kcal = rep(NA, nrow(items)),
                       SQ_maxW_prot = rep(NA, nrow(items)),
                       SQ_maxW_fat  = rep(NA, nrow(items)),
                       kcal_data = rep(NA, nrow(items)),
                       prot_data = rep(NA, nrow(items)),
                       fat_data = rep(NA, nrow(items))
)


for (i in 1:nrow(items)) {
  Diets_df$DGEgroup[i]  <- as.character(index$DGE_group[match(items$Item.Code[i],index$item_code)])
  Diets_df$SQ_minW_g[i]       <- sum(Y_SQ_diet_minW[index$item_code==items$Item.Code[i]])
  Diets_df$SQ_minW_kcal[i] <- items$Kcal[i]   * sum(Y_SQ_diet_minW[index$item_code==items$Item.Code[i]])
  Diets_df$SQ_minW_prot[i] <- items$G_prot[i] * sum(Y_SQ_diet_minW[index$item_code==items$Item.Code[i]])
  Diets_df$SQ_minW_fat[i]  <- items$G_fat[i]  * sum(Y_SQ_diet_minW[index$item_code==items$Item.Code[i]])
  Diets_df$SQ_maxW_g[i]  <- sum(Y_SQ_diet_maxW[index$item_code==items$Item.Code[i]])
  Diets_df$SQ_maxW_kcal[i] <- items$Kcal[i]   * sum(Y_SQ_diet_maxW[index$item_code==items$Item.Code[i]])
  Diets_df$SQ_maxW_prot[i] <- items$G_prot[i] * sum(Y_SQ_diet_maxW[index$item_code==items$Item.Code[i]])
  Diets_df$SQ_maxW_fat[i] <- items$G_fat[i]   * sum(Y_SQ_diet_maxW[index$item_code==items$Item.Code[i]])
  Diets_df$kcal_data[i] <- as.numeric(items$Kcal[i])
  Diets_df$prot_data[i] <- as.numeric(items$G_prot[i])
  Diets_df$fat_data[i]  <- as.numeric(items$G_fat[i])
}

# MAIN FILE:
Diets_df <- rbind(Diets_df, data.frame(product ="Sum", Item_code = "NA", dietgroup="NA" , t(sapply(Diets_df[,4:11], sum, na.rm = TRUE)), 
                                       kcal_data = NA, prot_data = NA, fat_data = NA))



# Main file without "excluded":
Diets_df <- Diets_df[!(Diets_df$DGEgroup == "excluded"), ]
write.csv(Diets_df, file = "output/Diets_SQ_min_max_waste.csv")

