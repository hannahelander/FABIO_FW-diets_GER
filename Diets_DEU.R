
#############################################
# Do different diets by using the indices and create alternative Y-matrices
# 
# Prerequisits: index (1_generate_index)




Y_currentDiet <- Y[,"DEU_Food"] # -> Build it up similarly to the waste data.frame in the waste script! 
Y_currentDiet[index$country=="DEU" & index$product=="Oats"] # I need to create a vector to get all these Numbers and find the composition of products in different product groups, then I will use the same composition for my diets 


# Example to change for certain product-groups and products:

Y_vegan <- Y[,"DEU_Food"]
Y_vegan[index$country=="DEU" & index$product_group=="Livestock products"] <- 0
Y_vegan[index$country=="DEU" & index$product=="Beer"] <- 0
Y_vegan[index$country=="DEU"] <- Y_vegan[index$country=="DEU"] * diet_scenario
FP_vegan <- t(t(MP) * Y_vegan)
sum(FP_vegan) / population