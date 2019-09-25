
# generate Index-data-frame to modify Y for different scenarios
NrOfCountries <- 192
NrOfProducts <- 130
index <- data.frame(country = rep(countries$ISO, each=NrOfProducts),
                    continent = rep(countries$Continent, each=NrOfProducts),
                    product = rep(products$Item, NrOfCountries),
                    item_code = rep(products$Item.Code, NrOfCountries),
                    com_group = rep(products$Com.Group, NrOfCountries),
                    product_group = rep(products$Group, NrOfCountries))



### ADDING NEW CATEGORIES
# DGE Recommendations (https://www.dge.de/fileadmin/public/doc/fm/dgeinfo/DGEinfo-06-2019-Vollwertige-Ernaehrung_aheadofprint.pdf)
# 6 groups (excluding drinks), with subcategories

# redefine some commodity groups in diet-groups:
fruits <- c(2611, 2612, 2613, 2614, 2615, 2616, 2617, 2618,  2619, 2620, 2625)                    # need to be adapted
vegetables <- c(2546, 2547,  2549, 2551, 2601, 2602, 2605,  677, 2640,  2641,  2642, 2645)        # need to be adapted

### DIET-GROUPS ### Expand index matrix and reallocate products to diet-group: 
index$diet_group <- index$com_group
levels(index$diet_group) <- c(levels(index$diet_group), "Cereals and potatoes", "Vegetables, pulses, spices", "Fruits", "Oil crops and nuts")

# Diet-group 1: Cereals and potatoes
index$diet_group[index$diet_group == "Cereals"] <- "Cereals and potatoes"
index$diet_group[index$com_group == "Roots and tubers"] <- "Cereals and potatoes"

# Diet-group 2: Vegetables, roots and legumes
index$diet_group[index$diet_group == "Vegetables, fruit, nuts, pulses, spices"] <- "Vegetables, pulses, spices"
index$diet_group[index$item_code %in% vegetables] <- "Vegetables, pulses, spices"


# Diet group 3: Fruits and nuts
index$diet_group[index$item_code %in% fruits] <- "Fruits"
index$diet_group[index$com_group == "Oil crops"] <- "Oil crops and nuts"
index$diet_group[index$product == 	"Nuts and products"] <- "Oil crops and nuts"

# Diet group: Meat and animal-based products
index$diet_group[index$com_group == "Animal fats"] <- "Meat"

### ADDING DGE CATEGORIES ### Create an additional category based on DGE's 6 groups:
index$DGE_group <- index$diet_group
levels(index$DGE_group) <- c(levels(index$DGE_group), "Meat, sausages, fish, eggs", "Alcohol, sugar and honey", "vegetables incl. legumes", "excluded")
index$DGE_group[index$DGE_group == "Eggs"] <- "Meat, sausages, fish, eggs"
index$DGE_group[index$DGE_group == "Fish"] <- "Meat, sausages, fish, eggs"
index$DGE_group[index$DGE_group == "Meat"] <- "Meat, sausages, fish, eggs"
index$DGE_group[index$DGE_group == "Sugar, sweeteners"] <- "Alcohol, sugar and honey"
index$DGE_group[index$DGE_group == "Oil crops and nuts"] <- "vegetables incl. legumes"
index$DGE_group[index$DGE_group == "Vegetables, pulses, spices"] <- "vegetables incl. legumes"
index$DGE_group[index$DGE_group == "Alcohol"] <- "Alcohol, sugar and honey"
index$DGE_group[index$DGE_group == "Honey"] <- "Alcohol, sugar and honey"
index$DGE_group[!index$DGE_group %in% c("Meat, sausages, fish, eggs", "vegetables incl. legumes", "Cereals and potatoes", 
                                        "Vegetable oils", "Milk", "Fruits", "Alcohol, sugar and honey")]  <- "excluded"

# exclude pet food from DGE_groups
index$DGE_group[index$product =="Pet food"] <- "excluded"

write.csv2(index, file = "data/index_data_frame.csv")     # write to file in data-folder! 

######################################


#unique(index$diet_group)
#unique(products$Com.Group)
#length(unique(index$diet_group))
