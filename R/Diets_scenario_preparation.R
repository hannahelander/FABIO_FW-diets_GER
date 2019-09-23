






# load food supply data for Germany (kg/capita/year)
supply_DE <- read.csv("C:/Hanna/CIRCULUS/PHASE 2 Application on the food system/MethodologyAndCalculations/Data FAO/FAOSTAT_data_6-20-2019(2).csv", sep = ";")
# load Fabio Classification (includes FAO.name and Com.Group)
Fabio_classification <- read.csv2("C:/Hanna/CIRCULUS/PHASE 2 Application on the food system/MethodologyAndCalculations/Data FAO/fabio_classifications_diets_recommendations.CSV")


# Create list with commodity groups for FABIO
Supply_Groups <- c(as.character(Fabio_classification$Com.Group))
Supply_Groups <- unique(Supply_Groups)


# create list with own commodity groups
diet_groups <- c("Cereals", "Roots and tubers", "Fruits","Vegetables", "Vegetable oils", "Oil crops", "Sugar, sweeteners", "Animal fats", "Milk", "Eggs", "Meat","Fish",
"Alcohol", "Sugar, Sweeteners", "Coffee, tea, cocoa", "Honey", "Tabacco") 


#Allocate products from supply_DE to diet_groups
#length(Fabio_classification$Com.Code)
#length(supply_DE$Item.Code)
#namevector <- c("diet_group", "supply")
#Fabio_classification[ , namevector] <- NA

#Product_allocation <- cbind(Fabio_classification$Com.Group,c() )



# create matrix
supply_Diet.Groups <- matrix(NA,nrow = length(supply_DE$Domain.Code), ncol= 2)
supply_Diet.Groups <- cbind(supply_DE$Item.Code, as.character(supply_DE$Item), as.numeric(supply_DE$Value), supply_Diet.Groups)
colnames(supply_Diet.Groups) <- c("Product.Code", "Product", "Supply_DE", "Com.group", "Diet.Group" )
#Supply_Com.Groups$Com.Group <-Fabio_classification$Com.Group
#remove products with zero
supply_Diet.Groups <- supply_Diet.Groups[-c(11, 70, 83), ] # 11 = sugar beets (value 0), 70 = body oil, 83 = infant food


# extract Product.Codes that not exist in supply_DE or Fabio_Classification
notIn_Supply_DE <-c()
notIn_Fabio_Clas <-c()
for(j in 1:nrow(Fabio_classification)){
    if (!any(grepl(Fabio_classification[j,"FAO.Code"], supply_Diet.Groups[,"Product.Code"]))){
    value <- Fabio_classification[j,"FAO.Code"]
    notIn_Supply_DE <- append(notIn_Supply_DE, value, after = length(notIn_Supply_DE))   
    }
}
cat("the length of notIn_Supply_DE is", length(notIn_Supply_DE))

for(j in 1:nrow(supply_Diet.Groups)){
  if (!any(grepl(supply_Diet.Groups[j,"Product.Code"], Fabio_classification[j,"FAO.Code"]))){
    value <- supply_Diet.Groups[j,"Product.Code"]
    notIn_Fabio_Clas <- append(notIn_Fabio_Clas, value, after = length(notIn_Fabio_Clas))   
  }
}
cat("the length of notIn_Fabio_Clas is", length(notIn_Fabio_Clas))



# Fill supply_Diet.Group with values for "Com.group" *********OBS Loop doesn't work because the if-loop doesn't work**********
for(i in 1:nrow(supply_Diet.Groups)){
  if (!supply_Diet.Groups[i,"Product.Code"] %in% notIn_Fabio_Clas) {  
     supply_Diet.Groups[i,"Com.group"] <- as.character(Fabio_classification[which(grepl(supply_Diet.Groups[i,"Product.Code"], Fabio_classification[,"FAO.Code"])),"Com.Group"])
  }
  
}

##### Write to File #####
write.csv(supply_Diet.Groups, file ="C:/Users/hhelander/Documents/Germany_foodWaste_diets/output/supply_Diet.Groups.csv")


# List products to be categorized (product numbers)
Fruits <- c(2611, 2612, 2613, 2614, 2615, 2616, 2617, 2618,  2619, 2620, 2625)
Vegetables <- c(2546, 2547,  2549, 2551, 2601, 2602, 2605,  677, 2640,  2641,  2642, 2645)
Fish <- c(2761, 2762, 2763, 2764, 2765, 2766, 2767, 2769, 2775, 2782)


# Assign Diet.Groups to all products in supply_Diet.Groups
for (i in 1:nrow(supply_Diet.Groups)){
  if (supply_Diet.Groups[i, "Com.group"] %in% diet_groups){
    supply_Diet.Groups[i, "Diet.Group"] <- as.character(supply_Diet.Groups[i, "Com.group"])
  } else if (supply_Diet.Groups[i, "Product.Code"] %in% Fruits){
    supply_Diet.Groups[i, "Diet.Group"] <- "Fruits"
  } else if (supply_Diet.Groups[i, "Product.Code"] %in% Vegetables){
    supply_Diet.Groups[i, "Diet.Group"] <- "Vegetables"
  } else if (supply_Diet.Groups[i, "Product.Code"] %in% Fish){
    supply_Diet.Groups[i, "Diet.Group"] <- "Fish"
  } else if (supply_Diet.Groups[i, "Product.Code"] == 2743){
    supply_Diet.Groups[i, "Diet.Group"] <- "Milk"
  } else if (supply_Diet.Groups[i, "Product.Code"] == 2542){
    supply_Diet.Groups[i, "Diet.Group"] <- "Sugar, sweeteners"
  } else {
    cat("no group found for i =", i, "and ")
  }
}

# Aggregate data per Diet.Group
sum_product_group <- as.data.frame(NA, nrow = length(diet_groups), ncol= 2)

#sum_product_group$Diet.Group <- as.character(sum_product_group$Diet.Group)
#Diet.Group$Supply_DE <- as.numeric(supply_Diet.Groups$Supply_DE)

sum_product_group <- aggregate(as.numeric(Supply_DE) ~ Diet.Group, supply_Diet.Groups, sum) # aggregate data per Product-group alphabetically
names(sum_product_group)[2] <- 'sum_product_group'

#num_products <- aggregate(Supply_DE ~ Diet.Group, supply_Diet.Groups,length)
#names(num_products)[2] <- 'num_products'
#Diet_Groups_Data <- as.matrix(merge(num_products,sum_product_group))

barplot(sum_product_group$as.numeric(Supply_DE), names.arg=sum_product_group$Diet.Group)

# Add DGE Recommendations
MAX_values <- c(7.30, 7.56, 167.49, 0, 9.39, 11.47,91.25, 0, 31.29, 113.15, 36.91, 0, 26.67, 146.00)
# sum_product_group$maxvalues = MAX_values # OR:
Comparison <- cbind(sum_product_group, MAX_values) 


# Bar Plot SUPPLY
barplot(as.numeric(Diet_Groups_Data[,"sum_product_group"]), main="Supply Data Germany", xlab="Product group", ylab = "kg/capita/year", names.arg=as.character(Diet_Groups_Data[, "Diet.Group"]),  las=2)
#barplot(t(as.matrix(sum_product_group)), main="Supply Data Germany", xlab="Product group", ylab = "kg/capita/year", names.arg=as.character(Diet_Groups_Data[, "Diet.Group"]),  las=2)

#Barplot Supply and rec.
barplot(t(as.matrix(Comparison)), beside=TRUE)
col=c("darkblue","red")

  
  
mydf <- data.frame( X1=c(A=2, B=4, C=1), X2=c(3,2,NA), X3=c(4,1,NA) )
  
  
  
  
  
  
  


  
###########################################################
# ÜBUNG
##########################################################
  
  # Original data with repeats removed. These do the same:
#  unique(df)
#>   label value
#> 1     A     4
#> 2     B     3
#> 3     C     6
#> 5     B     1
#> 6     A     2
#df[!duplicated(df),]
#>   label value
#> 1     A     4
#> 2     B     3
#> 3     C     6
#> 5     B     1
#> 6     A     2