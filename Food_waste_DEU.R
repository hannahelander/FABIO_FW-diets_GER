
###############
## Functions to calculate food waste related to German consumption 


# aggregate function: This is just a function! 
#agg <- function(x){
#  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
#  return(x)}


# see Food waste script 
#country <- 'DEU'                                 # for Germany 
#NrOfCountries <- 192
#Y_waste <- Y[,paste0(country,"_Food")] * rep(waste_shares$final_consumption[waste_shares$Region==as.character(countries$Group[countries$ISO==country])],NrOfCountries) / 100
#sum(Y_waste) / sum(Y[,paste0(country,"_Food")])



#########################################################################################################
# create data frame for waste (for each country, for all products in each stage)
NrOfProducts <- 130
waste <- data.frame(ISO = rep(countries$ISO, each=NrOfProducts),                    # 
                    country = rep(countries$Country, each=NrOfProducts),
                    group = rep(countries$Group, each=NrOfProducts))
waste$country <- as.character(waste$country)
waste$group   <- as.character(waste$group)

waste$com_code  <- waste_shares$Com.Code[1:NrOfProducts]          # adds Com.Code
waste$com_name  <- waste_shares$FAO.Name[1:NrOfProducts]          # adds Product names
waste$com_group <- waste_shares$Com.Group[1:NrOfProducts]        # adds Name of Commodity group

waste$harvest_production <- waste_shares$harvest_production[match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$storage_transport  <- waste_shares$storage_transport [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$processing         <- waste_shares$processing        [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$distribution       <- waste_shares$distribution      [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$final_consumption  <- waste_shares$final_consumption [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]


write.csv2(waste, file = "data/waste_data_frame")     # write to file in data-folder! 



##### Prepare result data frame: 
#FP__chain <- data.frame(Scenario = rep("SQ", NrOfProducts),
#                        products = rep(index$product, each = NrOfProducts),
#                        product_group = rep(index$product_group, each = NrOfProducts))



## from Food Waste Script:
# waste$group[waste$country %in% "Germany"] <- waste$country[waste$country %in% "Germany"] ##  change group to "Germany!


######################################
# Scenarios for Food waste reduction 
######################################
# 1) Halfing food waste in Households and distribution




