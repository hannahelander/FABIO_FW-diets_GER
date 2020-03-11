# -----------------------------------------
# prepare data structure for food waste data
# ---------------------------------------
# This script is adapted from Alice Piccolo <alice.piccolo94@gmail.com> and Elena Fourcroy <elenafourcroy@gmail.com>
# The food waste data comes mainly from FAO and is given per region (continent). 
# For Germany, national data is used, based on two sources (Schmidt et al 2019 and WWF).

# load food waste data
waste_shares <- read.csv2("input/waste_shares_ger.csv")  
waste_shares[is.na(waste_shares)] <- 0           # set NA to 0
#waste_shares <- read.csv2("input/foodwaste_shares_minimum.csv")  
#waste_shares[is.na(waste_shares)] <- 0           # set NA to 0
#waste_shares <- read.csv2("input/waste_shares_maximum.csv")  
#waste_shares[is.na(waste_shares)] <- 0           # set NA to 0

################## - WASTE DATA - #########################
# create data frame for waste (for each country, for all products in each stage)
NrOfProducts <- 130
waste <- data.frame(ISO = rep(countries$ISO, each=NrOfProducts),                    # 
                    country = rep(countries$Country, each=NrOfProducts),
                    group = rep(countries$Group, each=NrOfProducts))
waste$country <- as.character(waste$country)
waste$group   <- as.character(waste$group)
waste$group[waste$country == "Germany"] <- "Germany"

waste$com_code  <- waste_shares$Com.Code[1:NrOfProducts]          # adds Com.Code
waste$com_name  <- waste_shares$FAO.Name[1:NrOfProducts]          # adds Product names
waste$com_group <- waste_shares$Com.Group[1:NrOfProducts]         # adds Name of Commodity group

waste$harvest_production <- waste_shares$harvest_production[match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$storage_transport  <- waste_shares$storage_transport [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$processing         <- waste_shares$processing        [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$distribution       <- waste_shares$distribution      [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$final_consumption  <- waste_shares$final_consumption [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]


write.csv2(waste, file = "data/waste_data_old.csv")     # write to file in data-folder! 
#write.csv2(waste, file = "data/waste_data_maximum.csv")    
#write.csv2(waste, file = "data/waste_data_minimum.csv")    

############# Halfing Food waste #####################
waste <- read.csv2("data/waste_data_maximum.csv")
sum(waste$final_consumption) # 173599.8
waste <- read.csv2("data/waste_data.csv")
sum(waste$final_consumption) #115733.2
waste <- read.csv2("data/waste_data_mininum.csv")
sum(waste$final_consumption) # 57866.6

waste <- read.csv2("data/waste_data_minimum.csv")
waste_halfFW <- waste
waste_halfFW$distribution <-  waste$distribution / 2
waste_halfFW$final_consumption <-  waste$final_consumption / 2

sum(waste$distribution)
sum(waste_halfingFW$distribution)

#write.csv2(waste_halfFW, file = "data/waste_data_halfingFW.csv")    
#write.csv2(waste_halfFW, file = "data/waste_data_halfingFW_MAX.csv")    
write.csv2(waste_halfFW, file = "data/waste_data_halfingFW_MIN.csv")  

