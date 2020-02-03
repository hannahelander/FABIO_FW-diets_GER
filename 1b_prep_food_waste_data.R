# -----------------------------------------
# prepare data structure for food waste data
# ---------------------------------------
# This script is adapted from Alice Piccolo <alice.piccolo94@gmail.com> and Elena Fourcroy <elenafourcroy@gmail.com>
# The food waste data comes mainly from FAO and is given per region (continent). 
# For Germany, national data is used, based on two sources (Schmidt et al 2019 and WWF).

# load food waste data
waste_shares <- read.csv2("input/waste_shares_ger.csv")  # change source!

# uncertatinty analysis
#waste_shares <- read.csv2("input/foodwaste_shares_minimum.csv") # waste data minimum
waste_shares <- read.csv2("input/foodwaste_shares_maximum.csv") # waste data maximum

waste_shares[is.na(waste_shares)] <- 0                            # set NA to 0




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


#write.csv2(waste, file = "data/waste_data.csv")     # write to file in data-folder! 
#write.csv2(waste, file = "data/waste_data_maximum.csv")    
#write.csv2(waste, file = "data/waste_data_minimum.csv")    

############# Half Food waste #####################

waste_halfingFW <- waste
waste_halfingFW$distribution[waste_halfingFW$country == "Germany"] <-  waste$distribution[waste$country == "Germany"] / 2
waste_halfingFW$final_consumption[waste_halfingFW$country == "Germany"] <-  waste$final_consumption[waste$country == "Germany"] / 2

#write.csv2(waste_halfingFW, file = "data/waste_data_maximum_halfingFW.csv")     # write to file in data-folder! 
write.csv2(waste_halfingFW, file = "data/waste_data_halfingFW_MAX.csv")     # write to file in data-folder! 


