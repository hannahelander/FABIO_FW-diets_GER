# -----------------------------------------
# prepare data structure for food waste data
# ---------------------------------------
# This script is adapted from Alice Piccolo <alice.piccolo94@gmail.com> and Elena Fourcroy <elenafourcroy@gmail.com>

# load food waste data
waste_shares <- read.csv2(paste0(path,"fabio_waste_shares.csv"))  # change source!
waste_shares[is.na(waste_shares)] <- 0                            # set NA to 0

# create data frame for waste (for each country, for all products in each stage)
NrOfProducts <- 130
waste <- data.frame(ISO = rep(countries$ISO, each=NrOfProducts),                    # 
                    country = rep(countries$Country, each=NrOfProducts),
                    group = rep(countries$Group, each=NrOfProducts))
waste$country <- as.character(waste$country)
waste$group   <- as.character(waste$group)

waste$com_code  <- waste_shares$Com.Code[1:NrOfProducts]          # adds Com.Code
waste$com_name  <- waste_shares$FAO.Name[1:NrOfProducts]          # adds Product names
waste$com_group <- waste_shares$Com.Group[1:NrOfProducts]         # adds Name of Commodity group

waste$harvest_production <- waste_shares$harvest_production[match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$storage_transport  <- waste_shares$storage_transport [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$processing         <- waste_shares$processing        [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$distribution       <- waste_shares$distribution      [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]
waste$final_consumption  <- waste_shares$final_consumption [match(paste0(waste$group,waste$com_code),paste0(waste_shares$Region,waste_shares$Com.Code))]


write.csv2(waste, file = "data/waste_data.csv")     # write to file in data-folder! 


