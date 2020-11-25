###############-------------------------------Water Scarcity Footprints ----------------------------------------------------#############
### Compiling envronmental extention data and AWARE factors to calculate Water Scarcity Footprints (WSF) 
# according to Schomberg and Bringezu (2020). Used for the analysis of spatial footprints. 
# NOT USED in Helander et al. 2020


path <- "C:/Hanna/FABIO_FW-diets_GER/input/"
E <- readRDS(paste0(path,"2013_E.rds"))
E_WF <- E     # rename E (modified data is not the original FABIO extention)

#### 1. Calculate WF_min and WF_max
# OUT_inc = procuct mass*RWC, RWC (relative water content) = moisture content. 
# Thus, for the E_matrix, only moisture content is needed.
RWC <- products[, c(2,6)]

#This makes no sense! I cannot add a "rate" to the E-matrix. In that case I kind of need to multiply it with the total production?
E_WF <- merge(E_WF, RWC, by.x="Item.Code", by.y="item_code", # add moisture to E_WF
              all.x=T, all.y=T, sort = FALSE)

recharge_rate <- 0        # if reharge_rate = 0, max and min is the same
E_WF$bw_ep <- E_WF$Blue_water * (1 - recharge_rate)  #bw_ep
E_WF$ep_min <- E_WF$Green_water - E_WF$rwc + bw_ep     # ep_min ### RECONSIDER
E_WF$ep_max <- E_WF$Green_water - E_WF$rwc + E_WF$Blue_water   #ep_max ### RECONSIDER

E_WF$WF_min <- E_WF$rwc + E_WF$ep_min
E_WF$WF_max <- E_WF$rwc + E_WF$ep_max


#### 2. Add AWARE coefficients to get a water scarcity footprint extention
CF_AWARE <- read.csv2(paste0(path,"AWARE_country_regions_Improved.csv"))
CF_AWARE <- CF_AWARE[,c(1,4)] # select CF for "unspecified"

## Check name-mismatches  
excFabio <- countries[!countries$country %in% CF_AWARE$X,] 
levels(countries$country) <- c(levels(countries$country),"Côte d'Ivoire")
countries$country[78] <- "Côte d'Ivoire"
nrow(excFabio)
excFabio
excAWARE <- CF_AWARE[!CF_AWARE$X %in% E_WF$Country,] # Get country-names that exclusively exist in AWARE data
#nrow(excAWARE) #112 countries

# rename countries according to FABIO names
levels(CF_AWARE$X) <- c(levels(CF_AWARE$X),"Syrian Arab Republic", "United Republic of Tanzania", 
                        "Iran (Islamic Republic of)", "China, mainland", "The former Yugoslav Republic of Macedonia", 
                        "Cabo Verde", "Democratic Republic of the Congo", "Côte d'Ivoire", "Timor-Leste", "Brunei Darussalam", 
                        "Guinea-Bissau", "Lao People's Democratic Republic", "Republic of Moldova", "Russian Federation",
                        "Democratic People's Republic of Korea", "Republic of Korea", "China, Taiwan Province of", "Bahamas",
                        "Venezuela (Bolivarian Republic of)", "Viet Nam", "Bolivia (Plurinational State of)")
CF_AWARE$X[183] <- "Syrian Arab Republic"
CF_AWARE$X[186] <- "United Republic of Tanzania"
CF_AWARE$X[89] <-"Iran (Islamic Republic of)"
CF_AWARE$X[40] <-"China, mainland"
CF_AWARE$X[112] <-"The former Yugoslav Republic of Macedonia"
CF_AWARE$X[29] <-"Brunei Darussalam"
CF_AWARE$X[24] <-"Bolivia (Plurinational State of)"
CF_AWARE$X[36] <- "Cabo Verde"
CF_AWARE$X[44] <-"Democratic Republic of the Congo"
CF_AWARE$X[46] <-"Côte d'Ivoire"
CF_AWARE$X[55] <-"Timor-Leste"
CF_AWARE$X[81] <-"Guinea-Bissau"
CF_AWARE$X[103] <-"Lao People's Democratic Republic"
CF_AWARE$X[123] <- "Republic of Moldova"
CF_AWARE$X[140] <- "Democratic People's Republic of Korea"
CF_AWARE$X[155] <- "Russian Federation"
CF_AWARE$X[170] <- "Republic of Korea"
CF_AWARE$X[184] <- "China, Taiwan Province of"
CF_AWARE$X[188] <- "Bahamas"
CF_AWARE$X[206] <- "Venezuela (Bolivarian Republic of)"
CF_AWARE$X[207] <- "Viet Nam"

## Merge data (i.e. add aware coefficients to E_WF)
E_WF <-   merge(E_WF,CF_AWARE, by.x = "Country", by.y="X", all.x=T, all.y=F, sort = FALSE)

### Multiply with AWARE coefficients to get a water scarcity footprint extention
E_WF$Agg_CF_irri <- as.numeric(E_WF$Agg_CF_unspecified)
E_WF$WSF_min <- E_WF$WF_min * E_WF$Agg_CF_unspecified
E_WF$WSF_max <- E_WF$WF_max * E_WF$Agg_CF_unspecified

write.table(E_WF, file = paste0(path, "E_wsf.csv"), dec = ".", sep = ";", row.names = FALSE) 


