#install.packages("sp")
#install.packages("rgdal")
#install.packages("data.table")
library(sp)
library(rgdal)
library(data.table) 
path <- "C:/Hanna/FABIO_FW-diets_GER/input/"

########### MAIN SCRIPT ################

#loading geographic data defining countries as a SpatialpolygonsDataFrame # source: http://thematicmapping.org/downloads/world_borders.php
World_boarders <- readOGR(dsn=path.expand("C:/Hanna/GlobalFoodprints/inputs"), layer = "TM_WORLD_BORDERS-0.3")

######### Step 1: Control of countrienames and comparison with worldboarders ##########

# Get country-names that exclusively exist in FABIO countries
excFabio <- countries$iso3c[!countries$iso3c %in% World_boarders@data$ISO3]  #(ISO3 has length 246), excFabio= BLX CSK SCG SUN YUG SSD ROW
#List of countries excluded in FABIO
excWB <- World_boarders@data[!World_boarders@data$ISO3  %in% countries$iso3c,]
nrow(excWB) #61 countries
#View(World_boarders@data$NAME)


######### Step 3: Add footprint data to geographic data ##########

# Read footprints data
Footprints <- read.csv(file = "output/spatial_FP/footprints_wsf_aware_wsf2.csv", dec = ".", sep = ";")
class(Footprints) # data.frame
sapply(Footprints, class)
#Footprints$land <- as.numeric(Footprints$land)
#Footprints$biomass <- as.numeric(Footprints$biomass)
#Footprints$GHG <- as.numeric(Footprints$GHG)
#Footprints$water_scarcity <- as.numeric(Footprints$water_scarcity)


# merge on common variable, here called 'key'
World_boarders@data$ISO3 <- as.character(World_boarders@data$ISO3)
World_boarders@data$idx = 1:nrow(World_boarders@data) # add an index to keep the order
Footprints$source_iso <- as.character(Footprints$source_iso)

Footprints_WB <-   merge(World_boarders@data, Footprints, by.x = "ISO3", 
                         by.y="source_iso", all.x=T, all.y=F, sort = FALSE) # given all.y=F I need to check if there are some data excluded
# add merged data to "SpatialPolygonsDataFrame"
World_boarders@data = Footprints_WB[order(Footprints_WB$idx),] #idx maintains the order so that polygons and country-data match

# Export back to shapefile
writeOGR(World_boarders, dsn="C:/Hanna/CIRCULUS/Spatial_footprints/GIS-project", layer = "Footprints_wsf",
         driver="ESRI Shapefile", overwrite_layer= TRUE)    # 8 warnings of "Value 126218553.015699998 of field water of feature 64 not successfully written. Possibly due to too larger number with respect to field width" 
                                                            # need to deal with these numbers at one point.


#### OLD NOTES
#Footprints[,2:length(Footprints)] <-lapply(Footprints[,2:length(Footprints),], as.numeric)
#Footprints2 <- merge(Footprints, data, by = intersect("source_iso", "source_iso"), all.x=T, all.y=T, sort = FALSE)
