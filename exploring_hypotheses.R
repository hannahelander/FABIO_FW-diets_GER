# Calculating percentage of food waste in each stage - means for plant-based and animal-based
supply_tonnes <- read.csv2("output/supply_chain_tonnes.csv")

plant_hp <- supply_tonnes$harvest_production[2] /supply_tonnes$harvest_production[1] 
plant_hp

animal_hp <- supply_tonnes$harvest_production[4] /supply_tonnes$harvest_production[3]
animal_hp

plant_st <- supply_tonnes$storage_transport[2] /supply_tonnes$storage_transport[1]
plant_st

animal_st <- supply_tonnes$storage_transport[4] /supply_tonnes$storage_transport[3]
animal_st


plant_pr <- supply_tonnes$processing[2] /supply_tonnes$processing[1]
plant_pr

animal_pr <- supply_tonnes$processing[4] /supply_tonnes$processing[3]
animal_pr

plant_distr <- supply_tonnes$distribution[2] /supply_tonnes$distribution[1]
plant_distr

animal_distr <- supply_tonnes$distribution[4] /supply_tonnes$distribution[3]
animal_distr


plant_cons <- supply_tonnes$Consumption[2] /supply_tonnes$Consumption[1]
plant_cons

animal_cons <- supply_tonnes$Consumption[4] /supply_tonnes$Consumption[3]
animal_cons

