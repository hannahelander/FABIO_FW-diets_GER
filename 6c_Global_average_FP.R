



GlobalPop <- 7169640140 # From worldbank, tot population 2013 https://data.worldbank.org/indicator/SP.POP.TOTL?end=2013&locations=1W&start=1960

Y_global <- Y[ ,SUM..."DEU_Food"]                           # Status Quo


FP_global = e*L*Y

dimnames(Y)[2]
# change line below and use it in a loop through the dimnames of Y. generate output with which or if.
substr("test_Food",nchar("test_Food")-4,nchar("test_Food")) == "_Food"
# alternative: function "counains" from package dplyr


# read data
L <- readRDS(paste0(path,"2013_L_mass.rds"))
X <- readRDS(paste0(path,"2013_X.rds"))
E <- readRDS(paste0(path,"2013_E.rds"))
E_wsf <- read.csv(paste0(path,"E_wsf.csv"), sep = ";", dec = ".")
load(paste0(path,"E_ghg_2013.RData"))

# List indicators
ind_list <- list("land"= E$Landuse, 
                 "biomass" = E$Biomass,
                  #"water_cons" = as.numeric(E_wsf$cons_water),
                 "WSF1" = as.numeric(E_wsf$WSF),
                 "GHG" = colSums(E_ghg[ , 2:ncol(E_ghg)]))
rm(E)
rm(E_ghg)
rm(E_wsf)
gc()

Global_aver <- data.frame("land"= NA, "biomass"= NA, "WSF1"= NA, "GHG"=NA)

for (j in names(ind_list)){
  e <- ind_list[[j]] / X
  MP <- e * L
  FP_tot <- sum(t(t(MP) * Y_global))   # Total footprint  
  FP_cap <- FP_tot/GlobalPop
  
  }
 
