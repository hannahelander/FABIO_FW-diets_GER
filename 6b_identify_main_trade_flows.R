### Identify main resource trade flows in terms of both product group and
### country of footprint occurence

########----------FUNCTION: FIND MAX VALUES --------#####
max.Flows <- function(FP_products){
  outtest = FP_products[,2:ncol(FP_products)]
  rownames(outtest) = FP_products$Group.1
  
  outtestv = as.vector(as.matrix(outtest)) # column-wise, starting top left.
  modulo <- nrow(outtest) # use for modulo and ceiling
  
  outtestind = order(outtestv, decreasing = T)
  # outtestv[outtestind[1:10]] # list the highest values
  
  outdf = data.frame(country = rep(NA,20), product=rep(NA,20), value=rep(NA,20))
  for (x in 1:20){
    ind = outtestind[x]
    coln = ceiling(ind/modulo)
    rown = ind %% modulo
    outdf$product[x] <- colnames(outtest)[coln]
    outdf$country[x] <- rownames(outtest)[rown]
    outdf$value[x] <- outtestv[outtestind[x]]
  } 
  return(outdf)
}

path <- "C:/Hanna/FABIO_FW-diets_GER/output/spatial_FP/"
FP <- c("biomass", "GHG", "land")

i <- 1

FP_SQ <- read.csv(paste0(path,"footprints_products_", FP[i], "_SQ.csv"), sep = ";", dec = ".")

outdf_SQ <- max.Flows(FP_SQ)
colnames(outdf_SQ) <- c("SQ_products", "SQ_country", "SQ_value") 

FP_Scen1 <- read.csv(paste0(path,"footprints_products_land_Scen1.csv"), sep = ";", dec = ".")
outdf_Scen1 <- max.Flows(FP_Scen1)
colnames(outdf_Scen1) <- c("Scen1_products", "Scen1_country", "Scen1_value")
FP_tradeflows <- cbind(outdf_SQ, outdf_Scen1)

write.table(FP_tradeflows, file = paste0("output/spatial_FP/TradeFlows_", FP[i], ".csv"), dec = ".", sep = ";",row.names = FALSE)  

