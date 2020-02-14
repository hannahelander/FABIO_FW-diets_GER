##########################################################
###   -----  Sensitivity analysis for SQ   -------- #####
##########################################################

### Y vectors on eaten food for minimum-/maximum waste scenarios SQ
Y_SQ_eaten_minW <- read.csv2(file = "data/Y_SQ_eaten_minW.csv")
Y_SQ_eaten_minW <- Y_SQ_eaten_minW[,2]
Y_SQ_eaten_minW[index$DGE_group == "excluded"] <- 0  # need to adjust some inconsistency in data
Y_SQ_diet_minW <- Y_SQ_eaten_minW * 1000000 / population / 365 #Converting unit (from "national tons/y" to "eaten food g/p/day")

#sum(Y_SQ_eaten_minW) # = 69231969
#sum(Y_SQ_diet_minW) # 2351.977