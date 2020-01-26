
# Food amount per capita comparison Europe
# 
Y_capita <- data.frame(country = unique(index$country[which(index$continent %in% c("EUR", "EU"))]),
                       pop = c("ALB" = 2895092,
                               "AUT" = 8479823,
                               "BLX" = NA,
                               "BGR"= 7265115,
                               "CYP" =1143866,
                               "CSK" = NA,
                               "DNK" = 5614932,
                               "BLR" = 9465997,
                               "EST" = 1317997,
                               "FIN" = 5438972,
                               "FRA" = 65998687,
                               "DEU" = 80645605,
                               "BIH" =3542605,
                               "GRC" =10965211,
                               "HUN" =9893082, 
                               "HRV" =4255689, 
                               "ISL"= 323764,
                               "IRL" = 4623816,
                               "ITA"= 60233948,
                               "LVA"= 2012647,
                               "LTU"= 2957689,
                               "MLT"= 425967,
                               "MDA"= 3558566,
                               "NLD"= 16804432,
                               "MKD"= 2076067,
                               "NOR"= 5079623,
                               "CZE"= 10514272,
                               "POL"= 38040196,
                               "PRT"= 10457295,
                               "ROU"= 19983693,
                               "SCG"= NA,
                               "SVN"= 2059953,
                               "SVK"= 5413393,
                               "ESP"= 46620045,
                               "SWE"= 9600379,
                               "CHE"= 8089346,
                               "TUR"= 75928564,
                               "GBR" = 64128226,
                               "UKR"= 45489600,
                               "YUG"= NA,
                               "BEL"= 11159407,
                               "LUX"= 543360,
                               "SRB"= 7164132,
                               "MNE"= 621207))

#Y_capita$pop <- match(Y_capita$country, pop)

for (i in 1:nrow(Y_capita)){
Y_capita$Y[i] <-  sum(as.numeric(Y[,paste0(Y_capita$country[i],"_Food")]))
}


Y_capita$Food <- Y_capita$Y/Y_capita$pop


write.csv2(Y_capita, "output/foodEUR.csv")
