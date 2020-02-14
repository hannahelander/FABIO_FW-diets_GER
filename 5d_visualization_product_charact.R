data1 = iris

library(plotly)

prod_cat <- read.csv2("input/product_characteristics.csv")

p <- plot_ly(data = prod_cat, x = ~FW, y = ~Biomass, color= ~X, colors = "Set1", size = 3)
p <- plot_ly(data = prod_cat, x = ~FW, y = ~Land, color= ~X, colors = "Set1", size = 3)
p <- plot_ly(data = prod_cat, x = ~FW, y = ~Water, color= ~X, colors = "Set3", size = 3)

p

