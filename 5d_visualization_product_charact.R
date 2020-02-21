library(plotly)
library(tidyr)

prod_cat <- read.csv2("input/product_characteristics.csv")
prod_cat <- prod_cat[-9,]


#colors = list(cereals = 'rgba(212, 162, 106, 0.6)', ) 

f <- list(
  family = "Arial",
  size = 12,
  color = "black")

#### Biomass

x <- list(
  title = "Food waste shares",
  titlefont = f)
y <- list(
  title = "Biomass footprint (kg / 1000 kcal)",
  titlefont = f)

p <- plot_ly(data = prod_cat, x = ~FW, y = ~Biomass, color= ~X, colors = "Paired", size = 3) %>%
  layout(xaxis = x, yaxis = y)

p

#### Cropland

x <- list(
  title = "Food waste shares",
  titlefont = f)
y <- list(
  title = "Cropland footprint (hectars / 1000 kcal)",
  titlefont = f)

p <- plot_ly(data = prod_cat, x = ~FW, y = ~Land, color= ~X, colors = "Paired", size = 3) %>%
  layout(xaxis = x, yaxis = y)

p


#### Blue water

x <- list(
  title = "Food waste shares",
  titlefont = f)
y <- list(
  title = "Blue water footprint (m3 / 1000 kcal)",
  titlefont = f)

p <- plot_ly(data = prod_cat, x = ~FW, y = ~Water, color= ~X, colors = "Paired", size = 3) %>%
  layout(xaxis = x, yaxis = y)



p

