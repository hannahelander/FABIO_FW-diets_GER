library(plotly)
library(tidyr)


prod_cat <- read.csv2("input/product_characteristics.csv")
prod_cat <- prod_cat[-9,]


color_values = c(rgb(212, 162, 106, maxColorValue = 255), rgb(128, 77, 21,   maxColorValue = 255), 
                      rgb(0, 61, 25,  maxColorValue = 255),rgb(41, 122, 74,   maxColorValue = 255),
                      rgb(2, 38, 54,  maxColorValue = 255), rgb(39, 87, 107,   maxColorValue = 255),
                      rgb(212, 127, 106,  maxColorValue = 255), rgb(170, 78, 57,  maxColorValue = 255),
                      rgb(85, 16, 0,  maxColorValue = 255),rgb(107, 117, 159,  maxColorValue = 255))



prod_cat$X <- factor(prod_cat$X, levels = c('Cereals' , 'Potatoes & roots','Vegetables', 
                                            'Fruits','Pulses, beans & nuts', 'Vegetable oils',
                                            'Milk & products','Eggs', 'Meat','Sugar & Alcohol')
                                            )

f <- list(
  family = "Arial",
  size = 18,
  color = "black")


#### Biomass

x <- list(
  title = "Food waste shares",
  titlefont = f)
y <- list(
  title = "Biomass footprint (kg / 1000 kcal)",
  titlefont = f)


p <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW, y = ~Biomass, color= ~X,
            colors = color_values,  size = I(150))  %>%
 layout(xaxis = x, yaxis = y)

p



#### Cropland

x <- list(
  title = "Food waste shares",
  titlefont = f)
y <- list(
  title = "Cropland footprint (hectars / 1000 kcal)",
  titlefont = f)


p <- plot_ly(data = prod_cat,  type='scatter', mode = 'markers', x = ~FW, y = ~Land, color= ~X, 
             colors =  color_values, size = I(150)) %>% 
  
  layout(xaxis = x, yaxis = y)

p


#### Blue water

x <- list(
  title = "Food waste shares",
  titlefont = f)
y <- list(
  title = "Blue water footprint (m3 / 1000 kcal)",
  titlefont = f)


p <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW, y = ~Water, color= ~X, 
             colors = color_values, size = I(150)) %>%
  layout(xaxis = x, yaxis = y)

p



#Plot



#### ggplot(prod_cat, aes(x = FW, y = Biomass)) + 
#   geom_point(aes(colour = X, size=Land)) + ylab("Impacts") + xlab("Food waste")+
#   scale_colour_manual(values = c(rgb(212, 162, 106,  maxColorValue = 255),rgb(128, 77, 21,  maxColorValue = 255), 
#                                  rgb(0, 61, 25,  maxColorValue = 255),rgb(41, 122, 74,  maxColorValue = 255),
#                                  rgb(41, 122, 74,  maxColorValue = 255), rgb(39, 87, 107,  maxColorValue = 255),
#                                  rgb(212, 127, 106,  maxColorValue = 255), rgb(170, 78, 57,  maxColorValue = 255),
#                                  rgb(85, 16, 0,  maxColorValue = 255),rgb(107, 117, 159,  maxColorValue = 255)))
# colors = data.frame("Cereals" = 'rgba(212, 162, 106, 0.6)', 
#                     'Potatoes & roots'= 'rgba(128, 77, 21, 0.6)',
#                     'Vegetables'      = 'rgba(0, 61, 25, 0.6)',
#                     'Fruits' = 'rgba(41, 122, 74, 0.6)',
#                     'Pulses, beans & nuts' = 'rgba(2, 38, 54, 0.6)',
#                     'Vegetable oils' = 'rgba(39, 87, 107, 0.6)',
#                     'Milk & products' = 'rgba(212, 127, 106, 0.6)',
#                     'Eggs' = 'rgba(170, 78, 57, 0.6)',
#                     'Fish' = 'rgba(128, 42, 21, 0.6)',
#                     'Meat' = 'rgba(85, 16, 0, 0.6)',
#                     "Alcohol & Sugar" = 'rgba(107, 117, 159, 0.6)' )

