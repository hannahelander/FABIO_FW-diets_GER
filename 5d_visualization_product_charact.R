library(plotly)
library(tidyr)


####### READ DATA ###########

prod_cat <- read.csv("output/product_characterization.csv",na.strings = "NaN", colClasses = c("factor",rep("numeric",8)), sep=";" )
prod_cat <- prod_cat[-9,] # there is no land footprints for fish

###### prepare data #########

prod_cat$biomass_kg.kcal <- prod_cat$Biomass / prod_cat$kcal # unit ok!
prod_cat$land_ha.kcal <- prod_cat$Land / prod_cat$kcal # unit ok!
prod_cat$water_l.kcal <- prod_cat$Water / prod_cat$kcal
prod_cat$biomass_kg.10gP <- prod_cat$Biomass / prod_cat$prot
prod_cat$land_ha.10gP <- prod_cat$Land / prod_cat$prot # ha per 10g Prot
prod_cat$water_l.10gP <- prod_cat$Water /  prod_cat$prot

prod_cat$X <- factor(prod_cat$product_group, levels = c('Cereals' , 'Potatoes & roots','Vegetables', 
                                                    'Fruits','Pulses, beans & nuts', 'Vegetable oils',
                                                    'Milk & products','Eggs','Meat','Sugar & Alcohol')
                                                     )

#### Define colors and font for plots ####

color_values = c(rgb(212, 162, 106, maxColorValue = 255), rgb(128, 77, 21,   maxColorValue = 255), 
                rgb(0, 61, 25,  maxColorValue = 255),rgb(41, 122, 74,   maxColorValue = 255),
                rgb(2, 38, 54,  maxColorValue = 255), rgb(39, 87, 107,   maxColorValue = 255),
                rgb(212, 127, 106,  maxColorValue = 255), rgb(170, 78, 57,  maxColorValue = 255),
                rgb(85, 16, 0,  maxColorValue = 255),rgb(107, 117, 159,  maxColorValue = 255))

f <- list(
  family = "Arial",
  size = 16,
  color = "black")

######### Footprints verses FW #############
# Footprints are given per kg of EATEN Food (accounting for that food is thrown away)


#### Biomass
 
x <- list(
  title = "Food waste shares",
  titlefont = f,
  showline = F,
  gridcolor = "darkgrey")
y1 <- list(
  title = "Biomass footprint (kg kg<sup>-1</sup>)",
  titlefont = f,
  gridcolor = "darkgrey")


p1 <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW_shares, y = ~Biomass, color= ~X,
             colors = color_values,  size = I(150))  %>%
  layout(xaxis = x, yaxis = y1)
 # layout(xaxis = list(gridcolor = "black", x), yaxis = list(gridcolor = "black", y1))
#fig.update_xaxes(zeroline=True, zerolinewidth=2, zerolinecolor='LightPink')
#p1.update_yaxes(zeroline=True, zerolinewidth=2, zerolinecolor='LightPink')
# marker.line.color = I("black")
p1


#### Cropland

# per kg food
y2 <- list(
  title = "Cropland footprint (ha t<sup>-1</sup>)",
  titlefont = f,
  gridcolor = "darkgrey")

p2 <- plot_ly(data = prod_cat,  type='scatter', mode = 'markers', x = ~FW_shares, y = ~Land, color= ~X, 
             colors =  color_values, size = I(150)) %>% 
  layout(xaxis = x, yaxis = y2)
p2

#### Blue water

y3 <- list(
  title = "Blue water footprint (m<sup>3</sup> t<sup>-1</sup>)",
  titlefont = f,
  gridcolor = "darkgrey")

p3 <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW_shares, y = ~Water, color= ~X, 
             colors = color_values, size = I(150)) %>%
  layout(xaxis = x, yaxis = y3)
p3

#subplot(p1,p2,p3)


######### Footprints verses FW #############
# Footprints are given per kg of EATEN Food (accounting for that food is thrown away)

#### Biomass
x <- list(
  title = "Food waste shares",
  titlefont = f,
  gridcolor = "darkgrey")
y1 <- list(
  title = "Biomass footprint (kg kcal<sup>-1</sup>)",
  titlefont = f,
  gridcolor = "darkgrey")

p1 <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW_shares, y = ~biomass_kg.kcal, color= ~X,
              colors = color_values,  size = I(150))  %>%
  layout(xaxis = x, yaxis = y1)
p1

#### Cropland
x2 <- list(
  title = "Food waste shares",
  titlefont = f,
  showline = T,
  gridcolor = "darkgrey")
y2 <- list(
  title = "Cropland footprint (ha kcal<sup>-1</sup>)",
  titlefont = f,
  gridcolor = "darkgrey")

p2 <- plot_ly(data = prod_cat,  type='scatter', mode = 'markers', x = ~FW_shares, y = ~land_ha.kcal, color= ~X, 
              colors =  color_values, size = I(150)) %>% 
  layout(xaxis = x2, yaxis = y2)
p2

#### Blue water

y3 <- list(
  title = "Blue water footprint (m<sup>3</sup> kcal<sup>-1</sup>)",
  titlefont = f,
  gridcolor = "darkgrey")

p3 <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW_shares, y = ~water_l.kcal, color= ~X, 
              colors = color_values, size = I(150)) %>%
  layout(xaxis = x, yaxis = y3)
p3





######### Footprints per kcal vs calorie content #############

x <- list(
  title = "Energy content (cal g<sup>-1</sup>)",
  titlefont = f)

#Biomass
y1 <- list(
  title = "Biomass footprint (kg kcal<sup>-1</sup>)",
  titlefont = f)

p1 <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~kcal, y = ~biomass_kg.kcal, color= ~X,
            colors = color_values,  size = I(150))  %>%
 layout(xaxis = x, yaxis = y1)

p1

#Land
y2 <- list(
  title = "Cropland footprint (ha kcal<sup>-1</sup>)",
  titlefont = f)

p2 <- plot_ly(data = prod_cat,  type='scatter', mode = 'markers', x = ~kcal, y = ~land_ha.kcal, color= ~X, 
             colors =  color_values, size = I(150)) %>% 
  layout(xaxis = x, yaxis = y2)
p2

# water
y3 <- list(
  title = "Blue water footprint (m<sup>3</sup> kcal)",
  titlefont = f)

p3 <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~kcal, y = ~water_l.kcal, color= ~X, 
             colors = color_values, size = I(150)) %>%
  layout(xaxis = x, yaxis = y3)
p3


######### Footprints per Protein vs protein content #############

prod_cat <- prod_cat[-3,] # No protein in vegies (or too little) -> footprint -> infinity

color_values = c(rgb(212, 162, 106, maxColorValue = 255), rgb(128, 77, 21,   maxColorValue = 255), 
                 rgb(41, 122, 74,   maxColorValue = 255),
                 rgb(2, 38, 54,  maxColorValue = 255), rgb(39, 87, 107,   maxColorValue = 255),
                 rgb(212, 127, 106,  maxColorValue = 255), rgb(170, 78, 57,  maxColorValue = 255),
                 rgb(85, 16, 0,  maxColorValue = 255),rgb(107, 117, 159,  maxColorValue = 255))

prod_cat$X <- factor(prod_cat$X, levels = c('Cereals' , 'Potatoes & roots', 
                                            'Fruits','Pulses, beans & nuts', 'Vegetable oils',
                                            'Milk & products','Eggs','Meat','Sugar & Alcohol'))

x <- list(
  title = "Protein content (g 100g<sup>-1</sup>)",
  titlefont = f)

##Biomass


y <- list(
  title = "Biomass footprint (kg 10g <sup>-1</sup> protein)",
  titlefont = f)

p <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~prot, y = ~biomass_kg.10gP, color= ~X,
             colors = color_values,  size = I(150))  %>%
  layout(xaxis = x, yaxis = y)
p

##Cropland
y <- list(
  title = "Land footprint (ha 10g <sup>-1</sup> protein)",
  titlefont = f)

p <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~prot, y = ~land_ha.10gP, color= ~X, 
             colors = color_values, size = I(150)) %>%
  layout(xaxis = x, yaxis = y)
p

# Water
y <- list(
  title = "Blue water footprint (l 10g <sup>-1</sup> protein)",
  titlefont = f)

p <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~prot, y = ~water_l.10gP, color= ~X, 
             colors = color_values, size = I(150)) %>%
  layout(xaxis = x, yaxis = y)
p


#per protein

### Alternative plot
#plot(as.numeric(prod_cat$FW_shares),as.numeric(prod_cat$Biomass_kg.kkcal), pch=19, col=color_values, cex=2, bty="n")
#grid(10,10, col="lightgray", lty = 1)



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
#library(ggplot2)
#library("grid")
#library(subplot)


#plotList <- function(nplots) {
#  lapply(seq_len(nplots), function(x) plot_ly())
#}

## Multiple plot function#
#multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
# require(grid)
# 
# # Make a list from the ... arguments and plotlist
# plots <- c(list(...), plotlist)
# 
# numPlots = length(plots)
# 
# # If layout is NULL, then use 'cols' to determine layout
# if (is.null(layout)) {
#   # Make the panel
#   # ncol: Number of columns of plots
#   # nrow: Number of rows needed, calculated from # of cols
#   layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                    ncol = cols, nrow = ceiling(numPlots/cols))
# }
# 
# if (numPlots==1) {
#   print(plots[[1]])
#   
# } else {
#   # Set up the page
#   grid.newpage()
#   pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#   
#   # Make each plot, in the correct location
#   for (i in 1:numPlots) {
#     # Get the i,j matrix positions of the regions that contain this subplot
#     matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#     
#     print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                     layout.pos.col = matchidx$col))
#   }
# }
