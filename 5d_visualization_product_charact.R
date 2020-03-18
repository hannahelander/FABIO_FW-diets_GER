library(plotly)
library(tidyr)
library(ggplot2)
library("grid")
library(subplot)


#plotList <- function(nplots) {
#  lapply(seq_len(nplots), function(x) plot_ly())
#}

## Multiple plot function#
#multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

prod_cat <- read.csv("input/product_characteristics.csv",na.strings = "NaN", colClasses = c("factor",rep("numeric",10)), sep=";" )
prod_cat <- prod_cat[-9,] # there is no land footprints for fish



color_values = c(rgb(212, 162, 106, maxColorValue = 255), rgb(128, 77, 21,   maxColorValue = 255), 
                rgb(0, 61, 25,  maxColorValue = 255),rgb(41, 122, 74,   maxColorValue = 255),
                rgb(2, 38, 54,  maxColorValue = 255), rgb(39, 87, 107,   maxColorValue = 255),
                rgb(212, 127, 106,  maxColorValue = 255), rgb(170, 78, 57,  maxColorValue = 255),
                 rgb(85, 16, 0,  maxColorValue = 255),rgb(107, 117, 159,  maxColorValue = 255))



prod_cat$X <- factor(prod_cat$X, levels = c('Cereals' , 'Potatoes & roots','Vegetables', 
                                            'Fruits','Pulses, beans & nuts', 'Vegetable oils',
                                            'Milk & products','Eggs','Meat','Sugar & Alcohol')
                                            )
f <- list(
  family = "Arial",
  size = 18,
  color = "black")


#### Biomass
 
# per kg
x <- list(
  title = "Food waste shares",
  titlefont = f)
y1 <- list(
  title = "Biomass footprint (t/t food)",
  titlefont = f)

p1 <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW_shares, y = ~Biomass_t.t, color= ~X,
             colors = color_values,  size = I(150))  %>%
  layout(xaxis = x, yaxis = y1)
p1

#### Cropland

# per kg
y2 <- list(
  title = "Cropland footprint (h/t food)",
  titlefont = f)

p2 <- plot_ly(data = prod_cat,  type='scatter', mode = 'markers', x = ~FW_shares, y = ~Land_h.t, color= ~X, 
             colors =  color_values, size = I(150)) %>% 
  layout(xaxis = x, yaxis = y2)
p2

#### Blue water

# per kg
y3 <- list(
  title = "Blue water footprint (m3/t food)",
  titlefont = f)

p3 <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW_shares, y = ~Water_m3.t, color= ~X, 
             colors = color_values, size = I(150)) %>%
  layout(xaxis = x, yaxis = y3)
p3

subplot(p1,p2,p3)



# per kcal

#Biomass
y1 <- list(
  title = "Biomass footprint (kg/1000 kcal)",
  titlefont = f)

p1 <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW_shares, y = ~Biomass_kg.kkcal, color= ~X,
            colors = color_values,  size = I(150))  %>%
 layout(xaxis = x, yaxis = y1)

p1

#Land
y2 <- list(
  title = "Cropland footprint (h/1000 kcal)",
  titlefont = f)

p2 <- plot_ly(data = prod_cat,  type='scatter', mode = 'markers', x = ~FW_shares, y = ~Land_h.kkcal, color= ~X, 
             colors =  color_values, size = I(150)) %>% 
  layout(xaxis = x, yaxis = y2)
p2

# water
y3 <- list(
  title = "Blue water footprint (l/1000 kcal)",
  titlefont = f)

p3 <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW_shares, y = ~Water_l.kkcal, color= ~X, 
             colors = color_values, size = I(150)) %>%
  layout(xaxis = x, yaxis = y3)
p3



#### Per Protein ###
prod_cat <- prod_cat[-3,] # No protein in vegies (or too little)

color_values = c(rgb(212, 162, 106, maxColorValue = 255), rgb(128, 77, 21,   maxColorValue = 255), 
                 rgb(41, 122, 74,   maxColorValue = 255),
                 rgb(2, 38, 54,  maxColorValue = 255), rgb(39, 87, 107,   maxColorValue = 255),
                 rgb(212, 127, 106,  maxColorValue = 255), rgb(170, 78, 57,  maxColorValue = 255),
                 rgb(85, 16, 0,  maxColorValue = 255),rgb(107, 117, 159,  maxColorValue = 255))

prod_cat$X <- factor(prod_cat$X, levels = c('Cereals' , 'Potatoes & roots', 
                                            'Fruits','Pulses, beans & nuts', 'Vegetable oils',
                                            'Milk & products','Eggs','Meat','Sugar & Alcohol'))

##Biomass
y <- list(
  title = "Biomass footprint (kg/100g prot)", # specify!
  titlefont = f)

p <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW_shares, y = ~Biomass_kg.100gprot, color= ~X,
             colors = color_values,  size = I(150))  %>%
  layout(xaxis = x, yaxis = y)
p

##Cropland
y <- list(
  title = "Land footprint (m2/100g prot)",
  titlefont = f)

p <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW_shares, y = ~Land_m2.100gProt, color= ~X, 
             colors = color_values, size = I(150)) %>%
  layout(xaxis = x, yaxis = y)
p

# Water
y <- list(
  title = "Blue water footprint (l/100g prot)",
  titlefont = f)

p <- plot_ly(data = prod_cat, type='scatter', mode = 'markers', x = ~FW_shares, y = ~Water_l.100gprot, color= ~X, 
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

