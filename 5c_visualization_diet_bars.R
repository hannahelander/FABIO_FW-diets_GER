library(plotly)
library(tidyr)

Diets <- read.csv2(file ="input/Diets_3.0.csv" )

y <- c('D. Status Quo ', 'C. German Rec. ',  'B. Sust. Diet ', 'A. Vegetarian ')
data_f <- data.frame( "Cereals"          = c(sum(Diets[Diets$category == "Cereals ", 5]), sum(Diets[Diets$category == "Cereals ", 6]), 
                                           sum(Diets[Diets$category == "Cereals ", 7]),  sum(Diets[Diets$category == "Cereals ", 8])),
                    "Potatoes"        = c(sum(Diets[Diets$category == "Potatoes & roots", 5]), sum(Diets[Diets$category == "Potatoes & roots", 6]), 
                                          sum(Diets[Diets$category == "Potatoes & roots", 7]),  sum(Diets[Diets$category == "Potatoes & roots", 8])),
                    "Vegetables"       = c(sum(Diets[Diets$category == "Vegetables", 5]), sum(Diets[Diets$category == "Vegetables", 6]), 
                                           sum(Diets[Diets$category == "Vegetables", 7]), sum(Diets[Diets$category == "Vegetables", 8])),
                    "Fruits"           = c(sum(Diets[Diets$category == "Fruits", 5]), sum(Diets[Diets$category == "Fruits", 6]), 
                                           sum(Diets[Diets$category == "Fruits", 7]), sum(Diets[Diets$category == "Fruits", 8])),
                    "Pulses_beans_nuts" = c(sum(Diets[Diets$category == "Pulses, beans & nuts", 5]), sum(Diets[Diets$category == "Pulses, beans & nuts", 6]),
                                            sum(Diets[Diets$category == "Pulses, beans & nuts", 7]),  sum(Diets[Diets$category == "Pulses, beans & nuts", 8])),
                    "Vegetable_oils"    = c(sum(Diets[Diets$category == "Vegetable oils", 5]),sum(Diets[Diets$category == "Vegetable oils", 6]), 
                                            sum(Diets[Diets$category == "Vegetable oils", 7]), sum(Diets[Diets$category == "Vegetable oils", 8])),
                    "Milk"              = c(sum(Diets[Diets$category == "Milk & products", 5]), sum(Diets[Diets$category == "Milk & products", 6]),
                                            sum(Diets[Diets$category == "Milk & products", 7]), sum(Diets[Diets$category == "Milk & products", 8])),
                    "Eggs"              = c(sum(Diets[Diets$category == "Eggs", 5]), sum(Diets[Diets$category == "Eggs", 6]),
                                            sum(Diets[Diets$category == "Eggs", 7]), sum(Diets[Diets$category == "Eggs", 8])),
                    "Fish"              = c(sum(Diets[Diets$category == "Fish", 5]), sum(Diets[Diets$category == "Fish", 6]), 
                                            sum(Diets[Diets$category == "Fish", 7]), sum(Diets[Diets$category == "Fish", 8])),
                    "Meat"              = c(sum(Diets[Diets$category == "Meat", 5]), sum(Diets[Diets$category == "Meat", 6]), 
                                            sum(Diets[Diets$category == "Meat", 7]), sum(Diets[Diets$category == "Meat", 8])),
                    "Sugar_Alcohol"     = c(sum(Diets[Diets$category == "Sugar & Alcohol", 5]),sum(Diets[Diets$category == "Sugar & Alcohol", 6]), 
                                            sum(Diets[Diets$category == "Sugar & Alcohol", 7]),  sum(Diets[Diets$category == "Sugar & Alcohol", 8])),
                    row.names = c('Status Quo', 'National recommendations', 'EAT Lancet sustainable reference diet', 'EAT veg'))

# covert milk (primary product equivalents) in Y_eaten to consumed weight
Milk_exclButter <- 7297058.899 # in Tonnes, from BMEL (Milcherzeugnis zusammen minus buttermilcherzeugnis)
conv_rate <- sum(Y_SQ_eaten[index$DGE_group == "Milk"])/Milk_exclButter
data_f$Milk <- data_f$Milk /conv_rate # do hopefully apply to all!

#write.csv2(data_f, "output/diets_overview_cons.Weight.csv") # documentation of diets in actual weight

data <- cbind(y, data_f)


                  

p <- plot_ly(data, x = ~Cereals, y = y, type = 'bar', orientation = 'h', name = 'Cereals',
             marker = list(color = 'rgba(212, 162, 106, 0.6)',
                           line = list(color = 'rgba(2212, 162, 106, 1.0)',
                                       width = 0))) %>%
  add_trace(x = ~Potatoes, name = 'Potatoes & roots',
            marker = list(color = 'rgba(128, 77, 21, 0.6)',
                          line = list(color = 'rgba(128, 77, 21, 1.0)',
                                      width = 0))) %>%
  add_trace(x = ~Vegetables, name = 'Vegetables',
            marker = list(color = 'rgba(0, 61, 25, 0.6)',
                          line = list(color = 'rgba(0, 61, 25, 1.0)',
                                      width = 0))) %>%
  add_trace(x = ~Fruits, name = 'Fruits',
            marker = list(color = 'rgba(41, 122, 74, 0.6)',
                          line = list(color = 'rgba(41, 122, 74, 1.0)',
                                      width = 0))) %>%
  add_trace(x = ~Pulses_beans_nuts, name = 'Pulses, beans & nuts',
            marker = list(color = 'rgba(2, 38, 54, 0.6)',
                          line = list(color = 'rgba(2, 38, 54, 1.0)',
                                      width = 0))) %>%
  add_trace(x = ~Vegetable_oils, name = 'Vegetable oils',
            marker = list(color = 'rgba(39, 87, 107, 0.6)',
                          line = list(color = 'rgba(112, 80, 80, 1.0)',
                                      width = 0))) %>%
  add_trace(x = ~Milk, name = 'Milk & products',
            marker = list(color = 'rgba(212, 127, 106, 0.6)',
                          line = list(color = 'rgba(212, 127, 106, 1.0)',
                                      width = 0))) %>%
  add_trace(x = ~Eggs, name = 'Eggs',
            marker = list(color = 'rgba(170, 78, 57, 0.6)',
                          line = list(color = 'rgba(170, 78, 57, 1.0)',
                                      width = 0))) %>%
  add_trace(x = ~Fish, name = 'Fish',
            marker = list(color = 'rgba(128, 42, 21, 0.6)',
                          line = list(color = 'rgba(128, 42, 21, 1.0)',
                                      width = 0))) %>%
  add_trace(x = ~Meat, name = 'Meat',
            marker = list(color = 'rgba(85, 16, 0, 0.6)',
                          line = list(color = 'rgba(85, 16, 0,  1.0)',
                                      width = 0))) %>%
  add_trace(x = ~Sugar_Alcohol, name = 'Sugar & Alcohol',
            marker = list(color = 'rgba(107, 117, 159, 0.6)',
                          line = list(color = 'rgba(107, 117, 159, 1.0)',
                                      width = 0))) %>%
  
  layout(barmode = 'stack',
         xaxis = list(title = "", size = 30),
         yaxis = list(title = ""))

p




# y <- c('Status Quo ', 'German Rec. ',  'EAT Lancet rec.', 'German alternative ', 'EAT Lancet alt ')
# data <- data.frame( "Cereals"          = c(sum(Diets[Diets$category == "Cereals ", 5]), sum(Diets[Diets$category == "Cereals ", 6]), sum(Diets[Diets$category == "Cereals ", 7]), 
#                                            sum(Diets[Diets$category == "Cereals ", 8]), sum(Diets[Diets$category == "Cereals ", 9])),
#                     "Potatoes"        = c(sum(Diets[Diets$category == "Potatoes & roots", 5]), sum(Diets[Diets$category == "Potatoes & roots", 6]), sum(Diets[Diets$category == "Potatoes & roots", 7]), 
#                                            sum(Diets[Diets$category == "Potatoes & roots", 8]), sum(Diets[Diets$category == "Potatoes & roots", 9])),
#                     "Vegetables"       = c(sum(Diets[Diets$category == "Vegetables", 5]), sum(Diets[Diets$category == "Vegetables", 6]), sum(Diets[Diets$category == "Vegetables", 7]), 
#                                            sum(Diets[Diets$category == "Vegetables", 8]), sum(Diets[Diets$category == "Vegetables", 9])),
#                     "Fruits"           = c(sum(Diets[Diets$category == "Fruits", 5]), sum(Diets[Diets$category == "Fruits", 6]), sum(Diets[Diets$category == "Fruits", 7]), 
#                                            sum(Diets[Diets$category == "Fruits", 8]), sum(Diets[Diets$category == "Fruits", 9])),
#                    "Pulses_beans_nuts" = c(sum(Diets[Diets$category == "Pulses, beans & nuts", 5]), sum(Diets[Diets$category == "Pulses, beans & nuts", 6]), sum(Diets[Diets$category == "Pulses, beans & nuts", 7]), 
#                                               sum(Diets[Diets$category == "Pulses, beans & nuts", 8]), sum(Diets[Diets$category == "Pulses, beans & nuts", 9])),
#                    "Vegetable_oils"    = c(sum(Diets[Diets$category == "Vegetable oils", 5]),sum(Diets[Diets$category == "Vegetable oils", 6]), sum(Diets[Diets$category == "Vegetable oils", 7]),
#                                            sum(Diets[Diets$category == "Vegetable oils", 8]), sum(Diets[Diets$category == "Vegetable oils", 9])),
#                    "Milk"              = c(sum(Diets[Diets$category == "Milk & products", 5]), sum(Diets[Diets$category == "Milk & products", 6]), sum(Diets[Diets$category == "Milk & products", 7]),
#                                            sum(Diets[Diets$category == "Milk & products", 8]), sum(Diets[Diets$category == "Milk & products", 9])),
#                    "Eggs"              = c(sum(Diets[Diets$category == "Eggs", 5]), sum(Diets[Diets$category == "Eggs", 6]), sum(Diets[Diets$category == "Eggs", 7]), 
#                                            sum(Diets[Diets$category == "Eggs", 8]), sum(Diets[Diets$category == "Eggs", 9])),
#                    "Fish"              = c(sum(Diets[Diets$category == "Fish", 5]), sum(Diets[Diets$category == "Fish", 6]), sum(Diets[Diets$category == "Fish", 7]), 
#                                            sum(Diets[Diets$category == "Fish", 8]), sum(Diets[Diets$category == "Fish", 9])),
#                    "Meat"              = c(sum(Diets[Diets$category == "Meat", 5]), sum(Diets[Diets$category == "Meat", 6]), sum(Diets[Diets$category == "Meat", 7]), 
#                                            sum(Diets[Diets$category == "Meat", 8]), sum(Diets[Diets$category == "Meat", 9])),
#                    "Sugar_Alcohol"     = c(sum(Diets[Diets$category == "Sugar & Alcohol", 5]),sum(Diets[Diets$category == "Sugar & Alcohol", 6]), sum(Diets[Diets$category == "Sugar & Alcohol", 7]), 
#                                            sum(Diets[Diets$category == "Sugar & Alcohol", 8]), sum(Diets[Diets$category == "Sugar & Alcohol", 9])),
#                    row.names = c('Status Quo', 'National recommendations', 'EAT Lancet sustainable reference diet', 'Plant based reference diet', 'EAT veg'))

# ##############
# data <- data.frame( "Cereals"          = c(sum(Diets[Diets$category == "Cereals ", 5]), sum(Diets[Diets$category == "Cereals ", 6]), sum(Diets[Diets$category == "Cereals ", 7]), 
#                                            sum(Diets[Diets$category == "Cereals ", 8]), sum(Diets[Diets$category == "Cereals ", 9])),
#                     "Potatoes"        = c(sum(Diets[Diets$category == "Potatoes & roots", 5]), sum(Diets[Diets$category == "Potatoes & roots", 6]), sum(Diets[Diets$category == "Potatoes & roots", 7]), 
#                                           sum(Diets[Diets$category == "Potatoes & roots", 8]), sum(Diets[Diets$category == "Potatoes & roots", 9])),
#                     "Vegetables"       = c(sum(Diets[Diets$category == "Vegetables", 5]), sum(Diets[Diets$category == "Vegetables", 6]), sum(Diets[Diets$category == "Vegetables", 7]), 
#                                            sum(Diets[Diets$category == "Vegetables", 8]), sum(Diets[Diets$category == "Vegetables", 9])),
#                     "Fruits"           = c(sum(Diets[Diets$category == "Fruits", 5]), sum(Diets[Diets$category == "Fruits", 6]), sum(Diets[Diets$category == "Fruits", 7]), 
#                                            sum(Diets[Diets$category == "Fruits", 8]), sum(Diets[Diets$category == "Fruits", 9])),
#                     "Pulses_beans_nuts" = c(sum(Diets[Diets$category == "Pulses, beans & nuts", 5]), sum(Diets[Diets$category == "Pulses, beans & nuts", 6]), sum(Diets[Diets$category == "Pulses, beans & nuts", 7]), 
#                                             sum(Diets[Diets$category == "Pulses, beans & nuts", 8]), sum(Diets[Diets$category == "Pulses, beans & nuts", 9])),
#                     "Vegetable_oils"    = c(sum(Diets[Diets$category == "Vegetable oils", 5]),sum(Diets[Diets$category == "Vegetable oils", 6]), sum(Diets[Diets$category == "Vegetable oils", 7]),
#                                             sum(Diets[Diets$category == "Vegetable oils", 8]), sum(Diets[Diets$category == "Vegetable oils", 9])),
#                     "Milk"              = c(sum(Diets[Diets$category == "Milk & products", 5]), sum(Diets[Diets$category == "Milk & products", 6]), sum(Diets[Diets$category == "Milk & products", 7]),
#                                             sum(Diets[Diets$category == "Milk & products", 8]), sum(Diets[Diets$category == "Milk & products", 9])),
#                     "Eggs"              = c(sum(Diets[Diets$category == "Eggs", 5]), sum(Diets[Diets$category == "Eggs", 6]), sum(Diets[Diets$category == "Eggs", 7]), 
#                                             sum(Diets[Diets$category == "Eggs", 8]), sum(Diets[Diets$category == "Eggs", 9])),
#                     "Fish"              = c(sum(Diets[Diets$category == "Fish", 5]), sum(Diets[Diets$category == "Fish", 6]), sum(Diets[Diets$category == "Fish", 7]), 
#                                             sum(Diets[Diets$category == "Fish", 8]), sum(Diets[Diets$category == "Fish", 9])),
#                     "Meat"              = c(sum(Diets[Diets$category == "Meat", 5]), sum(Diets[Diets$category == "Meat", 6]), sum(Diets[Diets$category == "Meat", 7]), 
#                                             sum(Diets[Diets$category == "Meat", 8]), sum(Diets[Diets$category == "Meat", 9])),
#                     "Sugar_Alcohol"     = c(sum(Diets[Diets$category == "Sugar & Alcohol", 5]),sum(Diets[Diets$category == "Sugar & Alcohol", 6]), sum(Diets[Diets$category == "Sugar & Alcohol", 7]), 
#                                             sum(Diets[Diets$category == "Sugar & Alcohol", 8]), sum(Diets[Diets$category == "Sugar & Alcohol", 9])),
#                     row.names = c('Status Quo', 'National recommendations', 'EAT Lancet sustainable reference diet', 'Plant based reference diet', 'EAT veg'))
########################


#EXAMPLE:
library(plotly)
library(tidyr)

y <- c('giraffes', 'orangutans', 'monkeys')
SF_Zoo <- c(20, 14, 23)
LA_Zoo <- c(12, 18, 29)
data1 <- data.frame(y, SF_Zoo, LA_Zoo)

p <- plot_ly(data, x = ~SF_Zoo, y = ~y, type = 'bar', orientation = 'h', name = 'SF Zoo',
             marker = list(color = 'rgba(246, 78, 139, 0.6)',
                           line = list(color = 'rgba(246, 78, 139, 1.0)',
                                       width = 3))) %>%
  add_trace(x = ~LA_Zoo, name = 'LA Zoo',
            marker = list(color = 'rgba(58, 71, 80, 0.6)',
                          line = list(color = 'rgba(58, 71, 80, 1.0)',
                                      width = 3))) %>%
  layout(barmode = 'stack',
         xaxis = list(title = ""),
         yaxis = c('Status Quo ', 'National rec. ', 'EAT Lancet ', 'German plant-based '))
p
