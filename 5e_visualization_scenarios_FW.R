library(plotly)
library(tidyr)

FW_scenarios <- read.csv2("input/product_FW_scenarios2.csv")


y <- c('D. Status Quo ', 'C. German Rec. ',  'B. Sust. Diet ', 'A. Vegetarian ')
data_f <- data.frame("Cereals" = list(FW_scenarios[1,2:5]), 
                     "Potatoes" = list(FW_scenarios[2,2:5]),
                     "Veg" = list(FW_scenarios[3,2:5]),
                     "Fruits" = list(FW_scenarios[4,2:5]),
                     "Pulses" = list(FW_scenarios[5,2:5]),
                     "Veg_oil" = list(FW_scenarios[6,2:5]),
                     "Milk" = list(FW_scenarios[7,2:5]),
                     "Eggs"= list(FW_scenarios[8,2:5]),
                     "Fish" = list(FW_scenarios[9,2:5]),
                     "Meat" = list(FW_scenarios[10,2:5]),
                     "Sug_alc" = list(FW_scenarios[11,2:5]),
                     row.names = c('Status Quo', 'National recommendations', 
                                   'EAT Lancet sustainable reference diet', 'EAT veg'))

data_f <- FW_scenarios

# covert milk (primary product equivalents) in Y_eaten to consumed weight


data <- cbind(y, data_f)

data <- FW_scenarios
Milk_exclButter <- 7297058.899 # in Tonnes, from BMEL (Milcherzeugnis zusammen minus buttermilcherzeugnis)
conv_rate <- sum(Y_SQ_eaten[index$DGE_group == "Milk"])/Milk_exclButter
data$Milk <- data$Milk /conv_rate 

p <- plot_ly(data, x = ~Cereals, y = y, type = 'bar', name = 'Cereals',
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
  add_trace(x = ~Pulses, name = 'Pulses, beans & nuts',
            marker = list(color = 'rgba(2, 38, 54, 0.6)',
                          line = list(color = 'rgba(2, 38, 54, 1.0)',
                                      width = 0))) %>%
  add_trace(x = ~Veg_oils, name = 'Vegetable oils',
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
  add_trace(x = ~Sug_alc, name = 'Sugar & Alcohol',
            marker = list(color = 'rgba(107, 117, 159, 0.6)',
                          line = list(color = 'rgba(107, 117, 159, 1.0)',
                                      width = 0))) %>%
  
  layout(barmode = 'stack',
         xaxis = list(title = "", size = 30),
         yaxis = list(title = ""))

p


