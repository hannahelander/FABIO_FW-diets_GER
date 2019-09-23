# Library
#install.packages("networkD3")
library(networkD3)
library(dplyr)

# load data to visualize
supply_chain <- read.csv2(file = "output/supply_chain_tonnes_SDG.csv")


# Make a connection data frame
links <- data.frame(
  source = c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4),
  target = c(1,6,1,6,2,6,2,6,3,6,3,6,4,6,4,6,5,6,5,6),
  value =  c(supply_chain$harvest_production, supply_chain$storage_transport, supply_chain$processing, 
             supply_chain$distribution, supply_chain$Consumption),
  group = as.factor(c("plant","plant","animal","animal","plant","plant","animal", "animal", "plant", "plant",
                      "animal","animal","plant","plant","animal","animal","plant","plant","animal","animal"))
  )

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name = c("Production", "Storage & Transport", "Processing", "Distribution", "Consumption", "Eaten", "Wastage"),
                    group = as.factor(c("process"))) 



# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
#links$IDsource <- match(links$source, nodes$name)-1 
#inks$IDtarget <- match(links$target, nodes$name)-1

my_color <- 'd3.scaleOrdinal() .domain(["plant", "animal", "process"]) .range(["#69b3a2", "steelblue", "grey"])'

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target", 
                   Value = "value", NodeID = "name", 
                   colourScale=my_color, LinkGroup="group", NodeGroup="group", fontSize = 12)

 
p


# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyColor1.html"))