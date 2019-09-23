library(plotly)


####### Pie ###############

# Diets <- read.csv2(file = " ")

data1 <- as.data.frame(t(Diets[1,1:length(Diets)-1]))
data2 <- as.data.frame(t(Diets_clean[2,1:length(Diets_clean)-1]))

# SQ in terms of DGE_groups
data3 <- as.data.frame(t(Diets_DGEgroups))

data3 <- as.data.frame(t(Diets_food[1,1:length(Diets_food)-1]))

data1 <- SQ_DGEgroups

p <- plot_ly(data1, labels = rownames(data1), values = ~SQ_capita,  type = 'pie') %>%
  layout(title = 'National recommendations',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#p <- plot_ly(data, labels = rownames(data), values = ~SQ_percentage,  type = 'pie') %>%
#  layout(title = 'Status Quo',
#         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


p
#################Do a data to specify the different colors for different links! Put all data in a proper data frame
#### Links


# Add a 'group' column to the nodes data frame:
#nodes$group=as.factor(c("a","a","a","a","a","b","b","b"))

# Give a color for each group:
#my_color <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["blue", "pink"])'


# Add a 'group' column to each connection:
#links$group=as.factor(c("type_a","type_a","type_a","type_b","type_b","type_b"))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
#nodes$group=as.factor(c("my_unique_group"))


############# Sankey Diagram ###################
# with Plotly


supply_chain <- read.csv2(file = "output/supply_chain_Biomass.csv")

links <- data.frame(
  source = c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4),
  target = c(1,6,1,6,2,6,2,6,3,6,3,6,4,6,4,6,5,6,5,6),
  value =  c(supply_chain$harvest_production, supply_chain$storage_transport, supply_chain$processing, 
             supply_chain$distribution, supply_chain$Consumption),
  group = as.factor(c("type_a","type_a","type_b","type_b","type_a","type_a","type_b", "type_b", "type_a", "type_a",
                      "type_b","type_b","type_a","type_a","type_b","type_b","type_a","type_a","type_b","type_b"))
#  color = 
 # label = sprintf("<b>%s</b><br>%.2f %% of total", df$item, df$value/sum(df$value)*100)
  )
# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b"]) .range(["blue", "grey"])'

#################

#
link_list <- list(
  source = links$source, # index of the nodes you specified above, starting with 0
  target = links$target, # index of the nodes you specified above, starting with 0
  value = kinks$value, 
  colourScale=my_color, 
  LinkGroup="group")

 # color = df$color, # this can also be an rgba() color, which means you can control
  # the opacity of the link in the last parameter, e.g. rgba(38, 166, 91, .3) is
  # 30% opacity of rgb(38, 166, 91) = rgba(38, 166, 91, 1)
#  label = sprintf("<b>%s</b><br>%.2f %% of total", df$item, df$value/sum(df$value)*100)
#)
#```





###########
# SANKEY DIAGRAM WITH ONE NODE FOR WASTE

p <- plotly::plot_ly(
  type = "sankey",
  orientation = "h",
  node = list(
    label = c("Production", "Storage & Transport", "Processing", "Distribution", "Consumption", "Eaten", "Wastage"),
    color = c("blue", "red", "green", "grey", "purple", "yellow", "black"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
      )
    ),
  link = list(
    source = links$source, # index of the nodes you specified above, starting with 0
    target = links$target, 
    value = links$value, 
    colourScale=my_color, 
    LinkGroup=links$group)
#  link = list(
#    source = c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4),
#    target = c(1,6,1,6,2,6,2,6,3,6,3,6,4,6,4,6,5,6,5,6),
#    value =  c(supply_chain$harvest_production, supply_chain$storage_transport, supply_chain$processing, supply_chain$distribution, supply_chain$Consumption)
#    )
  ) %>% 
  plotly::layout(
    title = "Supply chain in tonnes",
    font = list(
      size = 10
      )
    )
p


#######
###########
# SANKEY DIAGRAM WITH SEVERAL NODES FOR WASTE
# in plotly beginns index with 0 (like in java, see example from Jakobs code as well)

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c(colnames(supply_chain_Y[4:8])),
    color = c("blue", "blue", "blue", "blue", "blue", "blue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = c(0,1,0,2,3,3),
    target = c(2,3,3,4,4,5),
    value =  c(8,4,2,8,4,2)
  )
) %>% 
  layout(
    title = "Basic Sankey Diagram",
    font = list(
      size = 10
    )
  )

p

##########
######


# in plotly beginns index with 0 (like in java, see example from Jakobs code as well)

p <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c(colnames(supply_chain_Y[4:8])),
    color = c("blue", "blue", "blue", "blue", "blue", "blue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = c(0,1,0,2,3,3),
    target = c(2,3,3,4,4,5),
    value =  c(8,4,2,8,4,2)
  )
) %>% 
  layout(
    title = "Basic Sankey Diagram",
    font = list(
      size = 10
    )
  )

p
###################################

chart_link = api_create(p, filename="sankey-basic-example")
chart_link
