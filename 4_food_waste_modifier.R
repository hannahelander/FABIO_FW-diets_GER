
#### This script 

### Waste scenario: halfing the food waste in consumption and production chain (for each product group - compare with "avoidable food waste")
waste$distribution[index$country=="DEU"] <- waste$distribution[index$country=="DEU"] * 0.5
waste$consumption[index$country=="DEU"] <- waste$consumption[index$country=="DEU"] * 0.5

## Afterwards 2_footprints and 2b_quantaties can be run again

# teständring