library(tidyverse)

# remove memory
rm(list=ls())

# load data
r89p3 = readRDS("r89p3")
u89p3 = readRDS("u89p3")
r90p3 = readRDS("r90p3")
u90p3 = readRDS("u90p3")

# len of dataframe
lengths(r89p3)

## important --> durables are negative why?

row_r = c(#"cost_r_durables", 
          "cost_r_food",
          "cost_r_furniture",
          "cost_r_health",
          "cost_r_housing",
          #"cost_r_investment",
          "cost_r_transport", 
          "cost_r_clothing",
          "cost_r_communication",
          "cost_r_miscellaneous",
          "cost_r_tobacco",
          "cost_r_recreation",
          "cost_r_restaurant")

r89_sum_cost = cbind(r89p3$Address, rowSums(r89p3[row_r], na.rm = T))
r90_sum_cost = cbind(r90p3$Address, rowSums(r90p3[row_r], na.rm = T))

u89_sum_cost = cbind(u89p3$Address, rowSums(u89p3[row_r], na.rm = T))
u90_sum_cost = cbind(u90p3$Address, rowSums(u90p3[row_r], na.rm = T))

saveRDS(r89_sum_cost, file="r89_sum_cost")
saveRDS(r90_sum_cost, file="r90_sum_cost")
saveRDS(u89_sum_cost, file="u89_sum_cost")
saveRDS(u90_sum_cost, file="u90_sum_cost")

