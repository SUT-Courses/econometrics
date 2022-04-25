library(tidyverse)

rename_col <- function(df) {
  df <- df %>%
    rename(
      Address = X1,
      cost = X2
    )
  return(df)
}

control_rural <- readRDS("control_group_recieving_subsyde_rural")
treat_rural <- readRDS("treatment_group_no_subsyde_rural")
control_urban <- readRDS("control_group_recieving_subsyde_urban")
treat_urban <- readRDS("treatment_group_no_subsyde_urban")

cost_rural_89 = data.frame(readRDS("r89_sum_cost"))
cost_urban_89 = data.frame(readRDS("u89_sum_cost"))
cost_rural_90 = data.frame(readRDS("r90_sum_cost"))
cost_urban_90 = data.frame(readRDS("u90_sum_cost"))

cost_rural_89 <- rename_col(cost_rural_89)
cost_rural_90 <- rename_col(cost_rural_90)
cost_urban_89 <- rename_col(cost_urban_89)
cost_urban_90 <- rename_col(cost_urban_90)

rural<-merge(cost_rural_89, cost_rural_90,by="Address")
rural <- rural %>%
    rename(
      cyear89 = cost.x,
      cyear90 = cost.y
    )
urban<-merge(cost_urban_89, cost_urban_90,by="Address")
urban <- urban %>%
  rename(
    cyear89 = cost.x,
    cyear90 = cost.y
  )

head(rural)
head(urban)

print("89 vs. 90 based on rural/urban")
summary(rural)
summary(urban)

######################################################################
print("treat vs. control 89-90 rural")
sprintf("control")
summary(rural[rural$Address %in% control_rural, ])
sprintf("treat")
summary(rural[rural$Address %in% treat_rural, ])


######################################################################
print("treat vs. control 89-90 urban")
sprintf("control")
summary(urban[urban$Address %in% control_urban, ])
sprintf("treat")
summary(urban[urban$Address %in% treat_urban, ])

#####################################################################
############################# regression ############################
#####################################################################

control <- rbind(rural[rural$Address %in% control_rural, ], urban[urban$Address %in% control_urban, ])
control$urban <- ifelse(control$Address > 2000000000, 1, 0)
control$treat <- 0

treat <- rbind(rural[rural$Address %in% treat_rural, ], urban[urban$Address %in% treat_urban, ])
treat$urban <- ifelse(treat$Address > 2000000000, 1, 0)
treat$treat <- 1

fdf <- rbind(treat, control)

summary(fdf)
tail(fdf)


n1 <- subset(x = fdf,
       select = c(Address, cyear89, urban, treat)) 
n1$year = 0
n2 <- subset(x = fdf,
             select = c(Address, cyear90, urban, treat)) 
n2$year = 1
n1 <- n1 %>%
  rename(cost = cyear89)
n2 <- n2 %>%
  rename(cost = cyear90)

head(n1)
head(n2)

final <- rbind(n1,n2)
final$did <- final$year * final$treat
head(final)

# # address fixed effect
# mean_by_addr <- aggregate(cost~ Address, final, mean )
# head(mean_by_addr)
# for (i in 1:lengths(mean_by_addr)){
#   idx = mean_by_addr[i,]$Address
#   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
# }
# 
# # time fixed effect
# final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
# final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])

# main regression 
## TODO log
final$cost = final$cost / 10000
head(final)
#DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
#sprintf("DID: %f", DID)
didreg = lm(cost ~ treat + year + did, data = final)
summary(didreg)
summary(final)

# # poor
# new_final = final[final$cost < -73.48, ]
# head(new_final)
# DID = mean(new_final$cost[new_final$treat == 1 & new_final$year == 1]) - mean(new_final$cost[new_final$treat == 1 & new_final$year == 0]) - mean(new_final$cost[new_final$treat == 0 & new_final$year == 1]) 
# sprintf("DID: %f", DID)
# didreg = lm(cost ~ treat + year + did + 0, data = new_final)
# summary(didreg)
# summary(new_final)
# 
# # wealthy
# new_final = final[final$cost > 73.48, ]
# head(new_final)
# DID = mean(new_final$cost[new_final$treat == 1 & new_final$year == 1]) - mean(new_final$cost[new_final$treat == 1 & new_final$year == 0]) - mean(new_final$cost[new_final$treat == 0 & new_final$year == 1]) 
# sprintf("DID: %f", DID)
# didreg = lm(cost ~ treat + year + did + 0, data = new_final)
# summary(didreg)
# summary(new_final)
# 
# # rural poor
# new_final = final[final$urban == 0 & final$cost < -73.48, ]
# head(new_final)
# DID = mean(new_final$cost[new_final$treat == 1 & new_final$year == 1]) - mean(new_final$cost[new_final$treat == 1 & new_final$year == 0]) - mean(new_final$cost[new_final$treat == 0 & new_final$year == 1]) 
# sprintf("DID: %f", DID)
# didreg = lm(cost ~ treat + year + did + 0, data = new_final)
# summary(didreg)
# summary(new_final)
# 
# # urban poor
# new_final = final[final$urban == 1 & final$cost < -73.48, ]
# head(new_final)
# DID = mean(new_final$cost[new_final$treat == 1 & new_final$year == 1]) - mean(new_final$cost[new_final$treat == 1 & new_final$year == 0]) - mean(new_final$cost[new_final$treat == 0 & new_final$year == 1]) 
# sprintf("DID: %f", DID)
# didreg = lm(cost ~ treat + year + did + 0, data = new_final)
# summary(didreg)
# summary(new_final)
# 
# # rural rich
# new_final = final[final$urban == 0 & final$cost > 73.48, ]
# head(new_final)
# DID = mean(new_final$cost[new_final$treat == 1 & new_final$year == 1]) - mean(new_final$cost[new_final$treat == 1 & new_final$year == 0]) - mean(new_final$cost[new_final$treat == 0 & new_final$year == 1]) 
# sprintf("DID: %f", DID)
# didreg = lm(cost ~ treat + year + did + 0, data = new_final)
# summary(didreg)
# summary(new_final)
# 
# # urban rich
# new_final = final[final$urban == 1 & final$cost > 73.48, ]
# head(new_final)
# DID = mean(new_final$cost[new_final$treat == 1 & new_final$year == 1]) - mean(new_final$cost[new_final$treat == 1 & new_final$year == 0]) - mean(new_final$cost[new_final$treat == 0 & new_final$year == 1]) 
# sprintf("DID: %f", DID)
# didreg = lm(cost ~ treat + year + did + 0, data = new_final)
# summary(didreg)
# summary(new_final)



################## another method ################### poor 
n1_prim <- n1[n1$cost < 3382872,]
n2_prim <- n2[n1$cost < 3382872,]

final <- rbind(n1_prim,n2_prim)
final$did <- final$year * final$treat
head(final)

# # address fixed effect
# mean_by_addr <- aggregate(cost~ Address, final, mean )
# head(mean_by_addr)
# for (i in 1:lengths(mean_by_addr)){
#   idx = mean_by_addr[i,]$Address
#   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
# }
# 
# # time fixed effect
# final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
# final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])

# main regression
final$cost = final$cost / 10000
head(final)
#DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
#sprintf("DID: %f", DID)
didreg = lm(cost ~ treat + year + did, data = final)
summary(didreg)
summary(final)




################## another method ################### rich
n1_prim <- n1[n1$cost > 7687795,]
n2_prim <- n2[n1$cost > 7687795,]

final <- rbind(n1_prim,n2_prim)
final$did <- final$year * final$treat
head(final)

# # address fixed effect
# mean_by_addr <- aggregate(cost~ Address, final, mean )
# head(mean_by_addr)
# for (i in 1:lengths(mean_by_addr)){
#   idx = mean_by_addr[i,]$Address
#   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
# }
# 
# # time fixed effect
# final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
# final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])

# main regression
final$cost = final$cost / 10000
head(final)
# DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
# sprintf("DID: %f", DID)
didreg = lm(cost ~ treat + year + did, data = final)
summary(didreg)
summary(final)


################## another method ################### urban poor 
  n1_prim <- n1[n1$cost < 3382872& n1$urban==1,]
  n2_prim <- n2[n1$cost < 3382872&n1$urban==1,]
  
  final <- rbind(n1_prim,n2_prim)
  final$did <- final$year * final$treat
  head(final)
  
  # # address fixed effect
  # mean_by_addr <- aggregate(cost~ Address, final, mean )
  # head(mean_by_addr)
  # for (i in 1:lengths(mean_by_addr)){
  #   idx = mean_by_addr[i,]$Address
  #   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
  # }
  # 
  # # time fixed effect
  # final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
  # final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])
  
  # main regression
  final$cost = final$cost / 10000
  head(final)
  # DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
  # sprintf("DID: %f", DID)
  didreg = lm(cost ~ treat + year + did, data = final)
  summary(didreg)
  summary(final)




################## another method ################### urban rich
  n1_prim <- n1[n1$cost > 7687795&n1$urban==1, ]
  n2_prim <- n2[n1$cost > 7687795&n1$urban==1, ]
  
  final <- rbind(n1_prim,n2_prim)
  final$did <- final$year * final$treat
  head(final)
  
  # # address fixed effect
  # mean_by_addr <- aggregate(cost~ Address, final, mean )
  # head(mean_by_addr)
  # for (i in 1:lengths(mean_by_addr)){
  #   idx = mean_by_addr[i,]$Address
  #   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
  # }
  # 
  # # time fixed effect
  # final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
  # final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])
  # 
  # main regression
  final$cost = final$cost / 10000
  head(final)
  # DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
  # sprintf("DID: %f", DID)
  didreg = lm(cost ~ treat + year + did, data = final)
  summary(didreg)
  summary(final)

  
  
  ################## another method ################### rural poor 
  n1_prim <- n1[n1$cost < 3382872& n1$urban==0,]
  n2_prim <- n2[n1$cost < 3382872&n1$urban==0,]
  
  final <- rbind(n1_prim,n2_prim)
  final$did <- final$year * final$treat
  head(final)
  
  # # address fixed effect
  # mean_by_addr <- aggregate(cost~ Address, final, mean )
  # head(mean_by_addr)
  # for (i in 1:lengths(mean_by_addr)){
  #   idx = mean_by_addr[i,]$Address
  #   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
  # }
  # 
  # # time fixed effect
  # final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
  # final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])
  # 
  # main regression
  final$cost = final$cost / 10000
  head(final)
  # DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
  # sprintf("DID: %f", DID)
  didreg = lm(cost ~ treat + year + did, data = final)
  summary(didreg)
  summary(final)
  
  
  
  
  ################## another method ################### rural rich
  n1_prim <- n1[n1$cost > 7687795&n1$urban==0, ]
  n2_prim <- n2[n1$cost > 7687795&n1$urban==0, ]
  
  final <- rbind(n1_prim,n2_prim)
  final$did <- final$year * final$treat
  head(final)
  
  # # address fixed effect
  # mean_by_addr <- aggregate(cost~ Address, final, mean )
  # head(mean_by_addr)
  # for (i in 1:lengths(mean_by_addr)){
  #   idx = mean_by_addr[i,]$Address
  #   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
  # }
  # 
  # # time fixed effect
  # final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
  # final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])
  # 
  # main regression
  final$cost = final$cost / 10000
  head(final)
  # DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
  # sprintf("DID: %f", DID)
  didreg = lm(cost ~ treat + year + did, data = final)
  summary(didreg)
  summary(final)
  
  
  
  ######################################################################
  ########################## log (cost) ################################
  ######################################################################
  final <- rbind(n1,n2)
  final$did <- final$year * final$treat
  head(final)
  
  # # address fixed effect
  # mean_by_addr <- aggregate(cost~ Address, final, mean )
  # head(mean_by_addr)
  # for (i in 1:lengths(mean_by_addr)){
  #   idx = mean_by_addr[i,]$Address
  #   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
  # }
  # 
  # # time fixed effect
  # final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
  # final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])
  
  # main regression 
  ## TODO log
  final$cost = final$cost / 10
  head(final)
  #DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
  #sprintf("DID: %f", DID)
  didreg = lm(log(cost) ~ treat + year + did, data = final)
  summary(didreg)
  summary(final)
  
  # # poor
  # new_final = final[final$cost < -73.48, ]
  # head(new_final)
  # DID = mean(new_final$cost[new_final$treat == 1 & new_final$year == 1]) - mean(new_final$cost[new_final$treat == 1 & new_final$year == 0]) - mean(new_final$cost[new_final$treat == 0 & new_final$year == 1]) 
  # sprintf("DID: %f", DID)
  # didreg = lm(cost ~ treat + year + did + 0, data = new_final)
  # summary(didreg)
  # summary(new_final)
  # 
  # # wealthy
  # new_final = final[final$cost > 73.48, ]
  # head(new_final)
  # DID = mean(new_final$cost[new_final$treat == 1 & new_final$year == 1]) - mean(new_final$cost[new_final$treat == 1 & new_final$year == 0]) - mean(new_final$cost[new_final$treat == 0 & new_final$year == 1]) 
  # sprintf("DID: %f", DID)
  # didreg = lm(cost ~ treat + year + did + 0, data = new_final)
  # summary(didreg)
  # summary(new_final)
  # 
  # # rural poor
  # new_final = final[final$urban == 0 & final$cost < -73.48, ]
  # head(new_final)
  # DID = mean(new_final$cost[new_final$treat == 1 & new_final$year == 1]) - mean(new_final$cost[new_final$treat == 1 & new_final$year == 0]) - mean(new_final$cost[new_final$treat == 0 & new_final$year == 1]) 
  # sprintf("DID: %f", DID)
  # didreg = lm(cost ~ treat + year + did + 0, data = new_final)
  # summary(didreg)
  # summary(new_final)
  # 
  # # urban poor
  # new_final = final[final$urban == 1 & final$cost < -73.48, ]
  # head(new_final)
  # DID = mean(new_final$cost[new_final$treat == 1 & new_final$year == 1]) - mean(new_final$cost[new_final$treat == 1 & new_final$year == 0]) - mean(new_final$cost[new_final$treat == 0 & new_final$year == 1]) 
  # sprintf("DID: %f", DID)
  # didreg = lm(cost ~ treat + year + did + 0, data = new_final)
  # summary(didreg)
  # summary(new_final)
  # 
  # # rural rich
  # new_final = final[final$urban == 0 & final$cost > 73.48, ]
  # head(new_final)
  # DID = mean(new_final$cost[new_final$treat == 1 & new_final$year == 1]) - mean(new_final$cost[new_final$treat == 1 & new_final$year == 0]) - mean(new_final$cost[new_final$treat == 0 & new_final$year == 1]) 
  # sprintf("DID: %f", DID)
  # didreg = lm(cost ~ treat + year + did + 0, data = new_final)
  # summary(didreg)
  # summary(new_final)
  # 
  # # urban rich
  # new_final = final[final$urban == 1 & final$cost > 73.48, ]
  # head(new_final)
  # DID = mean(new_final$cost[new_final$treat == 1 & new_final$year == 1]) - mean(new_final$cost[new_final$treat == 1 & new_final$year == 0]) - mean(new_final$cost[new_final$treat == 0 & new_final$year == 1]) 
  # sprintf("DID: %f", DID)
  # didreg = lm(cost ~ treat + year + did + 0, data = new_final)
  # summary(didreg)
  # summary(new_final)
  
  
  
  ################## another method ################### poor 
  n1_prim <- n1[n1$cost < 3382872,]
  n2_prim <- n2[n1$cost < 3382872,]
  
  final <- rbind(n1_prim,n2_prim)
  final$did <- final$year * final$treat
  head(final)
  
  # # address fixed effect
  # mean_by_addr <- aggregate(cost~ Address, final, mean )
  # head(mean_by_addr)
  # for (i in 1:lengths(mean_by_addr)){
  #   idx = mean_by_addr[i,]$Address
  #   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
  # }
  # 
  # # time fixed effect
  # final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
  # final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])
  
  # main regression
  final$cost = final$cost
  head(final)
  #DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
  #sprintf("DID: %f", DID)
  didreg = lm(log(cost) ~ treat + year + did, data = final)
  summary(didreg)
  summary(final)
  
  
  
  
  ################## another method ################### rich
  n1_prim <- n1[n1$cost > 7687795,]
  n2_prim <- n2[n1$cost > 7687795,]
  
  final <- rbind(n1_prim,n2_prim)
  final$did <- final$year * final$treat
  head(final)
  
  # # address fixed effect
  # mean_by_addr <- aggregate(cost~ Address, final, mean )
  # head(mean_by_addr)
  # for (i in 1:lengths(mean_by_addr)){
  #   idx = mean_by_addr[i,]$Address
  #   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
  # }
  # 
  # # time fixed effect
  # final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
  # final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])
  
  # main regression
  final$cost = final$cost 
  head(final)
  # DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
  # sprintf("DID: %f", DID)
  didreg = lm(log(cost) ~ treat + year + did, data = final)
  summary(didreg)
  summary(final)
  
  
  ################## another method ################### urban poor 
  n1_prim <- n1[n1$cost < 3382872& n1$urban==1,]
  n2_prim <- n2[n1$cost < 3382872&n1$urban==1,]
  
  final <- rbind(n1_prim,n2_prim)
  final$did <- final$year * final$treat
  head(final)
  
  # # address fixed effect
  # mean_by_addr <- aggregate(cost~ Address, final, mean )
  # head(mean_by_addr)
  # for (i in 1:lengths(mean_by_addr)){
  #   idx = mean_by_addr[i,]$Address
  #   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
  # }
  # 
  # # time fixed effect
  # final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
  # final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])
  
  # main regression
  final$cost = final$cost 
  head(final)
  # DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
  # sprintf("DID: %f", DID)
  didreg = lm(log(cost) ~ treat + year + did, data = final)
  summary(didreg)
  summary(final)
  
  
  
  
  ################## another method ################### urban rich
  n1_prim <- n1[n1$cost > 7687795&n1$urban==1, ]
  n2_prim <- n2[n1$cost > 7687795&n1$urban==1, ]
  
  final <- rbind(n1_prim,n2_prim)
  final$did <- final$year * final$treat
  head(final)
  
  # # address fixed effect
  # mean_by_addr <- aggregate(cost~ Address, final, mean )
  # head(mean_by_addr)
  # for (i in 1:lengths(mean_by_addr)){
  #   idx = mean_by_addr[i,]$Address
  #   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
  # }
  # 
  # # time fixed effect
  # final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
  # final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])
  # 
  # main regression
  final$cost = final$cost 
  head(final)
  # DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
  # sprintf("DID: %f", DID)
  didreg = lm(log(cost) ~ treat + year + did, data = final)
  summary(didreg)
  summary(final)
  
  
  
  ################## another method ################### rural poor 
  n1_prim <- n1[n1$cost < 3382872& n1$urban==0,]
  n2_prim <- n2[n1$cost < 3382872&n1$urban==0,]
  
  final <- rbind(n1_prim,n2_prim)
  final$did <- final$year * final$treat
  head(final)
  
  # # address fixed effect
  # mean_by_addr <- aggregate(cost~ Address, final, mean )
  # head(mean_by_addr)
  # for (i in 1:lengths(mean_by_addr)){
  #   idx = mean_by_addr[i,]$Address
  #   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
  # }
  # 
  # # time fixed effect
  # final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
  # final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])
  # 
  # main regression
  final$cost = final$cost
  head(final)
  # DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
  # sprintf("DID: %f", DID)
  didreg = lm(log(cost) ~ treat + year + did, data = final)
  summary(didreg)
  summary(final)
  
  
  
  
  ################## another method ################### rural rich
  n1_prim <- n1[n1$cost > 7687795&n1$urban==0, ]
  n2_prim <- n2[n1$cost > 7687795&n1$urban==0, ]
  
  final <- rbind(n1_prim,n2_prim)
  final$did <- final$year * final$treat
  head(final)
  
  # # address fixed effect
  # mean_by_addr <- aggregate(cost~ Address, final, mean )
  # head(mean_by_addr)
  # for (i in 1:lengths(mean_by_addr)){
  #   idx = mean_by_addr[i,]$Address
  #   final$cost[final$Address == idx] = final$cost[final$Address == idx] - mean_by_addr$cost[mean_by_addr$Address == idx]
  # }
  # 
  # # time fixed effect
  # final$cost[final$year == 0] = final$cost[final$year == 0] - mean(final$cost[final$year == 0])
  # final$cost[final$year == 1] = final$cost[final$year == 1] - mean(final$cost[final$year == 1])
  # 
  # main regression
  final$cost = final$cost
  head(final)
  # DID = mean(final$cost[final$treat == 1 & final$year == 1]) - mean(final$cost[final$treat == 1 & final$year == 0]) - mean(final$cost[final$treat == 0 & final$year == 1]) 
  # sprintf("DID: %f", DID)
  didreg = lm(log(cost) ~ treat + year + did, data = final)
  summary(didreg)
  summary(final)
  
  
  
  saveRDS(final, file = "finalforregression")
  final = readRDS("finalforregression")

