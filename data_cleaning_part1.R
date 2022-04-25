# import 1390 household budget data
rm(list = ls())
load("./cleaned/HEIS90.Rdata")
typeof(R90)

# address of the households
addrs <- R90["address"]
hist(R90$MahMorajeh)

# access by reference
idx = 2000081293
idx_w = R90_weight$weight[R90_weight$Address == idx]
sprintf("%d : %f", idx, idx_w)

# filtering by columns of list object
typeof(R90P1)
R90P1[R90P1$gender == "Male" & R90P1$age < 30, ]

# household expenditures
library(tidyverse)
R90P3 <- bind_rows(R90P3S01, R90P3S02, R90P3S03, R90P3S04, R90P3S05, R90P3S06, R90P3S07, R90P3S08, R90P3S09, R90P3S11, R90P3S12, R90P3S13, R90P3S14)
U90P3 <- bind_rows(U90P3S01, U90P3S02, U90P3S03, U90P3S04, U90P3S05, U90P3S06, U90P3S07, U90P3S08, U90P3S09, U90P3S11, U90P3S12, U90P3S13, U90P3S14)
r90p3 <- R90P3 %>%
  mutate(Table = case_when(
    table == 1 ~ "food",
    table == 2 ~ "tobacco",
    table == 3 ~ "clothing",
    table == 4 ~ "housing",
    table == 5 ~ "furniture",
    table == 6 ~ "health",
    table == 7 ~ "transport",
    table == 8 ~ "communication",
    table == 9 ~ "recreation",
    table == 11 ~ "restaurant",
    table == 12 ~ "miscellaneous",
    table == 13 ~ "durables",
    table == 14 ~ "investment",
    TRUE ~ NA_character_)) %>%
  group_by(Address,Table) %>%
  summarize(cost = sum(value, na.rm = TRUE),
            cost_r = sum(value_r, na.rm = TRUE)) %>%
  pivot_wider(Address, 
              names_from = "Table", 
              values_from = c("cost","cost_r"), 
              values_fill = list(cost = 0))

u90p3 <- U90P3 %>%
  mutate(Table = case_when(
    table == 1 ~ "food",
    table == 2 ~ "tobacco",
    table == 3 ~ "clothing",
    table == 4 ~ "housing",
    table == 5 ~ "furniture",
    table == 6 ~ "health",
    table == 7 ~ "transport",
    table == 8 ~ "communication",
    table == 9 ~ "recreation",
    table == 11 ~ "restaurant",
    table == 12 ~ "miscellaneous",
    table == 13 ~ "durables",
    table == 14 ~ "investment",
    TRUE ~ NA_character_)) %>%
  group_by(Address,Table) %>%
  summarize(cost = sum(value, na.rm = TRUE),
            cost_r = sum(value_r, na.rm = TRUE)) %>%
  pivot_wider(Address, 
              names_from = "Table", 
              values_from = c("cost","cost_r"), 
              values_fill = list(cost = 0))


# import 1389 household budget data
load("./cleaned/HEIS89.Rdata")
typeof(R89)

# address of the households
addrs <- R89["address"]
hist(R89$MahMorajeh)

# access by reference
idx = 2000081293
idx_w = R89_weight$weight[R89_weight$Address == idx]
sprintf("%d : %f", idx, idx_w)

# filtering by columns of list object
typeof(R89P1)
R89P1[R89P1$gender == "Male" & R89P1$age < 30,]

# household expenditures || used this source: https://m-hoseini.github.io/HEIS/some-applications-of-household-level-data.html#gas-price-reform-in-november-2019-in-iran-and-poverty
library(tidyverse)
R89P3 <- bind_rows(R89P3S01, R89P3S02, R89P3S03, R89P3S04, R89P3S05, R89P3S06, R89P3S07, R89P3S08, R89P3S09, R89P3S11, R89P3S12, R89P3S13, R89P3S14)
U89P3 <- bind_rows(U89P3S01, U89P3S02, U89P3S03, U89P3S04, U89P3S05, U89P3S06, U89P3S07, U89P3S08, U89P3S09, U89P3S11, U89P3S12, U89P3S13, U89P3S14)
r89p3 <- R89P3 %>%
  mutate(Table = case_when(
    table == 1 ~ "food",
    table == 2 ~ "tobacco",
    table == 3 ~ "clothing",
    table == 4 ~ "housing",
    table == 5 ~ "furniture",
    table == 6 ~ "health",
    table == 7 ~ "transport",
    table == 8 ~ "communication",
    table == 9 ~ "recreation",
    table == 11 ~ "restaurant",
    table == 12 ~ "miscellaneous",
    table == 13 ~ "durables",
    table == 14 ~ "investment",
    TRUE ~ NA_character_)) %>%
  group_by(Address,Table) %>%
  summarize(cost = sum(value, na.rm = TRUE),
            cost_r = sum(value_r, na.rm = TRUE)) %>%
  pivot_wider(Address, 
              names_from = "Table", 
              values_from = c("cost","cost_r"), 
              values_fill = list(cost = 0))

u89p3 <- U89P3 %>%
  mutate(Table = case_when(
    table == 1 ~ "food",
    table == 2 ~ "tobacco",
    table == 3 ~ "clothing",
    table == 4 ~ "housing",
    table == 5 ~ "furniture",
    table == 6 ~ "health",
    table == 7 ~ "transport",
    table == 8 ~ "communication",
    table == 9 ~ "recreation",
    table == 11 ~ "restaurant",
    table == 12 ~ "miscellaneous",
    table == 13 ~ "durables",
    table == 14 ~ "investment",
    TRUE ~ NA_character_)) %>%
  group_by(Address,Table) %>%
  summarize(cost = sum(value, na.rm = TRUE),
            cost_r = sum(value_r, na.rm = TRUE)) %>%
  pivot_wider(Address, 
              names_from = "Table", 
              values_from = c("cost","cost_r"), 
              values_fill = list(cost = 0))


# save r89p3 u89p3 u90p3 u90p3
saveRDS(r89p3, file="r89p3")
saveRDS(u89p3, file="u89p3")
saveRDS(r90p3, file="r90p3")
saveRDS(u90p3, file="u90p3")
#### l = readRDS(file="./r89p3")
#### View(l)
# view rural and urban expenditure based on address
View(r89p3)
View(u89p3)
View(r90p3)
View(u90p3)

################

# INTERVIEWED IN WINTER
  # winter interview for 90 - rural part
  last_3_mon_addr = R90$address[R90$MahMorajeh >= 10]
  valid_subsidy_90_based_on_addr <- R90P4S04[R90P4S04$Address %in% last_3_mon_addr, ]
  hist(valid_subsidy_90_based_on_addr$subsidy_month)
  saveRDS(valid_subsidy_90_based_on_addr, "Rsubsidy_90_based_on_addr")
  rural_winter_addr = valid_subsidy_90_based_on_addr$Address

  # winter interview for 90 - urban part
  last_3_mon_addr = U90$address[U90$MahMorajeh >= 10]
  valid_subsidy_90_based_on_addr <- U90P4S04[U90P4S04$Address %in% last_3_mon_addr, ]
  hist(valid_subsidy_90_based_on_addr$subsidy_month)
  saveRDS(valid_subsidy_90_based_on_addr, "Usubsidy_90_based_on_addr")
  urban_winter_addr = valid_subsidy_90_based_on_addr$Address

# ROTATING HOUSEHOLDS
  #rm(list=ls())
  #load("./cleaned/HEIS89.Rdata")
  #load("./cleaned/HEIS90.Rdata")
  addr_rotating_rural89 = R89_weight$Address[R89_weight$Address %in% R90$address] 
  addr_rotating_urban89 = U89_weight$Address[U89_weight$Address %in% U90$address]
  length(addr_rotating_rural89)
  length(addr_rotating_urban89)
  sprintf("Number of initial rotating households: %d households", length(addr_rotating_rural89) + length(addr_rotating_urban89))
  
  ###
  #length(R89Data$Address[R89Data$Jaygozin == 2][R89Data$Address[R89Data$Jaygozin == 2] %in% addr_rotating_rural89]) # rural
  #length(U89Data$Address[U89Data$Jaygozin == 2][U89Data$Address[U89Data$Jaygozin == 2] %in% addr_rotating_urban89]) # urban
  ###
  rotating_addr_rural <- R90Data$Address[R90Data$Takmil == 1][R90Data$Addres[R90Data$Takmil == 1]%in%addr_rotating_rural89]
  rotating_addr_urban <- U90Data$Address[U90Data$Takmil == 1][U90Data$Addres[U90Data$Takmil == 1]%in%addr_rotating_urban89]
  sprintf("Number of found rotating households: %d households", length(rotating_addr_urban) + length(rotating_addr_rural))
  saveRDS(rotating_addr_urban, file="rotating_addr_urban")
  saveRDS(rotating_addr_rural, file="rotating_addr_rural")

# rotating and in winter
  rotating_winter_addr_rural = rural_winter_addr[rural_winter_addr %in% rotating_addr_rural]
  rotating_winter_addr_urban = urban_winter_addr[urban_winter_addr %in% rotating_addr_urban]
  length(rotating_winter_addr_urban)
  length(rotating_winter_addr_rural)
  saveRDS(rotating_winter_addr_rural, file="rotating_winter_addr_rural")
  saveRDS(rotating_winter_addr_urban, file="rotating_winter_addr_urban")

  
#### control group  
  # rural control group
  r89<-data.frame(R89P4S03)
  r90<-data.frame(R90P4S03)
  r89<-r89[r89$Address%in%rotating_winter_addr_rural,]
  r90<-r90[r90$Address%in%rotating_winter_addr_rural,]
  z<-merge(r89, r90, by.x = "Address", by.y = "Address")
  z<-z[z$income_aid.x - z$income_aid.y > 455000 & !is.na(z$income_aid.x - z$income_aid.y)& !is.na(z$income_aid.x)& !is.na(z$income_aid.y) & z$income_aid.x != 0,]
  control_group_recieving_subsyde_rural<-z$Address
  saveRDS(control_group_recieving_subsyde_rural, file="control_group_recieving_subsyde_rural")
  
  # urban control group
  u89<-data.frame(U89P4S03)
  u90<-data.frame(U90P4S03)
  u89<-u89[u89$Address%in%rotating_winter_addr_urban,]
  u90<-u90[u90$Address%in%rotating_winter_addr_urban,]
  z<-merge(u89, u90, by.x = "Address", by.y = "Address")
  z<-z[z$income_aid.x - z$income_aid.y > 455000 & !is.na(z$income_aid.x - z$income_aid.y)& !is.na(z$income_aid.x)& !is.na(z$income_aid.y) & z$income_aid.x != 0,]
  control_group_recieving_subsyde_urban<-z$Address
  saveRDS(control_group_recieving_subsyde_urban, file="control_group_recieving_subsyde_urban")
  

#### treatment group
  '%!in%' <- function(x,y)!('%in%'(x,y))
  # rural treatment group
  treatment_group_no_subsyde_rural <- rotating_winter_addr_rural[rotating_winter_addr_rural %!in% control_group_recieving_subsyde_rural]
  sprintf("number of rotating winter rural(treat + control) : %d",  length(treatment_group_no_subsyde_rural) + length(control_group_recieving_subsyde_rural))
  sprintf("number of rotating winter rural(whole data) : %d", length(rotating_winter_addr_rural))
  saveRDS(treatment_group_no_subsyde_rural, file="treatment_group_no_subsyde_rural")
  # rural treatment group
  treatment_group_no_subsyde_urban <- rotating_winter_addr_urban[rotating_winter_addr_urban %!in% control_group_recieving_subsyde_urban]
  sprintf("number of rotating winter urban(treat + control) : %d",  length(treatment_group_no_subsyde_urban) + length(control_group_recieving_subsyde_urban))
  sprintf("number of rotating winter urban(whole data) : %d", length(rotating_winter_addr_urban))
  saveRDS(treatment_group_no_subsyde_urban, file="treatment_group_no_subsyde_urban")
    
    
    
    
    
    
    