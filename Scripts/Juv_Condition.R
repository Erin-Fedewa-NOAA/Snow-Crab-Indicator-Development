# notes ----]
# Develop indicator for juvenile snow crab condition, annual avg % DWt of hepatopancreas

# Erin Fedewa
# last updated: 2023/8/24

# load ----
library(tidyverse)
library(ggridges)

sc_condition <- read.csv("./Data/opilio_condition.csv")

#data wrangling 
sc_condition %>%
  filter(lme != "NA", #one crab collected outside the sampling design
         lme != "NBS",
         !vial_id %in% c("2019-65","2019-67","2019-68","2019-71","2019-66"),
         maturity != 1) %>%
  group_by(year) %>%
  summarise(avg_condition = mean(Perc_DWT, na.rm=T)) %>%
  write.csv(file="./Output/opilio_condition.csv")


