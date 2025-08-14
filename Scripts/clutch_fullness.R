#Calculate proportion full clutches (reproductive potential) and proportion 
  #empty clutches (reproductive failure) in mature female snow crab

#NOTE: using shell 2 only for proportion full due to variation in fecundity by
  #shell condition, and filtering data to 1998+ due to changes in clutch codes 
  #(i.e.clutch code 7 use prior to 1998)

#NOTE: Switch to proportion empty clutches as indicator in 2025 due to change in 
  #protocol mentioned above. Note that proportion empty includes SC 2-5, so can't 
  #disentangle empty clutches due to scenescence, and we're excluding SC 1-2 to avoid
  #including mature females who haven't had time to extrude after molting 

#Author: Erin Fedewa

library(tidyverse)
library(ggridges)

## Read in setup
source("./Scripts/get_crab_data.R")

##############################################
#Proportion of primiparous females with full clutches

#calculate abundance of all primiparous mature females
primip <- snow
primip$specimen <- primip$specimen %>%
  filter(SEX == 2,
         CLUTCH_SIZE > 0,
         SHELL_CONDITION == 2)

primip_abund <- calc_bioabund(crab_data = primip,
                              species = "SNOW",
                              region = "EBS",
                              year = years,
                              spatial_level = "region") %>%
  mutate(primip_abun = ABUNDANCE) %>%
  select(YEAR, primip_abun)


# calculate abundance of just primiparous mature females with full clutches
primip_full <- snow
primip_full$specimen <- primip_full$specimen %>%
  filter(SEX == 2,
         SHELL_CONDITION == 2,
         CLUTCH_SIZE %in% c(5,6))

primip_full_abund <- calc_bioabund(crab_data = primip_full,
                                   species = "SNOW",
                                   region = "EBS",
                                   year = years,
                                   spatial_level = "region") %>%
  mutate(full_abun = ABUNDANCE) %>%
  select(YEAR, full_abun)

# calculate proportion full
prop_full <- primip_abund %>%
full_join(primip_full_abund) %>%
  mutate(prop_full = full_abun/primip_abun) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  filter(YEAR >= 1998) %>%
  arrange(YEAR) 

#plot proportion full
prop_full %>%
  ggplot(aes(x= YEAR, y=prop_full)) +
  geom_point() + 
  geom_line() +
  geom_hline(aes(yintercept = mean(prop_full, na.rm=TRUE)), linetype = 5) +
  theme_bw()

###########################################################
#Proportion of mature females with empty clutches

#calculate abundance of all mature females
mature <- snow
mature$specimen <- mature$specimen %>%
  filter(SEX == 2,
         CLUTCH_SIZE > 0,
         SHELL_CONDITION %in% (2:5))

mature <- calc_bioabund(crab_data = mature,
                              species = "SNOW",
                              region = "EBS",
                              year = years,
                              spatial_level = "region") %>%
  mutate(mature_abun = ABUNDANCE) %>%
  select(YEAR, mature_abun)


# calculate abundance of just mature females with empty clutches
barren <- snow
barren$specimen <- barren$specimen %>%
  filter(SEX == 2,
         SHELL_CONDITION %in% (2:5),
         EGG_CONDITION %in% c(0,3,4),
         CLUTCH_SIZE > 0) 

barren <- calc_bioabund(crab_data = barren,
                                   species = "SNOW",
                                   region = "EBS",
                                   year = years,
                                   spatial_level = "region") %>%
  mutate(barren_abun = ABUNDANCE) %>%
  select(YEAR, barren_abun)

# calculate proportion empty clutches
prop_empty <- mature %>%
  full_join(barren) %>%
  mutate(prop_empty = (barren_abun/mature_abun) *100) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) 

#plot proportion empty
prop_empty %>%
  ggplot(aes(x= YEAR, y=prop_empty)) +
  geom_point() + 
  geom_line() +
  geom_hline(aes(yintercept = mean(prop_empty, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#write csv for indicator 
prop_empty %>%
  select(YEAR, prop_empty) %>%
  write.csv(file="./Output/reproductive_potential.csv")
  




