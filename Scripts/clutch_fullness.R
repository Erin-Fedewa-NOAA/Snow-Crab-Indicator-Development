#Calculate proportion full clutches in mature female snow crab- using shell 2 only
  #here to ease interpretation of variations in fecundity by shell condition 

#Author: Erin Fedewa

library(tidyverse)
library(ggridges)

## Read in setup
source("./Scripts/get_crab_data.R")

##############################################
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

# calculate proportion
prop <- primip_abund %>%
full_join(primip_full_abund) %>%
  mutate(prop_full = full_abun/primip_abun) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) 

#plot
prop %>%
  ggplot(aes(x= YEAR, y=prop_full)) +
  geom_point() + 
  geom_line() +
  geom_hline(aes(yintercept = mean(prop_full, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#write csv for indicator 
prop %>%
  select(YEAR, prop_full) %>%
  write.csv(file="./Output/clutch_full.csv")
  




