#Calculate proportion full clutches (reproductive potential) and proportion 
  #empty clutches (reproductive failure) in mature female snow crab

#NOTE: using shell 2 only for proportion full due to variation in fecundity by
  #shell condition, and filtering data to 1998+ due to changes in clutch codes 
  #(i.e.clutch code 7 use prior to 1998). Howeever, proportion empty includes SC 2-5, so can't 
  #disentangle empty clutches due to scenescence, and we're excluding SC 1-2 to avoid
  #including mature females who haven't had time to extrude after molting 

#Author: Erin Fedewa

library(tidyverse)
library(ggridges)

## Read in setup
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

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
indicator_prop_empty <- mature %>%
  full_join(barren) %>%
  mutate(prop_empty = (barren_abun/mature_abun) *100) %>%
  rename_with(tolower) %>%
  select(year, prop_empty) %>%
  complete(year = min(year):max(year)) %>%
  arrange(year)  

#plot proportion empty
indicator_prop_empty %>%
  ggplot(aes(x= year, y=prop_empty)) +
  geom_point() + 
  geom_line() +
  geom_hline(aes(yintercept = mean(prop_empty, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#########################################

#write csv for indicator 
write.csv(indicator_prop_empty, file="./Output/indicator_clutch.csv", row.names=F)
  
############################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Summer_Snow_Crab_Female_Reproductive_Failure_SEBS_Survey"

##EDITABLE TEXT
description <- paste0("The proportion of hardshell mature female snow crab with no eggs, empty egg cases, or dead 
                      eggs. Hardshell crab are designated as code “2” to “5” from the EBS bottom trawl survey shell 
                      condition indices. Proposed sign of the relationship is negative.")

status_trends <- paste0("In 2026, ~3% of the mature female population had empty clutches, and the current year estimate 
                        is well below the time series mean.")

factors <- paste0("Female reproductive failure is driven by the inability to find a mate, and/or utilize stored 
                  sperm reserves to fertilize egg clutches (Webb et al., 2016; Murphy et al., 2017). An increased 
                  frequency of clutch failure may indicate sperm limitation, or energetic limitations imposed on 
                  extrusion and fertilization. Barren clutches can also be attributed to senescence, suggesting 
                  that a mature female population composed of old-shell females (primarily shell condition 5) may 
                  drive year to year trends in the proportion of empty clutches.")

implications <- paste0("A low proportion of mature females with empty clutches indicates high reproductive potential, 
                       and suggests that the majority of females were able to find mates or utilize stored sperm during 
                       the mating season.")

references <- paste0("Webb, J. B., Slater, L. M., Eckert, G. L., and Kruse, G. H. 2016. The contribution of fecundity and embryo quality to reproductive potential of eastern Bering Sea snow crab (Chionoecetes opilio). Canadian Journal of Fisheries and Aquatic Sciences, 73: 1800-1814.
Murphy, J. T., Rugolo, L. J., and Turnock, B. J. 2017. Integrating demographic and environmental variables to calculate an egg production index for the Eastern Bering Sea snow crab (Chionoecetes opilio). Fisheries Research, 193: 143-157.")


##INDICATOR DATA
indicator_data <- indicator_prop_empty %>%
  rename(indicator = prop_empty)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)


