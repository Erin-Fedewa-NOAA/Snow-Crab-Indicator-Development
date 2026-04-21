#Snow crab recruitment timeseries from EBS BT survey to use as 
  #response variable for indicator analysis 

#Author: EJF

#FOLLOW UPS for September CPT: Produce these estimates using maturity workflow
  #to include newshell immature crab only 

# load ----
library(tidyverse)

## Read in setup
source("./Scripts/get_crab_data.R")

#----------------------------#
#Recruitment data
#----------------------------#

#Snow crab recruitment: 
  #i.e. survey-derived abundance of 65-80mm CW male snow crab
  #(~6.7-7.7 years post settlement, 1-2 molts from terminal)

recruit_abun <- calc_bioabund(crab_data = snow,
                              species = "SNOW",
                              region = "EBS",
                              years = years,
                              sex = "male",
                              size_min = 65,
                              size_max = 80,
                              shell_condition = "new_hardshell") %>%
  select(YEAR, ABUNDANCE) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) %>%
  mutate(recruit_abun = as.numeric(ABUNDANCE/1e6)) %>%
  rename_with(tolower) %>%
  select(-abundance)

#Plot
recruit_abun %>%
  ggplot(aes(x = year, y = recruit_abun)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#Pre-recruit abundance: 
  #i.e. survey-derived abundance of 40-55mm CW male snow crab
  #(~4-5 years post settlement)

prerecruit_abun <- calc_bioabund(crab_data = snow,
                                 species = "SNOW",
                                 region = "EBS",
                                 years = years,
                                 sex = "male",
                                 size_min = 40,
                                 size_max = 55,
                                 shell_condition = "new_hardshell") %>%
  select(YEAR, ABUNDANCE) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) %>%
  mutate(prerecruit_abun = as.numeric(ABUNDANCE/1e6)) %>%
  rename_with(tolower) %>%
  select(-abundance)

#Plot
prerecruit_abun %>%
  ggplot(aes(x = year, y = prerecruit_abun)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#Save output
recruit_abun %>%
  full_join(prerecruit_abun) %>%
  rename_with(tolower) %>%
  write_csv("./Output/recruit_abundance.csv")
