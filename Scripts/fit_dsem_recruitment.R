#Goals ---- 
#Evaluate causal linkages between snow crab recruitment and ESP ecosystem indicators

#Author: E. Fedewa

#load
library(tidyverse)
library(corrplot)
library(patchwork)
library(ggplot2)
library(viridis)
library(ggthemes)
library(BAS)
library(readxl)
library(gbm)

## Read in setup for crab data
source("./Scripts/get_crab_data.R")

#Read in indicator data
indicators <- read.csv("./Output/snow_esp_indicator_timeseries.csv")

# Set years
current_year <- 2025
years <- 1988:current_year

############################################################
#calculate pre-recruit abundance as our response: 
#i.e. survey-derived abundance of 65-80mm CW male snow crab
#Size range selected using St. Marie 1995 size at age estimates
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
  mutate(ABUNDANCE = as.numeric(ABUNDANCE/1e6)) %>%
  rename_with(tolower)

#Plot
recruit_abun %>%
  ggplot(aes(x = year, y = abundance)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#Write output 
write_csv(recruit_abun, "./Output/BAS_recruit_abundance.csv", row.names = F)

#join indicator and response
recruit_abun %>%
  right_join(indicators) %>%
  arrange(year) -> model_dat
