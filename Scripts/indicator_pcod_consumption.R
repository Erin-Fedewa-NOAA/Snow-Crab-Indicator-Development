#Pacific cod consumption of snow crab
  #Contributor: Kerim Aydin

# Erin Fedewa

# load ----
library(tidyverse)
library(mgcv)

## Read in setup and data
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

indicator_pcod <- read_csv("./Output/Contributor data/indicator_cod_consumption.csv")

#########################################################

#Plot
indicator_pcod  %>%
  ggplot(aes(x = as.numeric(year), y = pcod_consumption)) +
  geom_point() +
  geom_line()+
  labs(y = "Consumption", x = "") +
  geom_hline(aes(yintercept = mean(pcod_consumption, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#######################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Summer_Snow_Crab_Consumption_Pacific_cod_Model"

##EDITABLE TEXT
description <- paste0("The daily summer consumption of snow crab (mt/day) by Pacific cod in the eastern Bering Sea, 
                      estimated from Pacific cod diet compositions, EBS trawl survey CPUE, and temperature adjusted 
                      length-specific maximum consumption rates. Pacific cod consumption estimates include unidentified 
                      Chionocetes sp. as well as identified C. opilio from stomach contents. Proposed sign of the 
                      relationship is negative.")

status_trends <- paste0("Daily consumption of juvenile snow crab by Pacific cod in 2026 was very similar to 2025, and remains below the long-term mean.")

factors <- paste0("Predation on snow crab in the eastern Bering Sea is primarily driven by Pacific cod biomass density, 
                  while cod–crab spatial overlap and cold pool extent exert smaller, size-dependent influences on overall consumption (Reum et al., 2026). 
                  Annual consumption estimates are also driven by juvenile snow crab population size, and as generalists, Pacific cod tend 
                  to predate heavily on snow crab when the abundance of small juveniles is high.")

implications <- paste0("Peaks in Pacific cod consumption of snow crab in 2016 coincided with a very large snow crab recruitment event. 
                       Recent declines in consumption despite new recruitment following the population collapse in 2021 suggest 
                       reduced top-down predation pressure and a positive outlook for stock rebuilding.")

references <- paste0("Reum, J. C. P., Thorson, J. T., & Fedewa, E. J. (2026). Joint spatiotemporal models for the estimation of prey consumption and predator–prey overlap: Dynamics of Pacific cod predation on snow and Tanner crab in the Eastern Bering Sea. Fisheries Oceanography, 35(3), 370–385.")


##INDICATOR DATA
indicator_data <- indicator_pcod  %>%
  rename(indicator = pcod_consumption)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)