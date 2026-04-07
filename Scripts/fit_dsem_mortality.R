#Goals ---- 
#Evaluate causal linkages between snow crab mortality and ESP ecosystem indicators

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

## Read in C. Szuwalski time-varying mortality estimates for immature snow crab
mort <- read.csv("./Data/mortality_estimates_assmt.csv")

#Read in indicator data
indicators <- read.csv("./Output/snow_esp_indicator_timeseries.csv")

# Set years
current_year <- 2025
years <- 1988:current_year

#Something to think about- mortality estimates are both sexes!