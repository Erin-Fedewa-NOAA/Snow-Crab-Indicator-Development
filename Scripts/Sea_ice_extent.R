# notes ----
# Bering Sea ice extent via NSIDC: (https://nsidc.org/data/g02135/versions/3#anchor-1)
    #Processing: Get data -> seaice_analysis -> Sea_Ice_Index_Regional_Monthly (pulled Bering Sea data tab)

#Author: Erin Fedewa

#2025 follow up:  
  #Move to ORAS5 reanalysis product via https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-oras5?tab=overview 
  #Just can't figure out how to combine .nc files (output is by year/month)

# load ----
library(tidyverse)
library(tidync)
library(lubridate)
library(magrittr)

dat <- read.csv("./Data/NSIDC_sea_ice_extent.csv")

#Tidy up and take winter (Jan-Feb) average
dat %>%
  pivot_longer(c(2:13), names_to="month", values_to="ice_extent") %>%
  filter(month %in% c("January", "February")) %>%
  group_by(Year) %>%
  summarize(JanFeb_avg = mean(ice_extent,  na.rm=T)) -> ice_extent

#Plot 
ice_extent %>%
  ggplot(aes(Year, JanFeb_avg)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = mean(JanFeb_avg, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#Write output for sea ice indicator     
ice_extent %>%
  write.csv(file="./Output/NSIDCseaice_output.csv")


