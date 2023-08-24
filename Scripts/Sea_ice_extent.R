# notes ----
# Bering Sea ice concentration via ORAS5: http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_eed0_4cbd_14b1.html

# last updated: 2023/8/23

#This script needs follow ups in 2024!! 
  #The original snow crab sea ice indicator used data from ERA5. These data were downloaded in 
  #2023 via https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-oras5?tab=overview 
  #(see ERA5 sea ice folder) but unsure of how to combine monthly average and spatially subset
  #Approach #2 for 2023 after this failed was to use a NOAA ERDDAP (http://apdrc.soest.hawaii.edu/erddap/griddap/index.html?page=1&itemsPerPage=1000)
  #to pull data but there seems to be an issue with the file itself (can't open) - but these data only go to 2018?
  #Final dataset used in 2023 snow crab report card is NOAA@NSIDC data (https://nsidc.org/data/g02135/versions/3#anchor-1)
  #Get data -> seaice_analysis -> Sea_Ice_Index_Regional_Monthly (pulled Bering Sea data tab)

# load ----
library(tidyverse)
library(tidync)
library(lubridate)
library(magrittr)

#Using NOAA SWFSC ERDDAP to pull data 
download.file(url = "http://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_eed0_4cbd_14b1.htmlTable?ileadfra[(2018-12-15T00:00:00Z):1:(2018-12-15T00:00:00Z)][(55.5):1:(65.5)][(160):1:(180)]",
              mode="wb",
              destfile = "./Data/erddap_sea_ice.nc")
    
    
##Process ------------------------

ice <- tidync("./Data/erddap_sea_ice.nc") %>% hyper_tibble()
  head(ice)
  
#Hmmm...why is this not working??
  
#Ughhh let's move on to NSIDC data 
  
betterdat <- read.csv("./Data/NSIDC_sea_ice_extent.csv")

#Tidy up and take winter (Jan-Feb) average
betterdat %>%
  pivot_longer(c(2:13), names_to="month", values_to="ice_extent") %>%
  filter(month %in% c("January_area", "February_area")) %>%
  group_by(Year) %>%
  summarize(JanFeb_avg = mean(ice_extent,  na.rm=T)) -> ice_extent

#Plot 
ice_extent %>%
  ggplot(aes(Year, JanFeb_avg)) +
  geom_point() +
  geom_line()

#Write output for sea ice indicator     
ice_extent %>%
  write.csv(file="./Output/NSIDCseaice_output.csv")
 
    
