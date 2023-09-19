# notes ----
#Chl-a product (2003 to 2019) is from MODIS: 4x4 km resolution and aggregated as 8-day composites
    #Spatial coverage is by BSIERP regions in EBS/NBS and by crab mgmt area 

#FOLLOW UP NOTE: This script was used to spatiotemporally subset satellite products for the 
# draft 2022 snow crab ESP. As of fall 2022, look up tables for crab management areas are being 
#used to pull data direct online and spatially subset for indicator submission- no 
#further post-processing needed.

# Erin Fedewa
# last updated: 2021/9/5

# load ----
library(tidyverse)
library(lubridate)

# data ----

chl2 <- readRDS("./Data/merged_8day_2003_2021_EBS.RDS")
levels(chl2$bsierp_name)
#Based on Buck's IBM's, BSIERP regions "Pribilofs", "St. Matthew" most relevant to snow crab
  #but should plot in future!

#MODIS only chla indicator by BSIERP region
  
#Calcuate Apr-June chl-a biomass for Pribs region
  chl2 %>%
    mutate(month=month(date),
           year=year(date)) %>% 
    filter(bsierp_name == "Pribilofs",
           month %in% c(4, 5, 6)) %>%
    group_by(year) %>%
    summarise(chla = mean(chlorophyll, na.rm=TRUE)) %>%
    rename(YEAR=year) ->prib_modis
  
  #plot
    prib_modis %>%
    ggplot(aes(x = YEAR, y = chla)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(y = 'Chlorophyll-a [ug/l]', x = "")
    
#Calcuate Apr-June chl-a biomass for St. Matt region
    chl2 %>%
      mutate(month=month(date),
             year=year(date)) %>% 
      filter(bsierp_name == "St. Matthew",
             month %in% c(4, 5, 6)) %>%
      group_by(year) %>%
      summarise(chla = mean(chlorophyll, na.rm=TRUE)) %>%
      rename(YEAR=year) ->stmatt_modis
    
    #plot
    stmatt_modis %>%
      ggplot(aes(x = YEAR, y = chla)) +
      geom_point() +
      geom_line() +
      theme_bw() +
      labs(y = 'Chlorophyll-a [ug/l]', x = "")
    
#Hmmm fairly different trends between regions. But for now let's take a cross region average of the 
    #two regions. Might make sense to use regions that overlap with Buck's successfull settlement 
    #index in future 
#Also see Jordon script here on manipulating data
    
#Calculate cross region chl-a average for St. Matt and Pribs 
  chl2 %>%
      mutate(month=month(date),
             year=year(date)) %>% 
      filter(bsierp_name %in% c("St. Matthew", "Pribilofs"),
             month %in% c(4, 5, 6)) %>%
      group_by(year) %>%
      summarise(chla = mean(chlorophyll, na.rm=TRUE)) %>%
      rename(YEAR=year) ->crossregion_modis
    
    #plot
    crossregion_modis %>%
      ggplot(aes(x = YEAR, y = chla)) +
      geom_point() +
      geom_line() +
      theme_bw() +
      labs(y = 'Chlorophyll-a [ug/l]', x = "")

# Save output
write_csv(crossregion_modis, file = "./Output/chla_timeseries.csv")





