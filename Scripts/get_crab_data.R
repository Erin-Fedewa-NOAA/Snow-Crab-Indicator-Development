#Central script to load EBS BT survey data via crabpack R package and define
  #stations, maturity and core area for subsetting indicators

#Script Authors: Shannon Hennessey & Erin Fedewa 

#FOLLOW UP: Next year create a dynamic core area that varies annually 

## Load packages
library(crabpack)
library(tidyverse)
library(tidync)
library(lubridate)
library(sf)
library(httr)
library(akgfmaps)
library(rnaturalearth)

##########################################################

# Set years ----
current_year <- 2025
years <- 1988:current_year

## Pull haul and snow crab specimen data ----
snow <- get_specimen_data(species = "SNOW",
                            region = "EBS",
                            channel = "KOD")

snow_nbs <- get_specimen_data(species = "SNOW",
                          region = "NBS",
                          channel = "KOD")

#Define standard stations and corner stations ----
  #(NOTE: corner stations dropped in 2024)
stations <- read.csv("Y:/KOD_Survey/EBS Shelf/Data_Processing/Data/lookup_tables/station_lookup.csv")

corners <- stations %>% 
  filter(STATION_TYPE == "MTCA_CORNER") %>%
  pull(STATION_ID)

#Read in size at 50% probability of maturity ----
  #assign static mean cutline to years with no chela data:
mat_size <- get_male_maturity(species = "SNOW", 
                              region = "EBS")$model_parameters %>% 
  select(-c("A_EST", "A_SE")) %>%
  rename(MAT_SIZE = B_EST, 
         STD_ERR = B_SE) %>%
  right_join(., expand_grid(YEAR = years,
                            SPECIES = "SNOW", 
                            REGION = "EBS",
                            DISTRICT = "ALL")) %>%
  mutate(MAT_SIZE = case_when(is.na(MAT_SIZE) ~ mean(MAT_SIZE, na.rm=T), 
                              TRUE ~ MAT_SIZE)) %>%
  filter(YEAR != 2020)

#Calculate station-level snow crab CPUE ----
cpue <- crabpack::calc_cpue(crab_data = snow,
                            species = "SNOW",
                            years = years)

#Define snow crab core area for spatially subsetting indicators ----
  #i.e. stations containing 50th percentile of snow crab CPUE
cpue50_core <- cpue %>%
  filter(!STATION_ID %in% corners) %>% #remove corner stations
  group_by(STATION_ID) %>%
  summarise(MEAN_CPUE = mean(CPUE)) %>%
  filter(MEAN_CPUE > quantile(MEAN_CPUE, 0.50)) %>%
  left_join(., stations %>% select(STATION_ID, LATITUDE, LONGITUDE))

#Quick plot for visual check 
ggplot(ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf() +
  geom_point(data = cpue50_core, aes(x = LONGITUDE, y = LATITUDE), size = 2, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-180, -159), ylim = c(54, 64), expand = FALSE) +
  theme_bw() #looks about right! 

#Write csv for stations in 50th percentile of avg CPUE  
write.csv(cpue50_core, file="./Output/snow_core_area.csv", row.names = FALSE)




