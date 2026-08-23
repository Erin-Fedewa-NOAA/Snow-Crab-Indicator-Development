#Central script to load EBS BT survey data via crabpack R package and define
  #stations, maturity and core area for subsetting indicators

#Follow ups: 
#Next year: Use immature/male male station-level CPUE data output 
#from sdmTMB modeling workflow to generate indicators by sex/maturity 
#stage instead of mean size at 50% maturity as annual cutline

## Load packages
library(crabpack)
library(tidyverse)
library(tidync)
library(lubridate)

##########################################################

# Set years ----
current_year <- 2026
years <- 1988:current_year

## Pull haul and snow crab specimen data ----
snow <- get_specimen_data(species = "SNOW",
                            region = "EBS",
                            channel = "KOD")

snow_nbs <- get_specimen_data(species = "SNOW",
                          region = "NBS",
                          channel = "KOD")

haul <- snow$haul

#Define standard stations and corner stations ----
  #(NOTE: corner stations dropped in 2024)
stations <- read.csv("./Data/station_lookup.csv")

corners <- stations %>% 
  filter(STATION_TYPE == "MTCA_CORNER") %>%
  pull(STATION_ID)

#Read in size at 50% probability of maturity ----
  #assign static mean cutline to years with no chela data:
mat <- read.csv("./Output/Contributor data/indicator_male_sam.csv") %>%
  select(YEAR, SAM) %>%
  rename_with(tolower)

#Calculate station-level snow crab CPUE ----
cpue <- crabpack::calc_cpue(crab_data = snow,
                            species = "SNOW",
                            years = years)

#Define spatially varying snow crab core area
  #ie Stations containing the upper 75th percentile of snow crab CPUE within each year
cpue75_core <- cpue %>%
  filter(!STATION_ID %in% corners) %>%
  group_by(YEAR) %>%
  mutate(cpue_75th = quantile(CPUE, 0.75, na.rm = TRUE)) %>%
  filter(CPUE >= cpue_75th) %>%
  ungroup()

#plot
ggplot(cpue75_core, aes(LONGITUDE, LATITUDE)) +
  geom_point(aes(size = CPUE)) +
  facet_wrap(~YEAR)

#Write csv for stations in core habitat each year  
write.csv(cpue75_core, file="./Output/snow_core_area.csv", row.names = FALSE)

#Pull stations for filtering in indicator script
snow_core <- cpue75_core %>%
                select(YEAR, STATION_ID)




