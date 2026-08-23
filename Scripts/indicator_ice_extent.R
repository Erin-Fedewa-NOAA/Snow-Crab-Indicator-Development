# Calculate Bering Sea average sea ice concentration, masked for survey grid
#Source: ERA5 ice cover data, monthly averaged reanalysis
#https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means?tab=overview

#Authors: E. Ryznar & E Fedewa

# load ----
library(tidyverse)
library(tidync)
library(lubridate)
library(magrittr)
library(akgfmaps)
library(ecmwfr)
library(purrr)
library(akgfmaps)

#read in function to write text file
source("./Scripts/make_indicator_text.R")

current_year <- 2026

##############################

#pull ice data using climate data store API

# specify login credentials for the climate data store
user_id = "erin.fedewa@noaa.gov"
api_key = "3e25eba0-9dda-48d8-9d53-df47aeae8620"

#set key
wf_set_key(user = user_id,
           key = api_key) 

#specify years 
  #Note: run below requests sequentially, otherwise file size is too big
  #Only pulling current year data and appending to previously saved output 
#ice.years <- 1980:1988
#ice.years <-1989:2000
#ice.years <- 2001:2013
#ice.years <- 2014:2026
ice.years <- current_year

#specify request for current year
request <- list(
  "dataset_short_name" = "reanalysis-era5-single-levels-monthly-means",
  "product_type" = "monthly_averaged_reanalysis",
  "variable" = c("sea_ice_cover"),    
  "year" = ice.years,                     
  "month" = sprintf("%02d", 1:12),
  "day" = sprintf("%02d", 1:31),
  "time" = sprintf("%02d:00", 0:23),
  "area" = c(66, -179, 54, -158),      "format" = "netcdf",                  
  "target" = paste0("ERA5_ice_", min(ice.years), "-", max(ice.years), ".nc")) # target file name

# run request (you may need to manually click accept license on website --> follow link in error message if it appears)
wf_request(
  user     = user_id,
  request  = request,
  transfer = TRUE,
  path     = paste0("./data/"), # where do you want the data to be saved?
  verbose = TRUE)

#process ice data

#pull all ice files
ice_files <- list.files("./Data", pattern = "ERA5_ice")

ice_all <- map_dfr(ice_files, \(file) {
  
  tidync(file.path("./data", file)) %>%
    hyper_tibble() %>%
    separate(valid_time, into = c("year", "month", "time"), sep = "-") %>%
    select(-time) %>%
    mutate(across(c(year, month, latitude, longitude), as.numeric)) %>%
    filter(month %in% 1:4)})

#plot

#Load survey layers
region_layers <- akgfmaps::get_base_layers("ebs")
survey_area <- region_layers$survey.area
akland <- region_layers$akland

#compute annual grid averages across Jan-Apr period
siconc_yearly <- ice_all %>%
  group_by(longitude, latitude, year) %>%
  summarise(siconc = mean(siconc, na.rm = TRUE),
            .groups = "drop")

#plot faceted by year 
ggplot() +
  # sea ice raster
  geom_raster(data = siconc_yearly,
    aes(longitude, latitude, fill = siconc)) +
  # land
  geom_sf(data = akland,
          fill = "grey85", color = NA) +
  # survey boundary
  geom_sf(data = survey_area,
          fill = NA, color = "white", linewidth = 0.4) +
  scale_fill_viridis_c(name = "Sea Ice\nConcentration",
    limits = c(0,1), option = "C") +
  coord_sf(xlim = region_layers$plot.boundary$x,
    ylim = region_layers$plot.boundary$y, expand = FALSE) +
  facet_wrap(~year, ncol = 8) +
  theme_bw(base_size = 11) +
  labs(title = "Annual Mean Sea Ice Concentration in the Bering Sea",
       x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(face = "bold", size = 14),
        strip.text = element_text(face = "bold", size = 8),
        strip.background = element_rect(fill = "grey90"),
        panel.grid = element_blank(),
        panel.spacing = unit(0.6, "lines"),
        legend.position = "bottom",
        legend.key.width = unit(2.2, "cm"))

#API data request uses EBS/NBS survey grid max/min lat and longs, but because 
#this box ends up being quite a bit bigger than the survey grid, we'll 
#spatially subset the sea ice data to the EBS/NBS grid 

#Calculate spring (Mar-Apr) ice extent in EBS/NBS grid only:

#Get survey grids for masking
ebs_survey_area <- akgfmaps::get_base_layers("sebs")$survey.area
bs_survey_area  <- akgfmaps::get_base_layers("ebs")$survey.area

# March–April spatial mean at each location
Mar_Apr_ice <- ice_all %>%
  filter(month %in% 3:4) %>%
  group_by(year, latitude, longitude) %>%
  summarise(ice_conc = mean(siconc, na.rm = TRUE),
    .groups = "drop")

#Mask to EBS + NBS survey area and calculate annual mean
indicator_sea_ice <- Mar_Apr_ice %>%
  st_as_sf(coords = c("longitude", "latitude"),
    crs = "EPSG:4326") %>%
  st_transform(st_crs(bs_survey_area)) %>%
  st_intersection(bs_survey_area) %>%
  st_drop_geometry() %>%
  drop_na(ice_conc) %>%
  group_by(year) %>%
  summarise(Mar_Apr_ice_EBS_NBS = mean(ice_conc)*100)

#plot
indicator_sea_ice %>%
  ggplot(aes(year, Mar_Apr_ice_EBS_NBS)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 15, color= "red") + #ice-free threshold
  geom_hline(aes(yintercept = mean(Mar_Apr_ice_EBS_NBS, na.rm = TRUE)), linetype = 5) 

##############################################
#save indicator 
write.csv(indicator_sea_ice, "./output/indicator_sea_ice.csv", row.names=F)

############################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Winter_Sea_Ice_Advance_BS_Satellite_ERA"

##EDITABLE TEXT
description <- paste0("March-April average winter sea ice concentration in the EBS and NBS bottom trawl survey area
                      from the ERA5 reanalysis dataset. Concentration values represent the percentage of the Bering Sea 
                      covered by sea ice, with values below 15% considered ice-free. Proposed sign of the relationship is positive")

status_trends <- paste0("Sea ice extent increased substantially from spring 2025 to spring 2026, and is well  
                        above the time series mean. The spring 2026 sea ice extent estimate is the highest 
                        observed the Bering Sea since the last cold stanza that ended in 2013")

factors <- paste0("Sea ice in the Bering Sea is driven by atmospheric CO2, ocean heat transport and winds.")

implications <- paste0("Low levels of sea ice are associated with dampened productivity of snow crab (Mullowney et al., 2023), 
                       so declines in sea ice concentration suggest poor conditions for recruitment.")

references <- paste0("Mullowney DRJ, Baker KD, Szuwalski CS, Boudreau SA, Cyr F, et al. (2023) Sub-Arctic no more: Short- and long-term global-scale prospects for snow crab (Chionoecetes opilio) under global warming. PLOS Climate 2(10): e0000294. https://doi.org/10.1371/journal.pclm.0000294")


##INDICATOR DATA
indicator_data <- indicator_sea_ice %>%
  rename(indicator = Mar_Apr_ice_EBS_NBS)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)






