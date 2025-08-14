# Calculate Bering Sea (EBS and NBS) average sea ice concentration

#Author: Erin Fedewa

#Notes:
  #In 2025, NSIDC sea ice extent index was replaced with average sea ice concentration from ERA5 reanalysis
  #To do in 2026: Automate ERA5 data pull in script: https://cds.climate.copernicus.eu/how-to-api 
 
# load ----
library(tidyverse)
library(tidync)
library(lubridate)
library(magrittr)
library(akgfmaps)

###########################################################
#To download and process ice cover data from ERA 5 monthly averaged data
# 1) Navigate here (will need to login): https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means?tab=overview
# 2) Click on "Download data" tab
# 3) Click on the product type you’d like. We have been using “Monthly averaged reanalysis”
# 4) For ice cover, click on the “Other” drop down, and then check the “Sea-ice cover” box. 
# 5) Select years you’d like the data to cover. Initial pull has been divided into two time stanzas, as
        #a larger request can result in an error with processing 
# 6) Select months you’d like the data to cover (Jan-Apr in this case) 
# 7) Select geographical area for the data. For the data below, the region has been set to:
        #North 65°, West -178°, South 56°, and East -165°. 
# 8) Select NetCDF(experimental) as the data format and unarchived as download format, and submit form to query/download data. 

################################################################
### Process Ice Data
early_dat <- tidync("./Data/ERA5_ice_1975_1999.nc") %>% 
  hyper_tibble() %>% 
  separate(valid_time, into = c("Year", "Month", "Time"), sep = "-") %>%
  select(-Time) %>%
  mutate(Year = as.numeric(Year),
         Month = as.numeric(Month))

recent_dat <- tidync("./Data/ERA5_ice_2000_2025.nc") %>% 
  hyper_tibble() %>% 
  separate(valid_time, into = c("Year", "Month", "Time"), sep = "-") %>%
  select(-Time) %>%
  mutate(Year = as.numeric(Year),
         Month = as.numeric(Month))

rbind(early_dat, recent_dat) %>%
  group_by(Year) %>%
  summarize(ice_avg = mean(siconc,  na.rm=T)) -> ice_extent

#Plot timeseries
ice_extent %>%
  ggplot(aes(Year, ice_avg)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = mean(ice_avg, na.rm=TRUE)), linetype = 5) +
  geom_hline(yintercept = 0.15, color= "red") + #ice-free threshold
  theme_bw()
#Less than 15% ice cover in Bering Sea in spring 2018-2019- below the threshold
  #for ice covered area

#write csv 
write.csv(ice_extent, "./Output/seaice_output.csv", row.names=F)

#plot data spatially

## SET COORDINATE REFERENCE SYSTEMS (CRS) --------------------------------------
in.crs <- "+proj=longlat +datum=NAD83" #CRS is in lat/lon
map.crs <- "EPSG:3338" # final crs for mapping/plotting: Alaska Albers

## LOAD SHELLFISH ASSESSMENT PROGRAM GEODATABASE -------------------------------
survey_gdb <- "./Figs/SAP_layers" 
survey_strata <- terra::vect(survey_gdb, layer = "EBS.NBS_surveyarea")
#EBS/NBS Boundary line
boundary <- st_read(layer = "EBS_NBS_divide", survey_gdb) 

## LOAD ALASKA REGION LAYERS (FROM AKGFMAPS R package) -----------------------------------
ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")
ebs_survey_areas <- ebs_layers$survey.area
ebs_survey_areas$survey_name <- c("Eastern Bering Sea", "Northern Bering Sea")

#Filter for sea ice extent threshold 
#We'll use 15% as our threshold for sea ice extent from estimates of sea ice 
#concentration (e.g. concentration >= 0.15 is ice-covered)- per NSIDC
rbind(early_dat, recent_dat) %>%
  filter(siconc >= 0.15) %>%
  rename(year = Year) %>%
  #transform sea ice data into spatial data frame
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = in.crs) %>%
  sf::st_transform(sf::st_crs(map.crs)) %>%
  vect(.) %>%
  mask(., survey_strata) %>% #this bounds ice extent data to survey region
  sf::st_as_sf() -> ice_extent  

#Add ice extent layer to map
ggplot() +
  geom_sf(data = ebs_layers$survey.grid, fill=NA, color=alpha("grey80"))+
  geom_sf(data = ebs_survey_areas, fill = NA) +
  geom_sf(data = ebs_layers$akland, fill = "grey80", color = "black") +
  #add ice extent
  geom_sf(data=ice_extent , aes(), color = "#9ECAE1", alpha = 0.25 ) +
  geom_sf(data= boundary, linewidth = 1, color = "grey40") +
  scale_x_continuous(limits = ebs_layers$plot.boundary$x,
                     breaks = ebs_layers$lon.breaks) +
  scale_y_continuous(limits = ebs_layers$plot.boundary$y,
                     breaks = ebs_layers$lat.breaks) +
  theme_bw() +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  facet_wrap(~year) 
ggsave("./Figs/sea_ice_map.png")
