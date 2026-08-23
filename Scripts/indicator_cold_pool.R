# Generate avg bottom temp, and cold pool extent indices from EBS BT timeseries 

#TO DO: Calculate cold pool extent with spatial interpolation

# Erin Fedewa

# load ----
library(tidyverse)
library(mgcv)

## Read in setup
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

#########################################################

# compute mean summer bottom temperature (not an indicator)
avg_bt <- haul %>%
  mutate(julian=yday(parse_date_time(START_DATE, "ymd", "US/Alaska"))) %>%
  filter(YEAR > 1987,
         !HAUL_TYPE == 17) %>%
  distinct(YEAR, STATION_ID, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(summer_bt = mean(GEAR_TEMPERATURE, na.rm = T)) 

#Plot
avg_bt %>%
  ggplot(aes(x = as.numeric(YEAR), y = summer_bt)) +
  geom_point() +
  geom_line()+
  labs(y = "Bottom temperature (C)", x = "") +
  geom_hline(aes(yintercept = mean(summer_bt, na.rm = TRUE)), linetype = 5) +
  theme_bw()


#compute cold pool areal extent (not an indicator)
cpa <- haul %>%
  filter(YEAR >= 1988,
         HAUL_TYPE != 17,
         !(STATION_ID %in% corners)) %>%
  distinct(YEAR, STATION_ID, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(extent_coldpool = sum(GEAR_TEMPERATURE <= 2, na.rm = T) * 401) 

#Plot
cpa %>%
  ggplot(aes(x = as.numeric(YEAR), y = extent_coldpool)) +
  geom_point() +
  geom_line()+
  labs(y = "Cold Pool Extent (nmi2)", x = "") +
  geom_hline(aes(yintercept = mean(extent_coldpool, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#############################################

#ESP indicator: waters <=0C since that's likely a more important 
  #threshold for snow crab 

extent_0c <- haul %>%
  filter(YEAR >= 1988,
         HAUL_TYPE != 17,
         !(STATION_ID %in% corners)) %>%
  distinct(YEAR, STATION_ID, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(extent_0C = sum(GEAR_TEMPERATURE <= 0, na.rm = T) * 401, .groups = "drop") 
  

#plot
extent_0c %>%
ggplot(aes(x = as.numeric(YEAR), y = extent_0C)) +
  geom_point() +
  geom_line()+
  labs(y = "0C Water Extent (nmi2)", x = "") +
  geom_hline(aes(yintercept = mean(extent_0C, na.rm=TRUE)), linetype = 5) +
  theme_bw()

###########################################

#save indicator output

indicator_cold_pool <- extent_0c %>%
  rename_with(tolower) %>%
  complete(year = min(year):max(year)) %>%
  arrange(year)

write_csv(indicator_cold_pool, "./Output/indicator_cold_pool.csv")

############################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Summer_Cold_Pool_SEBS_Survey"

##EDITABLE TEXT
description <- paste0("The total area (nmi²) of all EBS bottom trawl survey stations with bottom temperatures ≤ 0°C. 
                      Proposed sign of the relationship is positive. ")

status_trends <- paste0("The spatial extent of cold-water habitat ≤ 0°C increased from 2025 to 2026, and is
                        now slightly above the time series mean.")

factors <- paste0("The spatial extent of bottom waters ≤ 0°C is driven by winter sea ice extent and winds.")

implications <- paste0("The EBS snow crab population collapse was attributed to warming and sea ice loss 
                       during the 2018-2019 marine heatwave (Szuwalski et al., 2023; Litzow et al., 2024), 
                       and cold-water habitat ≤ 0°C is critical for supporting high snow crab densities, 
                       energetic condition and survival in rebuilding towards a pre-collapse state (Fedewa et al., 2025). ")

references <- paste0("Szuwalski, C. S., Aydin, K., Fedewa, E. J., Garber-Yonts, B., and Litzow, M. A. 2023. The collapse of eastern Bering Sea snow crab. Science, 382: 306-310.
                     Litzow, M. A., Fedewa, E. J., Malick, M. J., Connors, B. M., Eisner, L., Kimmel, D. G., Kristiansen, T., Nielsen, J.M., Ryznar, E.R. 2024. Human-induced borealization leads to the collapse of Bering Sea snow crab. Nature Climate Change.
                     Fedewa, E.F., Copeman, L.A., Litzow, M.A. 2025. Energetic limitations and mass mortality of	                	 Bering Sea snow crab: Interacting effects of warming and density on collapse and recovery. Canadian Journal of Fisheries and Aquatic Sciences. 82: 1-14. ")


##INDICATOR DATA
indicator_data <- indicator_cold_pool %>%
    rename(indicator = extent_0c)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)






