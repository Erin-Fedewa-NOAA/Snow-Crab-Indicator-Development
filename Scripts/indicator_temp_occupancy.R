#Calculate EBS snow crab temperatures of occupancy (CPUE weighted) 

# Author: Erin Fedewa

## Read in setup
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

##########################################
## compute cpue for immatures for each station
snow$specimen %>% 
  filter(YEAR %in% years) %>%
  left_join(mat_size) %>%
  filter(((SEX == 2 & CLUTCH_SIZE == 0) |
            (SEX == 1 & SIZE < MAT_SIZE))) %>% 
  group_by(YEAR, STATION_ID, AREA_SWEPT) %>%
  summarise(count = sum(SAMPLING_FACTOR)) %>%
  mutate(cpue = count / AREA_SWEPT) %>%
  ungroup()  -> cpue
  
#Temperature of Occupancy Calculations ----
cpue %>%
  left_join(., snow$haul %>% select(YEAR, GEAR_TEMPERATURE, STATION_ID)) %>%
  group_by(YEAR) %>% 
  summarise(temp_occ = weighted.mean(GEAR_TEMPERATURE, w = cpue, na.rm = T)) %>%
  print(n=50) -> temp_occ
  
#plot
temp_occ %>%
  ggplot(aes(x = YEAR, y = temp_occ))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(temp_occ, na.rm = TRUE)), linetype = 5) +
  theme_bw()

####################################

#Write output for Temp Occupancy indicator 
indicator_temp_occ <- temp_occ %>%
  rename_with(tolower) %>%
  complete(year = min(year):max(year)) %>%
  arrange(year)

write.csv(indicator_temp_occ, file="./Output/indicator_temp_occ.csv", row.names = F)

############################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Summer_Snow_Crab_Juvenile_Temperature_Occupancy"

##EDITABLE TEXT
description <- paste0("Juvenile snow crab temperature of occupancy is calculated as the mean bottom temperature 
                      weighted by immature snow crab CPUE. Proposed sign of the relationship is negative")

status_trends <- paste0("Temperatures occupied by juvenile snow crab declined by ~0.5 degrees C from 2025 to
                        2026.")

factors <- paste0("Temperatures occupied by juvenile snow crab are directly influenced by bottom temperatures in 
                  the Bering Sea and thermal preferences of stenothermic juveniles. Realized thermal niches are 
                  also influenced by species interactions. ")

implications <- paste0("Occupied temperatures < 0°C are critical for supporting high snow crab density and elevated 
                       energetic reserves (Fedewa et al., 2025). Occupied temperatures > 2°C have been suggested as 
                       an upper temperature threshold for juvenile snow crab (Murphy 2020). Marine heatwaves and the 
                       resulting loss of climate refugia are expected to increase temperatures occupied by juvenile
                       snow crab beyond thermal preferences.")

references <- paste0("Fedewa, E.F., Copeman, L.A., Litzow, M.A. 2025. Energetic limitations and mass mortality of Bering Sea snow crab: Interacting effects of warming and density on collapse and recovery. Canadian Journal of Fisheries and Aquatic Sciences. 82: 1-14. 
                     Murphy, J. T. 2020. Climate change, interspecific competition, and poleward vs. depth distribution shifts: Spatial analyses of the eastern Bering Sea snow and Tanner crab (Chionoecetes opilio and C. bairdi). Fisheries Research, 223, 105417.")


##INDICATOR DATA
indicator_data <- indicator_temp_occ %>%
  rename(indicator = temp_occ)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)
