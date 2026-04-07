#Calculate EBS snow crab temperatures of occupancy (CPUE weighted) 

#To do: impute missing station temperatures for early years 

# Author: Erin Fedewa

# load ----
library(tidyverse)

## Read in setup
source("./Scripts/get_crab_data.R")


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
  #Note that this is ignoring NA's in bottom temp data!!!
cpue %>%
  left_join(., snow$haul %>% select(YEAR, GEAR_TEMPERATURE, STATION_ID)) %>%
  group_by(YEAR) %>% 
  summarise(temp_occ = weighted.mean(GEAR_TEMPERATURE, w = cpue, na.rm = T)) %>%
  print(n=50) -> temp_occ
  
#plot
temp_occ %>%
  ggplot(aes(x = YEAR, y = TEMP_OCC))+
  geom_point(size=3)+
  geom_line() +
  theme_bw()

#Write output for Temp Occupancy indicator 
missing <- data.frame(YEAR = 2020)

temp_occ %>%
  bind_rows(missing) %>%
  arrange(YEAR) %>%
  write.csv(file="./Output/TempOcc_output.csv", row.names = F)


