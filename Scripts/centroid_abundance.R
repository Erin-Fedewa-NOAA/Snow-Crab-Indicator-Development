#Calculate snow crab Latitude center of abundance in EBS by size/sex category

#Author: Erin Fedewa

#Follow ups: 
#move toward spatiotemporal modeling approach to account for changing footprint

# load ----
library(tidyverse)

## Read in setup
source("./Scripts/get_crab_data.R")

#############################
## compute cpue by size-sex group for each station
cpue <- snow$specimen %>% 
    left_join(., mat_size) %>%
    mutate(CATEGORY = case_when((SEX == 1 & SIZE >= MAT_SIZE) ~ "mature_male_centroid",
                                (SEX == 1 & SIZE < MAT_SIZE) ~ "immature_male_centroid",
                                (SEX == 2 & CLUTCH_SIZE >= 1) ~ "mature_female_centroid",
                                (SEX == 2 & CLUTCH_SIZE == 0) ~ "immature_female_centroid",
                                TRUE ~ NA)) %>%
    filter(YEAR %in% years,
           !is.na(CATEGORY)) %>%
    group_by(YEAR, STATION_ID, LATITUDE, LONGITUDE, AREA_SWEPT, CATEGORY) %>%
    summarise(COUNT = round(sum(SAMPLING_FACTOR))) %>%
    pivot_wider(names_from = CATEGORY, values_from = COUNT) %>%
    mutate(population = sum(immature_male_centroid, mature_male_centroid, 
                            immature_female_centroid, mature_female_centroid, na.rm = T)) %>%
    pivot_longer(c(6:10), names_to = "CATEGORY", values_to = "COUNT") %>%
    filter(CATEGORY != "NA") %>%
    mutate(COUNT = replace_na(COUNT, 0),
           CPUE = COUNT / AREA_SWEPT) %>%
    ungroup() 

#Compute EBS snow crab COD's by size/sex
cpue %>%
  filter(!(STATION_ID %in% corners)) %>% #exclude corner stations
  group_by(YEAR, CATEGORY) %>%
  summarise(Lat_COD = weighted.mean(LATITUDE, w = CPUE),
            Lon_COD = weighted.mean(LONGITUDE, w = CPUE),
            mean_cpue = mean(CPUE)) -> COD #add a column for mean cpue of each group in each year

#plot center of latitude
COD %>%
  ggplot(aes(x = YEAR, y = Lat_COD))+
  geom_point(size=3)+
  geom_line() +
  theme_bw() +
  facet_wrap(~CATEGORY)

#plot center of longitude
COD %>%
  ggplot(aes(x = YEAR, y = Lon_COD))+
  geom_point(size=3)+
  geom_line() +
  theme_bw() +
  facet_wrap(~CATEGORY)

#plot just mature male lat COD, our indicator
COD %>%
  filter(CATEGORY == "mature_male_centroid") %>%
  ggplot(aes(x = YEAR, y = Lat_COD))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(Lat_COD, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#COD vs. density plot
COD %>%
  filter(!CATEGORY == "population") %>%
  ggplot(aes(x = mean_cpue, y = Lat_COD, group = CATEGORY, color = CATEGORY)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm') +
  labs(x = "CPUE", y = "Centroid of Abundance") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_wrap(~CATEGORY, scales = "free")


#COD vs. bottom temperature plot
haul %>%
  filter(!HAUL_TYPE == 17) %>%
  distinct(YEAR, STATION_ID, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(summer_bt = mean(GEAR_TEMPERATURE, na.rm = T)) %>%
  right_join(COD, by="YEAR") %>%
  ggplot(aes(x = summer_bt, y = Lat_COD, group = CATEGORY, color = CATEGORY)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm') +
  labs(x = "Bottom Temperature (C)", y = "Centroid of Abundance") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_wrap(~CATEGORY, scales = "free")

#Write output for COD indicator 
COD %>%
  select(YEAR, CATEGORY, Lat_COD) %>%
  pivot_wider(names_from = "CATEGORY", values_from = "Lat_COD") %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) %>%
  write.csv(file="./Output/COD_output.csv", row.names = FALSE)
  







