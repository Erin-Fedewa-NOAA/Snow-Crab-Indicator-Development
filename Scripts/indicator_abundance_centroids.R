#Calculate snow crab Latitude center of abundance in EBS by size/sex category

#Author: Erin Fedewa

#Follow ups: 
#move toward spatiotemporal modeling approach to account for changing footprint

# load ----
library(tidyverse)

## Read in setup
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

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
            mean_cpue = mean(CPUE), .groups = "drop") -> COD #add a column for mean cpue of each group in each year

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

##################################################

#Write output for COD indicator 
indicator_centroid <- COD %>%
  filter(CATEGORY == "mature_male_centroid") %>%
  select(YEAR, Lat_COD) %>%
  rename_with(tolower) %>%
  complete(year = seq(min(year), max(year))) %>%
  arrange(year)

write.csv(indicator_centroid, file="./Output/indicator_centroid.csv", row.names = FALSE)
  
############################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Summer_Snow_Crab_Male_Center_Distribution_SEBS_Survey"

##EDITABLE TEXT
description <- paste0("CPUE-weighted average latitude of the mature male snow crab stock during the EBS summer bottom 
                      trawl survey. Proposed sign of the relationship is positive.")

status_trends <- paste0("Mature male center of abundance changed very little from 2025 to 2026, and remains
                        near the time series mean.")

factors <- paste0("Historically, centroids of abundance have tracked bottom temperatures, and were further south 
                  with colder temperatures (Orensanz et al., 2005).")

implications <- paste0("Centroids of abundance are expected to shift north under warming as snow crab track preferred 
                       cold-water habitat. This may have implications for availability to the snow crab fishery and 
                       NOAA bottom trawl survey.")

references <- paste0("Orensanz, J., Ernst, B., Armstrong, D., Stabeno, P., and Livingston, P. 2005. Contraction of the geographic range of distribution of snow crab (Chionoecetes opilio) in the Eastern Bering Sea: An environmental ratchet? Reports of California Cooperative Oceanic Fisheries Investigations, 45: 65-79.")


##INDICATOR DATA
indicator_data <- indicator_centroid %>%
  rename(indicator = lat_cod)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)






