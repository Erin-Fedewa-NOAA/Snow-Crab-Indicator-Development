# notes ----
#Calculate EBS snow crab temperatures of occupancy (CPUE weighted) 

# Last Updated 8/20/23

#Updates to incorporate for 2024: 
#Use size at 50% maturity as male maturity cutline for each year 

# load ----
library(tidyverse)

## EBS haul data 
sc_catch <- read.csv("./Data/crabhaul_opilio.csv")

## compute cpue by size-sex group for each station
sc_catch %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE == 3, 
         SEX %in% 1:2,
         YEAR > 1987) %>%
  mutate(size_sex = ifelse(SEX == 1 & WIDTH_1MM < 95 | SEX == 2 & CLUTCH_SIZE == 0, "Immature",
                           ifelse(SEX == 2 & CLUTCH_SIZE >= 1 | SEX == 1 & WIDTH_1MM >= 95,  "Mature", NA))) %>%
  group_by(YEAR, GIS_STATION, MID_LATITUDE, MID_LONGITUDE, AREA_SWEPT, GEAR_TEMPERATURE, size_sex) %>%
  summarise(num_crab = round(sum(SAMPLING_FACTOR))) %>%
  filter(!is.na(AREA_SWEPT)) %>%
  pivot_wider(names_from = size_sex, values_from = num_crab) %>%
  mutate(pop = sum(Immature, Mature, na.rm = T)) %>%
  pivot_longer(c(7:9), names_to = "size_sex", values_to = "num_crab") %>%
  filter(size_sex != "NA") %>%
  mutate(num_crab = replace_na(num_crab, 0),
         cpue = num_crab / AREA_SWEPT) %>%
  ungroup() -> cpue_long

#Temperature of Occupancy Calculations ----
  #Note that this is ignoring NA's in bottom temp data!!!
cpue_long %>%
  group_by(YEAR) %>%
  mutate(AVG_BT = mean(GEAR_TEMPERATURE, na.rm=T)) %>%
  ungroup() %>%
  group_by(YEAR, size_sex, AVG_BT) %>%
  summarise(TEMP_OCC = weighted.mean(GEAR_TEMPERATURE, w = cpue, na.rm = T)) -> dat3

#plot
dat3 %>%
  ggplot(aes(x = YEAR, y = TEMP_OCC, group= size_sex, color = size_sex))+
  geom_point(size=3)+
  geom_line() +
  theme_bw()

#Write output for Temp Occupancy indicator     
dat3 %>%
  select(-AVG_BT) %>%
  pivot_wider(names_from = "size_sex", values_from = "TEMP_OCC") %>%
  write.csv(file="./Output/TempOcc_output.csv")


