# notes ----
#Calculate EBS snow crab temperatures of occupancy (CPUE weighted) 

# Author: Erin Fedewa

# load ----
library(tidyverse)

#To do: impute missing station temperatures for early years 

## EBS haul data 
sc_catch <- read.csv("./Data/crabhaul_opilio.csv")

#size at 50% prob of terminal molt lookup
  #we'll use this to assign male maturity by year, but b/c were missing 
  #years, we'll assign with static 83mm timeseries mean
read_csv("./Data/opilio_maturation_size.csv") %>%
  select(year, male_size_term_molt) %>%
  filter(year > 1987) %>%
  mutate(male_size_term_molt = replace_na(male_size_term_molt, 83)) %>%
  mutate(across(male_size_term_molt, round, 2)) %>%
  rename(YEAR = year) -> mat

##########################################
## compute cpue by size-sex group for each station
sc_catch %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  left_join(mat) %>%
  filter(HAUL_TYPE == 3, 
         SEX %in% 1:2,
         YEAR > 1987) %>%
  mutate(size_sex = ifelse(SEX == 1 & WIDTH_1MM < male_size_term_molt | SEX == 2 & CLUTCH_SIZE == 0, "Immature",
                           ifelse(SEX == 2 & CLUTCH_SIZE >= 1 | SEX == 1 & WIDTH_1MM >= male_size_term_molt,  "Mature", NA))) %>%
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

#now just immatures
dat3 %>%
  filter(size_sex == "Immature") %>%
  ggplot(aes(x = YEAR, y = TEMP_OCC))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(TEMP_OCC, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#Write output for Temp Occupancy indicator  
missing <- data.frame(YEAR = 2020, Immature = NA, Mature = NA)

dat3 %>%
  select(-AVG_BT) %>%
  pivot_wider(names_from = "size_sex", values_from = "TEMP_OCC") %>%
  bind_rows(missing) %>%
  arrange(YEAR) %>%
  write.csv(file="./Output/TempOcc_output.csv")


