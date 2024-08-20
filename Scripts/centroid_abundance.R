#notes ----
#Snow Crab Latitude center of abundance in EBS by size/sex category


#Last update: 8/22/23

#load----
library(tidyverse)
library(rsample)

# data ----

#Exclude corner stations
corner <- list("QP2625","ON2625","HG2019","JI2120","IH1918",
               "GF2221","HG1918","GF2019","ON2524","PO2726",
               "IH2221","GF1918","JI2221","JI2019","JI1918",
               "HG2221","QP2726","PO2423","IH2019","PO2625",
               "QP2423","IH2120","PO2524","HG2120","GF2120",
               "QP2524")

#size at 50% prob of terminal molt lookup
#we'll use this to assign male maturity by year, but b/c were missing 
#years, we'll assign with static 83mm timeseries mean
read_csv("./Data/opilio_maturation_size.csv") %>%
  select(year, male_size_term_molt) %>%
  filter(year > 1987) %>%
  mutate(male_size_term_molt = replace_na(male_size_term_molt, 83)) %>%
  mutate(across(male_size_term_molt, round, 2)) %>%
  rename(YEAR = year) -> mat

## EBS haul data 
sc_catch <- read.csv("./Data/crabhaul_opilio.csv")

#############################

## compute cpue by size-sex group for each station
sc_catch %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  left_join(mat) %>%
  filter(HAUL_TYPE != 17 , 
         SEX %in% 1:2,
         YEAR > 1987) %>%
  mutate(size_sex = ifelse(SEX == 1 & WIDTH_1MM < male_size_term_molt, "immature_male",
                           ifelse(SEX == 1 & WIDTH_1MM >= male_size_term_molt, "mature_male",
                                  ifelse(SEX == 2 & CLUTCH_SIZE >= 1, "mature_female",
                                         ifelse(SEX == 2 & CLUTCH_SIZE == 0, "immature_female", NA))))) %>%
  group_by(YEAR, GIS_STATION, MID_LATITUDE, MID_LONGITUDE, AREA_SWEPT, size_sex) %>%
  summarise(num_crab = round(sum(SAMPLING_FACTOR))) %>%
  filter(!is.na(AREA_SWEPT)) %>%
  pivot_wider(names_from = size_sex, values_from = num_crab) %>%
  mutate(pop = sum(immature_male, mature_male, immature_female, mature_female, na.rm = T)) %>%
  pivot_longer(c(6:10), names_to = "size_sex", values_to = "num_crab") %>%
  filter(size_sex != "NA") %>%
  mutate(num_crab = replace_na(num_crab, 0),
         cpue = num_crab / AREA_SWEPT) %>%
  ungroup() -> cpue_long

#Compute EBS snow crab COD's by size/sex
cpue_long %>%
  filter(!(GIS_STATION %in% corner)) %>% #exclude corner stations
  group_by(YEAR, size_sex) %>%
  summarise(Lat_COD = weighted.mean(MID_LATITUDE, w = cpue)) -> COD 

#plot
COD %>%
  select(YEAR, size_sex, Lat_COD) %>%
  ggplot(aes(x = YEAR, y = Lat_COD, group= size_sex, color = size_sex))+
  geom_point(size=3)+
  geom_line() +
  theme_bw()

#plot just mature males, our indicator
COD %>%
  select(YEAR, size_sex, Lat_COD) %>%
  filter(size_sex == "mature_male") %>%
  ggplot(aes(x = YEAR, y = Lat_COD))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(Lat_COD, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#Write output for COD indicator 
missing <- data.frame(YEAR = 2020)

COD %>%
  pivot_wider(names_from = "size_sex", values_from = "Lat_COD") %>%
  bind_rows(missing) %>%
  arrange(YEAR) %>%
  write.csv(file="./Output/COD_output.csv")
  







