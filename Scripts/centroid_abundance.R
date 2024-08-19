#notes ----
#Snow Crab Latitude center of abundance in EBS by size/sex category

#Updates to incorporate for 2024: 
  #Use size at 50% maturity as male maturity cutline for each year 

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

## EBS haul data 
sc_catch <- read.csv("./Data/crabhaul_opilio.csv")

## compute cpue by size-sex group for each station
sc_catch %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE != 17 , 
         SEX %in% 1:2,
         YEAR > 1987) %>%
  mutate(size_sex = ifelse(SEX == 1 & WIDTH_1MM < 95, "immature_male",
                           ifelse(SEX == 1 & WIDTH_1MM >= 95, "mature_male",
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

#Write output for COD indicator     
COD %>%
  pivot_wider(names_from = "size_sex", values_from = "Lat_COD") %>%
  write.csv(file="./Output/COD_output.csv")
  







