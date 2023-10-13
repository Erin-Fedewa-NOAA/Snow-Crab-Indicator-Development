# notes ----
#Determine stations that compose average snow crab core habitat across the EBS timeseries
#Summarize benthic invert mean CPUE across years in core area  

#FIX THIS SCRIPT TO use "./Data/gf_cpue_timeseries.csv" data product instead!
  #Also repeat the same for BBRKC ESP script 
#ALSO- gf_cpue_timeseries.csv contains NBS data, so need to specify either spatial area
  #or EBS only! 

# Erin Fedewa
# last updated: 2023/9/22

#Follow ups for 2024:
  #1) revise script (i.e. data fields/names) to accommodate pulling GAP 
  #survey data directly from oracle 
  #2) Organize these into broader benthic guilds? 
  
# load ----
library(tidyverse)
library(mgcv)

## EBS haul data 
sc_catch <- read.csv("./Data/crabhaul_opilio.csv")

#EBS strata data 
sc_strata <- read_csv("./Data/crabstrata_opilio.csv")

############################
#Core Area 

#Stations sampled in each year
sc_catch %>%
  group_by(CRUISE) %>%
  summarise(num_stations = length(unique(GIS_STATION))) %>%
  print(n=60)
#Lets determine core area from standardized timeseries (post-1987)

#Calculate CPUE by station for all snow crab 
sc_catch %>%
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE == 3, 
         SEX %in% 1:2,
         YEAR > 1987) %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT,MID_LATITUDE, MID_LONGITUDE, GEAR_TEMPERATURE) %>%
  summarise(N_CRAB = sum(SAMPLING_FACTOR, na.rm = T),
            CPUE = N_CRAB / mean(AREA_SWEPT)) %>%
  #join to zero catch stations
  right_join(sc_strata %>%
               filter(SURVEY_YEAR > 1987) %>%
               distinct(SURVEY_YEAR, STATION_ID, STRATUM, TOTAL_AREA) %>%
               rename_all(~c("YEAR", "GIS_STATION", 
                             "STRATUM", "TOTAL_AREA"))) %>%
  replace_na(list(CPUE = 0)) -> cpue

#stations in 50-100 CPUE percentile range
cpue %>%
  group_by(GIS_STATION) %>%
  summarise(AVG_CPUE = mean(CPUE)) %>%
  filter(AVG_CPUE > quantile(AVG_CPUE, 0.50)) -> perc50 #187 stations
#Lets go with the 50th percentile for defining core area 

#Join lat/long back in to perc50 dataset and plot
sc_strata %>%
  filter(SURVEY_YEAR == 2021) %>% #Just selecting a yr when all stations were sampled
  select(STATION_ID, LATITUDE, LONGITUDE) %>%
  dplyr::rename(GIS_STATION = STATION_ID) %>%
  right_join(perc50) -> perc50_core

#Write csv for stations in 50th percentile of avg CPUE  
write.csv(perc50_core, file="./Output/sc_area_50perc.csv")

##################################################
#Benthic invert CPUE

#Function to import data and filter for only benthic inverts 
import <- function(filename) {
  ebs <- read_csv(filename)
  ebs %>%
    filter(SID %in% c(41201:99909))
}

#Add all bottom trawl data files
ebs82 <- import("./Data/Groundfish Catch Data/ebs1982_1984.csv")
ebs85 <- import("./Data/Groundfish Catch Data/ebs1985_1989.csv")
ebs90 <- import("./Data/Groundfish Catch Data/ebs1990_1994.csv")
ebs95 <- import("./Data/Groundfish Catch Data/ebs1995_1999.csv")
ebs00 <- import("./Data/Groundfish Catch Data/ebs2000_2004.csv")
ebs05 <- import("./Data/Groundfish Catch Data/ebs2005_2008.csv")
ebs09 <- import("./Data/Groundfish Catch Data/ebs2009_2012.csv")
ebs13 <- import("./Data/Groundfish Catch Data/ebs2013_2016.csv")
ebs17 <- import("./Data/Groundfish Catch Data/ebs2017_2018.csv")
ebs19 <- import("./Data/Groundfish Catch Data/ebs2019.csv")
ebs21 <- import("./Data/Groundfish Catch Data/ebs2021.csv")
ebs22 <- import("./Data/Groundfish Catch Data/ebs2022.csv")
ebs23 <- import("./Data/Groundfish Catch Data/ebs2023.csv")

# combine datasets and save output
bind_rows(ebs82, ebs85, ebs90, ebs95, ebs00, ebs05, ebs09, ebs13, ebs17, ebs19, ebs21, ebs22, ebs23) %>%
  write_csv("./Output/benthic_timeseries.csv")
benthic <- read_csv("./Output/benthic_timeseries.csv")

#Use core area dataset to spatially subset invert data 
sta <- read_csv("./Output/sc_area_50perc.csv")
sta %>% 
  pull(GIS_STATION) -> core

#Num of stations with catch data each yr within core habitat
benthic %>%
  filter(STATION %in% core) %>%
  group_by(YEAR) %>%
  summarise(station = length(unique(STATION))) %>%
  print(n=50)
#A few missing stations in some years- 

#Calculate mean CPUE for each guild across years 
benthic %>%
  filter(STATION %in% core, 
         YEAR >= 1988,
         !(SID %in% c(68560, 68580, 69322, 69323))) %>% #remove commercial crab species 
  mutate(thoustons = ((WTCPUE*100*1371.9616)/1000000)) %>% #Convert WTCPUE in kg/HA to thoustons/km^2
  group_by(YEAR, STATION) %>%
  summarise(Gersemia_cpue = sum(thoustons[SID %in% c(41201:41221)], na.rm = T),
            Pennatulacea_cpue = sum(thoustons[SID %in% c(42000:42999)], na.rm = T),
            Actinaria_cpue = sum(thoustons[SID %in% c(43000:43999)], na.rm = T),
            Polychaeta_cpue = sum(thoustons[SID %in% c(50000:59099)], na.rm = T),
            Barnacles_cpue = sum(thoustons[SID %in% c(65100:65211)], na.rm = T),
            Shrimps_cpue = sum(thoustons[SID %in% c(66000:66912)], na.rm = T),
            Crabs_cpue = sum(thoustons[SID %in% c(68000:69599)], na.rm = T),
            Gastropods_cpue = sum(thoustons[SID %in% c(71000:73999)], na.rm = T),
            Bivalves_cpue = sum(thoustons[SID %in% c(74000:75799)], na.rm = T),
            Asteroidea_cpue = sum(thoustons[SID %in% c(80000:82499)], na.rm = T),
            Echinoidea_cpue = sum(thoustons[SID %in% c(82500:82729)], na.rm = T),
            Ophiuroidea_cpue = sum(thoustons[SID %in% c(83000:84999)], na.rm = T),
            Holothuroidea_cpue = sum(thoustons[SID %in% c(85000:85999)], na.rm = T),
            Porifera_cpue = sum(thoustons[SID %in% c(91000:91999)], na.rm = T),
            Bryozoans_cpue = sum(thoustons[SID %in% c(95000:95499)], na.rm = T),
            Ascidians_cpue = sum(thoustons[SID %in% c(98000:99909)], na.rm = T),
            Total_Benthic_cpue = sum(thoustons[SID %in% c(41201:99909)], na.rm = T)) %>%
  group_by(YEAR) %>%
  summarise(Gersemia = mean(Gersemia_cpue),
            Pennatulacea = mean(Pennatulacea_cpue),
            Actinaria = mean(Actinaria_cpue),
            Polychaeta = mean(Polychaeta_cpue),
            Barnacles = mean(Barnacles_cpue),
            Shrimps = mean(Shrimps_cpue),
            Crabs = mean(Crabs_cpue),
            Gastropods = mean(Gastropods_cpue),
            Bivalves = mean( Bivalves_cpue),
            Asteroidea = mean(Asteroidea_cpue),
            Echinoidea = mean(Echinoidea_cpue),
            Ophiuroidea = mean(Ophiuroidea_cpue),
            Holothuroidea = mean(Holothuroidea_cpue),
            Porifera = mean(Porifera_cpue),
            Bryozoans = mean(Bryozoans_cpue),
            Ascidians = mean(Ascidians_cpue),
            Total_Benthic = mean(Total_Benthic_cpue))-> SCbenthic_timeseries

write.csv(SCbenthic_timeseries, file = "./Output/SCbenthic_timeseries.csv")

#Plots 
SCbenthic_timeseries %>%
  pivot_longer(c(2:17), names_to = "benthic_guild", values_to = "thoustons") %>%
  ggplot(aes(x = YEAR, y = thoustons, group = factor(benthic_guild)))+
  geom_point(aes(colour = benthic_guild)) +
  geom_line(aes(colour = benthic_guild)) +
  # geom_hline(aes(yintercept = mean(thoustons)), linetype = 2)+
  labs(y = "Benthic Invert CPUE (1000t/km2)", x = "") +
  theme_bw()+
  theme(panel.grid = element_blank()) 

SCbenthic_timeseries %>%
  ggplot(aes(x = YEAR, y = Total_Benthic)) +
  geom_point() +
  geom_line() +
  labs(y = "Total Benthic Invert CPUE (1000t/km2)", x = "") +
  theme_bw()+
  theme(panel.grid = element_blank()) 


