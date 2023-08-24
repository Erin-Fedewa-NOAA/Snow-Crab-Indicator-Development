# notes ----
#Summarize benthic predator and pcod mean CPUE across years in snow crab core habitat
#NOTE: sc_area_50perc.csv spatial look-up table was developed in BenthicInvert.R

# Erin Fedewa
# last updated: 2022/9/24

#Updates for 2024:
  #Use 50% SAM as cutline for male maturity in EBS
  #How to account for 2018 and 2023 different grids for NBS prevalence? 

# load ----
library(tidyverse)

# data mgmt----

#Benthic predator species guild look up table
ben <- read_csv("./Data/ForagingGuildsSource_SID.csv")
ben %>% 
  pull(Benthic_predator)%>%
  na.omit() -> benpred

#Use core area dataset to spatially subset invert data 
sta <- read_csv("./Output/sc_area_50perc.csv")
sta %>% 
  pull(GIS_STATION) -> core

#Function to import data and filter for only benthic predators
import <- function(filename) {
  ebs <- read_csv(filename)
  ebs %>%
    filter(SID %in% c(benpred))
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

# combine datasets and save output
bind_rows(ebs82, ebs85, ebs90, ebs95, ebs00, ebs05, ebs09, ebs13, ebs17, ebs19, ebs21, ebs22) %>%
  write_csv("./Output/pred_timeseries.csv")
pred <- read_csv("./Output/pred_timeseries.csv")

################################
#Num of stations with catch data each yr within core area 
pred %>%
  filter(STATION %in% core) %>%
  group_by(YEAR) %>%
  summarise(station = length(unique(STATION))) %>%
  print(n=50)
#A few missing stations in early years

#Calculate mean CPUE for each guild across years 
pred %>%
  filter(STATION %in% core) %>%
  mutate(thoustons = ((WTCPUE*100*1371.9616)/1000000)) %>% #Convert WTCPUE in kg/HA to thoustons/km^2
  group_by(YEAR, STATION) %>%
  summarise(Sab_Hal_cpue = sum(thoustons[SID %in% c(20510, 10120)], na.rm = T),
            Pcod_cpue = sum(thoustons[SID %in% c(21720, 21722)], na.rm = T),
            Skates_cpue = sum(thoustons[SID %in% c(420,435,440,455,471,472,480,460,485)], na.rm = T),
            Flatfish_cpue = sum(thoustons[SID %in% c(10220,10115,10130,10140)], na.rm = T),
            Sculpin_cpue = sum(thoustons[SID %in% c(21347,21348,21368,21370,21388,21420,21311,21315,21390,21438,21371)], na.rm = T),
            Eelpout_cpue = sum(thoustons[SID %in% c(24184, 24191, 24185)], na.rm = T),  
            Wolfish_cpue = sum(thoustons[SID %in% c(20320, 20322)], na.rm = T), 
            Octopus_cpue = sum(thoustons[SID %in% c(78010, 78012, 78403)], na.rm = T),
            Total_Pred_cpue = sum(thoustons[SID %in% benpred], na.rm = T)) %>% 
  group_by(YEAR) %>%
  summarise(Sab_Hal = mean(Sab_Hal_cpue),
            Pcod = mean(Pcod_cpue),
            Skates = mean(Skates_cpue),
            Flatfish = mean(Flatfish_cpue),
            Sculpin = mean(Sculpin_cpue),
            Eelpout = mean(Eelpout_cpue),
            Wolfish = mean(Wolfish_cpue),
            Octopus = mean(Octopus_cpue),
            Total_Pred = mean(Total_Pred_cpue)) -> SCpred_timeseries
write.csv(SCpred_timeseries, file = "./Output/SCpred_timeseries.csv")

#Plots 
SCpred_timeseries %>%
  pivot_longer(c(2:8), names_to = "pred_guild", values_to = "thoustons") %>%
  ggplot(aes(x = YEAR, y = thoustons, group = factor(pred_guild)))+
  geom_point(aes(colour = pred_guild)) +
  geom_line(aes(colour = pred_guild)) +
  labs(y = "Benthic Predator CPUE (1000t/km2)", x = "") +
  xlim(1980, 2022) +
  theme_bw() +
  theme(legend.title=element_blank())

SCpred_timeseries %>%
  ggplot(aes(x = YEAR, y = Total_Pred)) +
  geom_point() +
  geom_line()+
  labs(y = "Benthic Predator CPUE (1000t/km2)", x = "") +
  theme_bw()+
  theme(panel.grid = element_blank()) 

#Just Pcod Plot
SCpred_timeseries %>%
  ggplot(aes(x = YEAR, y = Pcod)) +
  geom_point() +
  geom_line()+
  labs(y = "Pacific Cod CPUE (1000t/km2)", x = "") +
  theme_bw()





