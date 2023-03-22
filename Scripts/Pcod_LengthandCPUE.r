# notes ----
#Combine CPUE by Haul and Size Composition RACE survey data 
  #"CPUE by Haul" and "Size Composition by Haul" AKFIN datasets queried for Pacific Cod, EBS Shelf, 1982-2019 
#Output generated for Kerim Aydin cod consumption estimates

# Erin Fedewa
# last updated: 2020/8/26

# load ----
library(tidyverse)

## Read in Pcod CPUE data ----
pcod_cpue <- read.csv("./Data/Pcod consumption data/EBS_pcod_cpue_by_haul.csv")
head(pcod_cpue)
range(pcod_cpue$Weight..kg.) #Looks like there's zero catch hauls 
 
 #Remove hauls with no pcod and poor gear performance 
  pcod_cpue %>%
    filter(Weight..kg. > 0,
           Gear.Performance >= 0) -> pcod_cpue_nonzero
  
## Read in Pcod length data  
pcod_length <- read.csv("./Data/Pcod consumption data/EBS_pcod_length_by_haul.csv")
head(pcod_length)
nrow(pcod_length)

#Combine CPUE and Length datasets
  pcod_length %>%
    filter(Gear.Performance >= 0) %>%
    select(Haul.Join.ID, Length..mm., Frequency, Sex) %>% 
    full_join(pcod_cpue_nonzero) %>% tibble()-> pcod_cpue_length 
  
#Data check 
  nrow(pcod_length) 
  nrow(pcod_cpue_length) #New tibble has more rows? 
  
  pcod_cpue_length %>%
    filter(is.na(Length..mm.)) #Looks like 330 hauls don't have length data 

  #Non-matching haul join ID's:
pcod_cpue_nonzero[!(pcod_cpue_nonzero$Haul.Join.ID %in% pcod_length$Haul.Join.ID),] %>% as_tibble() %>%
  print(n=330)

#8/27/21 Conversation with Lyle Britt: Prior to 1993, not every species was measured every haul
  #Post-1993 missing lengths likely due to baskets thrown over in error before measurements taken 

#re-name and save output
names(pcod_cpue_length) <- c("haul_join", "fish_length", "frequency", "sex", "survey", "haul_num", "cruise_join", "catch_join", "vessel", "cruise", "haul", "start_lat",
                             "start_lon", "end_lat", "end_lon", "stratum", "stratum_area", "stratum_min_depth", "stratum_max_depth", "stratum_descr","stratum_area_name",
                             "stratum_type", "domain", "density", "gear_perf", "satisf_gear_perf", "gear_perf_note", "gear_depth", "bottom_depth", "surface_temp", "gear_temp",
                             "distance_fished", "net_width", "species_code", "common_name", "scient_name", "weight", "num_of_fish", "effort_km2", "weight_cpue", "num_cpue", "download_date")
                             
write.csv(pcod_cpue_length, file="./Output/EBS_Pcod_length_cpue.csv")
