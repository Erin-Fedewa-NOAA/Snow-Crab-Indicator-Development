# notes ----
# Pcod/Snow Crab Spatial Overlap in EBS and NBS 

#Updates to incorporate for 2024: 
  #Combined EBS/NBS overlap metric - need NBS cod data 
  #See notes at bottom of script: incorporate cod density threshold? 
 
# Erin Fedewa
# Last Updated 8/29/2022

# load ----
library(tidyverse)
library(cowplot)

## EBS haul data 
sc_catch <- read.csv("./Data/crabhaul_opilio.csv")

#EBS strata data 
sc_strata <- read_csv("./Data/crabstrata_opilio.csv")

#NBS haul data - need updated 2023 data!!!
nbs <- read_csv("./Data/crabhaul_opilio_nbs.csv")

#################################
#Calculate CPUE by station for all stations that caught snow crab <60mm
  #60mm cutoff based on Kerim Aydin prey size vrs predator size plots 

sc_catch %>%
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE == 3, 
         SEX %in% 1:2,
         YEAR > 1982,
         WIDTH_1MM <= 60) %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT,MID_LATITUDE, MID_LONGITUDE, GEAR_TEMPERATURE) %>%
  summarise(N_CRAB = sum(SAMPLING_FACTOR, na.rm = T),
            CRAB_CPUE = N_CRAB / mean(AREA_SWEPT)) -> cpue

#Join positive catch crab stations to positive catch cod stations 
pred <- read_csv("./Output/pred_timeseries.csv") #see BenPred.R for source script 

cpue %>%
  full_join(pred %>%
              filter(SID == 21720,
                     YEAR > 1982) %>%
              select(YEAR, LATITUDE, LONGITUDE, STATION, NUMCPUE) %>%
              rename(GIS_STATION=STATION, MID_LATITUDE=LATITUDE, MID_LONGITUDE=LONGITUDE,
                     COD_CPUE=NUMCPUE), by=c("GIS_STATION", "YEAR")) %>% 
  group_by(YEAR) %>%
# Overlap: % of positive snow crab stations that include cod
  summarise(overlap = sum((CRAB_CPUE > 0 & COD_CPUE > 0), na.rm = T) / sum((CRAB_CPUE > 0), na.rm = T) * 100) -> overlap 

#EBS Plot 
overlap %>%
ggplot(aes(x = YEAR, y = overlap)) +
  geom_point() +
  geom_line() +
  labs(y = expression(atop("EBS Snow crab Pacific cod spatial overlap (%)")), x = "")+
  theme_bw()+
  theme(panel.grid = element_blank())

#Not a very informative indicator...almost all stations that catch cod also caught
  #<60mm snow crab. Or maybe there's a better way to think of this?
#See: https://onlinelibrary.wiley.com/doi/abs/10.1111/geb.12984
#May need to factor in size of cod and density of both species (i.e. caluculate CPUE
#at each station for crab and cod, and then use a ratio for overlap to quantify)
#Rationale is that in warm years, theres more overlap of high density cod stations-
  #need to set a cod CPUE threshold to weed out stations where only catching a few 

