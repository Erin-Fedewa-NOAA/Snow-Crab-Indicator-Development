# notes ----
# Pcod/Snow Crab Spatial Overlap in EBS and NBS 

#Follow ups: combined NBS/EBS spatial overlap metric in future iterations?
 
# Erin Fedewa
# Last Updated 8/29/2022

# load ----
library(tidyverse)
library(cowplot)

## EBS haul data 
sc_catch <- read.csv("./Data/crabhaul_opilio.csv")

#EBS strata data 
sc_strata <- read_csv("./Data/crabstrata_opilio.csv")

#NBS haul data - need updated data!!!
nbs <- read_csv("./Data/nbs_timeseries.csv")

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

##################################################
#Calculate % overlap for NBS timeseries

nbs %>%
  group_by(YEAR) %>% 
  mutate(TOTAL_STATIONS = n_distinct(STATIONID)) %>%
  filter(SPECIES_CODE %in% c("21720", "68580")) %>%
  select(YEAR, STATIONID, SPECIES_CODE, wCPUE, TOTAL_STATIONS) %>%
  mutate(SPECIES_CODE = case_when(SPECIES_CODE == 68580 ~ "CRAB",
                                  SPECIES_CODE == 21720 ~ "COD")) %>%
  group_by(YEAR, STATIONID, SPECIES_CODE) %>%
  pivot_wider(names_from = SPECIES_CODE, values_from = wCPUE) %>%
  group_by(YEAR) %>%
  # method 1 -  % of total stations that include both cod and snow crab
  # method 2 - % of positive snow crab stations that included cod
  summarise(METHOD_1 = sum((CRAB > 0 & COD > 0), na.rm = T) / mean(TOTAL_STATIONS) * 100,
            METHOD_2 = sum((CRAB > 0 & COD > 0), na.rm = T) / sum((CRAB > 0), na.rm = T) * 100) %>%
  add_row(YEAR = 2011:2016, METHOD_1 = NA, METHOD_2 = NA) -> nbs_overlap  #Add NA's for missing yrs 

#NBS Method 1 Plot
ggplot(aes(x = YEAR, y = METHOD_1), data = nbs_overlap, na.rm = T) +
  geom_point() +
  geom_line() +
  labs(y = expression(atop("NBS Snow crab Pacific cod spatial overlap (%)")), x = "")+
  theme_bw()+
  theme(panel.grid = element_blank())

#Method 2 Plot
  ggplot(aes(x = YEAR, y = METHOD_2), data = nbs_overlap, na.rm = T) +
  geom_point(color = "#E69F00", size = 6) +
  geom_line() +
  labs(y = expression(atop("Snow crab-Pacific cod spatial overlap (%)")), x = "")+
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "grey85", size = 0.2)) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=11)) -> overlap

  
