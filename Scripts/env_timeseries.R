# notes ----
# Generate avg bottom temp, and cold pool extent indices from EBS BT timeseries 
#Arctic Oscillation is pulled from NOAA-NWS via:
#https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/ao.shtml

#TO DO: Date correct and impute missing temperatures 

# Erin Fedewa

# load ----
library(tidyverse)

#Corner station look up table for cold pool extent calculations 
corner <- c("QP2625","ON2625","HG2019","JI2120","IH1918",
            "GF2221","HG1918","GF2019","ON2524","PO2726",
            "IH2221","GF1918","JI2221","JI2019","JI1918",
            "HG2221","QP2726","PO2423","IH2019","PO2625",
            "QP2423","IH2120","PO2524","HG2120","GF2120",
            "QP2524")

# BT survey data ----
temp <- read_csv("./Data/crabhaul_opilio.csv") 

#Num of stations with data each yr 
temp %>%
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  group_by(YEAR) %>%
  summarise(station = length(unique(GIS_STATION))) %>%
  print(n=50)
#Missing stations in early years-lets pull 1988+, though still 
  #missing data that should be interpolated/imputed 

# compute mean summer bottom temperature
temp %>%
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  filter(YEAR >= 1988,
         HAUL_TYPE ==3) %>%
  distinct(YEAR, GIS_STATION, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(summer_bt = mean(GEAR_TEMPERATURE, na.rm = T))-> avg_bt

#Plot
avg_bt %>%
  ggplot(aes(x = as.numeric(YEAR), y = summer_bt)) +
  geom_point() +
  geom_line()+
  labs(y = "Bottom temperature (C)", x = "") +
  theme_bw()

#compute cold pool areal extent
temp %>%
  mutate(YEAR = str_extract(CRUISE, "\\d{4}")) %>%
  filter(YEAR >= 1988,
         HAUL_TYPE == 3,
         !(GIS_STATION %in% corner)) %>%
  distinct(YEAR, GIS_STATION, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(cpa = sum(GEAR_TEMPERATURE < 2, na.rm = T) * 401) -> cpa

#Plot
cpa %>%
  ggplot(aes(x = as.numeric(YEAR), y = cpa)) +
  geom_point() +
  geom_line()+
  labs(y = "Cold Pool Extent (nmi2)", x = "") +
  geom_hline(aes(yintercept = mean(cpa, na.rm=TRUE)), linetype = 5) +
  theme_bw()

###########################################
#Arctic Oscillation

AO<- read_csv("./Data/Arctic_oscillation.csv")

#Mean Winter Arctic Oscillation
AO %>% 
  pivot_longer(c(2:13), names_to="Month", values_to="Index") %>%
  filter(Year >= 1979,
         Month %in% c(1,2,3)) %>% 
  group_by(Year) %>%
  rename(., YEAR = Year) %>%
  summarize(Mean_AO = mean(Index)) -> mean_AO

#Plot
mean_AO %>%
  ggplot(aes(x = as.numeric(YEAR), y = Mean_AO)) +
  geom_point() +
  geom_line()+
  labs(y = "Arctic Oscillation Index", x = "") +
  geom_hline(aes(yintercept = mean(Mean_AO, na.rm=TRUE)), linetype = 5) +
  theme_bw()

# combine indices and save output
avg_bt %>%
  full_join(cpa) %>%
  full_join(mean_AO %>%
              mutate(YEAR = as.character(YEAR))) %>%
  arrange(YEAR) ->env
write_csv(env, "./Output/environmental_timeseries.csv")








