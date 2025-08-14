# notes ----
# Generate avg bottom temp, and cold pool extent indices from EBS BT timeseries 
#Arctic Oscillation is pulled from NOAA-NWS via:
#https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/ao.shtml

#TO DO: Impute missing temperatures (date correction added in 2025)

# Erin Fedewa

# load ----
library(tidyverse)
library(mgcv)

## Read in setup
source("./Scripts/get_crab_data.R")

#########################################################
#Num of stations with data each yr 
haul %>%
  group_by(YEAR) %>%
  summarise(station = length(unique(STATION_ID))) %>%
  print(n=50)
#Missing stations in early years-lets pull 1988+, though still 
  #missing data that should be interpolated/imputed 

# compute mean summer bottom temperature
haul %>%
  mutate(julian=yday(parse_date_time(START_DATE, "ymd", "US/Alaska"))) %>%
  filter(YEAR > 1987,
         !HAUL_TYPE == 17) %>%
  distinct(YEAR, STATION_ID, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(summer_bt = mean(GEAR_TEMPERATURE, na.rm = T)) -> avg_bt

#Plot
avg_bt %>%
  ggplot(aes(x = as.numeric(YEAR), y = summer_bt)) +
  geom_point() +
  geom_line()+
  labs(y = "Bottom temperature (C)", x = "") +
  theme_bw()

#Mean date sampled 
haul %>%
  mutate(julian=yday(parse_date_time(START_DATE, "ymd", "US/Alaska"))) %>%
  group_by(YEAR) %>%
  summarise(mean_date = mean(julian, na.rm=T),
            min_date = min(julian, na.rm=T),
            max_date = max(julian, na.rm=T))

#plot date sampled 
haul %>%
  mutate(julian=yday(parse_date_time(START_DATE, "ymd", "US/Alaska"))) %>%
  ggplot(aes(julian)) +
  geom_histogram(bins = 12, fill = "dark grey", color = "black") +
  facet_wrap(~YEAR)  

#Use GAM to "date correct" average bottom temperatures 
haul %>%
  filter(YEAR > 1987) %>%
  mutate(julian=yday(parse_date_time(START_DATE, "ymd", "US/Alaska")),
         YEAR = as.factor(YEAR)) %>%
  filter(!HAUL_TYPE == 17) -> temp.dat

temp.mod <- gam(GEAR_TEMPERATURE ~ s(julian, k = 5) + YEAR, 
                data = temp.dat)

summary(temp.mod) 
gam.check(temp.mod)  

#Back transform and extract year coefficient (1988 is our intercept)
c(coef(temp.mod)[1], coef(temp.mod)[1] + coef(temp.mod)[2:37]) -> est

year <- data.frame(YEAR = c(1988:2019, 2021:2025))
cbind(est,year) -> dat.2
as_tibble(dat.2) %>%
  rename(date_corrected_temp = est) -> date_temp

#Plot date corrected temp
ggplot(date_temp, aes(year, date_corrected_temp)) +
  geom_point() +
  geom_line()

#and plot both together 
date_temp %>%
  full_join(avg_bt) %>%
  pivot_longer(cols = c(1,3), names_to="index" , values_to = "avg_temp") %>%
  ggplot(aes(YEAR, avg_temp, group=index, color=index)) +
  geom_point() +
  geom_line()

########################################################
#compute cold pool areal extent
haul %>%
  filter(YEAR >= 1988,
         HAUL_TYPE != 17,
         !(STATION_ID %in% corners)) %>%
  distinct(YEAR, STATION_ID, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(extent_coldpool = sum(GEAR_TEMPERATURE < 2, na.rm = T) * 401) -> cpa

#Plot
cpa %>%
  ggplot(aes(x = as.numeric(YEAR), y = extent_coldpool)) +
  geom_point() +
  geom_line()+
  labs(y = "Cold Pool Extent (nmi2)", x = "") +
  geom_hline(aes(yintercept = mean(extent_coldpool, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#And now extent of waters <0C since that's likely a more important 
  #threshold for snow crab 
haul %>%
  filter(YEAR >= 1988,
         HAUL_TYPE != 17,
         !(STATION_ID %in% corners)) %>%
  distinct(YEAR, STATION_ID, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(extent_0C = sum(GEAR_TEMPERATURE <= 0, na.rm = T) * 401) -> zero

#and plot both together 
cpa %>%
  full_join(zero) %>%
  pivot_longer(cols = c(2:3), names_to="index" , values_to = "extent") %>%
  ggplot(aes(YEAR, extent, group=index, color=index)) +
  geom_point() +
  geom_line()

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
date_temp %>%
  full_join(avg_bt) %>%
  full_join(zero) %>%
  full_join(cpa) %>%
  full_join(mean_AO) %>% 
  arrange(YEAR) -> env
write_csv(env, "./Output/environmental_timeseries.csv")








