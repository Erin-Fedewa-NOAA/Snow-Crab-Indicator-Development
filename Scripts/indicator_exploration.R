#Preliminary and exploratory analyses for potential snow crab indicators 
#1) Prevalence of skip molting 
#2) Fecundity/SAM/sex ratio relationships
#3_ Predator overlap

#Author: EJF

# load ----
library(tidyverse)
library(mgcv)
library(cowplot)

## Read in setup
source("./Scripts/get_crab_data.R")

##################################################################################
#Prevalence of skip molting 

#as first look, we'll just assess for female snow crab, since maturity classification
  #makes male skip molters more tricky to detect 

#calculate abundance of all immature females
all_female <- calc_bioabund(crab_data = snow,
                        species = "SNOW",
                        region = "EBS",
                        year = years,
                        crab_category = "immature_female") %>%
  mutate(all_abun = ABUNDANCE) %>%
  select(YEAR, all_abun)


# calculate abundance of just old shell immature females, under the assumption that 
  #these are skip molters
skip <- snow
skip$specimen <- skip$specimen %>%
  filter(SEX == 2,
         SHELL_CONDITION %in% (3:5),
         CLUTCH_SIZE == 0) 

skip_female <- calc_bioabund(crab_data = skip,
                        species = "SNOW",
                        region = "EBS",
                        year = years) %>%
  mutate(skip_abun = ABUNDANCE) %>%
  select(YEAR, skip_abun)

# calculate proportion skip molters
prop_skip <- all_female %>%
  full_join(skip_female) %>%
  mutate(prop_skip = (skip_abun/all_abun) *100) 
  
#plot proportion empty
prop_skip %>%
  ggplot(aes(x= YEAR, y=prop_skip)) +
  geom_point() + 
  geom_line() +
  geom_hline(aes(yintercept = mean(prop_skip, na.rm=TRUE)), linetype = 5) +
  theme_bw()
#Huh that's interesting, very low prevalence- but no idea what was going on in 1996..

#Would be worthwhile to follow up with males once cutline method is integrated into
  #crabpack
#should also look at NBS data for fun- this is where we'd expect to see the most skip
  #molters, along with biennial females (could also look at this!)

##############################################################################################
#Maturity/Fecundity data exploration

#load
library(dplyr)
library(tidyverse)

## Indicator data 
sex_ratio <- read.csv("./Output/operational_sex_ratio.csv")
clutch <- read.csv("./Output/reproductive_potential.csv")
male_mat <- read.csv("./Output/snow_SAM.csv")
temp <- read.csv("./Output/TempOcc_output.csv")

#--------------------------------------------------------------
#Calculate abundance timeseries of large males (>= 95mm CW) and mature females
lgmale_abun <- calc_bioabund(crab_data = snow,
                              species = "SNOW",
                              region = "EBS",
                              years = years,
                              sex = "male",
                              size_min = 95) %>%
  select(YEAR, ABUNDANCE) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) %>%
  mutate(ABUNDANCE = as.numeric(ABUNDANCE/1e6)) 

#Plot
lgmale_abun %>%
  ggplot(aes(x = year, y = abundance)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#mature females
matfemale_abun <- calc_bioabund(crab_data = snow,
                             species = "SNOW",
                             region = "EBS",
                             years = years,
                             crab_category = "mature_female") %>%
  select(YEAR, ABUNDANCE) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) %>%
  mutate(ABUNDANCE = as.numeric(ABUNDANCE/1e6)) 

#Plot
matfemale_abun %>%
  ggplot(aes(x = YEAR, y = ABUNDANCE)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#combine all datasets --------------------------------------------
clutch %>%
  select(YEAR, prop_empty) %>%
  left_join(sex_ratio %>%
              select(YEAR, op_sex_ratio)) %>%
  left_join(male_mat %>%
              select(YEAR, male_maturity)) %>%
  left_join(lgmale_abun %>%
              rename(large_male_abun = ABUNDANCE)) %>%
  left_join(matfemale_abun %>%
              rename(mature_female_abun = ABUNDANCE)) %>%
  left_join(temp %>%
              mutate(temp_lag = lag(temp_occ, n=1, order_by = YEAR))) -> dat

#Does male skewed sex ratio relate to proportion of empty clutches? 
dat %>%
  ggplot(aes(x = op_sex_ratio, y = prop_empty, label = YEAR)) +
  geom_point() +
  geom_text(hjust=0, vjust=0) +
  geom_smooth(method = gam, formula = y~s(x, bs = "cs"))
#interesting- some years with really high proportion empty clutches also had more male
  #skewed sex ratio

#What about male size at maturity?
dat %>%
  ggplot(aes(x = male_maturity, y = prop_empty, label = YEAR)) +
  geom_point() +
  geom_text(hjust=0, vjust=0)

#and abundance of large males 
dat %>%
  ggplot(aes(x = large_male_abun, y =  prop_empty, label = YEAR)) +
  geom_point() +
  geom_text(hjust=0, vjust=0)

#abundance of large males vrs. 50% size at terminal molt 
mod <- gam(male_maturity ~ s(large_male_abun, k = 5), 
                data = dat)

acf(resid(mod), main="acf(resid(m1))") #not terrible, but we should include AR1 term
summary(mod) 
gam.check(mod) 

dat %>%
  ggplot(aes(x = large_male_abun, y = male_maturity, label = YEAR)) +
  geom_point() +
  geom_text(hjust=0, vjust=0) +
  geom_smooth(method = gam, formula = y~s(x, bs = "cs")) + 
  theme_bw() + 
  labs(x="Large male (>95mm) abundance", y="Size at 50% maturity (mm)")

#Abundance of mature females vrs male SAM- Hyp:
  #would encourage smaller sizes at maturity if lots of females? 
dat %>%
  ggplot(aes(x = mature_female_abun, y = male_maturity, label = YEAR)) +
  geom_point() +
  geom_text(hjust=0, vjust=0) +
  geom_smooth(method = gam, formula = y~s(x, bs = "cs")) 

#juvenile temperature occupied in year prior vrs male SAM
dat %>%
  ggplot(aes(x = male_maturity, y = temp_lag, label = YEAR)) +
  geom_point() +
  geom_text(hjust=0, vjust=0) +
  geom_smooth(method = gam, formula = y~s(x, bs = "cs")) 

#need to calculate competitive male index:
  #terminal molters > 94 mm/ summ of all terminal molted males 
#also link clutch failure to recruitment lagged?
#can we also look at lagged temp (moving window) SAM relationships? 
#think about shell condition for both male/female metrics
#should also calculate temperature occupied for large, imm males only, and use 
  #moving average as representative thermal conditions?


#Clutch fullness is a very coarse proxy for reproductive potential, and 
#observations of decline in prop of full clutches is rare in the timeseries
#trends really not consistent with male/sperm limitation like we see in 
#eastern Canadian snow crab stocks 

#############################################################################################
# Pcod/Snow Crab Spatial Overlap in EBS and NBS 
#Exploratory script only- NOT a formal ESP indicator since we already have pcod 
#consumption, which is likely a better picture of predation pressure 

#Updates to incorporate for 2024: 
#Combined EBS/NBS overlap metric - need NBS cod data 
#See notes at bottom of script: incorporate cod density threshold? 
#touch base with Sam on overlap metric? 

## EBS haul data 
sc_catch <- read.csv("./Data/crabhaul_opilio.csv")

#EBS strata data 
sc_strata <- read_csv("./Data/crabstrata_opilio.csv")

#NBS haul data - need updated 2023 data!!!
nbs <- read_csv("./Data/crabhaul_opilio_nbs.csv")

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
#Can also switch to spatiotemporal modeling approach- building SDMs to measure overlap 

