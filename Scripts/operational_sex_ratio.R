#Calculate the proportion of large males to mature females: i.e. operational sex ratio
  #Indicator of reproductive capacity/potential for sperm limitation

# Erin Fedewa

# load ----
library(tidyverse)
library(ggridges)

## Read in setup
source("./Scripts/get_crab_data.R")

########################################
## compute abundance timeseries for mature females 
mat_fem <- snow
mat_fem$specimen <- mat_fem$specimen %>%
  filter(SEX == 2,
         CLUTCH_SIZE > 0)

mat_fem_dat <- calc_bioabund(crab_data = mat_fem,
                              species = "SNOW",
                              region = "EBS",
                              year = years,
                              spatial_level = "region") %>%
  mutate(mat_fem_abun = ABUNDANCE) %>%
  select(YEAR, mat_fem_abun)

## Now compute abundance for large males 
lg_male <- snow
lg_male$specimen <- lg_male$specimen %>%
  filter(SEX == 1,
         SIZE > 95)

lg_male_dat <- calc_bioabund(crab_data = lg_male,
                               species = "SNOW",
                               region = "EBS",
                               year = years,
                               spatial_level = "region") %>%
  mutate(lg_male_abund = ABUNDANCE) %>%
  select(YEAR, lg_male_abund)

#combine datasets and calculate ratio
lg_male_dat %>%
  full_join(mat_fem_dat) %>%
  mutate(op_sex_ratio = lg_male_abund/mat_fem_abun) %>%
  right_join(., expand.grid(YEAR = years)) -> osr

#plot
osr %>%
  ggplot(aes(x = YEAR, y = op_sex_ratio))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(op_sex_ratio, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#Write csv for indicator
osr %>%
  select(YEAR, op_sex_ratio) %>%
  arrange(YEAR) %>%
write.csv(file="./Output/operational_sex_ratio.csv", row.names = F)



