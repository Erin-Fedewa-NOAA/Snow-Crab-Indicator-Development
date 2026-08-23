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
osr <- lg_male_dat %>%
  full_join(mat_fem_dat) %>%
  mutate(osr_male_female = lg_male_abund / mat_fem_abun,
         osr_female_male = mat_fem_abun / lg_male_abund) %>%
  right_join(., expand.grid(YEAR = years)) 

#plot
osr %>%
  ggplot(aes(x = YEAR, y = osr_female_male))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(osr_female_male, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#Though operational sex ratios are typically reported as male:female, given that 
  #there have historically always been more female snow crab than large males, it's much
  #easier to interpret the female:male ratio (e.g. 6 females for every 1 male)

######################################

#Write csv for indicator
indicator_osr <- osr %>%
  select(YEAR, osr_female_male) %>%
  rename_with(tolower) %>%
  complete(year = min(year):max(year)) %>%
  arrange(year) 

write.csv(indicator_osr, file="./Output/indicator_sex_ratio.csv", row.names = F)

############################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Summer_Snow_Crab_Operational_Sex_Ratio_SEBS_Survey"

##EDITABLE TEXT
description <- paste0("The ratio of mature female to large male (95 mm) snow crab abundance in the EBS")

status_trends <- paste0("The sex ratio of mature female to large male snow crab increased dramatically in 2026, 
                        reaching a time series high with 85 mature females observed for every large male. The population
                        has remained heavily female-biased in recent years due to depressed abundances of large males")

factors <- paste0("The operational sex ratio is directly influenced by the relative abundances of large males and mature females. 
                  Non-synchronous shifts in abundance between the two sexes, or a male population dominated by small mature males
                  are two mechanisms for a skewed operational sex ratio.")

implications <- paste0("A female-biased operational sex ratio suggests the possibility for sperm limitation (Baker et al., 2022), 
                       however, a high proportion of full clutches in mature females indicates that female sperm reserves are likely
                       sufficient for egg production. A female-biased sex ratio could also result in increased interspecies mating and 
                       hybridization due to snow crab mate limitation.")

references <- paste0("Baker, K. D., Mullowney, D. R. J., and Sainte-Marie, B. 2022. Large males matter: Low sperm reserves in female snow crab (Chionoecetes opilio) off Newfoundland, Canada. Fisheries Research, 253: 13.")


##INDICATOR DATA
indicator_data <- indicator_osr %>%
  rename(indicator = osr_female_male)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)

