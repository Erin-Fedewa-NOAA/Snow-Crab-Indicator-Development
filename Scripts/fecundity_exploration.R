#Maturity/Fecundity data exploration

#load
library(dplyr)
library(tidyverse)

## Indicator data 
sex_ratio <- read.csv("./Output/operational_sex_ratio.csv")
clutch <- read.csv("./Output/clutch_full.csv")
male_mat <- read.csv("./Data/Indicator contributions/opilio_maturation_size.csv")

#--------------------------------------------------------------
#Calculate abundance timeseries of large males (>= 90mm CW)
ebs_haul <- read.csv("./data/crabhaul_opilio.csv")
ebs_strata <- read.csv("./data/crabstrata_opilio.csv")

ebs_haul %>%
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE == 3,
         YEAR >= 1988, 
         SEX == 1, 
         WIDTH >= 90) %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT) %>%
  summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  ungroup %>%
  # compute cpue per nmi2
  mutate(cpue_cnt = ncrab / AREA_SWEPT) %>%
  # join to hauls that didn't catch crab 
  right_join(ebs_haul %>% 
               mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
               filter(HAUL_TYPE ==3,
                      YEAR >= 1988) %>%
               distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
  replace_na(list(cpue_cnt = 0)) %>%
  replace_na(list(ncrab = 0)) %>%
  
  #join to stratum
  left_join(ebs_strata %>%
              select(STATION_ID, SURVEY_YEAR, STRATUM, TOTAL_AREA) %>%
              filter(SURVEY_YEAR >= 1988) %>%
              rename_all(~c("GIS_STATION", "YEAR",
                            "STRATUM", "TOTAL_AREA"))) %>%
  #Scale to abundance by strata
  group_by(YEAR, STRATUM, TOTAL_AREA) %>%
  summarise(MEAN_CPUE = mean(cpue_cnt , na.rm = T),
            N_CPUE = n(),
            VAR_CPUE = (var(cpue_cnt)*(TOTAL_AREA^2))/N_CPUE,
            ABUNDANCE = (MEAN_CPUE * mean(TOTAL_AREA))) %>%
  distinct() %>%
  group_by(YEAR) %>%
  #Sum across strata
  summarise(ABUNDANCE_MIL = sum(ABUNDANCE)/1e6,
            SD_CPUE = sqrt(sum(VAR_CPUE)),
            ABUNDANCE_CI = (1.96*(SD_CPUE))/1e6) ->large_male_abun

#plot
large_male_abun %>%
  ggplot(aes(x=YEAR, y=ABUNDANCE_MIL)) +
  geom_point() + 
  geom_line()

#combine all datasets --------------------------------------------
clutch %>%
  select(YEAR, Prop_full) %>%
  left_join(sex_ratio %>%
              select(YEAR, op_sex_ratio)) %>%
  left_join(male_mat %>%
              select(year, male_size_term_molt, female_mean_size_mat) %>%
              rename(YEAR = year)) %>%
  left_join(large_male_abun %>%
              select(YEAR, ABUNDANCE_MIL) %>%
              rename(large_male_abun = ABUNDANCE_MIL))-> dat

#Does male skewed sex ratio result in more full clutches? 
dat %>%
  ggplot(aes(x = op_sex_ratio, y = Prop_full, label = YEAR)) +
    geom_point() +
  geom_text(hjust=0, vjust=0)

#What about male size at maturity?
dat %>%
  ggplot(aes(x = male_size_term_molt, y = Prop_full, label = YEAR)) +
  geom_point() +
  geom_text(hjust=0, vjust=0)

#and abundance of large males 
dat %>%
  ggplot(aes(x = large_male_abun, y = Prop_full, label = YEAR)) +
  geom_point() +
  geom_text(hjust=0, vjust=0)

#Clutch fullness is a very coarse proxy for reproductive potential, and 
  #observations of decline in prop of full clutches is rare in the timeseries
  #trends really not consistent with male/sperm limitation like we see in 
  #eastern Canadian snow crab stocks 



    