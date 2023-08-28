# notes ----]
# Calculate abundance of immature male snow crab 50-65mmm as response for BAS analysis
  #Size range selected using BSFRF selectivity curves and St. Marie 1995 size at 
  #age estimates (~5.7-6.7 years post settlement)

#To update for 2024: Use upper size threshold as size at 50% maturity? Or does this make it too difficult to
  #assign lags with potentially multiple cohorts/year classes?

# Erin Fedewa
# last updated: 2023/8/27

# load ----
library(tidyverse)
library(ggridges)

##############################################

## EBS haul data ----
sc_catch <- read.csv("./Data/crabhaul_opilio.csv")

#EBS strata data ----
strata_sc <- read.csv("./Data/crabstrata_opilio.csv")

########################################
sc_catch %>%
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE == 3,
         SEX == 1,
         WIDTH_1MM >= 50 & WIDTH_1MM <= 65,
         YEAR >= 1982) %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT) %>%
  summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  ungroup %>%
  # compute cpue per nmi2
  mutate(cpue_cnt = ncrab / AREA_SWEPT) %>%
  # join to hauls that didn't catch crab 
  right_join(sc_catch %>% 
               mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
               filter(HAUL_TYPE ==3,
                      YEAR >= 1982) %>%
               distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
                replace_na(list(cpue_cnt = 0)) %>%
                replace_na(list(ncrab = 0)) %>%

#join to stratum
  left_join(strata_sc %>%
              select(STATION_ID, SURVEY_YEAR, STRATUM, TOTAL_AREA) %>%
              filter(SURVEY_YEAR >= 1982) %>%
              rename_all(~c("GIS_STATION", "YEAR",
                                     "STRATUM", "TOTAL_AREA"))) %>%
  #Scale to abundance by strata
  group_by(YEAR, STRATUM, TOTAL_AREA) %>%
  summarise(MEAN_CPUE = mean(cpue_cnt , na.rm = T),
            ABUNDANCE = (MEAN_CPUE * mean(TOTAL_AREA))) %>%
  group_by(YEAR) %>%
  #Sum across strata
  summarise(ABUNDANCE_MIL = sum(ABUNDANCE)/1e6) -> abundance

#Plot
ggplot(abundance, aes(y=ABUNDANCE_MIL, x=YEAR)) +
  geom_point() +
  geom_line()

#Write csv as output (abundance in millions of crab)
write.csv(abundance, file = ("./Output/BAS_response.csv"))
