#Exploratory Plots

library(tidyverse)
library(ggridges)

##############################################

## EBS haul data ----
sc_catch <- read.csv("./Data/crabhaul_opilio.csv")

#EBS strata data ----
strata_sc <- read.csv("./Data/crabstrata_opilio.csv")

########################################
#Abundance of males not selected by fishery faceted by shell condition
sc_catch %>%
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE == 3,
         SEX == 1,
         WIDTH_1MM > 50 & WIDTH_1MM < 101,
         YEAR >= 1988) %>%
  mutate(SHELL_CONDITION = case_when(SHELL_CONDITION == 2 ~ "SC2",
                                SHELL_CONDITION == 3 ~ "SC3",
                                SHELL_CONDITION == 4 ~ "SC4",
                                SHELL_CONDITION == 5 ~ "SC5")) %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT, SHELL_CONDITION) %>%
  summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  ungroup %>%
  # compute cpue per nmi2
  mutate(cpue_cnt = ncrab / AREA_SWEPT) %>%
  # join to hauls that didn't catch crab 
  right_join(sc_catch %>% 
               mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
               filter(HAUL_TYPE ==3,
                      YEAR >= 1988) %>%
               distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
  replace_na(list(CPUE = 0)) %>%
  #join to stratum
  left_join(strata_sc %>%
              select(STATION_ID, SURVEY_YEAR, STRATUM, TOTAL_AREA) %>%
              filter(SURVEY_YEAR >= 1988) %>%
              rename_all(~c("GIS_STATION", "YEAR",
                            "STRATUM", "TOTAL_AREA"))) %>%
  filter(SHELL_CONDITION != "NA") %>%
  #Scale to abundance by strata
  group_by(YEAR, STRATUM, TOTAL_AREA, SHELL_CONDITION) %>%
  summarise(MEAN_CPUE = mean(cpue_cnt , na.rm = T),
            ABUNDANCE = (MEAN_CPUE * mean(TOTAL_AREA))) %>%
  group_by(YEAR, SHELL_CONDITION) %>%
  #Sum across strata
  summarise(ABUNDANCE_MIL = sum(ABUNDANCE)/1e6) -> abundance

abundance %>%
ggplot() +
  geom_bar(aes(x= YEAR, y=ABUNDANCE_MIL), stat='identity') + 
  facet_wrap(~SHELL_CONDITION)

############################################
#Proportion of mature females by clutch size (proxy for fecundity?)
sc_catch %>%
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE != 17,
         SEX == 2,
         CLUTCH_SIZE > 0,
         YEAR >= 1988) %>%
  mutate(SHELL_CONDITION = case_when(SHELL_CONDITION == 2 ~ "SC2",
                                     SHELL_CONDITION == 3 ~ "SC3",
                                     SHELL_CONDITION == 4 ~ "SC4",
                                     SHELL_CONDITION == 5 ~ "SC5"),
         CLUTCH_TEXT = case_when(CLUTCH_SIZE == 1 ~ "Mature Barren",
                                 CLUTCH_SIZE == 2 ~ "Trace",
                                 CLUTCH_SIZE == 3 ~ "Quarter Full",
                                 CLUTCH_SIZE == 4 ~ "Half Full",
                                 CLUTCH_SIZE == 5 ~ "Three Quarter Full",
                                 CLUTCH_SIZE == 6 ~ "Full")) -> female_ebs_spec
#Compute CPUE
female_ebs_spec %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT, SHELL_CONDITION, CLUTCH_TEXT) %>%
  summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  ungroup %>%
  # compute cpue per nmi2
  mutate(cpue = ncrab / AREA_SWEPT) %>%
  #add in data field for total mature female population
  bind_rows(sc_catch %>% 
              mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
              filter(HAUL_TYPE == 3,SEX == 2,
                     CLUTCH_SIZE > 0,
                     YEAR >= 1988) %>% 
              mutate(CLUTCH_TEXT = "All",
                     SHELL_CONDITION = case_when(SHELL_CONDITION == 2 ~ "SC2",
                                                 SHELL_CONDITION == 3 ~ "SC3",
                                                 SHELL_CONDITION == 4 ~ "SC4",
                                                 SHELL_CONDITION == 5 ~ "SC5")) %>%
              group_by(YEAR, GIS_STATION, AREA_SWEPT, SHELL_CONDITION, CLUTCH_TEXT) %>%
              summarise(ncrab = round(sum(SAMPLING_FACTOR,na.rm = T)))) %>%
  filter(!is.na(CLUTCH_TEXT)) %>%
  ungroup() %>%
  mutate(cpue = ncrab / AREA_SWEPT) %>%
#Join to stations that didn't catch crab 
right_join(sc_catch %>% 
               mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
               filter(HAUL_TYPE ==3,
                      YEAR >= 1988) %>%
               distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
               replace_na(list(cpue = 0)) -> catch
  

#join to stratum
catch %>%
  left_join(strata_sc %>%
              select(STATION_ID, SURVEY_YEAR, STRATUM, TOTAL_AREA) %>%
              filter(SURVEY_YEAR >= 1988) %>%
              rename_all(~c("GIS_STATION", "YEAR",
                            "STRATUM", "TOTAL_AREA"))) %>%
  filter(SHELL_CONDITION != "NA",
         CLUTCH_TEXT != "NA") %>%
  #Scale to abundance by strata
  group_by(YEAR, STRATUM, TOTAL_AREA, SHELL_CONDITION, CLUTCH_TEXT) %>%
  summarise(MEAN_CPUE = mean(cpue , na.rm = T),
            ABUNDANCE = (MEAN_CPUE * mean(TOTAL_AREA))) %>%
  group_by(YEAR, SHELL_CONDITION, CLUTCH_TEXT) %>%
  #Sum across strata
  summarise(ABUNDANCE_MIL = sum(ABUNDANCE)/1e6) -> fem_abundance

#Calculate proportion full  
fem_abundance %>%
  group_by(YEAR, SHELL_CONDITION) %>%
  summarise(Prop_full = (ABUNDANCE_MIL[CLUTCH_TEXT=="Full"]/ABUNDANCE_MIL[CLUTCH_TEXT=="All"])) -> full

#Plot
full %>%
  ggplot() +
  geom_point(aes(x= YEAR, y=Prop_full)) + 
  geom_line(aes(x= YEAR, y=Prop_full)) +
  facet_wrap(~SHELL_CONDITION)




