#Calculate proportion full clutches in mature female snow crab

#Author: Erin Fedewa

library(tidyverse)
library(ggridges)


##############################################

## EBS haul data ----
sc_catch <- read.csv("./Data/crabhaul_opilio.csv")

#EBS strata data ----
strata_sc <- read.csv("./Data/crabstrata_opilio.csv")

########################################
#Proportion of mature females by clutch size 
sc_catch %>%
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE == 3,
         SEX == 2,
         CLUTCH_SIZE > 0,
         YEAR >= 1988) %>%
  mutate(SHELL_CONDITION = case_when(SHELL_CONDITION %in% c(0,1,2) ~ "Primiparous",
                                     SHELL_CONDITION > 2 ~ "Multiparous"),
         CLUTCH_TEXT = case_when(CLUTCH_SIZE %in% c(1,2) ~ "Empty_trace",
                                 CLUTCH_SIZE %in% c(3,4) ~ "Quarter_half",
                                 CLUTCH_SIZE %in% c(5,6) ~ "Full")) -> female_ebs_spec
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
              filter(HAUL_TYPE == 3, SEX == 2,
                     CLUTCH_SIZE > 0,
                     YEAR >= 1988) %>% 
              mutate(CLUTCH_TEXT = "All",
                     SHELL_CONDITION = case_when(SHELL_CONDITION %in% c(0,1,2) ~ "Primiparous",
                                                 SHELL_CONDITION > 2 ~ "Multiparous")) %>%
              group_by(YEAR, GIS_STATION, AREA_SWEPT, SHELL_CONDITION, CLUTCH_TEXT) %>%
              summarise(ncrab = round(sum(SAMPLING_FACTOR,na.rm = T)))) %>%
  filter(!is.na(CLUTCH_TEXT)) %>%
  ungroup() %>%
  mutate(cpue = ncrab / AREA_SWEPT) %>%
#Join to stations that didn't catch crab 
right_join(expand_grid(SHELL_CONDITION = c("Primiparous", "Multiparous"),
                       CLUTCH_TEXT = c("Empty_trace", "Quarter_half", "Full", "All"),
                       strata_sc %>%
                         select(STATION_ID, SURVEY_YEAR, STRATUM, TOTAL_AREA) %>%
                         filter(SURVEY_YEAR >= 1988) %>%
                         rename_all(~c("GIS_STATION", "YEAR",
                                       "STRATUM", "TOTAL_AREA")))) %>%
               replace_na(list(ncrab = 0, cpue = 0)) %>%
#Scale to abundance by strata
  group_by(YEAR, STRATUM, TOTAL_AREA, SHELL_CONDITION, CLUTCH_TEXT) %>%
  summarise(MEAN_CPUE = mean(cpue , na.rm = T),
            ABUNDANCE = (MEAN_CPUE * mean(TOTAL_AREA))) %>%
  group_by(YEAR, SHELL_CONDITION, CLUTCH_TEXT) %>%
  #Sum across strata
  summarise(ABUNDANCE_MIL = sum(ABUNDANCE)/1e6) -> fem_abundance

#Calculate proportion full of primiparous/multiparous females 
fem_abundance %>%
  group_by(YEAR, SHELL_CONDITION) %>%
  summarise(Prop_full = (ABUNDANCE_MIL[CLUTCH_TEXT=="Full"]/ABUNDANCE_MIL[CLUTCH_TEXT=="All"])) -> full

#Plot
full %>%
  ggplot() +
  geom_point(aes(x= YEAR, y=Prop_full)) + 
  geom_line(aes(x= YEAR, y=Prop_full)) +
  facet_wrap(~SHELL_CONDITION) + 
  theme_bw()

#Plot primiparous females only
full %>%
  filter(SHELL_CONDITION == "Primiparous") %>%
  ggplot() +
  geom_point(aes(x= YEAR, y=Prop_full)) + 
  geom_line(aes(x= YEAR, y=Prop_full)) +
  geom_hline(aes(yintercept = mean(Prop_full, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#write csv for indicator 
missing <- data.frame(YEAR = 2020)

full %>%
  filter(SHELL_CONDITION == "Primiparous") %>%
  select(-SHELL_CONDITION) %>%
  bind_rows(missing) %>%
  arrange(YEAR) %>%
  write.csv(file="./Output/clutch_full.csv")
  




