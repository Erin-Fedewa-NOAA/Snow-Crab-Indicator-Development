#Calculate the proportion of large males to mature females: i.e. operational sex ratio
  #Indicator of reproductive capacity/potential for sperm limitation


# Erin Fedewa

# load ----
library(tidyverse)
library(ggridges)

#This script needs some cleaning up next year- very redundant! 

##############################################
#Opilio haul data 
crab_ebs <- read.csv("./Data/crabhaul_opilio.csv")
#Opilio strata data 
strata_ebs <- read_csv("./Data/crabstrata_opilio.csv")


########################################
## compute abundance timeseries for mature females 
crab_ebs %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE == 3, 
         YEAR > 1987) %>%
  mutate(size_sex = ifelse(SEX == 2 & CLUTCH_SIZE >= 1,  "Mature_female", NA)) %>%
  filter(!is.na(size_sex)) %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT) %>%
  summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  ungroup %>%
  # compute cpue per nmi2
  mutate(cpue_cnt = ncrab / AREA_SWEPT) %>%
  # join to hauls that didn't catch crab 
  right_join(crab_ebs %>% 
               mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
               filter(HAUL_TYPE ==3,
                      YEAR > 1987) %>%
               distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
  replace_na(list(cpue_cnt = 0)) %>%
  replace_na(list(ncrab = 0)) %>%
  
  #join to stratum
  left_join(strata_ebs %>%
              select(STATION_ID, SURVEY_YEAR, STRATUM, TOTAL_AREA) %>%
              filter(SURVEY_YEAR > 1987) %>%
              rename_all(~c("GIS_STATION", "YEAR",
                            "STRATUM", "TOTAL_AREA"))) %>%
  #Scale to abundance by strata
  group_by(YEAR, STRATUM, TOTAL_AREA) %>%
  summarise(MEAN_CPUE = mean(cpue_cnt , na.rm = T),
            ABUNDANCE = (MEAN_CPUE * mean(TOTAL_AREA))) %>%
  group_by(YEAR) %>%
  #Sum across strata
  summarise(ABUNDANCE_MIL = sum(ABUNDANCE)/1e6) %>%
  mutate(size_sex = "mature_female") -> mat_fem_abundance

###############

## Now compute abundance timeseries for large males 
crab_ebs %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  filter(HAUL_TYPE == 3, 
         YEAR > 1987) %>%
  mutate(size_sex = ifelse(SEX == 1 & WIDTH > 95,  "Large_male", NA)) %>%
  filter(!is.na(size_sex)) %>%
  group_by(YEAR, GIS_STATION, AREA_SWEPT) %>%
  summarise(ncrab = sum(SAMPLING_FACTOR, na.rm = T)) %>%
  ungroup %>%
  # compute cpue per nmi2
  mutate(cpue_cnt = ncrab / AREA_SWEPT) %>%
  # join to hauls that didn't catch crab 
  right_join(crab_ebs %>% 
               mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
               filter(HAUL_TYPE ==3,
                      YEAR > 1987) %>%
               distinct(YEAR, GIS_STATION, AREA_SWEPT)) %>%
  replace_na(list(cpue_cnt = 0)) %>%
  replace_na(list(ncrab = 0)) %>%
  
  #join to stratum
  left_join(strata_ebs %>%
              select(STATION_ID, SURVEY_YEAR, STRATUM, TOTAL_AREA) %>%
              filter(SURVEY_YEAR > 1987) %>%
              rename_all(~c("GIS_STATION", "YEAR",
                            "STRATUM", "TOTAL_AREA"))) %>%
  #Scale to abundance by strata
  group_by(YEAR, STRATUM, TOTAL_AREA) %>%
  summarise(MEAN_CPUE = mean(cpue_cnt , na.rm = T),
            ABUNDANCE = (MEAN_CPUE * mean(TOTAL_AREA))) %>%
  group_by(YEAR) %>%
  #Sum across strata
  summarise(ABUNDANCE_MIL = sum(ABUNDANCE)/1e6) %>%
  mutate(size_sex = "large_male") -> lg_male_abundance

#combine datasets and calculate ratio
missing <- data.frame(YEAR = 2020)

lg_male_abundance %>%
  full_join(mat_fem_abundance) %>%
#and calculate proportion 
group_by(YEAR) %>%
  mutate(op_sex_ratio = ABUNDANCE_MIL[size_sex == "large_male"]/ABUNDANCE_MIL[size_sex == "mature_female"]) %>%
  select(-ABUNDANCE_MIL, -size_sex) %>%
  group_by(YEAR) %>%
  summarise(op_sex_ratio = mean(op_sex_ratio)) %>%
  bind_rows(missing) %>%
  arrange(YEAR) -> osr

#plot
osr %>%
  ggplot(aes(x = YEAR, y = op_sex_ratio))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(op_sex_ratio, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#Write csv for indicator 
write.csv(osr, file="./Output/operational_sex_ratio.csv")



