# Calculate "D95" for each size sex group in EBS:
      #area of stations that make up 95% of the cumulative cpue

#Author: Erin Fedewa

#Follow ups: move to a spatiotemporal model/interpolation approach 
 

# load ----
library(tidyverse)

## Read in setup
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

##########################################
#Assign maturity to specimen data; calculate CPUE
cpue <- snow$specimen %>% 
  left_join(., mat_size) %>%
  mutate(CATEGORY = case_when((SEX == 1 & SIZE >= MAT_SIZE) ~ "mature_male",
                              (SEX == 2 & CLUTCH_SIZE >= 1) ~ "mature_female",
                              (SEX == 2 & CLUTCH_SIZE == 0 | SEX == 1 & SIZE < MAT_SIZE) ~ "immature",
                              TRUE ~ NA)) %>%
  filter(YEAR %in% years,
         !is.na(CATEGORY)) %>%
  group_by(YEAR, STATION_ID, LATITUDE, LONGITUDE, AREA_SWEPT, CATEGORY) %>%
  summarise(COUNT = round(sum(SAMPLING_FACTOR))) %>%
  pivot_wider(names_from = CATEGORY, values_from = COUNT) %>%
  mutate(population = sum(immature, mature_male, mature_female, na.rm = T)) %>%
  pivot_longer(c(6:9), names_to = "CATEGORY", values_to = "COUNT") %>%
  filter(CATEGORY != "NA") %>%
  mutate(COUNT = replace_na(COUNT, 0),
         CPUE = COUNT / AREA_SWEPT) %>%
  ungroup() 

# compute D95 by each size and sex category ----
# i.e. the number of stations contributing to 95% of cumulative cpue

# function to compute D95
f_d95_est <- function(x){
  x %>%
    arrange(-CPUE) %>% #sort by cpue (large:small)
    mutate(prop_cpue = CPUE/sum(CPUE),  #calculate the proportion of total cpue for each station
           cum_cpue = cumsum(prop_cpue)) %>%  
    filter(cum_cpue <= 0.95) %>% #T if in d95, F if not
    count() %>%
    mutate(d95 = (n + 1) * 401) %>% #add 1 station to n to push over 95%, multiply by 401 nm
    pull(d95)
}

# do the estimation
cpue %>%
  filter(!(STATION_ID %in% corners)) %>% #exclude corner stations
  nest(data = c(-YEAR, -CATEGORY)) %>%
  mutate(d95 = purrr::map_dbl(data, f_d95_est)) %>% #apply d95 function to each element 
  unnest(cols = c(data)) %>%
  group_by(YEAR, CATEGORY) %>%
  summarise(mean_cpue = mean(CPUE), # add a column for mean cpue of each group in each year
            d95 = mean(d95), .groups = "drop") -> d95 # take 'mean' just to get one value (they are all the same)
  
#plot by size/sex
d95 %>%
  select(YEAR, CATEGORY, d95) %>%
  ggplot(aes(x = YEAR, y = d95, group= CATEGORY, color = CATEGORY))+
  geom_point(size=3)+
  geom_line() +
  theme_bw() +
  facet_wrap(~CATEGORY)

#plot just mature males, our indicator
d95 %>%
  select(YEAR, CATEGORY, d95) %>%
  filter(CATEGORY == "mature_male") %>%
  ggplot(aes(x = YEAR, y = d95))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(d95, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#d95 vs. abund plot
d95 %>%
  filter(!CATEGORY == "population") %>%
ggplot(aes(x = mean_cpue, y = d95, group = CATEGORY, color = CATEGORY)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm') +
  labs(x = "CPUE", y = expression("Area Occupied ("~nmi^2~")")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_wrap(~CATEGORY, scales = "free")
#interesting male vrs female relationship!

#d95 vs. bottom temperature plot
haul %>%
  filter(!HAUL_TYPE == 17) %>%
  distinct(YEAR, STATION_ID, GEAR_TEMPERATURE) %>%
  group_by(YEAR) %>%
  summarise(summer_bt = mean(GEAR_TEMPERATURE, na.rm = T)) %>%
  right_join(d95, by="YEAR") %>%
ggplot(aes(x = summer_bt, y = d95, group = CATEGORY, color = CATEGORY)) +
  geom_point() +
  # geom_line() +
  geom_smooth(method = 'lm') +
  labs(x = "Bottom Temperature (C)", y = expression("Area Occupied ("~nmi^2~")")) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_wrap(~CATEGORY, scales = "free")

#############################################

#Write output for D95 indicator
indicator_d95 <- d95 %>%
  filter(CATEGORY == "mature_male") %>%
    select(YEAR, d95) %>%
    rename_with(tolower) %>%
    complete(year = seq(min(year), max(year))) %>%
    arrange(year)
  
write.csv(indicator_d95, file="./Output/indicator_d95.csv", row.names = F)
  
############################################
  
#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:
  
#Indicator name
indicator_name <- "Summer_Snow_Crab_Male_Area_Occupied_SEBS_Survey"
  
##EDITABLE TEXT
description <- paste0("The minimum area containing 95% of the cumulative mature male snow crab CPUE during the EBS 
                        summer bottom trawl survey. Proposed sign of the relationship is positive. ")
  
status_trends <- paste0("The spatial extent of mature male snow crab increased from 2025 to 2026 and is slightly
                        below the time series mean.")
  
factors <- paste0("The spatial extent of snow crab in the EBS contracts in response to warmer bottom temperatures 
                    and a smaller cold pool extent (Fedewa et al., 2021). Spatial extent is also influenced by snow 
                    crab abundance due to density dependent range contraction (Murphy et al., 2010).")
  
implications <- paste0("Declines in the spatial extent of mature male snow crab can result in density-dependent prey
                         limitation and starvation-induced mortality, which are exacerbated in warm temperatures due to 
                         increased metabolic demand (Szuwalski et al., 2023). Dramatic declines in the spatial extent of
                         mature males preceded the 2019-2021 population collapse, emphasizing the importance of this 
                         indicator as a potential red flag. ")
  
references <- paste0("Fedewa, E. J., Jackson, T. M., Richar, J. I., Gardner, J. L., and Litzow, M. A. 2020. Recent shifts in northern Bering Sea snow crab (Chionoecetes opilio) size structure and the potential role of climate-mediated range contraction. Deep Sea Research Part II: Topical Studies in Oceanography, 181-182: 104878.
Murphy, J. T., Hallowed, A. B., and Anderson, J. J. 2010. Snow crab spatial distributions: Examination of density-dependent and independent processes. In Biology and Management of Exploited Crab Populations under Climate Change, 25th edn, pp. 49-79. Ed. by G. H. Kruse, G. L. Eckert, R. J. Foy, R. N. Lipcius, B. Sainte-Marie, D. L. Stram, and D. Woodby. Alaska Sea Grant College Program, University of Alaska Fairbanks, Anchorage, AK. 
Szuwalski, C. S., Aydin, K., Fedewa, E. J., Garber-Yonts, B., and Litzow, M. A. 2023. The collapse of eastern Bering Sea snow crab. Science, 382: 306-310.")
  
  
##INDICATOR DATA
indicator_data <- indicator_d95 %>%
    rename(indicator = d95)
  
#CREATE TEXT FILE
create_indicator_file(
    indicator_name = indicator_name,
    indicator_data = indicator_data,
    description = description,
    status_trends = status_trends,
    factors = factors,
    implications = implications,
    references = references)

  
