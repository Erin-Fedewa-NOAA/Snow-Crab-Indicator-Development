# notes ----
# Calculate "D95" for each size sex group in EBS:
      #area of stations that make up 95% of the cumulative cpue

#Author: Erin Fedewa

#Follow ups: 
  #how to address handful of missing stations? -currently using only stations sampled every yr
  #Plot these trends alongside abundance - is there a better metric than d95? 

# load ----
library(tidyverse)



##########################################

#Stations sampled in each year
sc_catch %>%
  group_by(CRUISE) %>%
  summarise(num_stations = length(unique(GIS_STATION))) %>%
  print(n=60)
#Lets determine core area from standardized timeseries (post-1987), though
  #noting that 1992 D95 index will not be very accurate 

## compute cpue by size-sex group for each station
sc_catch %>% 
  mutate(YEAR = as.numeric(str_extract(CRUISE, "\\d{4}"))) %>%
  left_join(mat) %>%
  filter(HAUL_TYPE == 3, 
         SEX %in% 1:2,
         YEAR > 1987) %>%
  mutate(size_sex = ifelse(SEX == 1 & WIDTH_1MM < male_size_term_molt, "immature_male",
                        ifelse(SEX == 1 & WIDTH_1MM >= male_size_term_molt, "mature_male",
                              ifelse(SEX == 2 & CLUTCH_SIZE >= 1, "mature_female",
                                      ifelse(SEX == 2 & CLUTCH_SIZE == 0, "immature_female", NA))))) %>%
  group_by(YEAR, GIS_STATION, MID_LATITUDE, MID_LONGITUDE, AREA_SWEPT, size_sex) %>%
  summarise(num_crab = round(sum(SAMPLING_FACTOR))) %>%
  filter(!is.na(AREA_SWEPT)) %>%
  pivot_wider(names_from = size_sex, values_from = num_crab) %>%
  mutate(pop = sum(immature_male, mature_male, immature_female, mature_female, na.rm = T)) %>%
  pivot_longer(c(6:10), names_to = "size_sex", values_to = "num_crab") %>%
  filter(size_sex != "NA") %>%
  mutate(num_crab = replace_na(num_crab, 0),
         cpue = num_crab / AREA_SWEPT) %>%
  ungroup() -> cpue_long

# compute D95 by each size and sex category ----
# i.e. the number of stations contributing to 95% of cumulative cpue

# function to compute D95
f_d95_est <- function(x){
  x %>%
    arrange(-cpue) %>% #sort by cpue (large:small)
    mutate(prop_cpue = cpue/sum(cpue),  #calculate the proportion of total cpue for each station
           cum_cpue = cumsum(prop_cpue)) %>%  
    filter(cum_cpue <= 0.95) %>% #T if in d95, F if not
    count() %>%
    mutate(d95 = (n + 1) * 401) %>% #add 1 station to n to push over 95%, multiply by 401 nm
    pull(d95)
}

# do the estimation
cpue_long %>%
  filter(!(GIS_STATION %in% corner)) %>% #exclude corner stations
  nest(-YEAR, -size_sex) %>%
  mutate(d95 = purrr::map_dbl(data, f_d95_est)) %>% #apply d95 function to each element 
  unnest() %>%
  group_by(YEAR, size_sex) %>%
  summarise(cpue = sum(num_crab) / sum(AREA_SWEPT), # add a column for total cpue of each group in each year
            d95 = mean(d95)) -> d95 # take 'mean' just to get one value (they are all the same)
  
#plot
d95 %>%
  select(YEAR, size_sex, d95) %>%
  ggplot(aes(x = YEAR, y = d95, group= size_sex, color = size_sex))+
  geom_point(size=3)+
  geom_line() +
  theme_bw()

#plot just mature males, our indicator
d95 %>%
  select(YEAR, size_sex, d95) %>%
  filter(size_sex == "mature_male") %>%
  ggplot(aes(x = YEAR, y = d95))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(d95, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#Write output for D95 indicator
missing <- data.frame(YEAR = 2020)

  d95 %>%
    select(-cpue) %>%
    pivot_wider(names_from = "size_sex", values_from = "d95") %>%
    bind_rows(missing) %>%
    arrange(YEAR) %>%
  write.csv(file="./Output/D95_output.csv")
  


  
