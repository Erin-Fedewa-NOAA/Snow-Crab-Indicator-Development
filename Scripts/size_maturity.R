#Mean size at maturity (females) and size at 50% probability of
  #maturation (males) indicators 

#NOTE: changes to male maturity indicator reflect changes to maturity
  #cutline workflow in crabpack
#For 2026: measure shifts in male maturity as deviations (-/+) from mean maturity
  #curve? 50% proportion is a single inflection point estimate, and doesn't capture 
  #maturity dynamics across the range of sizes in the male population well

source("./scripts/get_crab_data.R")

##########################
##male size at terminal molt indicator
  #we use timeseries averages for years with missing data when classifying maturity 
  #in other indicator script, but for the raw indicator, we'll stick to NAs
get_male_maturity(species = "SNOW", 
                  region = "EBS")$model_parameters %>% 
  select(-c("A_EST", "A_SE")) %>%
  rename(MAT_SIZE = B_EST, 
         STD_ERR = B_SE) %>%
  right_join(., expand_grid(YEAR = years,
                            SPECIES = "SNOW", 
                            REGION = "EBS",
                            DISTRICT = "ALL")) -> male_mat

#plot
male_mat %>%
  ggplot(aes(x = YEAR, y = MAT_SIZE)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = mean(MAT_SIZE, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(MAT_SIZE, na.rm = TRUE) - sd(MAT_SIZE, na.rm = TRUE)), color = "green4") +
  geom_hline(aes(yintercept = mean(MAT_SIZE, na.rm = TRUE) + sd(MAT_SIZE, na.rm = TRUE)), color = "green4") +
  labs(x = "Year", y = "Male Snow Crab Size\nat 50% Maturity (mm)") +   
  xlim(min(years), max(years)) +
  theme_bw() +
  theme(legend.title = element_blank())

#Calculate female size at maturity
  #i.e. mean mature female size (new hardshell only)
snow$specimen %>%
  filter(SEX == 2 & CLUTCH_SIZE > 0 & SHELL_CONDITION == 2) %>%
  group_by(YEAR, SIZE, STATION_ID, AREA_SWEPT) %>%
  mutate(cpue = sum(SAMPLING_FACTOR) / AREA_SWEPT) %>%
  ungroup() %>%
  group_by(YEAR) %>%
  summarize(female_maturity = weighted.mean(SIZE, w = cpue)) -> female_mean_size
  
#plot
female_mean_size %>%
ggplot(aes(x = YEAR, y = female_maturity)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Mean Mature Female\nSnow Crab Size (mm)") +
  geom_hline(aes(yintercept = mean(female_maturity, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(female_maturity, na.rm = TRUE) - sd(female_maturity, na.rm = TRUE)), color = "green4") +
  geom_hline(aes(yintercept = mean(female_maturity, na.rm = TRUE) + sd(female_maturity, na.rm = TRUE)), color = "green4") +
  xlim(min(years), max(years)) +
  theme_bw()

#Save indicator output
male_mat %>%
  select(-SPECIES, -REGION, -DISTRICT) %>%
  rename(male_maturity = MAT_SIZE,
         male_std_err = STD_ERR) %>%
  left_join(female_mean_size) %>%
  write_csv("./Output/snow_SAM.csv")
