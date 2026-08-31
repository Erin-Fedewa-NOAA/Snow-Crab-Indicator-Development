#Develop snow crab recruit/pre-recruit estimates from EBS BT survey to use as 
  #response variables for indicator analysis 
#compare this to immature-only recruitment estimates from new SAP
  #maturity workflow (Ryznar et al): timeseries mean 50% SAM used for 
  #years with missing chela data

#Author: EJF

# load ----
library(tidyverse)

## Read in setup
source("./Scripts/get_crab_data.R")

#-----------------------------------------------#
#Recruitment estimates- survey abundance
#-----------------------------------------------#

#Snow crab recruitment: 
  #i.e. survey-derived abundance of 65-80mm CW male snow crab
  #(~6.7-7.7 years post settlement, 1-2 molts from terminal)

recruit_abun <- calc_bioabund(crab_data = snow,
                              species = "SNOW",
                              region = "EBS",
                              years = years,
                              sex = "male",
                              size_min = 65,
                              size_max = 80,
                              shell_condition = "new_hardshell") %>%
  select(YEAR, ABUNDANCE) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) %>%
  mutate(recruit_abun = as.numeric(ABUNDANCE/1e6)) %>%
  rename_with(tolower) %>%
  select(-abundance)

#Plot
recruit_abun %>%
  ggplot(aes(x = year, y = recruit_abun)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#Pre-recruit abundance: 
  #i.e. survey-derived abundance of 50-65mm CW male snow crab
  #(~5.6-6.7 years post settlement)

prerecruit_abun <- calc_bioabund(crab_data = snow,
                                 species = "SNOW",
                                 region = "EBS",
                                 years = years,
                                 sex = "male",
                                 size_min = 50,
                                 size_max = 65,
                                 shell_condition = "new_hardshell") %>%
  select(YEAR, ABUNDANCE) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) %>%
  mutate(prerecruit_abun = as.numeric(ABUNDANCE/1e6)) %>%
  rename_with(tolower) %>%
  select(-abundance)

#Plot
prerecruit_abun %>%
  ggplot(aes(x = year, y = prerecruit_abun)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#Juvenile abundance: 
#i.e. survey-derived abundance of 40-50mm CW male snow crab
#(~4.5-5.7 years post settlement)

juvenile_abun <- calc_bioabund(crab_data = snow,
                                 species = "SNOW",
                                 region = "EBS",
                                 years = years,
                                 sex = "male",
                                 size_min = 40,
                                 size_max = 50,
                                 shell_condition = "new_hardshell") %>%
  select(YEAR, ABUNDANCE) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) %>%
  mutate(juvenile_abun = as.numeric(ABUNDANCE/1e6)) %>%
  rename_with(tolower) %>%
  select(-abundance)

#Plot
juvenile_abun %>%
  ggplot(aes(x = year, y = juvenile_abun)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#----------------------------------------------------#
#Recruitment estimates- model based 
#----------------------------------------------------#

#This output is 40-50mm immature male snow crab abundance, 
  #estimated with sdmTMB maturity workflow

juvenile_abun_mod <- read.csv("./Output/response_4050immabund_model.csv") %>%
  rename_with(tolower) %>%
  mutate(juvenile_abun_mod = as.numeric(abundance/1e6)) %>%
  select(juvenile_abun_mod, year)

#combine with design-based recruitment timeseries from above
recruit_dat <- recruit_abun %>%
  full_join(prerecruit_abun) %>%
  full_join(juvenile_abun) %>%
  full_join(juvenile_abun_mod) 
  
#plot 40-50mm males via both workflows
recruit_dat %>%
  pivot_longer(-c(year), names_to = "method", values_to = "abundance") %>%
  filter(method %in% c("juvenile_abun", "juvenile_abun_mod")) %>%
  ggplot(aes(x = year, y = abundance, group=method)) +
  geom_point(aes(color=method)) +
  geom_line(aes(color=method))+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()
  
#these are very similar but we'll use model output of 40-50mm males
  #as recruitment response for DSEM indicator analysis

#-----------------------------------------------------------------#
#Compare model-based recruitment estimates to assessment output 
#-----------------------------------------------------------------#

#read in recruitment output from repo:
url <- "https://raw.githubusercontent.com/szuwalski/snow_sept/7846de93d38c2f3536981ab7acb7b655028a2157/Models/26_gmacs_update_newmat_plus_group/gmacs.rep1"

gmacs <- readLines(url)

recruitment <- gmacs %>%
  .[seq(which(. == "R_y: dataframe") + 2,
    which(. == "R_y: dataframe") + 45)] %>%
  tibble(line = .) %>%
  separate_wider_regex(line,
    patterns = c(year = "\\d{4}", "\\s+",
      recruitment = "[-+]?\\d*\\.?\\d+")) %>%
  mutate(year = as.integer(year),
    recruitment = as.numeric(recruitment))
#I think this is extracting what we want, but would be good to verify 

#plot
recruitment %>%
  ggplot(aes(year, recruitment)) +
  geom_point() + 
  geom_line()

#join to 40-50mm abundance and compare 
compare_dat <- recruitment %>%
  filter(year > 1987) %>%
  mutate(recruitment = recruitment / 1e3) %>%
  rename(recruitment_mod_output = recruitment) %>%
  full_join(juvenile_abun_mod)

#plot time series, although magnitude is very different 
compare_dat %>%
  pivot_longer(cols= -year, names_to = "dataset", values_to = "abundance") %>%
  ggplot(aes(x = year, y = abundance, linetype = dataset)) +
  geom_line(linewidth = 1) +
  geom_point()

#plot relationship between the two
ggplot(compare_dat, aes(
  x = recruitment_mod_output,
  y = juvenile_abun_mod)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "GMACS recruitment (millions)",
    y = "40–50 mm juvenile abundance (millions)") 
#Hmmm let's revisit this when we have a more stable snow crab model 

