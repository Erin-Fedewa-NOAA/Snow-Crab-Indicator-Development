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
#Recruitment estimates  ----
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
#Recruitment estimates - immature only ----
#----------------------------------------------------#

imm_abun <- read.csv("./Output/recruit_abundance_immature.csv")

#combine with recruitment timeseries from above
recruit_dat <- imm_abun %>%
  mutate(juvenile_abun_mod = as.numeric(juvenile_abun_mod/1e6),
         prerecruit_abun_mod = as.numeric(prerecruit_abun_mod/1e6),
         recruit_abun_mod = as.numeric(recruit_abun_mod/1e6)) %>%
  full_join(prerecruit_abun) %>%
  full_join(juvenile_abun) %>%
  full_join(recruit_abun) 
  
#plot recruits
recruit_dat %>%
  pivot_longer(2:7, names_to = "method", values_to = "abundance") %>%
  filter(method %in% c("recruit_abun", "recruit_abun_mod")) %>%
  ggplot(aes(x = year, y = abundance, group=method)) +
  geom_point(aes(color=method)) +
  geom_line(aes(color=method))+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()
  
#plot pre-recruits
recruit_dat %>%
  pivot_longer(2:7, names_to = "method", values_to = "abundance") %>%
  filter(method %in% c("prerecruit_abun", "prerecruit_abun_mod")) %>%
  ggplot(aes(x = year, y = abundance, group=method)) +
  geom_point(aes(color=method)) +
  geom_line(aes(color=method))+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#plot juveniles 
recruit_dat %>%
  pivot_longer(2:7, names_to = "method", values_to = "abundance") %>%
  filter(method %in% c("juvenile_abun", "juvenile_abun_mod")) %>%
  ggplot(aes(x = year, y = abundance, group=method)) +
  geom_point(aes(color=method)) +
  geom_line(aes(color=method))+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#--------------------------#
#Save output ----
#--------------------------#

#Save output
write_csv(recruit_dat, "./Output/response_recruit_abundance.csv")
