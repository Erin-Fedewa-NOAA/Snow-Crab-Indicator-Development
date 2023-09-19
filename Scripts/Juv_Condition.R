# notes ----]
# Develop indicator for juvenile snow crab condition, annual avg % DWt of hepatopancreas

# Erin Fedewa
# last updated: 2023/8/24

#UPDATES for 2024: replace master csv file- 2023 data DWT data added last minute and needs 
  #to be cleaned up 

# load ----
library(tidyverse)
library(ggridges)

sc_condition <- read.csv("./Data/opilio_condition.csv")

#data wrangling 
sc_condition %>%
  filter(lme != "NA", #one crab collected outside the sampling design
         lme != "NBS",
         !vial_id %in% c("2019-65","2019-67","2019-68","2019-71","2019-66"),
         maturity != 1) %>%
  group_by(year) %>%
  summarise(avg_condition = mean(Perc_DWT, na.rm=T)) -> cond

#Write output
write.csv(cond, file="./Output/opilio_condition.csv")

#Plot 
sc_condition %>%
  filter(lme != "NA", #one crab collected outside the sampling design
         lme != "NBS",
         !vial_id %in% c("2019-65","2019-67","2019-68","2019-71","2019-66"),
         maturity != 1) %>%
  group_by(year) %>%
  summarise(sd = sd(Perc_DWT, na.rm = TRUE),
    cond = mean(Perc_DWT, na.rm=T)) -> plot
    
plot %>%
  ggplot(aes(as.factor(year), cond, fill=year)) +
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = cond-sd, ymax = cond+sd), width = 0.2, color="gray45") +
  theme_classic() +
  labs(y = "Snow Crab Condition", x = "") +
  theme(axis.text=element_text(size=12)) +
  theme(legend.position = "none")
  

sc_condition %>%
  filter(lme != "NA", #one crab collected outside the sampling design
         lme != "NBS",
         !vial_id %in% c("2019-65","2019-67","2019-68","2019-71","2019-66"),
         maturity != 1) %>%
  ggplot(aes(as.factor(year), Perc_DWT, fill=as.factor(year))) +
  geom_boxplot() + 
  theme_classic()

