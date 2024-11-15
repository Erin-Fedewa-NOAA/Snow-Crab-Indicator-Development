# notes ----]
# Develop indicator for juvenile snow crab energetic condition:
  #annual mean % DWt of hepatopancreas

# Erin Fedewa

#2025 improvements: Use model-based bayesian output for annual mean to control
  #for julian date and crab size? 

# load ----
library(tidyverse)
library(ggridges)
library(akgfmaps)
library(patchwork)
library(viridis)

#######################################

sc_condition <- read.csv("./Data/condition_master.csv")

#data wrangling 
sc_condition %>%
  filter(lme != "NA", #one crab collected outside the sampling design
         lme != "NBS",
         !vial_id %in% c("2019-65","2019-67","2019-68","2019-71","2019-66"),
         maturity != 1) %>%
  group_by(year) %>%
  summarise(avg_condition = mean(Perc_DWT, na.rm=T)) -> cond

#Write output
missing <- data.frame(year = 2020)

cond %>%
  bind_rows(missing) %>%
  arrange(year) %>%
write.csv(file="./Output/opilio_condition.csv")

#####################################################
#Plots 
sc_condition %>%
  filter(lme != "NA", #one crab collected outside the sampling design
         !vial_id %in% c("2019-65","2019-67","2019-68","2019-71","2019-66"),
         maturity != 1) %>%
  group_by(year, lme) %>%
  summarise(sd = sd(Perc_DWT, na.rm = TRUE),
    cond = mean(Perc_DWT, na.rm=T)) %>%
  bind_rows(missing) %>%
  arrange(year) -> plot

#plot annual mean as bar plot    
plot %>%
  ggplot(aes(as.factor(year), cond, fill=year)) +
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = cond-sd, ymax = cond+sd), width = 0.2, color="gray45") +
  theme_bw() +
  labs(y = "Snow Crab Condition (% DWT)", x = "") +
  theme(axis.text=element_text(size=12)) +
  theme(legend.position = "none") +
  geom_hline(yintercept=22.6, color="red") + #adding prelim threshold from starvation lab experiment
  geom_rect(aes(xmin=0, xmax=Inf, ymin=22.6, ymax=22.6 + 2.9), fill="red", alpha = 0.05) + 
  geom_rect(aes(xmin=0, xmax=Inf, ymin=22.6-2.9, ymax=22.6), fill="red", alpha = 0.05) +
  facet_wrap(~lme)
  
#plot annual mean as dot plot
plot %>%
  ggplot(aes(as.factor(year), cond)) +
  geom_point(size=2) + 
  labs(y = "Snow Crab Condition", x = "") +
  theme(axis.text=element_text(size=12)) +
  theme(legend.position = "none") +
  geom_hline(aes(yintercept = mean(cond, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(cond, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(cond, .90, na.rm=TRUE)), linetype = 3)+
  theme_bw()

#size comp of crab sampled
sc_condition %>%
  filter(lme != "NA", #one crab collected outside the sampling design
         lme != "NBS",
         !vial_id %in% c("2019-65","2019-67","2019-68","2019-71","2019-66"),
         maturity != 1) %>%
  mutate(Sex = recode_factor(sex, '1' = "M", '2' = "F")) %>%
  filter(lme != "NA", sex != "NA") %>% #one crab collected outside the sampling design
  group_by(year) %>%
  ggplot() +
  geom_density(aes(x=cw, fill=Sex), position = "stack", binwidth = 2) +
  scale_fill_manual(values=c("#00BFC4", "#F8766D")) +
  facet_grid(lme~year) +
  theme_bw() +
  labs(x= "Snow crab carapace width (mm)", y = "Count")

#Spatial condition map 

# Plotting layers
ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")

ebs_survey_areas <- ebs_layers$survey.area

ebs_survey_areas$survey_name <- c("Eastern Bering Sea", "Northern Bering Sea")

#Transform crab data 
sc_condition %>%
  filter(lme != "NA", #one crab collected outside the sampling design
         !vial_id %in% c("2019-65","2019-67","2019-68","2019-71","2019-66"),
         maturity != 1) %>%
  group_by(year, mid_latitude, mid_longitude) %>%
  summarise(condition=mean(Perc_DWT)) %>%
  # Convert lat/long to an sf object
  st_as_sf(coords = c("mid_longitude", "mid_latitude"), crs = st_crs(4326)) %>%
  #st_as_sf needs crs of the original coordinates- need to transform to Alaska Albers
  st_transform(crs = st_crs(3338)) -> crab_dat

#And now plot survey grid with crab data  
ggplot() +
  geom_sf(data = ebs_layers$survey.grid, fill=NA, color=alpha("grey80"))+
  geom_sf(data = ebs_survey_areas, fill = NA) +
  geom_sf(data = ebs_layers$akland, fill = "grey80", color = "black") +
  #add crab layers
  geom_sf(data=crab_dat, aes(color=condition)) +
  scale_color_viridis() +
  scale_x_continuous(limits = ebs_layers$plot.boundary$x,
                     breaks = ebs_layers$lon.breaks) +
  scale_y_continuous(limits = ebs_layers$plot.boundary$y,
                     breaks = ebs_layers$lat.breaks) +
  #scale_size_continuous(range = c(1,4)) +
  labs(x="", y="", color = expression(paste("% DWT"))) +
  theme_bw() +
  facet_wrap(~year) 

