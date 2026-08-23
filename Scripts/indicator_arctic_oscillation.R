#Winter Arctic Oscillation, pulled from NOAA-NWS via:
#https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/ao.shtml

# Erin Fedewa

# load ----
library(tidyverse)
library(mgcv)

## Read in setup and data
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")
AO<- read_csv("./Data/Arctic_oscillation.csv")

#########################################################

#Mean Winter Arctic Oscillation
indicator_ao <- AO %>% 
  pivot_longer(c(2:13), names_to="Month", values_to="Index") %>%
  filter(year >= 1979,
         Month %in% c(1,2,3)) %>% 
  group_by(year) %>%
  summarize(Mean_AO = mean(Index), .groups = "drop") %>%
  rename_with(tolower)

#Plot
indicator_ao  %>%
  ggplot(aes(x = as.numeric(year), y = mean_ao)) +
  geom_point() +
  geom_line()+
  labs(y = "Arctic Oscillation Index", x = "") +
  geom_hline(aes(yintercept = mean(mean_ao, na.rm=TRUE)), linetype = 5) +
  theme_bw()

#######################################

#save output
write_csv(indicator_ao, "./Output/indicator_ao.csv")

############################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Winter_Spring_Arctic_Oscillation_Index_Model"

##EDITABLE TEXT
description <- paste0("January - March Arctic Oscillation index from the NOAA National Climate Data Center. 
                      Proposed sign of the relationship is negative.")

status_trends <- paste0("In winter/spring 2026, the Arctic Oscillation was in a slightly negative state.")

factors <- paste0("The Arctic Oscillation is a measure of the relative strength of low pressure over the Arctic 
                  and is defined by surface atmospheric weather patterns.")

implications <- paste0("Poor snow crab recruitment has been associated with positive values of the Arctic
                       Oscillation (Szuwalski et al., 2021).")

references <- paste0("Szuwalski, C., Cheng, W., Foy, R., Hermann, A. J., Hollowed, A., Holsman, K., Lee, J., et al. 2021. Climate change and the future productivity and distribution of crab in the Bering Sea. ICES Journal of Marine Science, 78: 502-515.")


##INDICATOR DATA
indicator_data <- indicator_ao %>%
  rename(indicator = mean_ao)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)






