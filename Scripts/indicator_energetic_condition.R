#Plot and create text file for snow crab energetic condition indicator
  #NOTE: Annual estimates of condition are produced in the "Snow Crab Condition
  #Monitoring" repository, and output directly to the Snow Crab ESP repository 

#Author: Erin Fedewa

## Read in functions and data 
source("./Scripts/make_indicator_text.R")
indicator_condition <- read.csv("./Output/indicator_opilio_condition.csv")

#######################################
#plot
indicator_condition %>% 
  ggplot(aes(as.factor(year), annual_mean)) +
  geom_bar(stat="identity", fill="#a6bddb") + 
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2, color="gray45") +
  theme_bw() +
  labs(y = "Snow Crab Condition (% DWT)", x = "") +
  theme(axis.text=element_text(size=12)) +
  theme(legend.position = "none") +
  geom_hline(yintercept=22.6, color="red") + #adding prelim threshold from starvation lab experiment
  geom_rect(aes(xmin=0, xmax=Inf, ymin=22.6, ymax=22.6 + 2.9), fill="red", alpha = 0.05) + 
  geom_rect(aes(xmin=0, xmax=Inf, ymin=22.6-2.9, ymax=22.6), fill="red", alpha = 0.05) 

##########################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Summer_Snow_Crab_Juvenile_Condition_SEBS_Survey"

##EDITABLE TEXT
description <- paste0("Summer snow crab juvenile energetic condition is estimated from water content in the hepatopancreas (% dry weight) 
                      sampled from snow crab on the EBS bottom trawl survey. Proposed sign of the relationship is positive.")

status_trends <- paste0("Energetic condition of juvenile snow crab declined slightly from 2025 to 2026. The current year estimate
                        remains well above a laboratory-derived starvation threshold and energetic condition estimates during the
                        snow crab collapse.")

factors <- paste0("Dramatic declines in energetic condition during the 2018-2019 snow crab population collapse were driven by warming 
                  during a marine heatwave and high snow crab population density (Fedewa et al., 2025). Declines in lipid storage in 
                  juvenile snow crab have also been linked to warmer temperatures and reduced food quality in the Bering Sea (Copeman et al., 2021).")

implications <- paste0("Increased energetic condition suggests favorable conditions for high survival and recruitment, whereas energetic 
                       condition that falls below a laboratory-derived threshold of 22.6% has been attributed to starvation-induced mortality (Copeman et al., in prep). ")

references <- paste0("Copeman, L. A., Ryer, C. H., Eisner, L. B., Nielsen, J. M., Spencer, M. L., Iseri, P. J., and Ottmar, M. L. 2021. Decreased lipid storage in juvenile Bering Sea crabs (Chionoecetes spp.) in a warm (2014) compared to a cold (2012) year on the southeastern Bering Sea. Polar Biology, 44: 1883-1901.
Fedewa, E.J., Copeman, L.A., Litzow, M.A. 2025. Energetic limitations and mass mortality of Bering Sea snow crab:
 Interacting effects of warming and density on collapse and recovery. Canadian Journal of Fisheries and Aquatic Sciences. 82: 1-14.")


##INDICATOR DATA
indicator_data <- indicator_condition %>%
  select(year, annual_mean) %>%
  rename(indicator = annual_mean)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)