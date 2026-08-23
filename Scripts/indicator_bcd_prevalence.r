# Calculate visual prevalence of Bitter Crab Disease in snow crab
  #in EBS and NBS (immature and total population)

#Author: Erin Fedewa and Shannon Hennessey

# load ----
library(tidyverse)
library(ggridges)

#read in pre-processing script
source("./Scripts/get_crab_data.R")
source("./Scripts/make_indicator_text.R")

##############################################
## EBS: Calculate abundance by maturity and disease status 

#EBS Abundance of total population infected with bcd 
  #i.e. filter specimen data and pass filtered dataframe through crabpack
  #calc_bioabund() function 
pop_bcd <- snow
pop_bcd$specimen <- pop_bcd$specimen %>%
  filter(DISEASE_CODE == 2) 

pop_bcd_abun <- calc_bioabund(crab_data = pop_bcd,
                         species = "SNOW",
                         years = 1989:current_year,
                         spatial_level = "region") %>%
  mutate(CATEGORY = "pop_bcd")

#EBS Abundance of total population (infected & not infected)
pop_abun <- calc_bioabund(crab_data = snow,
                              species = "SNOW",
                              years = 1989:current_year,
                              spatial_level = "region") %>%
  mutate(CATEGORY = "pop")

#EBS Abundance of immature snow crab infected with bcd
imm_bcd <- snow
imm_bcd$specimen <- imm_bcd$specimen %>%
  left_join(mat_size) %>%
  filter(((SEX == 2 & CLUTCH_SIZE == 0) |
            (SEX == 1 & SIZE < MAT_SIZE)),
         DISEASE_CODE == 2) 

imm_bcd_abun <- calc_bioabund(crab_data = imm_bcd,
                              species = "SNOW",
                              years = 1989:current_year,
                              spatial_level = "region") %>%
  mutate(CATEGORY = "imm_bcd")

#EBS Abundance of all immature snow crab
imm <- snow
imm$specimen <- imm$specimen %>%
  left_join(mat_size) %>%
  group_by(YEAR) %>%
  filter(((SEX == 2 & CLUTCH_SIZE == 0) |
            (SEX == 1 & SIZE < MAT_SIZE))) 

imm_abun <- calc_bioabund(crab_data = imm,
                              species = "SNOW",
                              years = 1989:current_year,
                              spatial_level = "region") %>%
  mutate(CATEGORY = "imm")

####################################################
## NBS: Calculate abundance by maturity and disease status 
  #Just calculating population prev in NBS because so few mature crab

#NBS Abundance of total population infected with bcd 
pop_nbs_bcd <- snow_nbs
pop_nbs_bcd$specimen <- pop_nbs_bcd$specimen %>%
  filter(DISEASE_CODE == 2) 

pop_nbs_bcd_abun <- calc_bioabund(crab_data = pop_nbs_bcd,
                              species = "SNOW",
                              region = "NBS",
                              years = 1989:current_year,
                              spatial_level = "region") %>%
  mutate(CATEGORY = "pop_nbs_bcd")

#NBS Abundance of total population (infected & not infected)
pop_nbs_abun <- calc_bioabund(crab_data = snow_nbs, 
                          species = "SNOW",
                          region = "NBS",
                          years = 1989:current_year,
                          spatial_level = "region") %>%
  mutate(CATEGORY = "pop_nbs")


#####################################################
## Combine ebs abundance estimates, calculate % prevalence 
prev_ebs <- rbind(pop_bcd_abun, pop_abun, imm_bcd_abun, imm_abun) %>%
  select(-c("ABUNDANCE_CV","ABUNDANCE_CI", "REGION",
            "BIOMASS_MT", "BIOMASS_MT_CV", "BIOMASS_MT_CI", 
            "BIOMASS_LBS", "BIOMASS_LBS_CV", "BIOMASS_LBS_CI")) %>%
  pivot_wider(names_from = CATEGORY, values_from = ABUNDANCE) %>%
  mutate(pop_prev_ebs = (pop_bcd/pop)*100,
         imm_prev_ebs = (imm_bcd/imm)*100) %>%
  select(YEAR, pop_prev_ebs, imm_prev_ebs) 

#Plot EBS only 
prev_ebs %>%
  pivot_longer(2:3, names_to="Maturity", values_to="Perc_prev") %>%
  filter(Maturity %in% c("pop_prev_ebs", "imm_prev_ebs")) %>%
  ggplot(aes(x = YEAR, y = Perc_prev, group = as.factor(Maturity))) +
  geom_point(aes(colour = Maturity), size=3) +
  geom_line(aes(colour = Maturity), size=1) +
  labs(y = "Disease Prevalence (%)", x = "") +
  theme_bw() +
  geom_hline(aes(yintercept = mean(Perc_prev, na.rm=TRUE)), linetype = 5)+
  theme(legend.text=element_text(size=11)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(axis.text.x=element_text(size=14), axis.text.y=element_text(size=12)) +
  theme(legend.title= element_blank()) 
#Very tightly coupled! 

## Combine nbs abundance estimates, calculate % prevalence
rbind(pop_nbs_bcd_abun, pop_nbs_abun) %>%
  select(-c("ABUNDANCE_CV","ABUNDANCE_CI", "REGION",
            "BIOMASS_MT", "BIOMASS_MT_CV", "BIOMASS_MT_CI", 
            "BIOMASS_LBS", "BIOMASS_LBS_CV", "BIOMASS_LBS_CI")) %>%
  pivot_wider(names_from = CATEGORY, values_from = ABUNDANCE) %>%
  mutate(pop_prev_nbs = (pop_nbs_bcd/pop_nbs)) %>%
  select(YEAR, pop_prev_nbs) %>%
  right_join(prev_ebs) %>%
  arrange(YEAR) -> prev

#Combined EBS/NBS plot
prev %>%
  pivot_longer(2:4, names_to="category", values_to="Perc_prev") %>%
  ggplot(aes(x = YEAR, y = Perc_prev)) +
  geom_point(aes(colour = category), size=3) +
  geom_line(aes(colour = category), size=1) +
  labs(y = "Disease Prevalence", x = "") +
  theme_bw() +
  geom_hline(aes(yintercept = mean(Perc_prev, na.rm=TRUE)), linetype = 5)+
  theme(legend.text=element_text(size=11)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(axis.text.x=element_text(size=10), axis.text.y=element_text(size=12)) +
  theme(legend.title= element_blank()) 
#Interestingly, visual prevalence is much lower in the NBS despite increased 
  #likelihood of seeing more advanced stage infections 

###############################################

##save output for BCD indicator
indicator_prev <- prev %>%
  select(-pop_prev_nbs, -pop_prev_ebs) %>%
  rename_with(tolower) %>%
  complete(year = min(year):max(year)) %>%
  arrange(year)

write.csv(indicator_prev, "./Output/indicator_prevalence.csv", row.names = FALSE)

############################################

#WRITE TEXT FILE FOR AKFIN INDICATOR SUBMISSION:

#Indicator name
indicator_name <- "Summer_Snow_Crab_Juvenile_Disease_Prevalence"

##EDITABLE TEXT
description <- paste0("Prevalence (%) of immature snow crab showing visual symptoms of Bitter Crab Disease (BCD) 
                      during the summer EBS bottom trawl survey, calculated as the abundance of visually positive 
                      immature crab divided by total immature abundance. Proposed sign of the relationship is negative.")

status_trends <- paste0("Disease prevalence decreased from 2025 to 2026, with less than 1% of the immature 
                        snow crab population in the EBS visually infected.")

factors <- paste0("Bitter crab disease tends to occur at stations with high population density of small, new shell crab, 
                  and 2 - 4°C bottom temperatures (Balstad et al., 2024). However, visual detection methods substantially 
                  underestimate disease prevalence, and infections detected with sensitive PCR assays at disease monitoring 
                  sites indicate that prevalence levels are much higher than reported here (Fedewa et al., 2025).")

implications <- paste0("Because bitter crab disease is assumed fatal and primarily affects small crab, increases in visual 
                       disease prevalence are expected to negatively impact survival and recruitment success. Disease monitoring 
                       is critical when large cohorts enter the population because an increased proportion of small snow crab in 
                       the system could lend to higher disease prevalence.")

references <- paste0("Balstad, L. J., Fedewa, E. J., and Szuwalski, C. S. 2024. Drivers of bitter crab disease occurrence in eastern Bering Sea snow crab (Chionoecetes opilio). ICES Journal of Marine Science.
Fedewa E.F., Jensen P.C., Small H.J., Litzow M.A., Malick M.J., Zacher L.S., Long W.C., Kotwicki S. 2025. Bitter crab disease dynamics in eastern Bering Sea Tanner and snow crab: an underestimated and emergent stressor. Fisheries Research.")


##INDICATOR DATA
indicator_data <- indicator_prev %>%
  rename(indicator = imm_prev_ebs)

#CREATE TEXT FILE
create_indicator_file(
  indicator_name = indicator_name,
  indicator_data = indicator_data,
  description = description,
  status_trends = status_trends,
  factors = factors,
  implications = implications,
  references = references)




   