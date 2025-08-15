# Calculate visual prevalence of Bitter Crab Disease in snow crab
  #in EBS and NBS (immature and total population)

#Author: Erin Fedewa and Shannon Hennessey

# load ----
library(tidyverse)
library(ggridges)

#read in pre-processing script
source("./Scripts/get_crab_data.R")

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

## Write .csv for BCD indicator
missing <- data.frame(YEAR = 2020)

prev %>%
  select(-pop_prev_nbs) %>%
  bind_rows(missing) %>%
  arrange(YEAR) %>%
  write.csv("./Output/bcd_prevalence.csv", row.names = FALSE)






   