#Create master csv of ecosystem indicators 
# Assess collinearity b/w snow crab indicators for BAS
#Create indicator timeseries plot 


# Erin Fedewa

# load ----
library(tidyverse)
library(corrplot)
library(patchwork)
library(mgcv)

#Ecosystem data to combine
invert <- read_csv("./Output/benthic_invert.csv")
env <- read_csv("./Output/environmental_timeseries.csv")
d95 <- read_csv("./Output/D95_output.csv")
bcd <- read_csv("./Output/bcd_prevalence.csv")
cod <- read_csv("./Output/COD_output.csv")
occ <- read_csv("./Output/TempOcc_output.csv")
ice <- read_csv("./Output/seaice_output.csv")
clutch <- read_csv("./Output/reproductive_potential.csv")
ratio <- read_csv("./Output/operational_sex_ratio.csv")
mat <- read_csv("./Output/snow_SAM.csv")
consump <- read_csv("./Data/cod_consumption.csv")
condition <- read_csv("./Data/opilio_condition.csv")

# Set years for plotting
current_year <- 2025

#########################################################

# combine indices and save output
invert %>%
  select(YEAR, Total_Benthic) %>%
  rename(beninvert_cpue = Total_Benthic) %>%
  full_join(env %>%
              select(YEAR, extent_coldpool, extent_0C, Mean_AO)) %>%
  full_join(d95 %>%
              select(YEAR, mature_male_d95)) %>%
  full_join(bcd %>%
              filter(REGION == "EBS") %>%
              select(YEAR, imm_prev_ebs) %>%
              rename(bcd_imm=imm_prev_ebs)) %>%
  full_join(cod %>%
              select(YEAR, mature_male_centroid)) %>%
  full_join(occ %>%
              select(YEAR, TEMP_OCC) %>%
              rename(temp_occ_imm = TEMP_OCC)) %>%
  full_join(ice %>%
              rename(YEAR=Year)) %>%
  full_join(clutch %>%
              select(YEAR, prop_empty) %>%
              rename(clutch_empty=prop_empty)) %>%
  full_join(ratio %>%
              select(YEAR, op_sex_ratio)) %>%
  full_join(mat %>%
              select(YEAR, male_maturity, female_maturity)) %>%
  full_join(consump %>%
              rename(YEAR=year)) %>%
  full_join(condition %>%
              select(year,annual_mean) %>%
              rename(YEAR=year)) %>%
  rename(year = YEAR) %>%
  arrange(year) -> eco_ind


#Assess collinearity b/w indicators 
eco_ind %>% 
  select(-year) %>%
  cor(use = "complete.obs") %>%
  corrplot(method="number")

################################################
#Larval Indicator Plots 

## Chl-A 
eco_ind %>%
  ggplot(aes(x = year, y = Chl_a))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(Chl_a, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(Chl_a, na.rm = TRUE) - sd(Chl_a, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(Chl_a, na.rm = TRUE) + sd(Chl_a, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Chlorophyll-a (ug/l)", x = "")+
  #scale_x_continuous(breaks = seq(1980, 2022, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Chlorophyll-a Biomass")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> chla

## Arctic Oscillation
eco_ind %>%
  ggplot(aes(x = year, y = Mean_AO ))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(Mean_AO, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(Mean_AO, na.rm = TRUE) - sd(Mean_AO, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(Mean_AO, na.rm = TRUE) + sd(Mean_AO, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin= (current_year - 0.5),xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Deviation", x = "") +
  scale_x_continuous(breaks = seq(1978,current_year, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Arctic Oscillation")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> AO
ggsave("./Figs/arctic_oscillation.png")

#Juvenile Indicator Plots

##Cold Pool Extent
eco_ind %>%
  ggplot(aes(x = year, y = extent_coldpool))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(extent_coldpool, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(extent_coldpool, na.rm = TRUE) - sd(extent_coldpool, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(extent_coldpool, na.rm = TRUE) + sd(extent_coldpool, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Cold Pool Extent (nmi)", x = "") +
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("EBS Cold Pool Extent")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> cp
ggsave("./Figs/cold_pool_extent.png")

##0C Water Extent
eco_ind %>%
  ggplot(aes(x = year, y = extent_0C))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(extent_0C, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(extent_0C, na.rm = TRUE) - sd(extent_0C, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(extent_0C, na.rm = TRUE) + sd(extent_0C, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Spatial Extent (nmi)", x = "") +
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle(expression("EBS Bottom Water < 0 " * degree * C * "")) +
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> zero
ggsave("./Figs/0C_extent.png")

## Immature Temperature of Occupancy  
eco_ind %>%
  ggplot(aes(x = year, y = temp_occ_imm)) +
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(temp_occ_imm, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(temp_occ_imm, na.rm = TRUE) - sd(temp_occ_imm, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(temp_occ_imm, na.rm = TRUE) + sd(temp_occ_imm, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression("Temperature of Occupancy ("*~degree*C*")"), x = "") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme(panel.grid = element_blank()) +
  ggtitle("Immature Snow Crab Temperature of Occupancy")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) +
  theme(axis.text=element_text(size=12))  -> occtemp
ggsave("./Figs/temp_occupancy.png")

## Sea Ice 
eco_ind %>%
  ggplot(aes(x = year, y = ice_avg))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(ice_avg, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(ice_avg, na.rm = TRUE) - sd(ice_avg, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(ice_avg, na.rm = TRUE) + sd(ice_avg, na.rm = TRUE)), linetype = 3) +
  geom_hline(yintercept = 0.15, color="#FF474C") +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Sea Ice Concentration", x = "")+
  scale_x_continuous(breaks = seq(1975, current_year, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Bering Sea Spring Sea Ice Extent")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> ice
ggsave("./Figs/sea_ice.png")

## Disease Prevalence  
eco_ind %>%
  ggplot(aes(x = year, y = bcd_imm))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(bcd_imm, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(bcd_imm, na.rm = TRUE) - sd(bcd_imm, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(bcd_imm, na.rm = TRUE) + sd(bcd_imm, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Disease Prevalence (%)", x = "")+
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1989, current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Immature Snow Crab Disease Prevalence")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> bcd
ggsave("./Figs/bitter_crab.png")

## Energetic Condition
eco_ind %>%
  ggplot(aes(x = year, y = annual_mean))+
  geom_bar(stat="identity") +
  geom_hline(aes(yintercept = mean(annual_mean, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(annual_mean, na.rm = TRUE) - sd(annual_mean, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(annual_mean, na.rm = TRUE) + sd(annual_mean, na.rm = TRUE)), linetype = 3) +
  geom_hline(yintercept = 22.6, color="#FF474C") +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Energetic Condition (% DWT)", x = "")+
  scale_x_continuous(limits=c(2018, current_year + .5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Juvenile Snow Crab Energetic Condition")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> condition
ggsave("./Figs/energetic_condition.png")

##Pcod_consumption
eco_ind %>%
  ggplot(aes(x = year, y = consumption))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(consumption, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(consumption, na.rm = TRUE) - sd(consumption, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(consumption, na.rm = TRUE) + sd(consumption, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Consumption (mt/day)", x = "")+
  scale_x_continuous(breaks = seq(1985, current_year, 5), limits = c(1985, current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Daily Consumption of Snow Crab by Pacific Cod")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) +
  theme(axis.text=element_text(size=12)) -> pcod
ggsave("./Figs/cod_consumption.png")

#Adult Indicators

## Male SAM 
eco_ind %>%
  ggplot(aes(x = year, y = male_maturity))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(male_maturity, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(male_maturity, na.rm = TRUE) - sd(male_maturity, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(male_maturity, na.rm = TRUE) + sd(male_maturity, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression("Size at 50% maturity (CW, mm)"), x = "") +
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Male Size at 50% Probability of Maturity")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> male_sam
ggsave("./Figs/male_sam.png")

## Female SAM 
eco_ind %>%
  ggplot(aes(x = year, y = female_maturity))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(female_maturity, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(female_maturity, na.rm = TRUE) - sd(female_maturity, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(female_maturity, na.rm = TRUE) + sd(female_maturity, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression("Mean size at maturation (CW, mm)"), x = "") +
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Female Mean Size at Maturation")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> female_sam
ggsave("./Figs/female_sam.png")

## Invert Density
eco_ind %>%
  ggplot(aes(x = year, y = beninvert_cpue ))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(beninvert_cpue, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(beninvert_cpue, na.rm = TRUE) - sd(beninvert_cpue, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(beninvert_cpue, na.rm = TRUE) + sd(beninvert_cpue, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Benthic Invert density (kg/km^2)", x = "") +
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Benthic Invertebrate Density")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> invert 
ggsave("./Figs/benthic_invert.png")

## Mature Male COD 
eco_ind %>%
  ggplot(aes(x = year, y = mature_male_centroid))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(mature_male_centroid, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(mature_male_centroid, na.rm = TRUE) - sd(mature_male_centroid, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(mature_male_centroid, na.rm = TRUE) + sd(mature_male_centroid, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression("Centroid " * degree * Latitude * ""), x = "") +
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Mature Male Center of Abundance")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> COD
ggsave("./Figs/male_cod.png")

## Mature male Area Occupied
eco_ind %>%
  ggplot(aes(x = year, y =mature_male_d95))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(mature_male_d95, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(mature_male_d95, na.rm = TRUE) - sd(mature_male_d95, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(mature_male_d95, na.rm = TRUE) + sd(mature_male_d95, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("Mature Male", "Area Occupied (nmi)")), x = "")+
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Mature Male Snow Crab Area Occupied")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> maleD95
ggsave("./Figs/male_d95.png")

#Female Reproductive Failure
eco_ind %>%
  ggplot(aes(x = year, y = clutch_empty))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(clutch_empty, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(clutch_empty, na.rm = TRUE) - sd(clutch_empty, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(clutch_empty, na.rm = TRUE) + sd(clutch_empty, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "% empty clutches", x = "")+
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Mature Female Reproductive Failure")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> clutch
ggsave("./Figs/empty_clutch.png")

#Operational Sex Ratio
eco_ind %>%
  ggplot(aes(x = year, y = op_sex_ratio))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(op_sex_ratio, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(op_sex_ratio, na.rm = TRUE) - sd(op_sex_ratio, na.rm = TRUE)), linetype = 3) +
  geom_hline(aes(yintercept = mean(op_sex_ratio, na.rm = TRUE) + sd(op_sex_ratio, na.rm = TRUE)), linetype = 3) +
  annotate("rect", xmin=(current_year - 0.5) ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Large male:mature female ratio", x = "")+
  scale_x_continuous(breaks = seq(1988, current_year, 5), limits=c(1988,current_year)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Operational Sex Ratio")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> sex_ratio
ggsave("./Figs/sex_ratio.png")  



 
