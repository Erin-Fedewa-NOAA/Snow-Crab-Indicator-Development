# notes ----
#Create master csv of ecosystem indicators 
# Assess collinearity b/w snow crab indicators for BAS
#Create indicator timeseries plot 


# Erin Fedewa
# last updated: 2022/8/22

# load ----
library(tidyverse)
library(corrplot)
library(cowplot)
library(mgcv)

#Ecosystem data to combine
invert <- read_csv("./Output/SCbenthic_timeseries.csv")
env <- read_csv("./Output/environmental_timeseries.csv")
d95 <- read_csv("./Output/D95_output.csv")
bcs <- read_csv("./Output/bcs_prev.csv")
cod <- read_csv("./Output/COD_output.csv")
occ <- read_csv("./Output/TempOcc_output.csv")
ice <- read_csv("./Output/seaice_output.csv")

# combine indices and save output
invert %>%
  select(YEAR, Total_Benthic) %>%
  rename(beninvert_cpue = Total_Benthic) %>%
  full_join(env %>%
              select(YEAR, cpa, Mean_AO)) %>%
  full_join(d95 %>%
              select(YEAR, mature_male) %>%
              rename(mat_male_d95=mature_male)) %>%
  full_join(bcs %>%
              select(YEAR, Immature) %>%
              rename(bcs_imm=Immature)) %>%
  full_join(cod %>%
              select(YEAR, mature_male) %>%
              rename(mat_male_COD = mature_male)) %>%
  full_join(occ %>%
              select(YEAR, Immature) %>%
              rename(temp_occ_imm = Immature)) %>%
  full_join(ice %>%
              select(year, Jan_ice) %>%
              rename(YEAR=year)) %>%
  rename(year = YEAR) %>%
  filter(year >= 1982) %>%
  arrange(year) -> eco_ind

write_csv(eco_ind, "./Data/snow_eco_indicators.csv")

#Assess collinearity b/w indicators 
eco_ind %>% 
  select(-year) %>%
  cor(use = "complete.obs") %>%
  corrplot(method="number")

################################################
#Ecosystem Plots 

#Reading in new data with cod consumption and SAM added manually from contributor csv's 
eco_ind <- read.csv("./Data/snow_2023_indicators.csv")

eco_ind %>%
  ## Male SAM 
  select(year, male_SAM) %>%
  ggplot(aes(x = year, y = male_SAM))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(male_SAM, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(male_SAM, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(male_SAM, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression("Size at 50% maturity (cw)"), x = "") +
  scale_x_continuous(breaks = seq(1980, 2022, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Female Size at 50% Maturity")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> SAM

eco_ind %>%
  ##Pcod_consumption
  select(YEAR, DATA_VALUE, PRODUCT) %>%
  filter(PRODUCT == "Summer_Snow_Crab_Consumption_Pacific_cod_Model") %>%
  ggplot(aes(x = YEAR, y = DATA_VALUE))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(DATA_VALUE, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(DATA_VALUE, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(DATA_VALUE, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2022.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Consumption (mt/day)", x = "")+
  scale_x_continuous(breaks = seq(1980, 2023, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Daily Consumption of Snow Crab by Pacific Cod")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) +
  theme(axis.text=element_text(size=12)) -> pcod

eco_ind %>%
  ## Invert Density
  select(year, beninvert_cpue ) %>%
  ggplot(aes(x = year, y = beninvert_cpue ))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(beninvert_cpue , na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(beninvert_cpue , .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(beninvert_cpue , .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Benthic Invert density", x = "") +
  scale_x_continuous(breaks = seq(1980, 2022, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Benthic Invert Density")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> invert 

eco_ind %>%
  ## Chl-A 
  select(year, Chl_a) %>%
  ggplot(aes(x = year, y = Chl_a))+
  geom_point(size=3)+
  geom_line() +
  #geom_smooth(method = gam, formula = y~s(x, bs = "cs")) +
  geom_hline(aes(yintercept = mean(Chl_a, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(Chl_a, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(Chl_a, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Chlorophyll-a (ug/l)", x = "")+
  scale_x_continuous(breaks = seq(1980, 2022, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Chlorophyll-a Biomass")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> chla


eco_ind %>%
  ##Cold Pool Extent
  select(year, cpa) %>%
  ggplot(aes(x = year, y = cpa))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(cpa, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(cpa, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(cpa, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Cold Pool Extent (nmi)", x = "") +
  scale_x_continuous(breaks = seq(1980, 2022, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("EBS Cold Pool Extent")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> cp 

eco_ind %>%
  ## Immature Temperature of Occupancy   
  select(YEAR, DATA_VALUE, PRODUCT) %>%
  filter(PRODUCT == "Summer_Snow_Crab_Juvenile_Temperature_Occupancy") %>%
  ggplot(aes(x = YEAR, y = DATA_VALUE))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(DATA_VALUE, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(DATA_VALUE, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(DATA_VALUE, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2022.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression("Temperature of Cccupancy ("*~degree*C*")"), x = "") +
  scale_x_continuous(breaks = seq(1980, 2023, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Immature Snow Crab Temperature of Occupancy")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) +
  theme(axis.text=element_text(size=12))  -> occtemp

eco_ind %>%
  ## Arctic Oscillation
  select(year, Mean_AO ) %>%
  ggplot(aes(x = year, y = Mean_AO ))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(Mean_AO, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(Mean_AO, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(Mean_AO, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Deviation", x = "") +
  scale_x_continuous(breaks = seq(1980, 2022, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Arctic Oscillation")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> AO

eco_ind %>%
  ## Mature Male COD  
  select(year, mat_male_COD) %>%
  ggplot(aes(x = year, y = mat_male_COD))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(mat_male_COD, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(mat_male_COD, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(mat_male_COD, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2022.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("Center of", "Distribution "( degree~Latitude))) , x = "") +
  scale_x_continuous(breaks = seq(1980, 2022, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Mature Male Center of Distribution")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> COD

eco_ind %>%
  ## BCS Prevalance  
  select(year, bcs_imm) %>%
  ggplot(aes(x = year, y = bcs_imm))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(bcs_imm, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(bcs_imm, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(bcs_imm, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Disease Prevalence", x = "")+
  scale_x_continuous(breaks = seq(1980, 2022, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Immature Snow Crab Disease Prevalence")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> BCS

eco_ind %>%
  ## Mature male D95 
  select(year,mat_male_d95) %>%
  ggplot(aes(x = year, y =mat_male_d95))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(mat_male_d95, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(mat_male_d95, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(mat_male_d95, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2021.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = expression(atop("Mature Male", "Area Occupied (nmi)")), x = "")+
  scale_x_continuous(breaks = seq(1980, 2022, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Mature Male Snow Crab Area Occupied")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> maleD95

eco_ind %>%
  ## Sea Ice 
  select(year,Jan_ice) %>%
  ggplot(aes(x = year, y =Jan_ice))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(Jan_ice, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(Jan_ice, .10, na.rm=TRUE)), linetype = 3)+
  geom_hline(aes(yintercept = quantile(Jan_ice, .90, na.rm=TRUE)), linetype = 3)+
  annotate("rect", xmin=2022.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "green") +
  labs(y = "Sea Ice Concentration", x = "")+
  scale_x_continuous(breaks = seq(1980, 2022, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Sea Ice Concentration")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> ice


#**************************
## Create combined plots -----
 
#Create Figure for ESP doc - 6 timeseries per page 
plot_grid(AO, cp, chla, invert, maleD95, occtemp,  
          label_size = 10,
          hjust = -4.5, vjust = 2.5,
          nrow = 6, align = "hv", axis = "l")  -> Fig1a

## write plot
ggsave(filename = "./Figs/Fig1a.png", device = "png", width = 6, height = 12, 
       dpi = 300)

plot_grid(COD, pcod, overlap, SAM, recruit, BCS,  
          label_size = 10,
          hjust = -4.5, vjust = 2.5,
          nrow = 7, align = "hv", axis = "l")  -> Fig1b

## write plot
ggsave(filename = "./Figs/Fig1b.png", device = "png", width = 6, height = 12, 
       dpi = 300)

#Other plots for presentation 

plot_grid(cp, occtemp,   
          label_size = 14,
          hjust = -4.5, vjust = 2.5,
          nrow = 2, align = "hv", axis = "l", rel_heights = c(1, 1))

#3 timeseries per page 
plot_grid(AO, cp, chla,  
           label_size = 12,
          hjust = -4.5, vjust = 2.5,
          nrow = 3, align = "hv", axis = "l")  -> one
## write plot
ggsave(filename = "./Figs/one.png", device = "png", width = 6, height = 12, 
       dpi = 300)

plot_grid(invert, recruit, SAM,  
          label_size = 10,
          hjust = -4.5, vjust = 2.5,
          nrow = 3, align = "hv", axis = "l")  -> two
## write plot
ggsave(filename = "./Figs/two.png", device = "png", width = 6, height = 12, 
       dpi = 300)

plot_grid(pcod, overlap, BCS,  
          label_size = 10,
          hjust = -4.5, vjust = 2.5,
          nrow = 3, align = "hv", axis = "l")  ->three
## write plot
ggsave(filename = "./Figs/three.png", device = "png", width = 6, height = 12, 
       dpi = 300)

plot_grid(COD, maleD95, occtemp,  
          label_size = 10,
          hjust = -4.5, vjust = 2.5,
          nrow = 3, align = "hv", axis = "l")  ->four
## write plot
ggsave(filename = "./Figs/four.png", device = "png", width = 6, height = 12, 
       dpi = 300)


