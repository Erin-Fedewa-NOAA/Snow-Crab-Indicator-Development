# notes ----
#Create master csv of ecosystem indicators 
# Assess collinearity b/w snow crab indicators for BAS
#Create indicator timeseries plot 


# Erin Fedewa

# load ----
library(tidyverse)
library(corrplot)
library(cowplot)
library(mgcv)

#Ecosystem data to combine
invert <- read_csv("./Output/benthic_invert.csv")
env <- read_csv("./Output/environmental_timeseries.csv")
d95 <- read_csv("./Output/D95_output.csv")
bcd <- read_csv("./Output/bcd_prev.csv")
cod <- read_csv("./Output/COD_output.csv")
occ <- read_csv("./Output/TempOcc_output.csv")
ice <- read_csv("./Output/NSIDCseaice_output.csv")
clutch <- read_csv("./Output/clutch_full.csv")
ratio <- read_csv("./Output/operational_sex_ratio.csv")
mat <- read_csv("./Data/opilio_maturation_size.csv")
consump <- read_csv("./Data/cod_consumption.csv")

# combine indices and save output
invert %>%
  select(YEAR, Total_Benthic) %>%
  rename(beninvert_cpue = Total_Benthic) %>%
  full_join(env %>%
              select(YEAR, cpa, Mean_AO) %>%
              rename(cold_pool=cpa)) %>%
  full_join(d95 %>%
              select(YEAR, mature_male) %>%
              rename(mat_male_d95=mature_male)) %>%
  full_join(bcd %>%
              select(YEAR, Immature) %>%
              rename(bcd_imm=Immature)) %>%
  full_join(cod %>%
              select(YEAR, mature_male) %>%
              rename(mat_male_COD = mature_male)) %>%
  full_join(occ %>%
              select(YEAR, Immature) %>%
              rename(temp_occ_imm = Immature)) %>%
  full_join(ice %>%
              select(Year, JanFeb_avg) %>%
              rename(YEAR=Year, sea_ice=JanFeb_avg)) %>%
  full_join(clutch %>%
              select(YEAR, Prop_full) %>%
              rename(clutch_full=Prop_full)) %>%
  full_join(ratio %>%
              select(YEAR, op_sex_ratio)) %>%
  #full_join(mat %>%
              #select(year, male_size_term_molt) %>%
              #rename(YEAR=year)) %>%
  #full_join(consump %>%
              #select(year, consumption) %>%
              #rename(YEAR=year)) %>%
  rename(year = YEAR) %>%
  filter(year >= 1982) %>%
  arrange(year) -> eco_ind


#Assess collinearity b/w indicators 
eco_ind %>% 
  select(-year) %>%
  cor(use = "complete.obs") %>%
  corrplot(method="number")

################################################
#Larval Indicator Plots 

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
  #scale_x_continuous(breaks = seq(1980, 2022, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle("Chlorophyll-a Biomass")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) -> chla

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
  select(year, temp_occ_imm) %>%
  ggplot(aes(x = year, y = temp_occ_imm))+
  geom_point(size=3)+
  geom_line() +
  geom_hline(aes(yintercept = mean(temp_occ_imm, na.rm=TRUE)), linetype = 5)+
  geom_hline(aes(yintercept = quantile(temp_occ_imm, .10, na.rm=TRUE)), linetype = 3)+
    annotate("rect", ymin=1.723647,ymax=Inf,xmin=-Inf,xmax=Inf, alpha=0.2, fill= "#DF5C47") +
  geom_hline(aes(yintercept = quantile(temp_occ_imm, .90, na.rm=TRUE)), linetype = 3)+
    annotate("rect", ymin=-Inf,ymax=-0.8497369 ,xmin=-Inf,xmax=Inf, alpha=0.2, fill= "#6B87B9") +
  annotate("rect", xmin=2023.5 ,xmax=Inf ,ymin=-Inf , ymax=Inf, alpha=0.2, fill= "grey") +
  labs(y = expression("Temperature of Occupancy ("*~degree*C*")"), x = "") +
  theme_bw() +
  xlim(1985, NA) +
  scale_x_continuous(breaks = seq(1985, 2023, 5)) +
  theme(panel.grid = element_blank()) +
  #ggtitle("Immature Snow Crab Temperature of Occupancy")+
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust=0.5)) +
  theme(axis.text=element_text(size=12))  -> occtemp


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

#Size at 50% probability of maturation
mat %>%
  select(year, male_size_term_molt, female_mean_size_mat) %>%
  pivot_longer(2:3, names_to="Sex", values_to="Size_Maturity") %>%
  filter(Sex == "male_size_term_molt") %>%
  ggplot(aes(x = year, y =Size_Maturity))+
  geom_point(size=3) +
  geom_line() +
  geom_smooth()
  geom_hline(aes(yintercept = mean(Size_Maturity, na.rm=TRUE)), linetype = 5)
  
  consump %>%
    ggplot(aes(x = year, y =consumption))+
    geom_point(size=3) +
    geom_line() +
    geom_hline(aes(yintercept = mean(consumption, na.rm=TRUE)), linetype = 5)

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




