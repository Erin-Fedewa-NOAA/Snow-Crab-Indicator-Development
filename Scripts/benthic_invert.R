#Calculate benthic invert mean CPUE across years in snow crab core area 
  #as a proxy for prey quantity- though note that major prey items like polychaetes
  #are not sampled well

# Erin Fedewa and Shannon Hennessey

#2026 improvements: transition this indicator to Jon Reum's spatiotemporal "predators
  #as samplers" approach to capture polychaetes 

# load ----
library(tidyverse)
library(mgcv)
library(rnaturalearth)
library(rnaturalearthdata)

#Load groundfish data queried directly from Racebase (see get_gf_data.R script)
benthic <- read.csv("./Data/gf_cpue_timeseries.csv") %>%
  rename(STATION_ID = STATION)

#read in pre-processing script
source("./Scripts/get_crab_data.R")

stations <- benthic %>%
  select(YEAR, STATION_ID) %>%
  filter(STATION_ID %in% snow_core, 
         YEAR %in% years) %>%
  distinct()

############################
#Define benthic invert guilds by species codes- validated by diet studies

pennatulacea <- c(42000:42999)
actinaria <- c(43000:43999)
polychaeta <- c(50000:59099)
worms_misc <- c(92000, 92500, 92502, 92511, 93100, 94000, 94500)
barnacles <- c(65000, 65100:65211)
shrimps <- c(66000:66912)
crabs <- c(68000:69599) 
crabs <- crabs[!crabs %in% c(68560, 68580, 68590, 69322, 69323, 69400, 69310, 68550, 68541)] # remove commercial species
gastropods <- c(71000:73999)
bivalves <- c(74000:75799)
asteroidea <- c(80000:82499)
echinoidea <- c(82500:82729, 82730, 82740)
ophiuroidea <- c(83000:84999)
holothuroidea <- c(85000:85999)
porifera <- c(91000:91999)
bryozoans <- c(95000:95499)
ascidians <- c(98000:99909)

guilds <- c(#"gersemia", "pennatulacea", "actinaria", "barnacles", "porifera", "ascidians"
  "shrimps", "crabs", "gastropods", "bivalves", "asteroidea", "echinoidea",
  "polychaeta",  "ophiuroidea", "holothuroidea",  "bryozoans")

# Calculate mean CPUE by guild and year  
ben_prey <- benthic %>%
  mutate(GUILD = case_when(# SPECIES_CODE %in% gersemia ~ "gersemia",
    # SPECIES_CODE %in% pennatulacea ~ "pennatulacea",
    # SPECIES_CODE %in% actinaria ~ "actinaria",
    SPECIES_CODE %in% polychaeta ~ "polychaeta",
    # SPECIES_CODE %in% barnacles ~ "barnacles",
    SPECIES_CODE %in% shrimps ~ "shrimps",
    SPECIES_CODE %in% crabs ~ "crabs",
    SPECIES_CODE %in% gastropods ~ "gastropods",
    SPECIES_CODE %in% bivalves ~ "bivalves",
    SPECIES_CODE %in% asteroidea ~ "asteroidea",
    SPECIES_CODE %in% echinoidea ~ "echinoidea",
    SPECIES_CODE %in% ophiuroidea ~ "ophiuroidea",
    SPECIES_CODE %in% holothuroidea ~ "holothuroidea",
    # SPECIES_CODE %in% porifera ~ "porifera",
    SPECIES_CODE %in% bryozoans ~ "bryozoans",
    # SPECIES_CODE %in% ascidians ~ "ascidians",
    TRUE ~ NA)) %>%             
  filter(STATION_ID %in% snow_core, 
         YEAR %in% years,
         !is.na(GUILD)) %>%
  # station-level cpue by guild
  group_by(YEAR, STATION_ID, GUILD) %>%
  summarise(CPUE_KGKM2 = sum(CPUE_KGKM2)) %>%
  # add in 0-catch stations by guild
  right_join(., expand_grid(stations, GUILD = guilds)) %>% 
  arrange(YEAR, STATION_ID, GUILD) %>%
  mutate(CPUE_KGKM2 = replace_na(CPUE_KGKM2, 0)) %>%
  # annual mean cpue by guild
  group_by(YEAR, GUILD) %>%
  summarise(CPUE_KGKM2 = mean(CPUE_KGKM2)) %>%
  right_join(., expand.grid(YEAR = years, 
                            GUILD = guilds)) %>%
  mutate(CPUE_KGKM2 = ifelse(is.na(CPUE_KGKM2), 0, CPUE_KGKM2),
         CPUE_KGKM2 = ifelse(YEAR == 2020, NA, CPUE_KGKM2)) %>%
  arrange(YEAR, GUILD) 

ben_prey <- ben_prey %>%
  group_by(YEAR) %>%
  summarise(GUILD = "total_invert",
            CPUE_KGKM2 = sum(CPUE_KGKM2)) %>%
  rbind(ben_prey)



# Plot 
guild_plot <- ben_prey %>%
  filter(!GUILD == "total_invert") %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2, group = factor(GUILD))) +
  geom_point(aes(colour = GUILD)) +
  geom_line(aes(colour = GUILD)) +
  labs(y = "Benthic Prey CPUE (kg/km2)", x = "Year") +
  theme_bw() +
  theme(legend.title = element_blank())
guild_plot # dominated by sponges, tunicates, crabs, stars

guild_facet <- ben_prey %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2)) +
  geom_point() +
  geom_line() +
  labs(y = "CPUE (kg/km2)", x = "Year") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_wrap(~GUILD, scales = "free_y", nrow = 4)
ggsave(paste0(fig_dir, "benthic_prey_facet.png"), guild_facet,
       height = 8, width = 10)


ben_prey %>%
  filter(GUILD == "total_invert",
         YEAR >= 1988) %>%
  ggplot(aes(x = YEAR, y = CPUE_KGKM2)) +
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = mean(CPUE_KGKM2, na.rm = TRUE)), linetype = 5) +
  geom_hline(aes(yintercept = mean(CPUE_KGKM2, na.rm = TRUE) - sd(CPUE_KGKM2, na.rm = TRUE)), color = "green4") +
  geom_hline(aes(yintercept = mean(CPUE_KGKM2, na.rm = TRUE) + sd(CPUE_KGKM2, na.rm = TRUE)), color = "green4") +
  labs(y = "Benthic Invertebrate\nPrey CPUE (kg/km2)", x = "Year") +
  theme_bw() +
  theme(legend.title = element_blank()) 
ggsave(paste0(fig_dir, "benthic_prey_density.png"),
       height = 2, width = 6)

## Write .csv output of benthic prey density
ben_prey %>%
  pivot_wider(names_from = GUILD, values_from = CPUE_KGKM2) %>%
  rename(year = YEAR) %>%
  filter(year >= 1988) %>%
  select(year, total_invert) %>%
  write_csv("./outputs/benthic_invert_density.csv")