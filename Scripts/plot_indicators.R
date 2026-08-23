#Create master csv of ecosystem indicators for indicator analysis
#Create indicator timeseries plots

#TO DO: add in trend analysis to communicate on report card plots as symbols
#analysis to detect trend (change in timeseries mean) or regime-like behavior

# Erin Fedewa

# load ----
library(tidyverse)
library(corrplot)
library(patchwork)
library(mgcv)
library(ggdist)
library(scales)

#########################################
#Read in all 15 snow crab ecosystem indicators
#including even those that haven't been updated with current year data

# Find all indicator files in both output folders
indicator_files <- c(list.files("./Output",
                                pattern = "^indicator_.*\\.csv$",
                                full.names = TRUE),
                     list.files("./Output/Contributor data",
                                pattern = "^indicator_.*\\.csv$",
                                full.names = TRUE))

# Read and join all indicator files by year
indicators <- indicator_files %>%
  map(read_csv) %>%
  reduce(full_join, by = "year") %>%
  arrange(year)

#save ouput for indicator analysis
write_csv(indicators, "./Output/snow_esp_indicator_timeseries.csv")

################################################

#Ecosystem Plots ----

# Set years for plotting
current_year <- 2026

# Plot information for each indicator
indicator_info <- tribble(
  ~indicator,               ~title,                                    ~y_label,                            ~filename,
  "mean_ao",                "Arctic Oscillation",                      "Deviation",                         "arctic_oscillation.png",
  "total_invert",           "Invertebrate Density",                    "Density (kg/km²)",                  "invert_density.png",
  "imm_prev_ebs",           "Juvenile Snow Crab Disease Prevalance",   "Prevalence (%)",                    "disease.png",
  "extent_0c",              "Cold Water Habitat Availability (≤ °C)",   "Area (nm²)",                       "cold_pool.png",
  "energetic_condition",    "Juvenile Snow Crab Energetic Condition",   "% dry weight of hepatopancreas",   "energetic_condition.png",
  "Mar_Apr_ice_EBS_NBS",    "Spring Sea Ice Extent",                    "% Cover",                          "ice.png",
  "osr_female_male",        "Operational Sex Ratio",                    "Mature female to large male ratio", "sex_ratio.png",
  "prop_empty",             "Female Snow Crab Reproductive Failure",    "% Barren",                          "clutch.png",
  "d95",                    "Mature Male Area Occupied",                "Area (nm²)",                         "male_d95.png",
  "temp_occ",               "Juvenile Snow Crab Temperature Occupied",  "Bottom Temperature (°C)",            "temp_occ.png",
  "ebs_consumption",        "Pacific Cod Consumption",                  "Tons/day",                           "consumption.png",
  "chla",                   "Chlorophyll a Concentration",              "Concentration (µg/L)",               "chla.png",
  "female_sam",             "Female Snow Crab Size at Maturity",        "Carapace width (mm)",                "female_sam.png",
  "male_sam",               "Male Snow Crab Size at Maturity",          "Carapace width (mm)",                "male_sam.png",
  "lat_cod",                 "Mature Male Center of Abundance",          "Degrees latitude",                   "cod.png"
)
  
#plotting function:
plot_indicator <- function(data, indicator, title, y_label, filename) {
  
  plot_data <- data %>%
    select(year, value = all_of(indicator)) %>%
    filter(!is.na(value))
  
  first_year <- min(plot_data$year)
  
  p <- ggplot(plot_data, aes(x = year, y = value)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = mean(plot_data$value, na.rm = TRUE),
               linetype = 5) +
    geom_hline(yintercept = mean(plot_data$value, na.rm = TRUE) -
                 sd(plot_data$value, na.rm = TRUE), linetype = 3) +
    geom_hline(yintercept = mean(plot_data$value, na.rm = TRUE) +
                 sd(plot_data$value, na.rm = TRUE), linetype = 3) +
    annotate("rect", xmin = current_year - 0.5, xmax = current_year + 0.5,
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "green") +
    labs(y = y_label, x = "") +
    scale_x_continuous(limits = c(first_year - 0.5, current_year + 0.5),
                       breaks = seq(ceiling(first_year / 5) * 5, current_year, by = 5)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(
        lineheight = .8,
        face = "bold",
        hjust = 0.5)) +
    ggtitle(title)
  
  ggsave(filename = file.path("./Figs", filename),
         plot = p, width = 8, height = 5, dpi = 300)
}

#generate all 15 ecosystem indicator plots and save
pwalk(indicator_info,
      ~ plot_indicator(data = indicators, indicator = ..1,
                       title = ..2, y_label = ..3, filename = ..4))

###############################################################
#Socioeconomic Indicators

#Skipper survey plots
skipper <- read_csv("./Output/Contributor data/Skipper Survey Q1_Q3.csv")

#question 1: perceived abundance
color_palette <- c("red" = "red", "blue" = "blue", "grey" = "grey")

q1 <- skipper %>% 
  filter(stock == "snow",
         question == "perceived_abundance") %>%
  mutate(bar_color = case_when(response %in% c("10_25_decrease", "25_plus_decrease") ~ "red",
                               response %in% c("no_change") ~ "grey",
                               response %in% c("10_25_increase","25_plus_increase") ~ "blue")) %>%
  mutate(response = factor(response, 
                           levels = c("25_plus_decrease", "10_25_decrease",
                                      "no_change","10_25_increase",
                                      "25_plus_increase"))) %>%
  ggplot(aes(number_responses, response, fill = bar_color)) +
  geom_bar(stat = "identity", alpha = .8) +
  scale_fill_manual(values = color_palette) +
  scale_y_discrete(labels = c("25_plus_decrease" = "25%+ Decrease", "10_25_decrease" = "10-25% Decrease", 
                              "no_change" = "No Change",
                              "10_25_increase" = "10-25% Increase", "25_plus_increase" = "25%+ Increase")) +
  labs(x = "Number of Responses", y = "") +
  theme_bw() +
  theme(legend.position = "none")

#save plot to correct size for report card
ggsave("./Figs/Skipper_survey_1.png", plot = q1,
       width = 3.968750, height = 1.333333,
       units = "in", dpi = 300, limitsize = FALSE)

#question 2: changes in fishing behavior
q2 <- skipper %>% 
  filter(stock == "snow",
         question == "fishing_practice",
  #Using only the top 4 responses here! 
  response != "longer_soak") %>%
  ggplot(aes(number_responses, response)) +
  geom_bar(stat = "identity", alpha = .8, fill = "grey") +
  scale_y_discrete(labels = c("no_change" = "No Change", "move_location" = "Moved Fishing Locations", 
                              "longer_soak" = "Longer Soak Times",
                              "move_deeper" = "Fished Deeper", "move_south" = "Moved South")) +
  labs(x = "Number of Responses", y = "") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = label_number(accuracy = 1))

ggsave("./Figs/Skipper_survey_2.png", plot = q2,
       width = 3.968750, height = 1.333333,
       units = "in", dpi = 300, limitsize = FALSE)

#question 3: motivation for changes in fishing behavior
q3 <- skipper %>% 
  filter(stock == "snow",
         question == "reason_change",
    #Using only the top responses here! 
         response != "no_change") %>%
  ggplot(aes(number_responses, response)) +
  geom_bar(stat = "identity", alpha = .8, fill = "grey") +
  scale_y_discrete(labels = c("no_change" = "No Change", "low_cpue" = "Low CPUE", 
                              "high_sorting" = "Too Much Sorting",
                              "trawlers" = "Trawlers in the Area",
                              "undesirable_crab" = "Undesirable Crab",
                              "weather_ice" = "Weather/Ice")) +
  labs(x = "Number of Responses", y = "") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = label_number(accuracy = 1))

ggsave("./Figs/Skipper_survey_3.png", plot = q3,
       width = 3.968750, height = 1.333333,
       units = "in", dpi = 300, limitsize = FALSE)
