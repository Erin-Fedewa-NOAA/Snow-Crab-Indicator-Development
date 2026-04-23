#Assess cross-correlations between causal relationships for testing 
#biologically constrained lags

#Author: EJF

library(tidyverse)
library(zoo)
library(janitor)
library(gt)
library(forecast)

#Read in indicator data
indicators <- read.csv("./Output/snow_esp_indicator_timeseries.csv") %>%
  rename(sea_ice = Mar_Apr_ice_EBS_NBS, 
         consumption = ebs_consumption)

#read in recruitment response 
recruit_abun <- read.csv("./Output/response_recruit_abundance.csv")

#-----------------------------------------#
#Data wrangling ----
#-----------------------------------------#
#join indicator and responses
recruit_abun %>%
  full_join(indicators) %>%
  arrange(year) %>%
  rename_with(tolower) -> snow_dat

#refine indicators to those included in recruitment DAG
#follow up on this list before CPT! 
model_dat <- snow_dat %>%
  rename_with(tolower) %>%
  mutate(across(everything(), as.numeric)) %>%
  select(year, total_invert, temp_occ, recruit_abun_mod, prerecruit_abun_mod,
         mean_ao, sea_ice, consumption, extent_0c, bcd_imm,
         energetic_condition, chla)

#Scale all variables and transform abundance variables 
scaled_dat <- model_dat %>%
  mutate(log_invert = log(total_invert),
         log_recruit_abun = log(recruit_abun_mod),
         log_prerecruit_abun = log(prerecruit_abun_mod)) %>%
  select(-total_invert, -recruit_abun_mod, -prerecruit_abun_mod) %>%
  mutate(across(-year, ~ as.numeric(scale(.))))

#------------------------------------------------#
# Define causal links for assessing lags ----
#------------------------------------------------#

links <- tibble::tribble(
  ~driver, ~response,
  "sea_ice","log_recruit_abun",
  "sea_ice","chla",
  "sea_ice","energetic_condition",
  "mean_ao","sea_ice",
  "chla","log_recruit_abun",
  "log_invert","energetic_condition",
  "energetic_condition","log_recruit_abun",
  "extent_0c","temp_occ",
  "temp_occ","log_recruit_abun",
  "extent_0c","bcd_imm",
  "bcd_imm", "log_recruit_abun",
  "extent_0c", "consumption",
  "consumption", "log_recruit_abun")

#---------------------------------#
# Cross-correlation function ----
#---------------------------------#

lag_cor <- function(x, y, lag){
  if(lag < 0){
    cor(x[1:(length(x)+lag)],
        y[(1-lag):length(y)],
        use="complete.obs")
  } else if(lag > 0){
    cor(x[(1+lag):length(x)],
        y[1:(length(y)-lag)],
        use="complete.obs")
  } else{
    cor(x,y,use="complete.obs")
  }
}

#-----------------------------------#
# Compute cross correlations ----
#-----------------------------------#

lags <- -6:0  #only negative lags are biologically relevant

results <- links %>%
  mutate(res = purrr::map2(driver, response, function(d, r){
    
    x <- scaled_dat[[d]]
    y <- scaled_dat[[r]]
    
    tibble(
      lag = lags,
      correlation = purrr::map_dbl(lags, ~lag_cor(x, y, .x))
    )
  })) %>%
  tidyr::unnest(res) %>%
  mutate(link = paste(driver, "→", response))

#-----------------------------
# Plot
#-----------------------------

results %>%
  ggplot(aes(x = lag, y = correlation)) +
  geom_col(width = 0.8, fill = "#0072B2") +
  facet_wrap(~link, scales = "free_y", ncol = 2) +
  labs(x = "Lag (years)",
       y = "Pearson correlation") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        strip.background = element_rect(fill="grey90"),
        strip.text = element_text(face="bold"),
        panel.grid.minor = element_blank())

#-----------------------------------------------
# Select top negative lags for each causal effect
#-----------------------------------------------
#select top negative lag 
  #(driver leads response by t minus lag years)
best_negative_lags <- results %>%
  filter(lag < 0) %>% # only negative lags
  group_by(link) %>%
  slice_max(correlation, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(link)

#table of results
best_negative_lags %>%
  select(link, lag, correlation) %>%
  # Combine lag and correlation into a single string for neater table
  mutate(lag_corr = paste0(lag, " (", round(correlation, 2), ")")) %>%
  select(link, lag_corr) %>%
  gt() %>%
  tab_header(
    title = "Best Negative Lags") %>%
  cols_label(link = "Link Type") 

#plot best 3 negative lags 
best3_negative_lags <- results %>%
  group_by(link) %>%
  slice_max(correlation, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(link) %>%
  group_by(link) %>%
  mutate(is_best = if_else(row_number(desc(correlation)) == 1, TRUE, FALSE)) %>%
  ungroup()

# Plot with different color for the best lag
best3_negative_lags %>%
  ggplot(aes(x = lag, y = correlation, fill = is_best)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "#D55E00",   # best lag in dark orange/red
                               "FALSE" = "#E69F00")) +  # other bars in base orange
  facet_wrap(~link, scales = "free_y", ncol = 2) +
  theme_bw(base_size = 12) +
  labs(title = "Best 3 Negative Lags",
       x = "Best Leading Lag (years)",
       y = "Pearson Correlation",
       fill = "Top Lag") +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "top")

