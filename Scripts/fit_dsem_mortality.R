#Goals ---- 
#Evaluate causal linkages between snow crab mortality and ESP ecosystem indicators
  #mortality estimates are for immature males/females

#Follow up: have not yet looked at DAGs, diagnostics, simulations etc. 

#Author: E. Fedewa

#load
library(tidyverse)
library(corrplot)
library(patchwork)
library(ggplot2)
library(viridis)
library(ggthemes)
library(BAS)
library(readxl)
library(gbm)

## Read in C. Szuwalski time-varying mortality estimates for immature snow crab
mort <- read.csv("./Data/mortality_estimates_assmt.csv")

#Read in indicator data
indicators <- read.csv("./Output/snow_esp_indicator_timeseries.csv") %>%
  rename(sea_ice = Mar_Apr_ice_EBS_NBS,
         consumption = ebs_consumption)

#Read in area occupied so we can replace mature male w/ immature d95
d95 <- read.csv("./Output/D95_output.csv") %>%
          select(YEAR, immature_d95) %>%
          rename(year = YEAR)

# Set years
current_year <- 2025
years <- 1988:current_year

#-----------------------------------------#
#Data wrangling ----
#-----------------------------------------#
#join indicator and responses
mort %>%
  select(year, mortality) %>%
  full_join(indicators) %>%
  full_join(d95) %>%
  arrange(year) %>%
  rename_with(tolower) -> snow_dat

#refine indicators to those included in mortality DAG
mort_dat <- snow_dat %>%
  mutate(across(everything(), as.numeric)) %>%
  select(year, mortality, temp_occ, 
         total_invert, sea_ice, consumption, extent_0c, bcd_imm,
         energetic_condition, immature_d95)

#plot
mort_dat %>%
  pivot_longer(-year, names_to="variable", values_to="value") %>%
  ggplot(aes(year, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", nrow=4) +
  theme_bw()

#Distributions of covariates
plot_histo <- function(data) {
  plots <- data %>%
    imap(~ ggplot(data, aes(x = .data[[.y]])) +
           geom_histogram(aes(y = after_stat(density)),
                          bins = 30,
                          fill = "skyblue", color = "black") +
           geom_density(color = "red") +
           labs(x = .y, y = "Density") +
           theme_minimal())
  patchwork::wrap_plots(plots)}

plot_histo(mort_dat %>% select(-year))

#Scale all variables and transform abundance variables 
scaled_dat <- mort_dat %>%
    mutate(across(-year, ~ as.numeric(scale(.))))

#Assess collinearity b/w variables
scaled_dat %>% 
  select(-year) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot(method="number")
#We're invoking different mechanisms/lag structures for some of these variables
#that are highly correlated

#plot all standardized/transformed variables
scaled_dat %>%
  pivot_longer(cols = -year, names_to="variable", values_to="value") %>%
  ggplot(aes(year, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", nrow=3) +
  theme_bw()

#-------------------------------#
#Prep data for dsem models ----
#-------------------------------#

data <- scaled_dat %>%
  select(-year) %>%
  ts()

#fix observation error SD = 0.1
family=list(consumption = gaussian_fixed_sd(sd=.1),
            extent_0c = gaussian_fixed_sd(sd=.1),
            bcd_imm = gaussian_fixed_sd(sd=.1),
            energetic_condition = gaussian_fixed_sd(sd=.1), 
            immature_d95 = gaussian_fixed_sd(sd=.1),
            total_invert = gaussian_fixed_sd(sd=.1),
            sea_ice = gaussian_fixed_sd(sd=.1),
            temp_occ = gaussian_fixed_sd(sd=.1),
            mortality = gaussian_fixed_sd(sd=.1))

#------------------------------------------------#
# Fit Base IID mortality model ----
#------------------------------------------------#

iid_sem <- "
  #temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   immature_d95 -> immature_d95, 1, ar_d95
   total_invert -> total_invert, 1, ar_invert
    sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
  mortality <-> mortality, 0, iid_mort"

iid_fit <- dsem(sem=iid_sem, tsdata=data, 
                family=family)

summary(iid_fit)

#------------------------------------------------#
# Fit Base AR1 mortality model ----
#------------------------------------------------#

ar_sem <- "
  #temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   immature_d95 -> immature_d95, 1, ar_d95
   total_invert -> total_invert, 1, ar_invert
    sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   mortality -> mortality, 1, ar_mort"

ar1_fit <- dsem(sem=ar_sem, tsdata=data, 
                family=family)

summary(ar1_fit)

#-------------------------------------------------#
#Fit Starvation Causal Model:  ----
#-------------------------------------------------#

starvation_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   immature_d95 -> immature_d95, 1, ar_d95
   total_invert -> total_invert, 1, ar_invert
    sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   mortality -> mortality, 1, ar_mort

#causal pathways
  sea_ice -> energetic_condition, 0, icetocondition
  energetic_condition -> mortality, 0, conditiontomort"

starvation_fit <- dsem(sem=starvation_sem, tsdata=data, 
                     family=family)

summary(starvation_fit) 

#-------------------------------------------------#
#Fit Thermal Stress Causal Model:  ----
#-------------------------------------------------#

thermal_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   immature_d95 -> immature_d95, 1, ar_d95
   total_invert -> total_invert, 1, ar_invert
    sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   mortality -> mortality, 1, ar_mort

#causal pathway
  extent_0c -> temp_occ, 0, cptotempocc
  temp_occ -> mortality, 0, tempocctomort"

thermal_stress_fit <- dsem(sem=thermal_sem, tsdata=data, 
                    family=family)

summary(thermal_stress_fit) 

#-------------------------------------------------#
#Fit Predation Causal Model:  ----
#-------------------------------------------------#

predation_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   immature_d95 -> immature_d95, 1, ar_d95
   total_invert -> total_invert, 1, ar_invert
    sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   mortality -> mortality, 1, ar_mort

#causal pathways
  extent_0c -> consumption, 0, cptocod
  consumption -> mortality, 1, codtomort"

predation_fit <- dsem(sem=predation_sem, tsdata=data, 
                      family=family)

summary(predation_fit) 

#-------------------------------------------------#
#Fit Density Dependent Mortality Causal Model:  ----
#-------------------------------------------------#

density_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   immature_d95 -> immature_d95, 1, ar_d95
   total_invert -> total_invert, 1, ar_invert
    sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   mortality -> mortality, 1, ar_mort

#causal pathways
  sea_ice -> immature_d95, 0, cptod95
  immature_d95 -> energetic_condition, 0, d95tocond
  immature_d95 -> bcd_imm, 0, d95tobcd
  bcd_imm -> mortality, 0, bcdtomort
  energetic_condition -> mortality, 0, condtomort"

density_fit <- dsem(sem=density_sem, tsdata=data, 
                    family=family)

summary(density_fit) 

#------------------------------------------------#
# Model comparison ----
#------------------------------------------------#

#marginal AIC comparison of base and causal models
AIC(iid_fit)
AIC(ar1_fit)
AIC(starvation_fit)
AIC(predation_fit)
AIC(thermal_stress_fit)
AIC(density_fit)

model_comp <- tibble(
  model = c("iid", "ar1", "starvation", "predation",
            "thermal_stress", "density"),
  AIC = c(AIC(iid_fit), AIC(ar1_fit), AIC(starvation_fit), AIC(predation_fit),
          AIC(thermal_stress_fit), AIC(density_fit))) %>%
  mutate(delta_AIC = AIC - min(AIC),
         weight = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC))) %>%
  arrange(AIC)

#print table
model_comp %>%
  gt() %>%
  fmt_number(columns = c(AIC, delta_AIC, weight), decimals = 3) %>%
  cols_label(model = "Model", AIC = "AIC",delta_AIC = "ΔAIC",
             weight = "AIC Weight")

#put all fitted models in a list
models <- list(
  iid = iid_fit,
  ar1 = ar1_fit,
  starvation = starvation_fit,
  thermal_stress = thermal_stress_fit,
  predation = predation_fit,
  density_dependence = density_fit)

# Function to extract recruitment deviations
extract_dev <- function(fit, model_name) {
  df <- as.data.frame(summary(fit))
  
  # Handle iid vs others (different parameter names)
  param_name <- if (model_name == "iid") "iid_mort" else "V[mortality]"
  
  df %>%
    filter(name == param_name) %>%
    transmute(
      model = model_name,
      Estimate = Estimate,
      Variance = Estimate^2, #variances are additive, so we need this for variance algebra below
      Std_Error = Std_Error)
}

# Apply across all models
dev_comp <- imap_dfr(models, extract_dev) %>%
  mutate(model = factor(model, levels = names(models))) %>%
  arrange(desc(Estimate))

#plot
ggplot(dev_comp, aes(x = model, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(
    ymin = Estimate - Std_Error,
    ymax = Estimate + Std_Error), width = 0.2) +
  theme_minimal() +
  labs(y= "sigma residual", x= "") +
  expand_limits(y = 0) +
  theme(axis.text.x = element_text(angle = 45))

#compute %reduction in estimated recruitment variance relative to IID model
iid_var <- dev_comp %>%
  filter(model == "iid") %>%
  pull(Variance)

reduction_table <- dev_comp %>%
  select(model, Variance) %>%
  mutate(pct_reduction = (iid_var - Variance) / iid_var * 100)
