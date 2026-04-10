#Goals ---- 
#Evaluate causal linkages between snow crab recruitment and ESP ecosystem indicators

#Author: E. Fedewa

#load
library(tidyverse)
library(dsem)
library(ggplot2)
library(dplyr)
library(dagitty)
library(ggdag)
library(knitr)
library(corrplot)
library(patchwork)
library(TMB)
library(knitr)
library(kableExtra)
library(phylopath)
library(broom)

#Read in indicator data
indicators <- read.csv("./Output/snow_esp_indicator_timeseries.csv") %>%
                rename(sea_ice = Mar_Apr_ice_EBS_NBS)

## Read in setup for crab data
source("./Scripts/get_crab_data.R")

# Set years
years <- 1988:current_year
start_year = 1988

#-----------------------------------------#
# Recruitment response ----
#-----------------------------------------#

#calculate recruit abundance as our response: 
  #i.e. survey-derived abundance of 65-80mm CW male snow crab
  #(~6.7-7.7 years post settlement, 1-2 molts from terminal)

recruit_abun <- calc_bioabund(crab_data = snow,
                              species = "SNOW",
                              region = "EBS",
                              years = years,
                              sex = "male",
                              size_min = 65,
                              size_max = 80,
                              shell_condition = "new_hardshell") %>%
  select(YEAR, ABUNDANCE) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) %>%
  mutate(recruit_abun = as.numeric(ABUNDANCE/1e6)) %>%
  rename_with(tolower) %>%
  select(-abundance)

#Plot
recruit_abun %>%
  ggplot(aes(x = year, y = recruit_abun)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#calculate pre-recruit abundance as intermediate covariate: 
  #i.e. survey-derived abundance of 40-55mm CW male snow crab
  #(~4-5 years post settlement)

prerecruit_abun <- calc_bioabund(crab_data = snow,
                              species = "SNOW",
                              region = "EBS",
                              years = years,
                              sex = "male",
                              size_min = 40,
                              size_max = 55,
                              shell_condition = "new_hardshell") %>%
  select(YEAR, ABUNDANCE) %>%
  right_join(., expand.grid(YEAR = years)) %>%
  arrange(YEAR) %>%
  mutate(prerecruit_abun = as.numeric(ABUNDANCE/1e6)) %>%
  rename_with(tolower) %>%
  select(-abundance)

#Plot
prerecruit_abun %>%
  ggplot(aes(x = year, y = prerecruit_abun)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#join indicator and responses
recruit_abun %>%
  full_join(prerecruit_abun) %>%
  full_join(indicators) %>%
  arrange(year) %>%
  rename_with(tolower) -> snow_dat

#-----------------------------------------#
# data wrangling ----
#-----------------------------------------#

#refine indicators to those included in recruitment DAG
  #follow up on this list before CPT! 
model_dat <- snow_dat %>%
  rename_with(tolower) %>%
  filter(year >= start_year) %>% 
  select(year, total_invert, temp_occ, recruit_abun, prerecruit_abun,
         mean_ao, sea_ice, consumption)

#plot
model_dat %>%
  pivot_longer(-year, names_to="variable", values_to="value") %>%
  ggplot(aes(year, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", nrow=3) +
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

plot_histo(model_dat %>% select(-year))

#Scale all variables and transform abundance variables 
scaled_dat <- model_dat %>%
  mutate(log_invert = log(total_invert),
         log_recruit_abun = log(recruit_abun),
         log_prerecruit_abun = log(prerecruit_abun),
         log_consumption = log(consumption)) %>%
  select(-total_invert, -recruit_abun, -prerecruit_abun, -consumption)
  mutate(across(-year, ~ as.numeric(scale(.))))
  
#check distributions now
plot_histo(scaled_dat %>% select(-year))

#Assess collinearity b/w variables
scaled_dat %>% 
  select(-year) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot(method="number")
#We're invoking different mechanisms/lag structures for sea ice and temperature 
  #occupied, but as expected they are highly correlated

#plot all standardized/transformed variables
scaled_dat %>%
  pivot_longer(cols = -year, names_to="variable", values_to="value") %>%
  ggplot(aes(year, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", nrow=3) +
  theme_bw()

#---------------------------------------#
# Test for DAG-data consistency ----
#---------------------------------------#
#Follow up on this!!! 

#download specified DAG from dagitty.net - conditional independencies are 
#identified based on structure of DAG drawn in dagitty
snow_dag <- dagitty('dag {
"Arctic oscillation" [exposure,pos="-1.566,-0.473"]
"Benthic prey density" [exposure,pos="-0.697,-0.124"]
"Cod consumption" [exposure,pos="-1.380,0.278"]
"Prerecruit abundance" [outcome,pos="-1.153,-0.252"]
"Recruit abundance" [outcome,pos="-0.767,-0.601"]
"Sea Ice" [exposure,pos="-1.706,-0.007"]
"Temperature occupied" [exposure,pos="-1.049,-0.780"]
"Arctic oscillation" -> "Prerecruit abundance"
"Benthic prey density" -> "Recruit abundance"
"Cod consumption" -> "Prerecruit abundance"
"Prerecruit abundance" -> "Recruit abundance"
"Sea Ice" -> "Prerecruit abundance"
"Temperature occupied" -> "Recruit abundance"
}')

#plot DAG
ggdag(snow_dag, layout = "nicely") +
  theme_dag()

plot(snow_dag) 

ggdag_status(snow_dag, text = FALSE, use_labels = "name") +
  #guides(color = "none") +  # Turn off legend
  theme_dag()

#identify paths
paths(snow_dag)
#10 open causal pathways 

#and plot paths of interest
snow_dag %>%
    ggdag_paths(from = "Sea Ice", to = "Recruit abundance",
                text = FALSE, use_labels = "name") +
    theme_dag()

#find open paths between variables
snow_dag %>%
  dag_paths(from = "Sea Ice", to = "Tanner crab abundance")

#find adjustment sets for response variable 
adjustmentSets(snow_dag, exposure="Sea Ice", outcome="Tanner crab abundance")
#This tells us that no covariate adjustment is necessary to identify the causal effect
#ie there are no open backdoor paths 

#and visualize adjustment sets, if there are any
ggdag_adjustment_set(snow_dag, shadow = TRUE) +
  theme_dag()

#find conditional independencies- i.e. two variables that are implied to 
# be independent and not correlated shouldn't be connected by a node
impliedConditionalIndependencies(snow_dag)
#our DAG doesn't imply any conditional independence relationships, and no
#d-separation claims exist 

#-------------------------------#
#Prep data for dsem models ----
#-------------------------------#

data <- scaled_dat %>%
  select(-year) %>%
  ts()

family <- rep("normal", ncol(data))

#------------------------------------------------#
# Fit Base IID and AR1 recruitment models ----
#------------------------------------------------#

#### IID Model ####

iid_sem <- "
  #temporal structure
   log_consumption -> log_consumption, 1, ar_consump
   log_invert -> log_invert, 1, ar_invert
   log_prerecruit_abun -> log_prerecruit_abun, 1, ar_prerec
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp,
  log_recruit_abun <-> log_recruit_abun, 0, iid_rec"

#build model without running it
#(needed so we can modify TMB inputs)
fit_build_iid <- dsem(sem = iid_sem, tsdata = data,
                        family = family,
                        estimate_delta0 = TRUE,
                        control = dsem_control(
                          run_model = FALSE))

pars_iid <- fit_build_iid$tmb_inputs$parameters
map_iid  <- fit_build_iid$tmb_inputs$map

# fix observation error SD = 0.1
pars_iid$lnsigma_j <- rep(log(0.1), ncol(data))

# prevent estimation of SD
map_iid$lnsigma_j <- factor(rep(NA, ncol(data)))

#run final model fit with Delta0 and fixed SD
iid_fit <- dsem(sem=iid_sem, tsdata=data, 
                 family=family,
                 estimate_delta0=TRUE,
                 control=dsem_control(parameters = pars_iid,
                                      map = map_iid,
                                      quiet = TRUE,
                                      getsd = TRUE))

summary(iid_fit)
AIC(iid_fit)

#### AR1 Model ####

ar_sem <- "
  #temporal structure
   log_consumption -> log_consumption, 1, ar_consump
   log_invert -> log_invert, 1, ar_invert
   log_prerecruit_abun -> log_prerecruit_abun, 1, ar_prerec
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp,
  log_recruit_abun -> log_recruit_abun, 1, ar_rec"

#build model without running it
fit_build_ar1 <- dsem(sem = ar_sem, tsdata = data,
                      family = family,
                      estimate_delta0 = TRUE,
                      control = dsem_control(
                        run_model = FALSE))

pars_ar1 <- fit_build_ar1$tmb_inputs$parameters
map_ar1  <- fit_build_ar1$tmb_inputs$map

# fix observation error at 0.1
pars_ar1$lnsigma_j <- rep(log(0.1), ncol(data))

# prevent estimation of SD
map_ar1$lnsigma_j <- factor(rep(NA, ncol(data)))

#run final model fit with Delta0 and fixed SD
ar1_fit <- dsem(sem=ar_sem, tsdata=data, 
                family=family,
                estimate_delta0=TRUE,
                control=dsem_control(parameters = pars_ar1,
                                     map = map_ar1,
                                     quiet = TRUE,
                                     getsd = TRUE))

summary(ar1_fit)
AIC(ar1_fit)

#### AR1 + prerecruit Model ####

arR_sem <- "
  #temporal structure
   log_consumption -> log_consumption, 1, ar_consump
   log_invert -> log_invert, 1, ar_invert
   log_prerecruit_abun -> log_prerecruit_abun, 1, ar_prerec
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp,
  log_recruit_abun -> log_recruit_abun, 1, ar_rec

  #causal pathway
  log_prerecruit_abun -> log_recruit_abun, 2, prerecruittorecruit"

#build model without running it
fit_build_arR <- dsem(sem = arR_sem, tsdata = data,
                      family = family,
                      estimate_delta0 = TRUE,
                      control = dsem_control(
                        run_model = FALSE))

pars_arR <- fit_build_arR$tmb_inputs$parameters
map_arR  <- fit_build_arR$tmb_inputs$map

# fix observation error at 0.1
pars_arR$lnsigma_j <- rep(log(0.1), ncol(data))

# prevent estimation of SD
map_arR$lnsigma_j <- factor(rep(NA, ncol(data)))

#run final model fit with Delta0 and fixed SD
ar1_recruit_fit <- dsem(sem=arR_sem, tsdata=data, 
                family=family,
                estimate_delta0=TRUE,
                control=dsem_control(parameters = pars_arR,
                                     map = map_arR,
                                     quiet = TRUE,
                                     getsd = TRUE))

summary(ar1_recruit_fit)
AIC(ar1_recruit_fit)

#getting singularity/convergence errors for all models- probably because we're 
  #estimating so many AR terms?
#But how to use AIC/the same dataset to test simpler models/different hypotheses?

#----------------------------------#
# Causal Models ----
#----------------------------------#















#------------------------------------------------#
# Model comparison ----
#------------------------------------------------#

AIC(iid_fit)
AIC(ar1_fit)
AIC(ar1_recruit_fit)
AIC(full_dsem_fit)

delta_AIC <- AIC_values - min(AIC_values)