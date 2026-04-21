#Goals ---- 
#Evaluate causal linkages between snow crab recruitment and ESP ecosystem indicators

#Author: E. Fedewa

#Follow ups for May CPT: add in 2025 consumption estimate, re-run with Emily
  #immature index, diagnostics, d-sep tests

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
library(gt)

#Read in indicator data
indicators <- read.csv("./Output/snow_esp_indicator_timeseries.csv") %>%
                rename(sea_ice = Mar_Apr_ice_EBS_NBS)

#read in recruitment response 
recruit_abun <- read.csv("./Output/recruit_abundance.csv")

# Set years
current_year = 2025
years <- 1988:current_year

#-----------------------------------------#
#Data wrangling ----
#-----------------------------------------#
#join indicator and responses
recruit_abun %>%
  full_join(recruit_abun) %>%
  full_join(indicators) %>%
  arrange(year) %>%
  rename_with(tolower) -> snow_dat

#refine indicators to those included in recruitment DAG
  #follow up on this list before CPT! 
model_dat <- snow_dat %>%
  rename_with(tolower) %>%
  mutate(across(everything(), as.numeric)) %>%
  select(year, total_invert, temp_occ, recruit_abun, prerecruit_abun,
         mean_ao, sea_ice, consumption, extent_0c, bcd_imm,
         energetic_condition, chla)

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
         log_prerecruit_abun = log(prerecruit_abun)) %>%
  select(-total_invert, -recruit_abun, -prerecruit_abun) %>%
  mutate(across(-year, ~ as.numeric(scale(.))))
  
#check distributions now
plot_histo(scaled_dat %>% select(-year))

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

#specify distribution for measurement error
family <- rep("normal", ncol(data))

#------------------------------------------------#
# Fit Base IID recruitment model ----
#------------------------------------------------#

iid_sem <- "
  #temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd,
   energetic_condition -> energetic_condition, 1, ar_cond, 
   chla -> chla, 1, ar_chla
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
                        estimate_delta0 = FALSE,
                        control = dsem_control(
                          run_model = FALSE))

pars_iid <- fit_build_iid$tmb_inputs$parameters #fixed and random effects
map_iid  <- fit_build_iid$tmb_inputs$map #fixed and mirrored parameters

# fix observation error SD = 0.1
pars_iid$lnsigma_j <- rep(log(0.1), ncol(data))

# prevent estimation of SD
map_iid$lnsigma_j <- factor(rep(NA, ncol(data)))

#run final model fit with Delta0 and fixed SD
iid_fit <- dsem(sem=iid_sem, tsdata=data, 
                 family=family,
                 estimate_delta0=FALSE,
                 control=dsem_control(quiet = TRUE,
                                      map=map_iid, 
                                      parameters=pars_iid,
                                      getsd = TRUE,
                                      newton_loops = 1))

summary(iid_fit)
AIC(iid_fit)

#------------------------------------------------#
# Fit Base AR1 recruitment model ----
#------------------------------------------------#

ar_sem <- "
  #temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd,
   energetic_condition -> energetic_condition, 1, ar_cond, 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   log_prerecruit_abun -> log_prerecruit_abun, 1, ar_prerec
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp,
  log_recruit_abun -> log_recruit_abun, 1, ar_rec"

#build model without running it
fit_build_ar1 <- dsem(sem = ar_sem, tsdata = data,
                      family = family,
                      estimate_delta0 = FALSE,
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
                estimate_delta0=FALSE,
                control=dsem_control(parameters = pars_ar1,
                                     map = map_ar1,
                                     quiet = TRUE,
                                     getsd = TRUE))

summary(ar1_fit)
AIC(ar1_fit)

#------------------------------------------------#
# Fit Base cohort progression model ----
#------------------------------------------------#

prerecruit_sem <- "
  #temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd,
   energetic_condition -> energetic_condition, 1, ar_cond, 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   log_prerecruit_abun -> log_prerecruit_abun, 1, ar_prerec
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp,
  log_recruit_abun -> log_recruit_abun, 1, ar_rec

  #causal pathway
  log_prerecruit_abun -> log_recruit_abun, 2, prerecruittorecruit"

#build model without running it
fit_build_prerecruit <- dsem(sem = prerecruit_sem, tsdata = data,
                      family = family,
                      estimate_delta0 = FALSE,
                      control = dsem_control(
                        run_model = FALSE))

pars_prerecruit <- fit_build_prerecruit$tmb_inputs$parameters
map_prerecruit  <- fit_build_prerecruit$tmb_inputs$map

# fix observation error at 0.1
pars_prerecruit$lnsigma_j <- rep(log(0.1), ncol(data))

# prevent estimation of SD
map_prerecruit$lnsigma_j <- factor(rep(NA, ncol(data)))

#run final model fit with Delta0 and fixed SD
prerecruit_fit <- dsem(sem=prerecruit_sem, tsdata=data, 
                family=family,
                estimate_delta0=FALSE,
                control=dsem_control(parameters = pars_prerecruit,
                                     map = map_prerecruit,
                                     quiet = TRUE,
                                     getsd = TRUE))

summary(prerecruit_fit)
plot(prerecruit_fit)

#-----------------------------------------#
#Fit Climate Forcing Causal Model:  ----
#-----------------------------------------#

climate_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd,
   energetic_condition -> energetic_condition, 1, ar_cond, 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   log_prerecruit_abun -> log_prerecruit_abun, 1, ar_prerec
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp,
  log_recruit_abun -> log_recruit_abun, 1, ar_rec

#causal pathways
  mean_ao -> sea_ice, 0, aotoice
  sea_ice -> log_recruit_abun, 2, icetorecruit"

#build model without running it
fit_build_climate <- dsem(sem = climate_sem, tsdata = data,
                             family = family,
                             estimate_delta0 = FALSE,
                             control = dsem_control(
                               run_model = FALSE))

pars_climate <- fit_build_climate$tmb_inputs$parameters
map_climate  <- fit_build_climate$tmb_inputs$map

# fix observation error at 0.1
pars_climate$lnsigma_j <- rep(log(0.1), ncol(data))

# prevent estimation of SD
map_climate$lnsigma_j <- factor(rep(NA, ncol(data)))

#run final model fit with Delta0 and fixed SD
climate_fit <- dsem(sem=climate_sem, tsdata=data, 
                       family=family,
                       estimate_delta0=FALSE,
                       control=dsem_control(parameters = pars_climate,
                                            map = map_climate,
                                            quiet = TRUE,
                                            getsd = TRUE))

summary(climate_fit) #neither causal pathway significant

#-------------------------------------------------#
#Fit Larval Food Availability Causal Model:  ----
#-------------------------------------------------#

larval_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd,
   energetic_condition -> energetic_condition, 1, ar_cond, 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   log_prerecruit_abun -> log_prerecruit_abun, 1, ar_prerec
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp,
  log_recruit_abun -> log_recruit_abun, 1, ar_rec

#causal pathways
  sea_ice -> chla, 0, icetochla
  chla -> log_recruit_abun, 6, chlatorecruit"

#build model without running it
fit_build_larval <- dsem(sem = larval_sem, tsdata = data,
                          family = family,
                          estimate_delta0 = FALSE,
                          control = dsem_control(
                            run_model = FALSE))

pars_larval <- fit_build_larval$tmb_inputs$parameters
map_larval  <- fit_build_larval$tmb_inputs$map

# fix observation error at 0.1
pars_larval$lnsigma_j <- rep(log(0.1), ncol(data))

# prevent estimation of SD
map_larval$lnsigma_j <- factor(rep(NA, ncol(data)))

#run final model fit with Delta0 and fixed SD
larval_fit <- dsem(sem=larval_sem, tsdata=data, 
                    family=family,
                    estimate_delta0=FALSE,
                    control=dsem_control(parameters = pars_larval,
                                         map = map_larval,
                                         quiet = TRUE,
                                         getsd = TRUE))

summary(larval_fit) #non-significant and in opposite directions as expected

#-------------------------------------------------#
#Fit Juvenile Food Availability Causal Model:  ----
#-------------------------------------------------#

juvenile_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd,
   energetic_condition -> energetic_condition, 1, ar_cond, 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   log_prerecruit_abun -> log_prerecruit_abun, 1, ar_prerec
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp,
  log_recruit_abun -> log_recruit_abun, 1, ar_rec

#causal pathways
  sea_ice -> energetic_condition, 0, icetocondition
  log_invert -> energetic_condition, 0, inverttocondition
  energetic_condition -> log_recruit_abun, 1, conditiontorecruit"

#build model without running it
fit_build_juvenile <- dsem(sem = juvenile_sem, tsdata = data,
                         family = family,
                         estimate_delta0 = FALSE,
                         control = dsem_control(
                           run_model = FALSE))

pars_juvenile <- fit_build_juvenile$tmb_inputs$parameters
map_juvenile  <- fit_build_juvenile$tmb_inputs$map

# fix observation error at 0.1
pars_juvenile$lnsigma_j <- rep(log(0.1), ncol(data))

# prevent estimation of SD
map_juvenile$lnsigma_j <- factor(rep(NA, ncol(data)))

#run final model fit with Delta0 and fixed SD
juvenile_fit <- dsem(sem=juvenile_sem, tsdata=data, 
                   family=family,
                   estimate_delta0=FALSE,
                   control=dsem_control(parameters = pars_juvenile,
                                        map = map_juvenile,
                                        quiet = TRUE,
                                        getsd = TRUE))

summary(juvenile_fit) #strong rxn between sea ice and condition

#-------------------------------------------------#
#Fit Habitat Quality Causal Model:  ----
#-------------------------------------------------#

habitat_sem <- "
#temporal structure
consumption -> consumption, 1, ar_consump
extent_0c -> extent_0c, 1, ar_cp
bcd_imm -> bcd_imm, 1, ar_bcd,
energetic_condition -> energetic_condition, 1, ar_cond, 
chla -> chla, 1, ar_chla
log_invert -> log_invert, 1, ar_invert
log_prerecruit_abun -> log_prerecruit_abun, 1, ar_prerec
mean_ao -> mean_ao, 1, ar_ao
sea_ice -> sea_ice, 1, ar_ice
temp_occ -> temp_occ, 1, ar_temp,
log_recruit_abun -> log_recruit_abun, 1, ar_rec

#causal pathway
extent_0c -> temp_occ, 0, cptotempocc
temp_occ -> log_recruit_abun, 1, tempocctorecruit"

#build model without running it
fit_build_habitat <- dsem(sem = habitat_sem, tsdata = data,
                          family = family,
                          estimate_delta0 = FALSE,
                          control = dsem_control(
                            run_model = FALSE))

pars_habitat <- fit_build_habitat$tmb_inputs$parameters
map_habitat  <- fit_build_habitat$tmb_inputs$map

# fix observation error at 0.1
pars_habitat$lnsigma_j <- rep(log(0.1), ncol(data))

# prevent estimation of SD
map_habitat$lnsigma_j <- factor(rep(NA, ncol(data)))

#run final model fit with Delta0 and fixed SD
habitat_fit <- dsem(sem=habitat_sem, tsdata=data, 
                    family=family,
                    estimate_delta0=FALSE,
                    control=dsem_control(parameters = pars_habitat,
                                         map = map_habitat,
                                         quiet = TRUE,
                                         getsd = TRUE))

summary(habitat_fit) #strong causal links between cp/temperature occupied and
  #temperature occupied and recruitment

#-------------------------------------------------#
#Fit Predation Causal Model:  ----
#-------------------------------------------------#

predation_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd,
   energetic_condition -> energetic_condition, 1, ar_cond, 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   log_prerecruit_abun -> log_prerecruit_abun, 1, ar_prerec
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp,
  log_recruit_abun -> log_recruit_abun, 1, ar_rec

#causal pathways
  extent_0c -> consumption, 0, cptocod
  consumption -> log_recruit_abun, 3, codtorecruit"

#build model without running it
fit_build_predation <- dsem(sem = predation_sem, tsdata = data,
                         family = family,
                         estimate_delta0 = FALSE,
                         control = dsem_control(
                           run_model = FALSE))

pars_predation <- fit_build_predation$tmb_inputs$parameters
map_predation  <- fit_build_predation$tmb_inputs$map

# fix observation error at 0.1
pars_predation$lnsigma_j <- rep(log(0.1), ncol(data))

# prevent estimation of SD
map_predation$lnsigma_j <- factor(rep(NA, ncol(data)))

#run final model fit with Delta0 and fixed SD
predation_fit <- dsem(sem=predation_sem, tsdata=data, 
                   family=family,
                   estimate_delta0=FALSE,
                   control=dsem_control(parameters = pars_predation,
                                        map = map_predation,
                                        quiet = TRUE,
                                        getsd = TRUE))

summary(predation_fit) #no significant pathways

#-------------------------------------------------#
#Fit Disease Causal Model:  ----
#-------------------------------------------------#

disease_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd,
   energetic_condition -> energetic_condition, 1, ar_cond, 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   log_prerecruit_abun -> log_prerecruit_abun, 1, ar_prerec
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp,
  log_recruit_abun -> log_recruit_abun, 1, ar_rec

#causal pathways
  extent_0c -> bcd_imm, 0, cptobcd
  bcd_imm -> log_recruit_abun, 1, bcdtorecruit"

#build model without running it
fit_build_disease <- dsem(sem = disease_sem, tsdata = data,
                            family = family,
                            estimate_delta0 = FALSE,
                            control = dsem_control(
                              run_model = FALSE))

pars_disease <- fit_build_disease$tmb_inputs$parameters
map_disease  <- fit_build_disease$tmb_inputs$map

# fix observation error at 0.1
pars_disease$lnsigma_j <- rep(log(0.1), ncol(data))

# prevent estimation of SD
map_disease$lnsigma_j <- factor(rep(NA, ncol(data)))

#run final model fit with Delta0 and fixed SD
disease_fit <- dsem(sem=disease_sem, tsdata=data, 
                      family=family,
                      estimate_delta0=FALSE,
                      control=dsem_control(parameters = pars_disease,
                                           map = map_disease,
                                           quiet = TRUE,
                                           getsd = TRUE))

summary(disease_fit) #cold pool to bcd significant

#------------------------------------------------#
# Model comparison ----
#------------------------------------------------#

#marginal AIC comparison of base and causal models
AIC(iid_fit)
AIC(ar1_fit)
AIC(prerecruit_fit)
AIC(climate_fit)
AIC(larval_fit)
AIC(juvenile_fit)
AIC(habitat_fit)
AIC(predation_fit)
AIC(disease_fit)

model_comp <- tibble(
  model = c("iid", "ar1", "prerecruit", "climate_forcing", "larval_food_availability",
            "juvenile_food_availability", "habitat_quality", "predation", "disease"),
  AIC = c(AIC(iid_fit), AIC(ar1_fit), AIC(prerecruit_fit), AIC(climate_fit), AIC(larval_fit),
          AIC(juvenile_fit), AIC(habitat_fit), AIC(predation_fit), AIC(disease_fit))) %>%
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
  prerecruit = prerecruit_fit,
  climate_forcing = climate_fit,
  larval_food_availability = larval_fit,
  juvenile_food_availability = juvenile_fit,
  habitat_quality = habitat_fit,
  predation = predation_fit,
  disease = disease_fit)

# Function to extract recruitment deviations
extract_dev <- function(fit, model_name) {
  df <- as.data.frame(summary(fit))
  
  # Handle iid vs others (different parameter names)
  param_name <- if (model_name == "iid") "iid_rec" else "V[log_recruit_abun]"
  
  df %>%
    filter(name == param_name) %>%
    transmute(
      model = model_name,
      Estimate = Estimate,
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
  ylab("log recruit deviation") +
  xlab("Model")

#compute %reduction in estimated recruitment variance relative to IID model
iid_var <- dev_comp %>%
  filter(model == "iid") %>%
  pull(Estimate)

reduction_table <- dev_comp %>%
  select(model, variance = Estimate) %>%
  mutate(pct_reduction = (iid_var - variance) / iid_var * 100)

#------------------------------------------------#
# Exploration: play around with forecasting ----
#------------------------------------------------#
# One step forecast

#append future year to dataset
ts_ext <- tsdata %>%
  as.data.frame() %>%
  mutate(year = time(tsdata)) %>%
  add_row(year = max(year) + 1)

# convert back to ts if needed
ts_ext <- ts(ts_ext[,-which(names(ts_ext)=="year")],
             start = start(tsdata),
             frequency = frequency(tsdata))

#set driver scenarios so forecasts are scenario dependent
#persistence
ts_ext[nrow(ts_ext), "snow_crab"] <- tail(tsdata[, "snow_crab"], 1)

#warm year
ts_ext[nrow(ts_ext), "snow_crab"] <- tail(tsdata[, "snow_crab"], 1)

#high biomass

#predict by holding path coefficients constant, re-optimizing latent states
  #and propagating system dynamics forward, noting that predictions are 
  #on log scale!
pred <- predict(fit_dsem, newdata = ts_ext, type = "response")

#Forecasts represent the causal propagation of system dynamics under specified driver scenarios

#---------------------------------------------------------------#
# Exploration: play around with skill testing out-of-sample
    #predictive skill for causal vrs predictive model----
#---------------------------------------------------------------#

#now hindcast skill testing (does the causal structure predict unseen data)
  #OOS predictive skill
  #can we predict the mortality event?
#fit model on data up to t, generature simulations of t +1, extract recruitment, 
  #compare predicted vrs observed

#setup
years <- time(tsdata)
n <- length(years)
start_i <- 10  # e.g., require 10 years before first forecast
nsim <- 500  

results <- vector("list", n - 1)

#hindcast loop
for (i in 1:(n - 1)) {
  
  # 1. Subset data
  ts_sub <- window(tsdata, end = years[i])
  
  # 2. Fit model
  fit_i <- dsem(sem = sem, tsdata = ts_sub)
  
  # 3. Extend one step
  ts_ext <- ts_sub
  ts_ext <- rbind(ts_ext, rep(NA, ncol(ts_ext)))
  
  # 4. Set driver assumptions (example: persistence)
  ts_ext[nrow(ts_ext), ] <- ts_ext[nrow(ts_ext) - 1, ]
  
  # 5. Simulate forward
  sims <- simulate(fit_i, nsim = nsim, newdata = ts_ext)
  
  # sims is typically an array: [time, variable, simulation]
  
  # extract recruitment at final timestep
  rec_sims <- sims[nrow(ts_ext), "recruitment", ]
  
  # 6. Summarize distribution
  results[[i]] <- data.frame(
    year = years[i + 1],
    pred_median = median(rec_sims, na.rm = TRUE),
    pred_lo = quantile(rec_sims, 0.025, na.rm = TRUE),
    pred_hi = quantile(rec_sims, 0.975, na.rm = TRUE),
    obs = tsdata[i + 1, "recruitment"]
  )
}

hindcast <- bind_rows(results)

#evaluate skill with uncertainty 
hindcast <- hindcast %>%
  mutate(
    covered = obs >= pred_lo & obs <= pred_hi,
    interval_width = pred_hi - pred_lo,
    error = pred_median - obs
  )

# Coverage probability (should be ~0.95 for 95% CI)
coverage <- mean(hindcast$covered, na.rm = TRUE)

# RMSE using median prediction
RMSE <- sqrt(mean((hindcast$error)^2, na.rm = TRUE))

#Prediction intervals generated by simulating from the fitted DSEM at each hindcast step 

# Nash–Sutcliffe Efficiency
NSE <- 1 - sum((hindcast$obs - hindcast$pred)^2) /
  sum((hindcast$obs - mean(hindcast$obs))^2)

#plot
ggplot(hindcast, aes(x = year)) +
  
  # prediction interval (ribbon)
  geom_ribbon(aes(ymin = pred_lo, ymax = pred_hi),
              alpha = 0.2) +
  
  # median prediction line
  geom_line(aes(y = pred_median), linewidth = 1) +
  
  # observed points
  geom_point(aes(y = obs), size = 2) +
  
  # optional: connect observed values
  geom_line(aes(y = obs), linetype = "dashed") +
  
  labs(
    x = "Year",
    y = "Recruitment",
    title = "Hindcast with 1-year-ahead predictions",
    subtitle = "Shaded area = 95% prediction interval"
  ) +
  
  theme_minimal()

#misses that are outside interval 
hindcast <- hindcast %>%
  mutate(outside = obs < pred_lo | obs > pred_hi)

ggplot(hindcast, aes(x = year)) +
  geom_ribbon(aes(ymin = pred_lo, ymax = pred_hi), alpha = 0.2) +
  geom_line(aes(y = pred_median)) +
  geom_point(aes(y = obs, color = outside), size = 2) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  theme_minimal()

#now compare oos predictive skill of GAM and compare skill
gamm_fit <- gamm(recruitment ~ 
                   s(snow_crab) +
                   s(temperature),
                 correlation = corAR1(),
                 data = ts_sub)

#rolling hindcast
years <- time(tsdata)
n <- length(years)

gam_results <- vector("list", n - 1)

for (i in 1:(n - 1)) {
  
  ts_sub <- window(tsdata, end = years[i])
  
  # fit GAM
  gam_fit <- gam(recruitment ~ s(snow_crab) + s(temperature),
                 data = as.data.frame(ts_sub))
  
  # predict t+1
  newdata <- as.data.frame(ts_sub[nrow(ts_sub), , drop = FALSE])
  
  pred <- predict(gam_fit, newdata = newdata)
  
  gam_results[[i]] <- data.frame(
    year = years[i + 1],
    pred = pred,
    obs  = tsdata[i + 1, "recruitment"]
  )
}

gam_hindcast <- dplyr::bind_rows(gam_results)

#compute predictive skill
gam_hindcast <- gam_hindcast %>%
  mutate(
    error = pred - obs,
    sq_error = error^2
  )

gam_rmse <- sqrt(mean(gam_hindcast$sq_error, na.rm = TRUE))
gam_cor <- cor(gam_hindcast$pred, gam_hindcast$obs)

#compare to dsem
comparison <- data.frame(
  model = c("DSEM", "GAM"),
  RMSE = c(
    sqrt(mean((hindcast$pred_median - hindcast$obs)^2)),
    gam_rmse
  ),
  correlation = c(
    cor(hindcast$pred_median, hindcast$obs),
    gam_cor
  )
)

#plot comparison
bind_rows(
  hindcast %>% mutate(model = "DSEM", pred = pred_median),
  gam_hindcast %>% mutate(model = "GAM")
) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = obs), linetype = "dashed") +
  geom_line(aes(y = pred, color = model)) +
  facet_wrap(~model) +
  theme_minimal()

#Wakefield: collapse, development of indicators that looking back showed early warning
  #use of predictive models to evaluate indicator importance, shift to causal models, 
  #does a more mechanistic, causal understanding improve our oos-prediction, and 
  #one-year forecast (see Ward et al)? End with assumption of non-stationarity, and rxn can change 
  #in both 