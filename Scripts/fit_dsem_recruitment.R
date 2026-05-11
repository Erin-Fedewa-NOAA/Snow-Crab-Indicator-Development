#Goals ---- 
#Evaluate causal linkages between snow crab recruitment and ESP ecosystem indicators

#Author: E. Fedewa

#TO DO: finalize diagnostics, d-sep tests, simulation validation 

#-----------------------------------------#
#Read in packages and data ----
#-----------------------------------------#

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
                rename(sea_ice = Mar_Apr_ice_EBS_NBS,
                       consumption = ebs_consumption)

#read in recruitment response - using immature-only estimates from
  #new SAP maturity workflow 
recruit_abun <- read.csv("./Output/response_recruit_abundance.csv") %>% 
  select(-recruit_abun, -prerecruit_abun, -juvenile_abun)

#-----------------------------------------#
#Functions ----
#-----------------------------------------#

#FUNCTION FOR PLOTTING DISTRIBUTIONS OF COVARIATES
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

#FUNCTION FOR PLOTTING DSEM MODEL FIT:
plot_fit <- function(Y, fit, start_year = NULL,
                     vars_to_plot = NULL){
  
  # Extract latent states (model estimates of underlying system state)
  ParHat <- fit$obj$env$parList()
  pred <- ParHat$x_tj
  
  # Extract standard errors around latent states
  sd_list <- as.list(fit$sdrep, what = "Std.")
  SD <- sd_list$x_tj
  
  #create time variable for missing data
  if (is.null(start_year)) {
    if (!is.null(rownames(Y))) {
      years <- as.numeric(rownames(Y))
    } else {
      years <- seq_len(nrow(Y))
    }
  } else {
    years <- start_year + seq_len(nrow(Y)) - 1
  }
  
  # If NULL, plot all variables
  if (is.null(vars_to_plot)) {
    vars_to_plot <- colnames(Y)
  }
  
  #build a dataset for each causal pathway
  keep_idx <- which(colnames(Y) %in% vars_to_plot)
  
  # Build plotting dataframe
  out <- lapply(keep_idx, function(i) {
    
    tmp <- data.frame(
      year = years,
      variable = colnames(Y)[i],
      obs = as.numeric(Y[, i]),
      pred = as.numeric(pred[, i]),
      sd = as.numeric(SD[, i]))
    
    tmp %>%
      mutate(
        lower = pred - ifelse(is.na(sd), 0, sd),
        upper = pred + ifelse(is.na(sd), 0, sd))
    
    #combine all variables
  }) %>% bind_rows()
  
  out$variable <- factor(out$variable,
                         levels = vars_to_plot)
  
  #plot output
  ggplot(out, aes(x = year)) +
    #uncertainty around latent state
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = "blue", alpha = 0.2) +
    #latent state (model estimate)
    geom_line(aes(y = pred), color = "blue", linewidth = 1) +
    #observed data
    geom_point(aes(y = obs), color = "red", size = 2, na.rm = TRUE) +
    geom_line(aes(y = obs), color = "red", linetype = "dashed") +
    facet_wrap(~variable, scales = "free_y", ncol = 1) +
    labs(x = "", y = "Value") +
    theme_bw(base_size = 12) +
    theme(strip.background = element_rect(fill = "grey90"),
          strip.text = element_text(face = "bold"))
}

#FUNCTION TO EXTRACT RECRUITMENT DEVIATIONS FROM MODEL OUTPUT
extract_dev <- function(fit, model_name) {
  df <- as.data.frame(summary(fit))
  
  # Handle iid vs others (different parameter names)
  param_name <- if (model_name == "iid") "iid_rec" else "V[log_juvenile_abun]"
  
  df %>%
    filter(name == param_name) %>%
    transmute(
      model = model_name,
      Estimate = Estimate,
      Variance = Estimate^2, #variances are additive, so we need this for variance algebra 
      Std_Error = Std_Error)
}

#-----------------------------------------#
#Data wrangling ----
#-----------------------------------------#
#join indicator and responses
recruit_abun %>%
  select(year, juvenile_abun_mod) %>%
  full_join(indicators) %>%
  arrange(year) %>%
  rename_with(tolower) -> snow_dat

#refine indicators to those included in recruitment DAG
model_dat <- snow_dat %>%
  rename_with(tolower) %>%
  mutate(across(everything(), as.numeric)) %>%
  select(year, total_invert, temp_occ, 
         mean_ao, sea_ice, consumption, extent_0c, bcd_imm,
         energetic_condition, chla, juvenile_abun_mod)

#plot covariates
model_dat %>%
  select(-juvenile_abun_mod) %>%
  rename("Juvenile snow crab disease prevalence" = bcd_imm, "Chl-a concentration" = chla,
         "Pacific cod consumption" = consumption,
         "Juvenile snow crab energetic condition" = energetic_condition,
         "Cold pool extent" = extent_0c, "Arctic Oscillation" = mean_ao,
         "Spring sea ice extent" = sea_ice, "Benthic prey density" = total_invert,
         "Juvenile snow crab temperature occupied" = temp_occ) %>%
  pivot_longer(-year, names_to="variable", values_to="value") %>%
  ggplot(aes(year, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", nrow=4) +
  theme_bw() +
  labs(x="")

#Plot distributions of covariates
plot_histo(model_dat %>% select(-year))

#Scale all variables and transform abundance variables 
scaled_dat <- model_dat %>%
  mutate(log_invert = log(total_invert),
         log_juvenile_abun = log(juvenile_abun_mod)) %>%
  select(-total_invert, -juvenile_abun_mod) %>%
  mutate(across(-year, ~ as.numeric(scale(.))))
  
#plot distributions after log transformation
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
            chla = gaussian_fixed_sd(sd=.1),
            log_invert = gaussian_fixed_sd(sd=.1),
            mean_ao = gaussian_fixed_sd(sd=.1),
            sea_ice = gaussian_fixed_sd(sd=.1),
            temp_occ = gaussian_fixed_sd(sd=.1),
            log_juvenile_abun = gaussian_fixed_sd(sd=.1))
            
#------------------------------------------------#
# Fit Base IID recruitment model ----
#------------------------------------------------#

iid_sem <- "
  #temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
  log_juvenile_abun <-> log_juvenile_abun, 0, iid_rec"

iid_fit <- dsem(sem=iid_sem, tsdata=data, 
                 family=family)

summary(iid_fit)

#------------------------------------------------#
# Fit Base AR1 recruitment model ----
#------------------------------------------------#

ar_sem <- "
  #temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   log_juvenile_abun -> log_juvenile_abun, 1, ar_juv"

ar1_fit <- dsem(sem=ar_sem, tsdata=data, 
                family=family)

summary(ar1_fit)

#-------------------------------------------------#
#Fit simple regression with sea ice:  ----
#-------------------------------------------------#

ice_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   log_juvenile_abun -> log_juvenile_abun, 1, ar_juv

#causal pathways
  sea_ice -> log_juvenile_abun, 1, icetocond"

ice_fit <- dsem(sem=ice_sem, tsdata=data, 
                family=family)

summary(ice_fit) 
  #single causal relationship collapses down to linear regression in dsem

#-----------------------------------------#
#Fit Climate Forcing Causal Model:  ----
#-----------------------------------------#

#1) TEST FOR DAG-DATA CONSISTENCY

#download specified DAG from dagitty.net - conditional independencies are 
#identified based on structure of DAG drawn in dagitty
climate_dag <- dagitty('dag {
log_juvenile_abun [outcome,pos="-0.272,0.239"]
mean_ao [exposure,pos="-1.490,-1.142"]
sea_ice [exposure,pos="-0.277,-1.231"]
mean_ao -> sea_ice
sea_ice -> log_juvenile_abun
}')

#plot DAG
plot(climate_dag) 

ggdag_status(climate_dag, text = FALSE, use_labels = "name") +
  #guides(color = "none") +  # Turn off legend
  theme_dag()

#identify paths
paths(climate_dag)
#2 open causal pathways 

#find adjustment sets for response variable 
adjustmentSets(climate_dag, exposure="mean_ao", outcome="log_juvenile_abun")
#This tells us that no covariate adjustment is necessary to identify the causal effect
  #ie there are no open backdoor paths 

#2) FIT CLIMATE FORCING DSEM MODEL

climate_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   log_juvenile_abun -> log_juvenile_abun, 1, ar_juv

#causal pathways
  mean_ao -> sea_ice, 0, aotoice
  sea_ice -> log_juvenile_abun, 1, icetorecruit"

climate_fit <- dsem(sem=climate_sem, tsdata=data, 
                       family=family)

summary(climate_fit) 

#3) DIAGNOSTICS

#Convergence diagnostics:
#Hessian/SE - should be no hessian warnings or NA SE estimates
climate_fit$sdrep

#check maximum final gradient- should be < 0.001
max(climate_fit$sdrep$gradient.fixed)

#Identifiability/overfitting
climate_fit$sdrep$pdHess # TRUE = good here
summary(climate_fit$sdrep, "fixed")[, "Std. Error"] #shouldn't be any NA/NaN

#Residual diagnostics:
res <- residuals(climate_fit, type = "response")

res_df <- as.data.frame(res) %>%
  mutate(time = 1:n()) %>%
  pivot_longer(-time, names_to = "variable", values_to = "residual")

#autocorrelation of residuals
acf_df <- res_df %>%
  group_by(variable) %>%
  summarise(acf = list(acf(na.omit(residual), plot = FALSE))) %>%
  mutate(lag = map(acf, ~ .x$lag),
         acf_val = map(acf, ~ .x$acf)) %>%
  unnest(c(lag, acf_val))

ggplot(acf_df, aes(x = lag, y = acf_val)) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = lag, yend = 0)) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Lag",
       y = "ACF",
       title = "Autocorrelation of Residuals") +
  theme_minimal()

#normality of residuals
ggplot(res_df, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "QQ Plots of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
#approximately normal, but some deviations in tails 

# calculate leave-one-out residuals and display using DHARMa
samples = loo_residuals(climate_fit, what="samples", track_progress=FALSE)
which_use = which(!is.na(data))
fitResp = loo_residuals(climate_fit, what="loo", track_progress=FALSE)[,'est']
simResp = apply(samples, MARGIN=3, FUN=as.vector)[which_use,]

res = DHARMa::createDHARMa(
  simulatedResponse = simResp,
  observedResponse = unlist(data)[which_use],
  fittedPredictedResponse = fitResp )
plot(res)
#need to follow up on this- not sure if dharma residuals are a valid approach 
  #for a state-space model containing lags? -But I think they work if you use
  #a fixed family 

#4) PLOT MODEL FIT

#specify variables for model being plotted
#i.e. specify if you don't want to plot variables included only as AR1 process
causal_vars <- c("sea_ice",
                 "log_juvenile_abun",
                 "mean_ao")

#plot
plot_fit(data, climate_fit, start_year = 1979, vars_to_plot = causal_vars)

#-------------------------------------------------#
#Fit Larval Food Availability Causal Model:  ----
#-------------------------------------------------#

#1) TEST FOR DAG-DATA CONSISTENCY

#download specified DAG from dagitty.net - conditional independencies are 
#identified based on structure of DAG drawn in dagitty
larval_dag <- dagitty('dag {
chla [exposure,pos="-0.277,-1.231"]
log_juvenile_abun [outcome,pos="-0.272,0.239"]
sea_ice [exposure,pos="-1.490,-1.142"]
chla -> log_juvenile_abun
sea_ice -> chla
}')

#plot DAG
plot(larval_dag) 

ggdag_status(larval_dag, text = FALSE, use_labels = "name") +
  theme_dag()

#identify paths
paths(larval_dag)
  #2 open causal pathways 

#find adjustment sets for response variable 
adjustmentSets(larval_dag)
  #no covariate adjustment necessary 

#2) FIT LARVAL FOOD DSEM MODEL

larval_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   log_juvenile_abun -> log_juvenile_abun, 1, ar_juv

#causal pathways
  sea_ice -> chla, 0, icetochla
  chla -> log_juvenile_abun, 4, chlatorecruit"

larval_fit <- dsem(sem=larval_sem, tsdata=data, 
                    family=family)

summary(larval_fit) 

#3) DIAGNOSTICS

#Convergence diagnostics:
#Hessian/SE - should be no hessian warnings or NA SE estimates
larval_fit$sdrep

#check maximum final gradient- should be < 0.001
max(larval_fit$sdrep$gradient.fixed)

#Identifiability/overfitting
larval_fit$sdrep$pdHess # TRUE = good here
summary(larval_fit$sdrep, "fixed")[, "Std. Error"] #shouldn't be any NA/NaN

#Residual diagnostics:
res <- residuals(larval_fit, type = "response")

res_df <- as.data.frame(res) %>%
  mutate(time = 1:n()) %>%
  pivot_longer(-time, names_to = "variable", values_to = "residual")

#autocorrelation of residuals
acf_df <- res_df %>%
  group_by(variable) %>%
  summarise(acf = list(acf(na.omit(residual), plot = FALSE))) %>%
  mutate(lag = map(acf, ~ .x$lag),
         acf_val = map(acf, ~ .x$acf)) %>%
  unnest(c(lag, acf_val))

ggplot(acf_df, aes(x = lag, y = acf_val)) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = lag, yend = 0)) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Lag",
       y = "ACF",
       title = "Autocorrelation of Residuals") +
  theme_minimal()

#normality of residuals
ggplot(res_df, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "QQ Plots of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

#4) PLOT MODEL FIT

#specify variables for model being plotted
#i.e. specify if you don't want to plot variables included only as AR1 process
causal_vars <- c("sea_ice",
                 "log_juvenile_abun",
                 "chla")

#plot
plot_fit(data, larval_fit, start_year = 1979, vars_to_plot = causal_vars)

#-------------------------------------------------#
#Fit Juvenile Food Availability Causal Model:  ----
#-------------------------------------------------#

#1) TEST FOR DAG-DATA CONSISTENCY

#download specified DAG from dagitty.net - conditional independencies are 
#identified based on structure of DAG drawn in dagitty
juvenile_dag <- dagitty('dag {
energetic_condition [exposure,pos="-0.277,-1.231"]
log_invert [exposure,pos="-0.811,-0.259"]
log_juvenile_abun [outcome,pos="-0.272,0.239"]
sea_ice [exposure,pos="-1.490,-1.142"]
energetic_condition -> log_juvenile_abun
log_invert -> energetic_condition
sea_ice -> energetic_condition
}')

#plot DAG
plot(juvenile_dag) 

ggdag_status(juvenile_dag, text = FALSE, use_labels = "name") +
  theme_dag()

#identify paths
paths(juvenile_dag)
#3 open causal pathways 

#find adjustment sets for response variable 
adjustmentSets(juvenile_dag)
#no covariate adjustment necessary 

#2) FIT JUVENILE FOOD DSEM MODEL

juvenile_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   log_juvenile_abun -> log_juvenile_abun, 1, ar_juv

#causal pathways
  sea_ice -> energetic_condition, 0, icetocondition
  log_invert -> energetic_condition, 0, inverttocondition
  energetic_condition -> log_juvenile_abun, 1, conditiontorecruit"

juvenile_fit <- dsem(sem=juvenile_sem, tsdata=data, 
                   family=family)

summary(juvenile_fit) 

#3) DIAGNOSTICS

#Convergence diagnostics:
#Hessian/SE - should be no hessian warnings or NA SE estimates
juvenile_fit$sdrep

#check maximum final gradient- should be < 0.001
max(juvenile_fit$sdrep$gradient.fixed)

#Identifiability/overfitting
juvenile_fit$sdrep$pdHess # TRUE = good here
summary(juvenile_fit$sdrep, "fixed")[, "Std. Error"] #shouldn't be any NA/NaN

#Residual diagnostics:
res <- residuals(juvenile_fit, type = "response")

res_df <- as.data.frame(res) %>%
  mutate(time = 1:n()) %>%
  pivot_longer(-time, names_to = "variable", values_to = "residual")

#autocorrelation of residuals
acf_df <- res_df %>%
  group_by(variable) %>%
  summarise(acf = list(acf(na.omit(residual), plot = FALSE))) %>%
  mutate(lag = map(acf, ~ .x$lag),
         acf_val = map(acf, ~ .x$acf)) %>%
  unnest(c(lag, acf_val))

ggplot(acf_df, aes(x = lag, y = acf_val)) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = lag, yend = 0)) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Lag",
       y = "ACF",
       title = "Autocorrelation of Residuals") +
  theme_minimal()

#normality of residuals
ggplot(res_df, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "QQ Plots of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

#4) PLOT MODEL FIT

#specify variables for model being plotted
#i.e. specify if you don't want to plot variables included only as AR1 process
causal_vars <- c("sea_ice", "log_invert",
                 "energetic_condition", "log_juvenile_abun")

#plot
plot_fit(data, juvenile_fit, start_year = 1979, vars_to_plot = causal_vars)

#-------------------------------------------------#
#Fit Habitat Quality Causal Model:  ----
#-------------------------------------------------#

#1) TEST FOR DAG-DATA CONSISTENCY

#download specified DAG from dagitty.net - conditional independencies are 
#identified based on structure of DAG drawn in dagitty
habitat_dag <- dagitty('dag {
cold_pool [exposure,pos="-1.490,-1.142"]
log_juvenile_abun [outcome,pos="-0.272,0.239"]
temp_occ [exposure,pos="-0.277,-1.231"]
cold_pool -> temp_occ
temp_occ -> log_juvenile_abun
}
')

#plot DAG
plot(habitat_dag) 

ggdag_status(habitat_dag, text = FALSE, use_labels = "name") +
  theme_dag()

#identify paths
paths(habitat_dag)
#2 open causal pathways 

#find adjustment sets for response variable 
adjustmentSets(habitat_dag)
#no covariate adjustment necessary 

#2) FIT HABITAT QUALITY DSEM MODEL

habitat_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   log_juvenile_abun -> log_juvenile_abun, 1, ar_juv

#causal pathway
  extent_0c -> temp_occ, 0, cptotempocc
  temp_occ -> log_juvenile_abun, 1, tempocctorecruit"

habitat_fit <- dsem(sem=habitat_sem, tsdata=data, 
                    family=family)

summary(habitat_fit) 

#3) DIAGNOSTICS

#Convergence diagnostics:
#Hessian/SE - should be no hessian warnings or NA SE estimates
habitat_fit$sdrep

#check maximum final gradient- should be < 0.001
max(habitat_fit$sdrep$gradient.fixed)

#Identifiability/overfitting
habitat_fit$sdrep$pdHess # TRUE = good here
summary(habitat_fit$sdrep, "fixed")[, "Std. Error"] #shouldn't be any NA/NaN

#Residual diagnostics:
res <- residuals(habitat_fit, type = "response")

res_df <- as.data.frame(res) %>%
  mutate(time = 1:n()) %>%
  pivot_longer(-time, names_to = "variable", values_to = "residual")

#autocorrelation of residuals
acf_df <- res_df %>%
  group_by(variable) %>%
  summarise(acf = list(acf(na.omit(residual), plot = FALSE))) %>%
  mutate(lag = map(acf, ~ .x$lag),
         acf_val = map(acf, ~ .x$acf)) %>%
  unnest(c(lag, acf_val))

ggplot(acf_df, aes(x = lag, y = acf_val)) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = lag, yend = 0)) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Lag",
       y = "ACF",
       title = "Autocorrelation of Residuals") +
  theme_minimal()

#normality of residuals
ggplot(res_df, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "QQ Plots of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

#4) PLOT MODEL FIT

#specify variables for model being plotted
#i.e. specify if you don't want to plot variables included only as AR1 process
causal_vars <- c("extent_0c", "temp_occ",
                 "log_juvenile_abun")

#plot
plot_fit(data, habitat_fit, start_year = 1979, vars_to_plot = causal_vars)

#-------------------------------------------------#
#Fit Predation Causal Model:  ----
#-------------------------------------------------#

#1) TEST FOR DAG-DATA CONSISTENCY

#download specified DAG from dagitty.net - conditional independencies are 
#identified based on structure of DAG drawn in dagitty
predation_dag <- dagitty('dag {
cold_pool [exposure,pos="-1.490,-1.142"]
consumption [exposure,pos="-0.277,-1.231"]
log_juvenile_abun [outcome,pos="-0.272,0.239"]
cold_pool -> consumption
consumption -> log_juvenile_abun
}
')

#plot DAG
plot(predation_dag) 

ggdag_status(predation_dag, text = FALSE, use_labels = "name") +
  theme_dag()

#identify paths
paths(predation_dag)
#2 open causal pathways 

#find adjustment sets for response variable 
adjustmentSets(predation_dag)
#no covariate adjustment necessary 

#2) FIT PREDATION DSEM MODEL

predation_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   log_juvenile_abun -> log_juvenile_abun, 1, ar_juv

#causal pathways
  extent_0c -> consumption, 0, cptocod
  consumption -> log_juvenile_abun, 1, codtorecruit"

predation_fit <- dsem(sem=predation_sem, tsdata=data, 
                   family=family)

summary(predation_fit) 

#3) DIAGNOSTICS

#Convergence diagnostics:
#Hessian/SE - should be no hessian warnings or NA SE estimates
predation_fit$sdrep

#check maximum final gradient- should be < 0.001
max(predation_fit$sdrep$gradient.fixed)

#Identifiability/overfitting
predation_fit$sdrep$pdHess # TRUE = good here
summary(predation_fit$sdrep, "fixed")[, "Std. Error"] #shouldn't be any NA/NaN

#Residual diagnostics:
res <- residuals(predation_fit, type = "response")

res_df <- as.data.frame(res) %>%
  mutate(time = 1:n()) %>%
  pivot_longer(-time, names_to = "variable", values_to = "residual")

#autocorrelation of residuals
acf_df <- res_df %>%
  group_by(variable) %>%
  summarise(acf = list(acf(na.omit(residual), plot = FALSE))) %>%
  mutate(lag = map(acf, ~ .x$lag),
         acf_val = map(acf, ~ .x$acf)) %>%
  unnest(c(lag, acf_val))

ggplot(acf_df, aes(x = lag, y = acf_val)) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = lag, yend = 0)) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Lag",
       y = "ACF",
       title = "Autocorrelation of Residuals") +
  theme_minimal()

#normality of residuals
ggplot(res_df, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "QQ Plots of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

#4) PLOT MODEL FIT

#specify variables for model being plotted
#i.e. specify if you don't want to plot variables included only as AR1 process
causal_vars <- c("extent_0c", "consumption",
                 "log_juvenile_abun")

#plot
plot_fit(data, predation_fit, start_year = 1979, vars_to_plot = causal_vars)

#-------------------------------------------------#
#Fit Disease Causal Model:  ----
#-------------------------------------------------#

#1) TEST FOR DAG-DATA CONSISTENCY

#download specified DAG from dagitty.net - conditional independencies are 
#identified based on structure of DAG drawn in dagitty
disease_dag <- dagitty('dag {
bcd [exposure,pos="-0.277,-1.231"]
cold_pool [exposure,pos="-1.490,-1.142"]
log_juvenile_abun [outcome,pos="-0.272,0.239"]
bcd -> log_juvenile_abun
cold_pool -> bcd
}
')

#plot DAG
plot(disease_dag) 

ggdag_status(disease_dag, text = FALSE, use_labels = "name") +
  theme_dag()

#identify paths
paths(disease_dag)
#2 open causal pathways 

#find adjustment sets for response variable 
adjustmentSets(disease_dag)
#no covariate adjustment necessary 

#2) FIT DISEASE DSEM MODEL

disease_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   log_juvenile_abun -> log_juvenile_abun, 1, ar_juv

#causal pathways
  extent_0c -> bcd_imm, 0, cptobcd
  bcd_imm -> log_juvenile_abun, 1, bcdtorecruit"

disease_fit <- dsem(sem=disease_sem, tsdata=data, 
                      family=family)

summary(disease_fit) 

#3) DIAGNOSTICS

#Convergence diagnostics:
#Hessian/SE - should be no hessian warnings or NA SE estimates
disease_fit$sdrep

#check maximum final gradient- should be < 0.001
max(disease_fit$sdrep$gradient.fixed)

#Identifiability/overfitting
disease_fit$sdrep$pdHess # TRUE = good here
summary(disease_fit$sdrep, "fixed")[, "Std. Error"] #shouldn't be any NA/NaN

#Residual diagnostics:
res <- residuals(disease_fit, type = "response")

res_df <- as.data.frame(res) %>%
  mutate(time = 1:n()) %>%
  pivot_longer(-time, names_to = "variable", values_to = "residual")

#autocorrelation of residuals
acf_df <- res_df %>%
  group_by(variable) %>%
  summarise(acf = list(acf(na.omit(residual), plot = FALSE))) %>%
  mutate(lag = map(acf, ~ .x$lag),
         acf_val = map(acf, ~ .x$acf)) %>%
  unnest(c(lag, acf_val))

ggplot(acf_df, aes(x = lag, y = acf_val)) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = lag, yend = 0)) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(x = "Lag",
       y = "ACF",
       title = "Autocorrelation of Residuals") +
  theme_minimal()

#normality of residuals
ggplot(res_df, aes(sample = residual)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "QQ Plots of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

#4) PLOT MODEL FIT

#specify variables for model being plotted
#i.e. specify if you don't want to plot variables included only as AR1 process
causal_vars <- c("extent_0c", "bcd_imm",
                 "log_juvenile_abun")

#plot
plot_fit(data, disease_fit, start_year = 1979, vars_to_plot = causal_vars)

#-------------------------------------------------#
#Fit Full Model:  ----
#-------------------------------------------------#

#testing a full model that includes the strongest causal linkages from 
  #hypothesis-based models above

full_sem <- "
#temporal structure
   consumption -> consumption, 1, ar_consump
   extent_0c -> extent_0c, 1, ar_cp
   bcd_imm -> bcd_imm, 1, ar_bcd
   energetic_condition -> energetic_condition, 1, ar_cond 
   chla -> chla, 1, ar_chla
   log_invert -> log_invert, 1, ar_invert
   mean_ao -> mean_ao, 1, ar_ao
   sea_ice -> sea_ice, 1, ar_ice
   temp_occ -> temp_occ, 1, ar_temp
   log_juvenile_abun -> log_juvenile_abun, 1, ar_juv

#causal pathways
  sea_ice -> energetic_condition, 0, icetocond
  energetic_condition -> log_juvenile_abun, 1, condtorecruit
  sea_ice -> extent_0c, 0, icetocp
  extent_0c -> temp_occ, 0, cptotempocc
  temp_occ -> log_juvenile_abun, 1, tempocctorecruit"

full_fit <- dsem(sem=full_sem, tsdata=data, 
                    family=family)

summary(full_fit) 

#While this model reduces recruitment variation by ~85%, it's largely 
  #driven by a 6-year energetic condition dataset, which is likely 
  #only fitting heatwave conditons well, and that's driving trends 

#Interestingly when we drop energetic condition from this model, a 
  #sea ice -> recruitment causal link is insignificant

#------------------------------------------------#
# Model comparison ----
#------------------------------------------------#

#marginal AIC comparison of base and causal models
AIC(iid_fit)
AIC(ar1_fit)
AIC(climate_fit)
AIC(larval_fit)
AIC(juvenile_fit)
AIC(habitat_fit)
AIC(predation_fit)
AIC(disease_fit)

model_comp <- tibble(
  model = c("iid", "ar1", "climate_forcing", "larval_food_availability",
            "juvenile_food_availability", "habitat_quality", "predation", "disease"),
  AIC = c(AIC(iid_fit), AIC(ar1_fit), AIC(climate_fit), AIC(larval_fit),
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
  climate_forcing = climate_fit,
  larval_food_availability = larval_fit,
  juvenile_food_availability = juvenile_fit,
  habitat_quality = habitat_fit,
  predation = predation_fit,
  disease = disease_fit)

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

#----------------------------------------#
# Compute direct/indirect effects ----
#----------------------------------------#

#We'll use our juvenile food availability model as an example

#extract estimate and p-value for causal pathways 
paths_of_interest <- subset(summary(full_fit),
                            (first == "sea_ice" & second == "energetic_condition") |
                              (first == "log_invert" & second == "energetic_condition") |
                              (first == "energetic_condition" & second == "log_juvenile_abun"))

paths_of_interest[, c("first", "second", "lag", "Estimate", "Std_Error", "p_value")]

#indirect effect of sea ice
icetocondition <- paths_of_interest %>%
  filter((first == "sea_ice" & second == "energetic_condition")) %>%
  pull(Estimate)

conditiontorecruit <- paths_of_interest %>%
  filter((first == "energetic_condition" & second == "log_juvenile_abun")) %>%
  pull(Estimate)

indirect_ice = icetocondition * conditiontorecruit #0.36

#-------------------------------------------#
# Compute cumulative effects ----
#-------------------------------------------#

#lag/cumulative effects plots:
  #lag: defines when the effect enters the system
  #variance partitioning over time shows how long a lag's influence persists 

# Calculate cumulative lagged effects
  #ie if variable X changes at time t, what is the cumulative effect of variable Y 
#after lag k, accounting for direct + indirect pathways
effect = total_effect(full_fit, n_lags = 2) 

# Plot total effect
ggplot( effect) + 
  geom_bar(aes(lag, total_effect, fill=lag), stat='identity', col='black', position='dodge' ) +
  facet_grid( from ~ to  )


#relative importance of variables as predictors of recruitment
  #i.e. at each time step, where proportion of recruitment variability is coming from
partition_variance(full_fit,
                   which_response = "log_juvenile_abun",
                   n_times = 10 )

#plot
var_df <- as.data.frame(partition_variance(full_fit,
                                           which_response = "log_juvenile_abun",
                                           n_times = 10)$proportion_variance_explained)

var_df$time <- 1:nrow(var_df)

# Pivot longer
var_long <- var_df %>%
  pivot_longer(cols = c(sea_ice, extent_0c, energetic_condition, temp_occ, log_juvenile_abun),
               names_to = "Component",
               values_to = "Proportion")

# Plot
ggplot(var_long, aes(x = time, y = Proportion, fill = Component)) +
  geom_area(alpha = 0.8, color = "black") +
  labs(x = "Time step",
       y = "Proportion of variance explained",
       fill = "Component",
       title = "Variance partitioning of recruitment dynamics") +
  theme_minimal()

#second view
ggplot(var_long, aes(x = time, y = Proportion, color = Component)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(x = "Time step",
       y = "Proportion of recruitment variance explained",
       title = "Recruitment variance contributions over time") +
  theme_minimal()

#plot at final time step 10 
var_df %>%
  filter(time == 10) %>%
  pivot_longer(cols = c(sea_ice, extent_0c, energetic_condition, temp_occ, log_juvenile_abun),
               names_to = "Component",
               values_to = "Proportion") %>%
  ggplot(aes(x = Component, y = Proportion, fill = Component)) +
  geom_col() +
  ylim(0,1) +
  theme_minimal() +
  labs(title = "Variance partitioning at equilibrium",
       y = "Proportion of variance explained")

#extract at final time step
var_df %>%
  filter(time == max(time))
#at time step 10, AR juvenile abundance explains 12%, sea ice
  #explains 26% and energetic condition explains 61% of recruitment variation

#-----------------------------------------------------------#
# Evaluate sensitivity to fixed observation error ----
#-----------------------------------------------------------#

#Note: this uses the old workflow of updating map and parameter outputs in tmb to
  #fix observation error- needs to be updated and run on models being used for 
  #causal inference in September! 

#test plausible range of fixed values
obs_sd_values <- c(0.1, 0.2, 0.3, 0.4, 0.5)

results_list <- list()

#loop through values and refit final model
for (i in seq_along(obs_sd_values)) {
  
  sd_val <- obs_sd_values[i]
  
  # update observation error
  pars_tmp <- pars_final
  map_tmp  <- map_final
  
  pars_tmp$lnsigma_j <- rep(log(sd_val), ncol(data))
  map_tmp$lnsigma_j  <- factor(rep(NA, ncol(data)))
  
  #fit model with each observation error value
  fit_tmp <- dsem(
    sem = sem_final,
    tsdata = data,
    family = family,
    estimate_delta0 = FALSE,
    control = dsem_control(
      parameters = pars_tmp,
      map = map_tmp,
      quiet = TRUE,
      getsd = TRUE))
  
  sm <- as.data.frame(summary(fit_tmp))
  
  # extract key pathways
  effects <- sm %>%
    filter(name %in% c("icetohybrid", "icetosnow", "snowtohybrid")) %>%
    mutate(obs_sd = sd_val)
  
  results_list[[i]] <- effects
}

sensitivity_results <- bind_rows(results_list)

#plot sensitivity
ggplot(sensitivity_results, aes(x = obs_sd, y = Estimate, color = name)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - Std_Error,
                    ymax = Estimate + Std_Error),
                width = 0.02) +
  labs(x = "Fixed Observation SD",
       y = "Effect Size",
       color = "Path") +
  theme_minimal()

#-------------------------------------------------#
# Monte Carlo Simulation-based validation ----
#-------------------------------------------------#

#Here, we'll simulate data from our habitat quality dsem model, refit the model to
#each simulated dataset, and compare parameter estimates to true values to check
#whether the fitted model can recover its own parameters when new data is simulated

#function modified from J. Bigman ATF ESP 

simTestDSEM <- function(fitDSEM, sem, n_sim, family) {
  
  DSEMlist <- list()
  
  for (i in 1:n_sim) {
    
    # Simulate data 
    simDSEM <- simulate(fitDSEM, resimulate_gmrf = TRUE, fill_missing = TRUE)
    
    try({
      
      sim_data <- simDSEM[[1]]
      
     # Fit model
      tempFit <- dsem(
        sem = sem,
        tsdata = ts(sim_data),
        family = family)
      
      #Extract parameters, fitted values and observed values 
      DSEMlist[[i]] <- list(
        beta = tempFit$opt$par[names(tempFit$opt$par) == "beta_z"],
        pred = predict(tempFit),
        obs  = sim_data)
    }, silent = TRUE)
  }
  
  DSEMlist <- DSEMlist[!sapply(DSEMlist, is.null)]
  
  return(DSEMlist)
}

#now run 500 simulations on our model 
selfSimExp <- simTestDSEM(
  fitDSEM = habitat_fit,
  sem = habitat_sem,
  n_sim = 500,   
  family = family)

#parameter recovery:
beta_list <- lapply(selfSimExp, function(x) x$beta) #extract beta parameter vectors
beta_mat <- do.call(cbind, beta_list) #rows = parameters, cols=simulations
df <- as.data.frame(beta_mat)

#add parameter labels from simulation
df <- df %>%
  mutate(Path  = habitat_fit$sem_full$path[habitat_fit$sem_full$parameter != 0],
         Param = habitat_fit$sem_full$name[habitat_fit$sem_full$parameter != 0]) %>%
  pivot_longer(cols = starts_with("V"), values_to = "Beta")

#true parameters from fitted model
df_true <- data.frame(
  Param = habitat_fit$sem_full$name[habitat_fit$sem_full$parameter != 0],
  Path  = habitat_fit$sem_full$path[habitat_fit$sem_full$parameter != 0],
  Beta_true = habitat_fit$opt$par[names(habitat_fit$opt$par) == "beta_z"])

#and join
param_df <- df %>%
  inner_join(df_true, by = c("Path", "Param"))
#the v[] params are our process variance for each variable

#plot parameter recovery results
ggplot(param_df %>% filter(!str_starts(Param, "V")),
       aes(x = Beta, y = Param)) +
  geom_violin(fill = "lightblue", scale = "width") +
  geom_point(aes(x = Beta_true), color = "red") +
  geom_vline(xintercept = 0) +
  theme_minimal() +
  ggtitle("Parameter Recovery")

#violin plots represent the distribution of estimated beta parameters across all 
#simulations, red dot is true parameter value from original fitted model, 0 line
#is "no effect"
#Looks good! Model is recovering parameters reliably

#compute bias and RMSE
summary_df <- param_df %>%
  filter(!str_starts(Param, "V"),
         !str_starts(Param, "ar_")) %>%
  group_by(Param) %>%
  summarise(
    Bias = mean(Beta - Beta_true),
    SD = sd(Beta),
    RMSE = sqrt(mean((Beta - Beta_true)^2)),
    Var = var(Beta),
    Rel_RMSE = RMSE / abs(mean(Beta_true)))
#precision= variability across simulations
#negligible bias, RMSE ~= SD, which tells us that error is random, not structural

#plot relative RMSE
ggplot(summary_df, aes(x = Rel_RMSE, y = reorder(Param, Rel_RMSE))) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Relative RMSE (scaled by true parameter magnitude)",
    x = "Relative RMSE",
    y = "Parameter")

#plot bias
ggplot(summary_df, aes(x = Bias, y = reorder(Param, Bias))) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Bias",
       x = "Mean error (Estimate - True)",
       y = "Parameter")

#now we can plot error and bias as distributions from simulations vrs
  #condensed point estimates
param_df_sim <- df %>%
  mutate(sim_id = rep(seq_along(selfSimExp), each = nrow(df) / length(selfSimExp))) %>%
  inner_join(df_true, by = c("Path", "Param"))

#and compute simulation-level bias + relative RMSE
sim_summary <- param_df_sim %>%
  filter(!is.na(Beta_true)) %>%
  filter(!str_starts(Param, "V"),
         !str_starts(Param, "ar_")) %>%
  group_by(sim_id, Param) %>%
  summarise(
    Bias = mean(Beta - Beta_true),
    RMSE = sqrt(mean((Beta - Beta_true)^2)),
    RMSE_rel = RMSE / (abs(mean(Beta_true)) + 1e-8),
    .groups = "drop")

#bias distribution plot
ggplot(sim_summary, aes(x = Bias, y = Param)) +
  geom_violin(fill = "grey80") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Distribution of Bias Across Simulations",
    x = "Bias (estimate - true)",
    y = "Parameter")

#relative RMSE distribution
ggplot(sim_summary, aes(x = RMSE_rel, y = Param)) +
  geom_violin(fill = "skyblue", alpha = 0.5) +
  #scale_x_log10() +
  theme_minimal() +
  labs(
    title = "Distribution of Relative RMSE Across Simulations",
    x = "Relative RMSE",
    y = "Parameter")


#Fit + residual diagnostics:
  #i.e. can our simulations reproduce residual structure seen in our final 
  #fitted model 

#build residual dataset from simulations 
sim_res_df <- selfSimExp %>%
  imap(~ tibble(
    sim_id = as.character(.y),
    fitted = as.numeric(.x$pred),
    resid  = as.numeric(.x$obs - .x$pred))) %>%
  bind_rows()

#extract residuals from final hybrid model fit
fit_res_df <- tibble(
  sim_id = "original",
  fitted = as.numeric(predict(habitat_fit)),
  resid  = as.numeric(residuals(habitat_fit)))

#combine
all_res_df <- bind_rows(
  sim_res_df %>% mutate(type = "Simulated"),
  fit_res_df %>% mutate(type = "Original"))

#plot to compare residual distributions
all_res_df %>%
  ggplot(aes(x = resid, fill = type)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Residual Distribution: Simulated vs Original")

#plot autocorrelation of residuals
acf_df <- all_res_df %>%
  group_by(type, sim_id) %>%
  summarise(acf1 = acf(resid, plot = FALSE, na.action = na.pass)$acf[2])

orig_acf1 <- acf_df %>%
  filter(type == "Original") %>%
  pull(acf1)

acf_df %>%
  filter(type == "Simulated") %>%
  ggplot(aes(x = acf1)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = orig_acf1, color = "red", linewidth = 1) +
  theme_minimal() +
  labs(title = "Lag-1 residual autocorrelation")

#residuals vrs fit plot
ggplot() +
  geom_point(data = all_res_df %>% filter(type == "Simulated"),
             aes(fitted, resid),alpha = 0.05, color = "grey") +
  geom_point(data = all_res_df %>% filter(type == "Original"),
             aes(fitted, resid), color = "red", alpha = 0.9) +
  theme_minimal() +
  labs(title = "Residual vs Fitted: Simulated (grey) vs Original (red)")

#----------------------------------------------------#
# Exploration: Forecasting recruitment ----
#----------------------------------------------------#

# One step forecast

#append future year to dataset
ts_ext <- scaled_dat %>%
  add_row(year = max(.$year) + 1) %>%
  select(-year) %>%
  ts()

#predict by holding path coefficients constant, re-optimizing latent states
  #and propagating system dynamics forward, noting that predictions are 
  #on log scale!
pred <- predict(full_fit, newdata = ts_ext, type = "response")
#Forecasts represent the causal propagation of system dynamics under specified driver scenarios

pred_last <- pred[nrow(pred), ]
pred_rec <- pred_last[, "log_juvenile_abun"] 

obs <- scaled_dat %>%
  mutate(type = "Observed")

forecast <- data.frame(
  log_juvenile_abun = as.numeric(pred),
  year = max(scaled_dat$year) + 1,
  type = "Forecast")

plot_df <- bind_rows(obs, forecast)

ggplot(plot_df, aes(x = year, y = log_juvenile_abun, color = type)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(y = "Recruitment", x = "Year")

#---------------------------------------------------------------#
# Exploration: retrospective skill testing ----
#---------------------------------------------------------------#

#one-step-ahead out of sample skill test: refit using data up to a certain
  #year, forecast the next year from the fitted model, compare predicted
  #vrs observed recruitment

#training dataset
train_dat <- scaled_dat %>%
  filter(year < yr)

train_ts <- train_dat %>%
  select(-year) %>%
  ts()

#fit full model with truncated data
full_fit_2022 <- dsem(sem=habitat_sem, tsdata=train_ts, 
                 family=family)

#pull coefficients
coef_df <- as.data.frame(summary(fit)) %>%
  transmute(year = yr,
            path,
            estimate = Estimate,
            se = Std_Error)

#build forecast
ts_ext <- scaled_dat %>%
  filter(year <= 2022) %>%
  add_row(year = 2023) %>%
  select(-year) %>%
  ts()

#predict 2023 x 500 simulations
sim_2023 <- simulate(full_fit_2022, nsim = 500, newdata = ts_ext)

#extract 2023 forecast
sim_rec <- sapply(sim_2023, function(sim) {
  sim[nrow(sim), "log_juvenile_abun"]
})

#observed data for 2023
obs_2023 <- scaled_dat %>%
  filter(year == 2023) %>%
  pull(log_juvenile_abun)

#Now let's expand to multiple years
years <- 2015:2020

hindcast_list <- map(years, function(yr) {
  
  #Train model up to year t-1 
  train_dat <- scaled_dat %>% 
    filter(year < yr) %>% select(-year) %>% 
    ts() 
  
  fit <- dsem( sem = habitat_sem, 
               tsdata = train_dat, 
               family = family) 
  #Extract causal estimates 
  coef_df <- as.data.frame(summary(fit)) %>% 
    transmute(year = yr, path, estimate = Estimate, se = Std_Error) 
  
  #This is presumably using predictions from 2019 for 2021 since we're missing 
  #so much covariate data, but this solution still isn't' working..... 
  lag_year <- ifelse(yr == 2021, yr - 2, yr - 1) 
  
  #build forecast input 
  future_row <- scaled_dat %>% 
    filter(year == lag_year) %>% 
    mutate(year = yr) 
  
  ts_ext <- scaled_dat %>% 
    filter(year < yr) %>% 
    bind_rows(future_row) %>% 
    select(-year) %>% 
    ts()
  
  #Simulate forecasts
  sims <- simulate(fit, nsim = 500, newdata = ts_ext) 
  
  sim_rec <- sapply(sims, function(sim) { 
    sim[nrow(sim), "log_juvenile_abun"] 
  })
  
  #add observed recruitment
  obs <- scaled_dat %>%
    filter(year == yr) %>%
    pull(log_juvenile_abun)
  
  #return output
  list( forecast = tibble( year = yr, sim = list(as.numeric(sim_rec)), 
                           obs = obs ), coefs = coef_df ) 
})

#Clean up for plotting
forecast_df <- map_df(hindcast_list, "forecast") 
plot_df <- forecast_df %>% unnest(sim) 
obs_df <- forecast_df %>% select(year, obs) 
# Coefficients 
coef_time <- map_df(hindcast_list, "coefs") 
path_of_interest <- "extent_0c -> log_juvenile_abun" # adjust as needed 
coef_plot_df <- coef_time %>% 
  filter(path == path_of_interest)

#hindcast skill
ggplot(plot_df, aes(x = factor(year), y = sim)) +
  geom_violin(fill = "skyblue", alpha = 0.5) +
  geom_point(
    data = obs_df,
    aes(x = factor(year), y = obs),
    color = "red",
    size = 2) +
  theme_bw() +
  labs(y = "Snow Crab Recruitment (mean-centered)",
       x = "")

#causal estimate over time
ggplot(coef_plot_df, aes(x = as.factor(year), y = estimate)) +
  geom_line() +
  geom_point(color="blue", size=2) +
  geom_errorbar(aes(
    ymin = estimate - 1.96 * se,
    ymax = estimate + 1.96 * se), width = 0.15, color="grey", alpha=.8) +
  theme_bw() +
  labs(y = "Path estimate",
       x = "Training end year")

#Follow up: I cannot figure out why the model is not generating 2021 predictions..
  #also this workaround means we're not forecasting future year from unknown future
  #covariates, but instead using covariates from previous year, appending them as 
  #a future row, and fitting the model. Not sure this is what we want?







