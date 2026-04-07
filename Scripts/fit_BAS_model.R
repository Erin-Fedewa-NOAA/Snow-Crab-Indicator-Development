#Purpose: To evaluate linkages between recruitment and a standard set of ecosystem indicators

#Creator: Curry Cunningham
#With additions from E. Fedewa

#Recent run: 8/19/25 chla/pcod consumption/benthic invert only updated thru 2024

#NOTES for 2026: Use MMB or time varying mortality as response instead of recruitment?
  #Explore a model run with recruitment model output- these are 25-40mm though, and 
  #some lags would be tough 

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

## Read in setup for crab data
source("./Scripts/get_crab_data.R")

#Read in indicator data
indicators <- read.csv("./Output/snow_esp_indicator_timeseries.csv")

# Set years
current_year <- 2025
years <- 1988:current_year

############################################################
#calculate pre-recruit abundance as our response: 
  #i.e. survey-derived abundance of 65-80mm CW male snow crab
  #Size range selected using St. Marie 1995 size at age estimates
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
                mutate(ABUNDANCE = as.numeric(ABUNDANCE/1e6)) %>%
                rename_with(tolower)

#Plot
recruit_abun %>%
  ggplot(aes(x = year, y = abundance)) +
  geom_point() +
  geom_line()+
  labs(y = "Number of crab (millions)", x = "") +
  theme_bw()

#Write output 
write_csv(recruit_abun, "./Output/BAS_recruit_abundance.csv", row.names = F)

#join indicator and response
recruit_abun %>%
  right_join(indicators) %>%
  arrange(year) -> model_dat

############################################################
# MODEL RUN 1: Using design-based BT survey estimate for male recruitment as response

#Assess collinearity b/w indicators 
model_dat %>% 
  select(-year) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot(method="color")
#Some highly correlated covariates- but we'll wait to reassess until we lag since
  #some indicators are representing different mechanisms

#Look at temporal coverage of indicators 
model_dat %>%
  select(-abundance) %>%
  pivot_longer(c(2:(ncol(model_dat)-1)), names_to="indicator", values_to="value") %>%
  ggplot(aes(year, indicator, size=value)) +
    geom_point() +
    theme_bw()
#Lets drop snow crab condition due to short timeseries
  
#Assign Lags for indicators - see metadata file in repo for rationales for lags
model_dat %>%
  #dropping indicators based on timeseries length/mechanistic link w/ recruitment
  select(-op_sex_ratio, -male_maturity, -female_maturity, -energetic_condition,
         -mature_male_d95, -mature_male_centroid) %>%
  mutate(cp_lag = lag(extent_0C, n=1, order_by = year), 
         ao_lag = lag(Mean_AO, n=7, order_by = year),
         ice_lag = lag(ice_avg, n=3, order_by = year), 
         consump_lag = lag(consumption, n=4, order_by = year), 
         bcd_lag = lag(bcd_imm, n=3, order_by = year), 
         invert_lag = lag(beninvert_cpue, n=1, order_by = year),
         tempocc_lag = lag(temp_occ, n=1, order_by = year), 
         clutch_lag = lag(clutch_empty, n=8, order_by = year),
         chla_lag = lag(chla, n=7, order_by = year))%>%
  select(-c(extent_0C, Mean_AO, ice_avg, consumption, clutch_empty,
         bcd_imm, temp_occ, beninvert_cpue, chla)) -> dat_lagged

#plot timeseries with lagged covariates 
dat_lagged %>%
  pivot_longer(c(2:(ncol(dat_lagged))), names_to="indicator", values_to="value") %>%
  ggplot(aes(year, value)) +
  geom_point() +
  geom_line() +
  facet_wrap(~indicator, scales = "free_y") +
  theme_bw()

#Temporal coverage with lags incorporated 
dat_lagged %>%
  select(-abundance) %>%
  pivot_longer(c(2:(ncol(dat_lagged)-1)), names_to="indicator", values_to="value") %>%
  ggplot(aes(year, indicator, size=value)) +
  geom_point(na.rm=T) +
  theme_bw()
#Missing 2020 survey is problematic- we're dropping most collapse/post-collapse years
  #Might also consider dropping reproductive failure b/c long lag limits our timeseries
  #length in the early years
#Also chla limits our timeseries to 16 years, starting in 2005 so let's drop

#Assess collinearity b/w lagged indicators 
dat_lagged %>% 
  select(-year) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot(method="number")
#let's pull cold pool since temp occupied is a more direct metric
#interesting on consumption/bcd- both probably increase with increased 
#abundance. We'll stick with cod consumption since it's likely a more 
  #direct metric

#Lets also look at distributions of potentially problematic covariates
hist(dat_lagged$abundance)
hist(dat_lagged$ice_lag)
hist(dat_lagged$invert_lag)
hist(dat_lagged$bcd_lag)
hist(dat_lagged$consump_lag)

# Final data wrangling 
dat_bas <- dat_lagged %>% 
  mutate(ln_rec=log(abundance)) %>%
  #dropping highly correlated indicators + chla
  select(-bcd_lag, -clutch_lag, -cp_lag, -chla_lag)

hist(dat_bas$ln_rec)

#Define covariates
covars <- names(dat_bas %>% select(-year, -abundance, -ln_rec))

# Plot Covariates
covar.list <- dat_bas %>% 
    select(-abundance, -ln_rec) %>% 
  gather(key=type, value=value, -year) 

ggplot(covar.list, aes(x=value, fill=type)) +
  theme_linedraw() +
  geom_histogram() +
  geom_density(alpha=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~type, scales='free') +
  theme(legend.position = "NA")

# Z-score Predictors that are bounded at zero 
dat_bas %>%
  mutate(across(c(3:7), ~ (.-mean(.,na.rm=T))/sd(.,na.rm=T), .names = "z_{.col}")) %>%
  select(-ao_lag, -ice_lag,-consump_lag,-invert_lag,-tempocc_lag,-abundance) %>%
  rename("Arctic Oscillation" = z_ao_lag, "Sea Ice Extent" = z_ice_lag, 
         "Cod Consumption" = z_consump_lag, "Benthic Prey" = z_invert_lag, 
         "Snow Crab Temperature Occupied" = z_tempocc_lag) -> dat_zscore
#When predictors are z-scored, the regression coefficients represent the change in the outcome variable
  #(in standard deviations) for a one-standard-deviation change in the predictor. 
  #This allows for direct comparison of the strength/importance of different predictors.

# final plot with lagged/z-scored indicators and log recruitment response
z.ts.plot <- dat_zscore %>%
  select(-ln_rec) %>%
  pivot_longer(c(2:(ncol(dat_zscore)-1)), names_to = "indicator", values_to = "value") %>%
  mutate(indicator = factor(indicator, 
                            levels = c("Arctic Oscillation", "Sea Ice Extent",
                                       "Cod Consumption","Benthic Prey",
                                       "Snow Crab Temperature Occupied")))


ggplot() +
  geom_point(data = z.ts.plot, aes(year, value), color="blue") + 
  geom_line(data = z.ts.plot, aes(year, value), color="blue") + 
  geom_line(data = dat_zscore %>%
              select(year, ln_rec), 
            aes(year, ln_rec), color = "grey50", linetype = 6) +
  labs(y = "", x = "") +
  facet_wrap(~ indicator, scales = "free_x") + 
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = NA, color = "white"),
        strip.background = element_blank())

#This doesn't look great because log recruitment not on same scale as z-scored predictors
  #so we'll do two seperate plots
timeseries_plot <- ggplot() +
  geom_point(data = z.ts.plot, aes(year, value), color="blue") + 
  geom_line(data = z.ts.plot, aes(year, value), color="blue") + 
  labs(y = "", x = "") +
  facet_wrap(~ indicator, scales = "free_x") + 
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = NA, color = "white"),
        strip.background = element_blank())

recruit_plot <- dat_zscore %>%
  ggplot(aes(year, ln_rec)) +
  geom_point(color="grey30") +
  geom_line(color = "grey30") +
  theme_bw() +
  labs(y="log snow crab recruitment", x="") +
  scale_x_continuous(limits = c(1988, current_year))

#combine and save
recruit_plot / timeseries_plot + plot_annotation(tag_levels = 'a')
ggsave("./Figs/BAS_Aug_2025/covariates.png")

#Fit Models ====================================
#remove year from dataset
dat_zscore %>%
  select(-year) -> dat_fit

# Bayesian Model Selection
bas.lm <-  bas.lm(ln_rec ~ ., data = dat_fit,
                  modelprior=uniform(), initprobs="Uniform",
                  method='BAS', MCMC.iterations=1e5, thin=10)

summary(bas.lm)
bas.lm

#Diagnostic Plots
plot(bas.lm)
plot(coef(bas.lm),ask=FALSE)
image(bas.lm, rotate = F, drop.always.included = TRUE)
lot(bas.lm, which = 4)
coef.mod <- coef(bas.lm)
plot(confint(coef.mod))

# Plot Model Predictions vs. Observed ==============================
pdf(file.path("Figs/BAS_Aug_2025","Model 1 Fit.pdf"), height=5, width=10)
par(oma=c(1,1,1,1), mar=c(4,4,1,1), mfrow=c(1,2))
pred.bas <- predict(bas.lm, estimator="BMA")

# Omit NAs
dat.temp.na.omit <- na.omit(dat_zscore)

plot(x=dat.temp.na.omit$ln_rec, y=pred.bas$Ybma,
     xlab="Observed ln(Recruitment)", ylab="Predicted ln(Recruitment)", pch=21, bg=rgb(1,0,0,alpha=0.5),
     main="")
# Title
mtext(paste("Snow Crab Aug 19"), side=3, outer=TRUE, font=2)
# plot(x=pred.bas$fit, y=pred.bas$Ybma) 
abline(a=0, b=1, col=rgb(0,0,1,alpha=0.5), lwd=3)

# Timeseries
plot(x=dat.temp.na.omit$year, y=dat.temp.na.omit$ln_rec,
     xlab="year", ylab="ln(Recruitment)", type='l', col=rgb(1,0,0,alpha=0.5),
     main="")
grid(lty=3, col='dark gray')
points(x=dat.temp.na.omit$year, y=dat.temp.na.omit$ln_rec,
       pch=21, bg=rgb(1,0,0,alpha=0.5))
lines(x=dat.temp.na.omit$year, y=pred.bas$Ybma, lwd=3, col=rgb(0,0,1, alpha=0.5))

legend('bottom', legend=c("Observed","Predicted"), lty=1, col=c(rgb(1,0,0,alpha=0.5),
                                                                  rgb(0,0,1, alpha=0.5)),
       bg="white")

dev.off()

## Plot inclusion probabilities ------------------------------------------------
inc.probs <- summary(bas.lm)[2:ncol(dat_fit), 1]

bas.names <- coef(bas.lm)$namesx
inc.probs <- coef(bas.lm)$probne0
post.mean <- coef(bas.lm)$postmean
post.sd <- coef(bas.lm)$postsd
low.95 <- confint(coef(bas.lm))[,1]
up.95 <- confint(coef(bas.lm))[,2]

# Make final plot of covariates
plot.df <- data.frame(bas.names, inc.probs, post.mean, post.sd, low.95, up.95)

# Parameter estimates/CI
plot.df %>%
  filter(bas.names != 'Intercept') %>%
  ggplot(aes(x = bas.names, post.mean, fill = 'royalblue4')) +
  theme_bw() +
  geom_errorbar(aes(ymin = low.95, ymax = up.95), width = 0.25) +
  geom_point(pch = 21, fill = 'royalblue4', size = 3) +
  geom_hline(yintercept = 0, col = 'red', alpha = 0.5) +
  ylab('Effect') +
  xlab("") +
  coord_flip() +
  theme(legend.position = 'none') -> effect

# Inclusion probability
plot.df %>%
  filter(bas.names != 'Intercept') %>%
  ggplot(aes(x = bas.names, y = inc.probs, fill = inc.probs)) +
  theme_bw() +
  geom_bar(stat = 'identity', color = 'black') +
  ylab('Inclusion\nProbability') +
  scale_y_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = c(0, 1)) +
  geom_hline(yintercept = 0.5, col = 'black', linetype = 5, alpha = 0.5) +
  theme(legend.position = 'none', axis.text.y = element_blank(), 
        axis.title.y = element_blank()) +
  coord_flip() +
  scale_fill_continuous_tableau() -> prob

# Bring figures together and save
effect + prob
ggsave("./Figs/BAS_Aug_2025/covariate_effects.png")

#This is really a shot in the dark with one lag for a single year, when we know 
  #some of these effects compound, and effects can be integrated. 
#Might be worth some exploration on changing lags/moving averages, but overall, 
  #BAS is not a great approch for timeseries with missing data. 

###################################################

#Exploration with Boosted Regression Trees =========================================
form.covars <- paste(covars, collapse=" + ")
form <- formula(paste("ln_rec", "~",form.covars))

gbm.fit <- gbm(formula=form, distribution = "gaussian", data=dat.fit, 
                 n.trees=1e5, interaction.depth=1,
                 shrinkage=0.001,
                 n.minobsinnode=3,
                 train.fraction=0.5)

summary(gbm.fit, las=2)

# Plot Fit
par(oma=c(1,1,1,1), mar=c(4,4,1,1), mfrow=c(1,2))
# pred.bas <- predict(bas.lm, estimator="BMA")

plot(x=dat.fit$ln_rec, y=gbm.fit$fit,
     xlab="Observed ln(MMB)", ylab="Predicted ln(MMB)", pch=21, bg=rgb(1,0,0,alpha=0.5),
     main="")
mtext(paste("BBRKC", model), side=3, outer=TRUE, font=2)
abline(a=0, b=1, col=rgb(0,0,1,alpha=0.5), lwd=3)

# Timeseries
plot(x=dat.fit$year, y=dat.fit$ln_rec,
     xlab="year", ylab="ln(MMB)", type='l', col=rgb(1,0,0,alpha=0.5),
     main="")
grid(lty=3, col='dark gray')
points(x=dat.fit$year, y=dat.fit$ln_rec,
       pch=21, bg=rgb(1,0,0,alpha=0.5))
lines(x=dat.fit$year, y=gbm.fit$fit, lwd=3, col=rgb(0,0,1, alpha=0.5))

legend('bottom', legend=c("Observed","Predicted"), lty=1, col=c(rgb(1,0,0,alpha=0.5),
                                                                rgb(0,0,1, alpha=0.5)),
       bg="white")

# Plot Partials
par(mfrow=c(2,3))

plot(gbm.fit, i.var=3)


# # Plot Fit =========
# 
# #Plot Fitted Model - POSTERIOR PREDICTIVE DISTRIBUTION
# # post.preds <- apply(out$BUGSoutput$sims.list$post.pred, 2, quantile, probs=c(0.025,0.25,0.5,0.75,0.975))
# pdf(file.path(dir.figs,'Fits and Other Params.pdf'), height=6, width=7)
# 
# preds <- predict(bas.lm)
# 
# post.preds <- preds$Ybma
# # pred.low <- preds$Ybma - 1.96* preds$se.bma.pred
# 
# y.lim <- c(min(input.rec, post.preds), max(input.rec, post.preds))
# x.lim <- c(min(years),max(years))
# 
# plot(x=NULL, y=NULL, xlab='Recruitment Year', ylab='log(recruitment)', pch=21, bg='blue',
#      ylim=y.lim, xlim=x.lim)
# abline(h=0)
# lines(x=years, y=input.rec, lwd=2, col='blue', lty=3)
# points(x=years, y=input.rec, pch=21, bg='blue')
# 
# #Fitted Model
# # polygon(x=c(years, rev(years)), y=c(post.preds[1,],rev(post.preds[5,])), col=rgb(1,0,0,alpha=0.25), border=FALSE)
# # polygon(x=c(years, rev(years)), y=c(post.preds[2,],rev(post.preds[4,])), col=rgb(1,0,0,alpha=0.25), border=FALSE)
# lines(x=years, y=post.preds[,1], col=rgb(1,0,0,alpha=0.5), lwd=2)
# legend('topleft', legend=c('Observed','Post. Pred.'), col=c('blue','red'), lty=c(3,1))
# 
# dev.off()
# 
# 
# #Plot Model Ranks =========
# png(file.path(dir.figs, 'Model Ranks.png'), height=6, width=9, units='in', res=500)
# par(oma=c(0,18,0,0))
# image(bas.lm, rotate=F)
# 
# dev.off()
# 
