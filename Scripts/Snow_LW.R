# notes ----
# Calculate length:weight regressions and residuals for snow crab
# Erin Fedewa
# last updated: 2022/8/26


#########

# load ----
library(tidyverse)

# data ----

snow <- read.csv("./Data/crabhaul_opilio.csv")

#look at the distribution
hist(snow$WEIGHT)
hist(log(snow$WEIGHT)) # log transformed - better

#Plots
snow %>%
  mutate(YEAR = str_extract(CRUISE, "\\d{4}"),
         logcw = log(WIDTH),
         logweight = log(WEIGHT)) %>%
  #Only SC2 as to not bias for weight of epibionts
  filter(SEX == 1, SHELL_CONDITION == 2,
  #Removing 1975 in case of potential biases in methods
         CRUISE != 197502,
          WEIGHT != "NA") -> male  

ggplot(male, aes(x = WIDTH, y = WEIGHT, group =  YEAR)) +
  geom_point(aes(colour = factor(YEAR)))

ggplot(male, aes(x = logcw, y = logweight, group = YEAR)) +
  geom_point(aes(colour = factor(YEAR)))

#Faceted
male %>%
  ggplot(aes(x = logcw, y = logweight)) +
  geom_point() +
  facet_wrap(~YEAR) #Very low sample size for some years!

#################################
#Linear model (natural log of power function W = a * L^b) ----
#See http://derekogle.com/fishR/examples/oldFishRVignettes/LengthWeight.pdf

fit1 <- lm(logweight~logcw, data=male) 
plot(fit1)
summary(fit1)
coef(fit1)

#Addressing outliers 
plot(cooks.distance(fit1), pch="*", cex=2, main="Influential Obs by Cook's distance") 
abline(h = 4/(nrow(male)), col="red")  # add cutoff line (critical Cooks D > 4/n)

male$cd <- cooks.distance(fit1)

keepers<-subset(male, cd < (4/(nrow(male)))) 
nrow(male) - nrow(keepers) #100 observations removed 

ggplot(keepers, aes(x = WIDTH, y = WEIGHT, group = YEAR)) +
  geom_point(aes(colour = factor(YEAR)))

#Re-fit model with outliers removed 
fit2 <- lm(logweight~logcw, data=keepers) 
plot(fit2)
summary(fit2)
coef(fit2)

#SC2 Male snow best-fit equations:
# log(W) = -8.104084  + 3.063959 * log(L) on transformed scale
# W = exp(-8.104084)*L^(3.063959)  on original scale 
# a = 0.0004035061, b = 3.063959 

#L:W residual calculations for males ----

#All SC2 males
keepers %>% 
  mutate(resid = residuals(lm(logweight~logcw))) %>%
  group_by(YEAR) %>%
  summarise(Avg_resid = mean(resid)) %>%
  ggplot(aes(YEAR, Avg_resid)) +
  geom_bar(stat = "identity")

#Large SC2 males 
keepers %>% 
  filter(WIDTH >= 70) %>%
  mutate(resid = residuals(lm(logweight~logcw))) %>%
  group_by(YEAR) %>%
  summarise(Avg_resid = mean(resid)) %>%
  ggplot(aes(YEAR, Avg_resid)) +
  geom_bar(stat = "identity")

#Small SC2 males 
keepers %>% 
  filter(WIDTH < 70) %>%
  mutate(resid = residuals(lm(logweight~logcw))) %>%
  group_by(YEAR) %>%
  summarise(Avg_resid = mean(resid)) %>%
  ggplot(aes(YEAR, Avg_resid)) +
  geom_bar(stat = "identity")

#Condition factor K for male SC2 snow ----
keepers %>%
  mutate(K=WEIGHT/(WIDTH^3)*100000) %>%
  group_by(YEAR) %>%
  summarise(Avg_cond = mean(K)) %>%
  ggplot(aes(as.numeric(YEAR), Avg_cond)) +
  geom_point(size=4) +#Seem to follow same trends as residuals 
  geom_line()
  
#Thoughts: Diff in L:W likely driven by molt timing/moisture content in muscle- are residuals 
#consistent with thermal regime in that cold years molt is delayed and crab weight less? 


