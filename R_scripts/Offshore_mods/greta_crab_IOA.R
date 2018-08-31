############
#Bayesian estimation of indices of abundance using JAGS/GRETA
#8/30/2018

#packages
library(greta)
library(tidyverse)
library(greta)
library(DiagrammeR)
library(bayesplot)
library(ggplot2)


#read in data
seamap <- read_csv("~/Documents/KrackN/Seamap_man/Historic_cleaned/seamap_cpue_2018.csv")

#Set up Season Column
seamap[str_detect(seamap$TITLE, "Fall"),"Season"] <- "Fall"
seamap[str_detect(seamap$TITLE, "Winter"),"Season"] <- "Winter"
seamap[str_detect(seamap$TITLE, "Summer"),"Season"] <- "Summer"


seamap$STAT_ZONE <- factor(seamap$STAT_ZONE, levels = c(22:11))
seamap$Crab_presence <- ifelse(seamap$Sapidus_Catch > 0, 1, 0) #binary presence absence
crabs <- filter(seamap, CPUE_towspd > 0) #positive catch subset

########### Greta Iris Example  ###########
# data
x <- as_data(iris$Petal.Length)
y <- as_data(iris$Sepal.Length)

# variables and priors
int <- normal(0, 5)
coef <- normal(0, 3)
sd <- lognormal(0, 3)

# operations
mean <- int + coef * x

# likelihood
distribution(y) = normal(mean, sd)

# defining the model
m <- model(int, coef, sd)

# plotting
plot(m)

# sampling
draws <- mcmc(m, n_samples = 1000)
summary(draws)
mcmc_trace(draws)
mcmc_intervals(draws)

#different distributions
?greta::distributions




########  Seamap test model  ############
seamap.test <- seamap %>% filter(Survey_Year %in% 2000:2005) #try one year to start

seamap %>% filter(Sapidus_Catch > 0) %>% ggplot(aes(Start_Depth, CPUE_towspd)) + geom_point() + geom_smooth(method = "lm")

#Model Idea, Ancova CPUE ~ Depth * Year/Season

# data
Season <- as_data(seamap.test$Season)
Stat_zone <- as_data(seamap.test$STAT_ZONE)
Catch_per_hectare <- as_data(seamap.test$CPUE_towspd)

# variables and priors
int = normal(0, 1)                             #intercept(s)
coef = normal(0, 3)                            #slope
sd = student(3, 0, 1, truncation = c(0, Inf))  #sd
