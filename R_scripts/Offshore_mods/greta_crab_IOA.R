############
#Bayesian estimation of indices of abundance using JAGS/GRETA
#8/30/2018

#packages
#library(greta)
library(DiagrammeR)
library(bayesplot)
library(ggplot2)
library(MASS)
library(R2jags)
library(tidyverse)


#read in data
seamap <- read_csv("~/Documents/KrackN/Seamap_man/Historic_cleaned/seamap_cpue_2018.csv")

#Set up Season Column
seamap[str_detect(seamap$TITLE, "Fall"),"Season"] <- "Fall"
seamap[str_detect(seamap$TITLE, "Winter"),"Season"] <- "Winter"
seamap[str_detect(seamap$TITLE, "Summer"),"Season"] <- "Summer"

#Order stat zones west to east
#Make presence absence column
#Make numeric survey year column
#Make seasons a factor
#Make month column
#Filter out stat zones that are NA
seamap <- seamap %>% 
  mutate(STAT_ZONE = factor(STAT_ZONE, levels = c(22:11)),
         Crab_presence = ifelse(Sapidus_Catch > 0, 1, 0),
         Survey_Year = as.numeric(Survey_Year),
         Season = factor(Season, levels = c("Summer","Fall","Winter")),
         Survey_Month = lubridate::month(Date),
         Survey_Month = factor(Survey_Month)) %>% 
  filter(!is.na(STAT_ZONE), STAT_ZONE != "NA")

#going to look from Destin West
seamap <- seamap %>% filter(Start_Long < -86.16607)

#diagnosing crazy high cpue
seamap <- seamap %>% filter(Spd_kmh != 0)  #these are infinite cpue

#Make positive catch only subset
crabs <- filter(seamap, Crab_presence == 1) #positive catch subset


########  Seamap test model  ############

seamap %>% filter(CPUE_towspd > 100) %>% dplyr::select(Sapidus_Catch, MIN_FISH, Spd_kmh)  #these are probably similis, can't make that call objectively
seamap %>% filter(Sapidus_Catch > 0) %>% ggplot(aes(Start_Depth, CPUE_towspd)) + geom_point() + geom_smooth(method = "lm")
seamap %>% filter(Sapidus_Catch > 0) %>% ggplot(aes(STAT_ZONE, CPUE_towspd)) + geom_boxplot(aes(group = STAT_ZONE))
seamap %>% filter(Sapidus_Catch > 0) %>% ggplot(aes(Survey_Year, CPUE_towspd)) + geom_boxplot(aes(group = Survey_Year))
seamap %>% filter(Sapidus_Catch > 0) %>% ggplot(aes(Survey_Year, CPUE_towspd, group = Season)) + geom_point(aes(color = Season)) + geom_smooth(method = "lm", aes(colour = Season))




# #Model Idea with lme4, Regression CPUE ~ Year + Month + Zone
# library(lme4)
# 
# #Negative binomial regression for counts, model doesn't run well because it is really unbalanced
# mod1 <- glmer.nb(Sapidus_Catch ~ Survey_Year + Survey_Month + (1 | STAT_ZONE), data = seamap)
# summary(mod1)
# coef(mod1)
# 
# #hurdle/delta model
# 
# 
# 
# 
# # #Or use inla
# # library(INLA)
# # mod2 <- inla(CPUE_towspd ~ Survey_Year  +Season + (1 | STAT_ZONE), family = "nbinomial", data = seamap)
# # mod2
# 
# 
# #Building up from simple to complex models
# ####  M1  --  single fixed effect, year as continuous
# seamap$Survey_Year <- as.numeric(seamap$Survey_Year)
# m1 <- lm(CPUE_towspd ~ Survey_Year, data = seamap)
# summary(m1)
# coef(m1)
# #Year is significant, but explains less than 1% of the variance
# library(plotly)
# p <- ggplot(data = seamap, aes(x = Survey_Year, y = CPUE_towspd, colour = Season)) +
#   geom_point() +
#   geom_line(aes(y = predict(m1))) 
# ggplotly(p)
# 
# 
# ####  M2  --  year continuous, stat zone and season intercept only  ####
# seamap_cc <- seamap %>% dplyr::select(Survey_Year, Survey_Month, STAT_ZONE, CPUE_towspd)
# seamap_cc <- seamap_cc[complete.cases(seamap_cc),]
# m2 <- lm(CPUE_towspd ~ Survey_Year + Survey_Month + STAT_ZONE, data = seamap)
# summary(m2)
# #year and seasons significant, but explains less than 1% of the variance
# 
# p <- ggplot(data = seamap_cc, aes(x = Survey_Year, y = CPUE_towspd, colour = Survey_Month)) +
#   geom_point() +
#   geom_line(aes(y = predict(m2))) 
# ggplotly(p)
# 
# 
# ####  M3  -- negative binomial model, years as continuous, season as fixed, stat zone as random  ####
# mod1
# summary(mod1)
# coef(mod1)
# ranef(mod1)
# 
# 
# 
# library(plotly)
# p <- ggplot(data = seamap_cc, aes(x = Survey_Year, y = CPUE_towspd, colour = Season)) +
#   geom_point() +
#   geom_line(aes(y = predict(mod1))) 
# ggplotly(p)


#Or if you hate your life try jags
write_csv(seamap, "SEAMAP_model_ready.csv")


#Jags Model Setup

############## sample model matrices, and jags.dat prep ######
seamap.test <- seamap %>% filter(Survey_Year > 2000) #try one year to start
MyStd <- function(x) { (x - mean(x)) / sd(x)} #Standardizaton function

# Define 0-1 data
seamap.test$Crab_presence <- as.numeric(seamap.test$Crab_presence)

# Step 1: Standardize any continuous covariates & format factors
#seamap$temp.std <- MyStd(seamap$Temp)
seamap.test$Survey_Year <- factor(seamap.test$Survey_Year)
seamap.test$Survey_Month <- factor(seamap.test$Survey_Month)
seamap.test$STAT_ZONE <- factor(seamap.test$STAT_ZONE , levels = c(22:11))

  # Step 2: Prepare data for JAGS
X <- model.matrix( ~ 1 + Survey_Year + Survey_Month + STAT_ZONE, 
                   data = seamap.test)
K    <- ncol(X)

JAGS.data <- list(Y = seamap.test$Crab_presence,  #Response
                  X = X,                          #Covariates
                  K = K,                          #Number of betas
                  N = nrow(seamap.test)           #Sample size
)                 

JAGS.data

####  Not Run -- What I thought we'd need to do with random effects  ####
#I0    <- as.numeric(seamap.test$Crab_presence)
#re1    <- as.numeric(as.factor(seamap.test$STAT_ZONE))
#NumRE1 <- length(unique(seamap.test$STAT_ZONE))
#re2    <- as.numeric(as.factor(seamap.test$Survey_Month))
#NumRE2 <- length(unique(seamap.test$Season))
# 
# 
# ######## Jags data ####
# JAGS.data <- list(Y = seamap.test$CPUE_towspd,  #Response
#                   Xc = Xc,                      #Covariates
#                   Kc = Kc,                      #Number of betas
#                   N = nrow(seamap.test),          #Sample Size
#                   re1 = re1,                      #Random Effects
#                   re2 = re2,
#                   NumRE1 = NumRE1,
#                   NumRE2 = NumRE2)                #Number of Random Effects
# 
# JAGS.data

######  Model Setup
setwd("~/Documents/KrackN/Seamap_man/Seamap_offshore_modeling/R_scripts/Offshore_mods") #where text file goes

# Step 3. JAGS modelling code
load.module("glm")
sink("BernoulliJAGS.txt")
cat("
    model{
    #1. Priors beta
    #In JAGS: dnorm(0, Precision) which is dnorm(0, 1/sigma^2)
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001)}
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]       ~ dbern(Pi[i])
    logit(Pi[i]) <- eta[i]
    eta[i]       <- inprod(beta[], X[i,])
    }
    
    #3. Pearson residuals
    for (i in 1:N){
    ExpY[i] <- Pi[i]
    VarY[i] <- (1 - Pi[i]) * Pi[i]
    E[i] <- (Y[i] - ExpY[i]) / sqrt(VarY[i])
    }
    }
    ",fill = TRUE)
sink()


# Steps 4 and 5: Initial values & parameters to save
inits  <- function () {
  list(
    beta  = rnorm(ncol(X), 0, 0.1)) }



#Step 5: Parameters to estimate
#params <- c("beta", "a1", "a2",  "sigma1", "size")

# Let's take the minimal output as a test
params <- c("beta")


# Step 6: Start JAGS
B1 <- jags(data       = JAGS.data,
           inits      = inits,
           parameters = params,
           model      = "BernoulliJAGS.txt",
           n.thin     = 10,
           n.chains   = 3,
           n.burnin   = 4000,
           n.iter     = 5000)
B2 <- update(B1, n.iter = 50000, n.thin = 10)
print(B2, digits = 3)

# Step 7: Assess mixing
out <- B2$BUGSoutput

# Figure
MyNames <- colnames(X)
MyBUGSChains(out, uNames("beta", K), PanelNames = MyNames)