# Jags IOA model code

library(tidyverse)
library(R2jags)


#Or if you hate your life try jags
write_csv(seamap, "SEAMAP_model_ready.csv")
read_csv("SEAMAP_model_ready.csv")

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