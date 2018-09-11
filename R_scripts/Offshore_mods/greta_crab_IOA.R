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

seamap %>% mutate(STAT_ZONE = factor(STAT_ZONE, levels = c(22:11)),
                  Crab_presence = ifelse(Sapidus_Catch > 0, 1, 0),
                  Survey_Year = factor(Survey_Year),
                  Season = factor(Season, levels = c("Summer","Fall","Winter")))


crabs <- filter(seamap, CPUE_towspd > 0) #positive catch subset

# ########### Greta Iris Example  ###########
# # data
# x <- as_data(iris$Petal.Length)
# y <- as_data(iris$Sepal.Length)
# 
# # variables and priors
# int <- normal(0, 5)
# coef <- normal(0, 3)
# sd <- lognormal(0, 3)
# 
# # operations
# mean <- int + coef * x
# 
# # likelihood
# distribution(y) = normal(mean, sd)
# 
# # defining the model
# m <- model(int, coef, sd)
# 
# # plotting
# plot(m)
# 
# # sampling
# draws <- mcmc(m, n_samples = 1000)
# summary(draws)
# mcmc_trace(draws)
# mcmc_intervals(draws)
# 
# #different distributions
# ?greta::distributions




########  Seamap test model  ############
#going to look from Destin West
seamap2 <- seamap[seamap$Start_Long < -86.16607,]
seamap.test <- seamap2 %>% filter(Survey_Year %in% 2000:2005) #try one year to start

seamap %>% filter(Sapidus_Catch > 0) %>% ggplot(aes(Start_Depth, CPUE_towspd)) + geom_point() + geom_smooth(method = "lm")
seamap %>% filter(Sapidus_Catch > 0) %>% ggplot(aes(STAT_ZONE, CPUE_towspd)) + geom_boxplot(aes(group = STAT_ZONE))
seamap %>% filter(Sapidus_Catch > 0) %>% ggplot(aes(Survey_Year, CPUE_towspd)) + geom_boxplot(aes(group = Survey_Year))
seamap %>% filter(Sapidus_Catch > 0) %>% ggplot(aes(Survey_Year, CPUE_towspd, group = Season)) + geom_point(aes(color = Season)) + geom_smooth(method = "lm", aes(colour = Season))

#something is up with the data, not seeing catches past 2004


#Model Idea, Regression CPUE ~   Year + Season + Zone
mod1 <- glm.nb(CPUE_towspd ~ Survey_Year + Season + STAT_ZONE)





#Jags Model

############## sample model matrices, and jags.dat prep ######
#need seperate matrices of covariates, one for binary portion and another for count portion
Xc <- model.matrix( ~ 1 + Start_Depth, data = seamap.test)
Kc    <- ncol(Xc)
I0    <- as.numeric(seamap.test$Crab_presence)
re1    <- as.numeric(as.factor(seamap.test$Survey_Year))
NumRE1 <- length(unique(seamap.test$Survey_Year))
re2    <- as.numeric(as.factor(seamap.test$Season))
NumRE2 <- length(unique(seamap.test$Season))

########## out of sample model matrices #######
#Standardize the covariates in seamap.NotIn before this
# Xc.notin <- model.matrix( ~ 1 + Start_Depth, data = seamap.test)

######## Jags data ####
JAGS.data <- list(Y = seamap.test$CPUE_towspd,  #Response
                  Xc = Xc,                      #Covariates
                  Kc = Kc,                      #Number of betas
                  N = nrow(seamap.test),          #Sample Size
                  re1 = re1,                      #Random Effects
                  re2 = re2,
                  NumRE1 = NumRE1,
                  NumRE2 = NumRE2)                #Number of Random Effects

JAGS.data

######  Model Seetup
sink("NB_testmod.txt")
cat("
    model{
    #1A. Priors regression parameters count part and binary part
    for (i in 1:Kc) { beta[i]  ~ dnorm(0, 0.0001) }  
    
    # 1B. Priors for random intercepts landscape
    for (i in 1:NumRE1) {a1[i] ~ dnorm(0, tau1) } 
    for (i in 1:NumRE2) {a2[i] ~ dnorm(0, tau1) } 
    
    # 1D. Priors for variances for random intercepts
    sigma1 ~ dunif(0.001, 5)
    tau1  <- 1 / (sigma1 * sigma1)
    
    # 1E. Priors for size
    size ~ dunif(0.001, 5)
    
    
    
    ###################################
    #2. Likelihood
    for (i in 1:N) {
    Y[i] ~  dnegbin(p[i], size)
    p[i] <- size / (size + mu[i])      	
    log(mu[i]) <- inprod(beta[], Xc[i,]) + a1[re1[i]] + a2[re2[i]]
    }     
    }
    ",fill = TRUE)
sink()


#Inits function
#Set initial values
inits  <- function () {
  list(beta   = rnorm(Kc, 0, 0.1),
       a1     = rnorm(NumRE, 0, 0.1),
       sigma1 = runif(1, 0.001, 5),
       size   = runif(1, 0.001, 5)
  )}



#Step 4: Parameters to estimate
params <- c("beta", "a1",  "sigma1", "size")



#Step 5: Run JAGS
Sys.time()
NB.1   <- jags(data       = JAGS.data,
               inits      = inits,
               parameters = params,
               model      = "NB_testmodt.txt",
               n.thin     = 10, 
               n.chains   = 3,
               n.burnin   = 4000,
               n.iter     = 5000)
Sys.time()
NB.2 <- update(NB.1, n.iter = 10000, n.thin = 10)  
Sys.time()
NB.3 <- update(NB.2, n.iter = 20000, n.thin = 10)  
Sys.time()