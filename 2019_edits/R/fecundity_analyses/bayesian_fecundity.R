
################################### Set up and run in bayesian ###################################
# set WD to location where post.summ function exists
# session > set working disrectory > To project location

#load packages
library(R2OpenBUGS)
library(rjags)
library(coda)

source("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/Rwork/reproductive_analysis/Repro_analysis/post_summ_function.R")


#read and prepare data
#seamap reproductive analyses
repros <- read.csv("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/Reproductive_analysis/seamap_frozencrabs.csv")

head(repros)
sapply(repros, class)

#select by unique ID for one row per crab, top rows contain the mean egg diameters and mean egg volumes
repros <- repros[!duplicated(repros$Unique_ID),]
#write.csv(repros,"L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/Reproductive_analysis/seamap_onerow.csv")

eggs <- repros[which(repros$ovigerous == 'yes'),]
eggs$Estimated_fecundity <- as.numeric(as.character(eggs$Estimated_fecundity))
noeggs <- repros[which(repros$ovigerous != 'no'),]

eggs$Estimated_fecundity <- eggs$Estimated_fecundity/1e+06



eggs[which(eggs$Estimated_fecundity < 2),]

#only using eggs for any fecundity analysis
dat <- eggs
head(dat); tail(dat)

# start with fecundity and carapace width
#check classes
sapply(dat, class)

plot(Estimated_fecundity ~ Carapace_width, data = dat)
pred.x <- seq(min(dat$Carapace_width), max(dat$Carapace_width), length = 30)





########################## Making Jags List ################################

# compile data into a list to pass to BUGS
jags.dat = list(n.obs = nrow(dat), y = dat$Estimated_fecundity, x1 = dat$Carapace_width, 
               pred.x = pred.x, n.pred = length(pred.x))


##### SPECIFY MODEL CODE #####
##### MODEL 1 ALL SEAMAP CRABS #####
mod = function() {
  # PRIORS
  b0 ~ dnorm(0,.001) #small precision = big variance
  b1 ~ dnorm(0,.001)
  sig ~ dunif(0,10)
  tau <- 1/(sig^2) #precision
  
  # LIKELIHOOD
  for (i in 1:n.obs) {
    y[i] ~ dnorm(y.hat[i], tau)
    y.hat[i] <- b0 + b1 * x1[i] #x1 is age in the data list
    resid[i] <- y[i] - y.hat[i]
  }
  
  for (i in 1:n.pred) {
    pred.y[i] <- b0 + b1 * pred.x[i]
  }
  
  #Posterior Predictive check
  for (i in 1:n.obs) {
    # generate data under the model
    y.new[i] ~ dnorm(y.hat[i], tau)
    
    # use pearson's residuals
    obs.resid[i] <- (y[i] - y.hat[i])/sig
    new.resid[i] <- (y.new[i] - y.hat[i])/sig
    
    # calculate squared residuals
    D.obs[i] <- pow(obs.resid[i], 2)
    D.new[i] <- pow(new.resid[i], 2)
  }
  
  # calculate deviations
  fit.obs <- sum(D.obs[])
  fit.new <- sum(D.new[])
  # "Bayesian P-value"
  bp <- step(fit.obs - fit.new)
  
  
}

# write model to a text file
model.file = "model.txt"
R2WinBUGS::write.model(mod, model.file)

##### INITIAL VALUES #####
inits1 = list(b0 = rnorm(1), b1 = rnorm(1), sig = rlnorm(1))
inits2 = list(b0 = rnorm(1), b1 = rnorm(1), sig = rlnorm(1))
inits = list(inits1, inits2)

##### PARAMETERS TO MONITOR #####
params = c("b0", "b1", "sig","resid",'pred.y', "y.hat","bp","fit.obs","fit.new")

##### MCMC DIMENSIONS #####
ni = 15000  # number of post-burn-in samples per chain
nb = 5000  # number of burn-in samples
nt = 1     # thinning rate
nc = 2     # number of chains

##### RUN THE MODEL IN BUGS #####
starttime = Sys.time()
jmod = jags.model(file = model.file, data = jags.dat, n.chains = nc, inits = inits, n.adapt = 1000)
update(jmod, n.iter = nb, by = 1, progress.bar = 'text')
mod1_dic <- dic.samples(jmod, n.iter = ni, thin = nt)
post = coda.samples(jmod, params, n.iter = ni, thin = nt)
Sys.time() - starttime



##### CONVERGENCE DIAGNOSTIC #####
# view BGR convergence diagnostic
#gelman.diag(post, multivariate = F)

# visualize trace and posterior plots
#windows(record = T)
#plot(post)

# plot(post[,'b0'])
# plot(post[,'b1'])

##### MAKE INFERENCE #####
b0.est = post.summ(post, "b0"); b0.est
b1.est = post.summ(post, "b1"); b1.est

b0.samps = post.summ(post, "b0")['mean']
b1.samps = post.summ(post, "b1")['mean']

bp.est = post.summ(post, "bp")

# plot(Estimated_fecundity ~ Carapace_width, data = dat, xlab = 'Carapace Width (mm)', ylab = 'Estimated Fecundity')
# for (i in 1:20) {
#   abline(c(b0.samps[i], b1.samps[i]), col = "grey")
# }
#abline(c(b0.est[1], b1.est[1]), lwd = 2, lty = 2, col = "blue")

## CONFIDENCE INTERVALS: don't run until we add prediction code ####
pred.y = post.summ(post, "pred.y[")

pred.y
dev.off()
png("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/Rwork/reproductive_analysis/linear_glm.png")
plot(Estimated_fecundity ~ Carapace_width, data = dat, xlab = 'Carapace Width (mm)', 
     ylab = 'Estimated Fecundity (millions)', las = 1)
lines(pred.y["mean",] ~ jags.dat$pred.x, lty = 2, col = "blue", lwd = 2)
lines(pred.y["2.5%",] ~ jags.dat$pred.x, lty = 2, col = "grey", lwd = 2)
lines(pred.y["97.5%",] ~ jags.dat$pred.x, lty = 2, col = "grey", lwd = 2)
dev.off()
#points(Estimated_fecundity ~Standard_cw, data = dat, col = 'black')


#residuals
resi <- post.summ(post, "resid[")
y.hat = post.summ(post,"y.hat[")
png("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/Rwork/reproductive_analysis/linear_glm_resids.png")
plot(resi[1,] ~ y.hat[1,], type = 'h', ylab = "Residual Values", las = 1) #want them spread above and below evenly
abline(h = 0, col = 'blue')
dev.off()
hist(resi[1,], main = 'Residual Frequencies', xlab = "Residual Values")

fit.obs <- post.summ(post, "fit.obs")
fit.est <- post.summ(post, "fit.new")


############################### Model 2 log(fecundity) ############################
#try non-linear
# par(mfrow = c(1,2))
# plot(Estimated_fecundity ~ Carapace_width, data = dat, xlab = 'Carapace Width (mm)', 
#      ylab = 'Estimated Fecundity (1e+06)', main = 'Fecundity ~ Carapace Width (mm)')
# 
# plot(log(Estimated_fecundity) ~ Carapace_width, data = dat, xlab = 'Carapace Width (mm)', 
#      ylab = 'Log(Fecundity) (1e+06)', main = 'log(Fecundity) ~ Carapace Width (mm)')
# 
# dev.off()


#for the credibility intervals
pred.x <- seq(min(dat$Carapace_width), max(dat$Carapace_width), length = 30)

# compile data into a list to pass to BUGS
jags.dat = list(n.obs = nrow(dat), y = dat$Estimated_fecundity,x1 = dat$Carapace_width, 
                 pred.x = pred.x, n.pred = length(pred.x))



##### MODEL 2 ALL SEAMAP CRABS, log-fecundity #####
##### SPECIFY MODEL CODE #####

mod = function() {
  # PRIORS
  b0 ~ dnorm(0,.001) #small precision = big variance
  b1 ~ dnorm(0,.001)
  sig ~ dunif(0,10)
  tau <- 1/(sig^2)
  # LIKELIHOOD
  for (i in 1:n.obs) {
    y[i] ~ dlnorm(log(y.hat[i]), tau)
    log(y.hat[i]) <- b0 + b1 * x1[i] #x1 is carapace with data
    resid[i] <- y[i] - y.hat[i]
  }
  
  for (i in 1:n.pred) {
    log(pred.y[i]) <- b0 + b1 * pred.x[i]
  }
  #Posterior Predictive check
  for (i in 1:n.obs) {
    # generate data under the model
    y.new[i] ~ dlnorm(log(y.hat[i]), tau)
    
    # use pearson's residuals
    obs.resid[i] <- (y[i] - y.hat[i])/sig
    new.resid[i] <- (y.new[i] - y.hat[i])/sig
    
    # calculate squared residuals
    D.obs[i] <- pow(obs.resid[i], 2)
    D.new[i] <- pow(new.resid[i], 2)
  }
  
  # calculate deviations
  fit.obs <- sum(D.obs[])
  fit.new <- sum(D.new[])
  # "Bayesian P-value"
  bp <- step(fit.obs - fit.new)
  
}

# write model to a text file
model.file = "model.txt"
R2WinBUGS::write.model(mod, model.file)

##### INITIAL VALUES #####
inits1 = list(b0 = rnorm(1), b1 = rnorm(1), sig = rlnorm(1))
inits2 = list(b0 = rnorm(1), b1 = rnorm(1), sig = rlnorm(1))
inits = list(inits1, inits2)

##### PARAMETERS TO MONITOR #####
params = c("b0", "b1", "sig","resid",'pred.y', "y.hat","bp")

##### MCMC DIMENSIONS #####
ni = 25000  # number of post-burn-in samples per chain
nb = 10000  # number of burn-in samples
nt = 1     # thinning rate
nc = 2     # number of chains

##### RUN THE MODEL IN BUGS #####
starttime = Sys.time()
jmod = jags.model(file = model.file, data = jags.dat, n.chains = nc, inits = inits, n.adapt = 1000)
update(jmod, n.iter = nb, by = 1, progress.bar = 'text')
mod2_dic <- dic.samples(jmod, n.iter = ni, thin = nt)
post = coda.samples(jmod, params, n.iter = ni, thin = nt)
Sys.time() - starttime

source("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/Rwork/GAM_plays/Zuuretal_2016_ZIM_AllData/MCMCSupportHighstatV4.R")


##### CONVERGENCE DIAGNOSTIC #####
# view BGR convergence diagnostic
#gelman.diag(post, multivariate = F)

# visualize trace and posterior plots
#plot(post)

#plot(post[,'b0'])
#plot(post[,'b1'])


b0.est = post.summ(post, "b0"); b0.est
b1.est = post.summ(post, "b1"); b1.est
bp.est = post.summ(post, "bp"); bp.est

## CONFIDENCE INTERVALS: don't run until we add prediction code ####
pred.y = post.summ(post, "pred.y[")

pred.y
#dev.off()
png("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/Rwork/reproductive_analysis/log_glm.png")
plot(Estimated_fecundity ~ Carapace_width, data = dat, xlab = 'Carapace Width (mm)', 
     ylab = 'Estimated Fecundity (millions)', las = 1)
lines(pred.y["mean",] ~ jags.dat$pred.x, lty = 2, col = "blue", lwd = 2)
lines(pred.y["2.5%",] ~ jags.dat$pred.x, lty = 2, col = "grey", lwd = 2)
lines(pred.y["97.5%",] ~ jags.dat$pred.x, lty = 2, col = "grey", lwd = 2)
dev.off()
#points(Estimated_fecundity ~Standard_cw, data = dat, col = 'black')



#residuals, something is off with residuals, log transformation makes them all positive
resi <- post.summ(post, "resid[")
y.hat = post.summ(post,"y.hat[")
png("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/Rwork/reproductive_analysis/log_glm_resids.png")
plot(resi[1,] ~ y.hat[1,], type = 'h', ylab = "Residual Values", las = 1) #want them spread above and below evenly
abline(h = 0, col = 'blue')
dev.off()
plot(resi[1,] ~ y.hat[1,])
#hist(resi[1,], main = 'Residual Frequencies', xlab = "Residual Values")





#################### model 3 bayesian GLM, gamma family, log link ##################################

##### SPECIFY MODEL CODE #####

# 
# #reparameterization by mean and sd
# mod = function() {
#   # PRIORS
#   b0 ~ dnorm(0,.001) #small precision = big variance
#   b1 ~ dnorm(0,.001)
#   
#   # parameterized by mean (mu) and standard deviation (sd)
#   sh <- pow(mu,2) / pow(sd,2)
#   ra <- mu/pow(sd/2)
#   mu ~ dunif(0,100)
#   sd ~ dunif(0,100)
#   
#   # likelihood
#   for(i in 1:n.obs) {
#     y[i] ~ dgamma(y.hat[i], ra)
#     y.hat[i] <- exp(b0 + b1 * x1[i])
#     resid <- y[i] - y.hat[i]
#   }
#   
#   # calculate residuals
#   for (i in 1:n.pred) {
#     pred.y[i] <- exp(b0 + b1 * pred.x[i])
#     
#   }
#   
#   
# }
# 
# # write model to a text file
# model.file = "model.txt"
# write.model(mod, model.file)
# 
# ##### INITIAL VALUES #####
# inits1 = list(b0 = rnorm(1), b1 = rnorm(1), mu = rnorm(1), sd = rlnorm(1))
# inits2 = list(b0 = rnorm(1), b1 = rnorm(1), mu = rnorm(1), sd = rlnorm(1))
# inits = list(inits1, inits2)
# 
# ##### PARAMETERS TO MONITOR #####
# params = c("b0", "b1", "mu", "sd","resid",'pred.y', "y.hat")
# 
# ##### MCMC DIMENSIONS #####
# ni = 30000  # number of post-burn-in samples per chain
# nb = 15000  # number of burn-in samples
# nt = 1     # thinning rate
# nc = 2     # number of chains
# 
# ##### RUN THE MODEL IN BUGS #####
# starttime = Sys.time()
# jmod = jags.model(file = model.file, data = jags.dat, n.chains = nc, inits = inits, n.adapt = 1000)
# update(jmod, n.iter = nb, by = 1, progress.bar = 'text')
# post = coda.samples(jmod, params, n.iter = ni, thin = nt)
# Sys.time() - starttime
# 
# #### look at convergence ####
# # view BGR convergence diagnostic
# gelman.diag(post, multivariate = F)
# 
# # visualize trace and posterior plots
# #plot(post)
# #plot(post[,'b0'])
# #plot(post[,'b1'])
# 
# pred.y = post.summ(post, "pred.y[")
# pred.y
# #dev.off()
# plot(Estimated_fecundity ~ Carapace_width, data = dat, xlab = 'Carapace Width (mm)', 
#      ylab = 'Fecundity (1e+06)', main = 'Gamma log-link')
# lines(pred.y["mean",] ~ jags.dat$pred.x, lty = 2, col = "blue", lwd = 2)
# lines(pred.y["2.5%",] ~ jags.dat$pred.x, lty = 2, col = "grey", lwd = 2)
# lines(pred.y["97.5%",] ~ jags.dat$pred.x, lty = 2, col = "grey", lwd = 2)
# #points(Estimated_fecundity ~Standard_cw, data = dat, col = 'black')


######################## Try a glm for it ############
#### model selection ####
#Carapace Width
#gaussian
glm(formula = Estimated_fecundity ~ Carapace_width, data = dat,family = gaussian(link = 'log'))
glm(formula = Estimated_fecundity ~ Carapace_width, data = dat,family = gaussian(link = 'identity'))
#Gamma
glm(formula = Estimated_fecundity ~ Carapace_width, data = dat,family = Gamma(link = 'identity'))
glm(formula = Estimated_fecundity ~ Carapace_width, data = dat,family = Gamma(link = 'log'))


#best model
cw_mod <- glm(formula = Estimated_fecundity ~ Carapace_width, data = dat,family = Gamma(link = 'log'))

#coefficients, Confidence intervals
coef(cw_mod);confint.default(cw_mod)

#plot best model
range(dat$Carapace_width) #get range of x values
xcw <- seq(120, 200, 0.1)

ypred <- predict(cw_mod, list(Carapace_width = xcw),type="response")
low_pred <- ypred- (sd(dat$Carapace_width))/sqrt(length(dat$Carapace_width))
high_pred <- ypred+ (sd(dat$Carapace_width))/sqrt(length(dat$Carapace_width))

plot(Estimated_fecundity ~ Carapace_width, data = dat, main = 'GLM Gamma log link', ylab = 'Fecundity (10e6)')
lines(xcw,ypred)
lines(xcw,low_pred)
lines(xcw,high_pred)

#standard carapace width
#gaussian
glm(formula = Estimated_fecundity ~ Standard_cw, data = dat,family = gaussian(link = 'log'))
glm(formula = Estimated_fecundity ~ Standard_cw, data = dat,family = gaussian(link = 'identity'))
#Gamma
glm(formula = Estimated_fecundity ~ Standard_cw, data = dat,family = Gamma(link = 'identity'))
glm(formula = Estimated_fecundity ~ Standard_cw, data = dat,family = Gamma(link = 'log'))

sw_mod <- glm(formula = Estimated_fecundity ~ Standard_cw, data = dat,family = Gamma(link = 'log'))

#plot models
range(dat$Standard_cw)
xsw <- seq(90,150,0.1)
ypred.sw <- predict(sw_mod, list(Standard_cw = xsw), type = 'response')
low_pred_sw <- ypred.sw - (sd(dat$Standard_cw))/sqrt(length(dat$Standard_cw))
high_pred_sw <- ypred.sw + (sd(dat$Standard_cw))/sqrt(length(dat$Standard_cw))

plot(Estimated_fecundity ~ Standard_cw, data = dat, main = 'Fecundity ~ Standard Width', las = 1,
     xlab = "Standard Carapace Width", ylab = "Estimated Fecundity (millions)")
lines(xsw,ypred.sw)
lines(xsw,low_pred_sw)
lines(xsw,high_pred_sw)

