############################################################################################
######################Length at Maturity Hierarchical model C. sapidus######################
############################################################################################
setwd("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/Rwork/reproductive_analysis/Repro_analysis")
# clear the workspace
rm(list = ls(all = T))

# load packages
library(R2OpenBUGS)
library(rjags)
library(coda)
library(lubridate)
library(chron)

source("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/Rwork/reproductive_analysis/Repro_analysis/post_summ_function.R")
#Bring in Data

#raw tagging
#tag <- read_excel("~/Dropbox (The Craboratory)/The Craboratory/craboratory_shared/tagging/tagging_data_master.xlsx")

#or after cleanup for most up to date data
#tag <- read.csv(paste("~/Dropbox (The Craboratory)/The Craboratory/Kemberling/TaggingCSV/R_troubleshoot/tags_",Sys.Date(),".csv",sep = ""))

#9-14 final data
tag <- read.csv('L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/TaggingCSV/Final_Data/tagging_final_2017-09-14.csv')


dat <- tag[,c('date','cw','Tag_state')]
dat$cw <- as.numeric(as.character(dat$cw))
dat <- dat[!is.na(dat$cw),] #get rid of NA rows
class(dat$date)
dat$date <- as.Date(dat$date, "%m/%d/%Y")
dat$year <- year(dat$date)
hist(dat$year)
dat$year <- (dat$year - 2016) +1
dat$state <- as.factor(dat$Tag_state)
dat <- dat[which(dat$Tag_state != '0'),]
dat$state <- as.numeric(as.factor(dat$state))

##### MODEL 2, STATE SPECIFIC, GULF MEAN #####

####Put it in a list to send to jags####
#pred.x = seq(3, 6, 0.5)

jags.dat = list(n.obs = nrow(dat), y = dat$cw, year = dat$year, nyrs = 2, state = dat$state, 
                nstates = length(unique(dat$state)))


#if you are having problems indexing by state
?complete.cases
length(complete.cases(dat)) #should be the same as in environment

#dat[which(dat$state == '1'),] #no 1's
dat$state <- dat$state-1 #AL = 1 FL = 2 LA = 3 MS = 4 TX = 5
summary(as.factor(dat$state))
summary(as.factor(dat$Tag_state)) #dont use this one

############################# this is prep for doing the states independently ###################
#state subsets
# y.al <- dat$cw[which(dat$state == 1)]
# y.fl <- dat$cw[which(dat$state == 2)]
# y.la <- dat$cw[which(dat$state == 3)] 
# y.ms <- dat$cw[which(dat$state == 4)] 
# y.tx <- dat$cw[which(dat$state == 5)]
#
#
#
# jags.dat = list(n.obs.g = nrow(dat), n.obs.1 = length(y.al), n.obs.2 = length(y.fl), 
#                 n.obs.3 = length(y.la), n.obs.4 = length(y.ms), n.obs.5 = length(y.tx),
#                 y.g = dat$cw, y.al = y.al, y.fl = y.fl, y.la = y.la,y.ms = y.ms, y.tx = y.tx, 
#                 year = dat$year, nyrs = 2, state = dat$state, 
#                 nstates = length(unique(dat$state)))
#
# rm(y.al)
# rm(y.fl)
# rm(y.la)
# rm(y.ms)
# rm(y.tx)







##### SPECIFY MODEL CODE #####
mod = function() {
  # PRIORS
  #gulf hyper population
  mu.g ~ dunif(0,400) #prior for gulf mean
  sig.g ~ dunif(0,100) #prior for gulf standard deviation
  tau.g <- 1/(sig.g^2)
  
  
  #loop for years or state whatever, these inform global paramaters
  for (i in 1:nstates) {
    mu.s[i] ~ dnorm(mu.g, tau.g) #state mean is distributed normally as a function of gulf mu and tau.g
    sig.s[i] ~ dunif(0,100)
    tau.s[i] ~ 1/(sig.s[i]^2)
  }
  
  # LIKELIHOOD
  for (i in 1:n.obs) {
    y[i] ~ dnorm(mu.s[state[i]], tau.s[state[i]])
  }
  
  # POSTERIOR PREDICTIVE CHECK
  
} 


# write model to a text file
model.file = "model.txt"
write.model(mod, model.file)


##### INITIAL VALUES #####
inits1 = list(mu.g = 150, sig.g = 0.7, mu.s[1] = 145, mu.s[2] = 145, mu.s[3] = 145, mu.s[4] = 145, mu.s[5] = 145,
              sig.s[1] = 10, sig.s[2] = 10, sig.s[3] = 10, sig.s[4] = 10, sig.s[5] = 10)
inits2 = list(mu.g = 190, sig.g = 0.1, mu.s[1] = 150, mu.s[2] = 155, mu.s[3] = 155, mu.s[4] = 155, mu.s[5] = 155,
              sig.s[1] = 11, sig.s[2] = 11, sig.s[3] = 11, sig.s[4] = 11, sig.s[5] = 11)
inits = list(inits1, inits2)

##### PARAMETERS TO MONITOR #####
params = c("mu.g", "sig.g",'mu.s', 'sig.s')

##### MCMC DIMENSIONS #####
ni = 5000  # number of post-burn-in samples per chain
nb = 1000  # number of burn-in samples
nt = 1     # thinning rate
nc = 2     # number of chains

##### RUN THE MODEL IN JAGS #####
starttime = Sys.time()
jmod = jags.model(file = model.file, data = jags.dat, n.chains = nc, inits = inits, n.adapt = 1000)
update(jmod, n.iter = nb, by = 1, progress.bar = 'text')
post = coda.samples(jmod, params, n.iter = ni, thin = nt)
Sys.time() - starttime

starttime = Sys.time()
jmod = jags.model(file = model.file, data = jags.dat, n.chains = nc, n.adapt = 1000)
update(jmod, n.iter = nb, by = 1, progress.bar = 'text')
post = coda.samples(jmod, params, n.iter = ni, thin = nt)
Sys.time() - starttime









##### CONVERGENCE DIAGNOSTIC #####
# view BGR convergence diagnostic
gelman.diag(post, multivariate = F)

# visualize trace and posterior plots
#windows(record = T)
#plot(post)

##### MAKE INFERENCE #####
mu.g.est = post.summ(post, "mu.g"); mu.g.est
tau.g.est = post.summ(post, "tau.g["); tau.g.est
mu.s.est = post.summ(post, "mu.s["); mu.s.est  

#Gulf
hist(post.summ(post, "mu.g", do.post = T)$posterior, breaks = 100, 
     col = "grey90", main = "Posterior of Gulf Mean CW", xlab = "Mean Length (mm)")
abline(v = mu.g.est['mean'],lwd = 2, col = 'blue')
abline(v = mu.g.est["2.5%"], lwd = 2, col = 'orange')
abline(v = mu.g.est["97.5%"], lwd = 2, col = 'orange')

#Alabama
hist(post.summ(post, "mu.s[1]", do.post = T)$posterior, breaks = 100, 
     col = "grey90", main = "Posterior of Alabama Mean CW", xlab = "Mean Length (mm)")
#ablines at mean and 95% C.I.
abline(v = mu.s.est[1,1], lwd = 2, col = 'blue')
abline(v = mu.s.est[4,1], lwd = 2, col = 'orange')
abline(v = mu.s.est[5,1], lwd = 2, col = 'orange')

#curve of predicted distribution
hist(dat$cw[which(dat$state == 1)], main = 'Alabama CW at Maturity', xlim = c(0,220), freq = F)
curve(dnorm(x, mu.s.est[1,1], mu.s.est[2,1]), from = min(jags.dat$y), 
      to = max(jags.dat$y), add = T, col = "blue", lwd = 2)


#Florida
hist(post.summ(post, "mu.s[2]", do.post = T)$posterior, breaks = 100, 
     col = "grey90", main = "Posterior of Florida Mean CW", xlab = "Mean Length (mm)")
abline(v = mu.s.est[1,2], lwd = 2, col = 'blue')
abline(v = mu.s.est[4,2], lwd = 2, col = 'orange')
abline(v = mu.s.est[5,2], lwd = 2, col = 'orange')

#Louisiana
hist(post.summ(post, "mu.s[3]", do.post = T)$posterior, breaks = 100, 
     col = "grey90", main = "Posterior of Louisiana Mean CW", xlab = "Mean Length (mm)")
abline(v = mu.s.est[1,3], lwd = 2, col = 'blue')
abline(v = mu.s.est[4,3], lwd = 2, col = 'orange')
abline(v = mu.s.est[5,3], lwd = 2, col = 'orange')

#Mississippi
hist(post.summ(post, "mu.s[4]", do.post = T)$posterior, breaks = 100, 
     col = "grey90", main = "Posterior of Mississippi Mean CW", xlab = "Mean Length (mm)")
abline(v = mu.s.est[1,4], lwd = 2, col = 'blue')
abline(v = mu.s.est[4,4], lwd = 2, col = 'orange')
abline(v = mu.s.est[5,4], lwd = 2, col = 'orange')

#Texas
hist(post.summ(post, "mu.s[5]", do.post = T)$posterior, breaks = 100, 
     col = "grey90", main = "Posterior of Texas Mean CW", xlab = "Mean Length (mm)")
abline(v = mu.s.est[1,5], lwd = 2, col = 'blue')
abline(v = mu.s.est[4,5], lwd = 2, col = 'orange')
abline(v = mu.s.est[5,5], lwd = 2, col = 'orange')



# plot the estimated distribution over top of the data
#Gulf
hist(jags.dat$y, breaks = 30, col = "grey", freq = F, main = "Size at Maturity, Gulf C. sap", 
     xlab = "Length (mm)", ylim = c(0,.05))
curve(dnorm(x, mu.g.est["mean"], mu.g.est["sd"]), from = min(jags.dat$y), to = max(jags.dat$y), add = T, col = "blue", lwd = 2)



# DRAW CURVES FOR EACH YEAR
#size.range = seq(min(dat$cw), max(dat$cw), 1)
pred.p = matrix(NA, nrow = length(size.range), ncol = length(dat$year))

#this is for logistic curve, need equation for distribution
for (y in 1:7) {
  pred.p[,y] = exp(b0.est["mean",y] + b1.est["mean",y] * temp.range)/(1 + exp(b0.est["mean",y] + b1.est["mean",y] * temp.range))
}

