# Posterior Summary for Requested Parameters

# AUTHOR: BEN STATON
# DATE: 8/7/17

# This function allows you to pull out single variables by name from a mcmc.list and plot if desired
  # post.samp: an object of class 'mcmc.list'
  # var: the variable you wish to view
  # do.post: logical. Return the whole posterior along with summary? Default is FALSE
  # do.plot: logical. Plot the posterior and trace plots? Default is FALSE

post.summ = function(post.samp, var, do.post = F, do.plot = F){
  require(coda)
  
  if (!is.mcmc.list(post.samp)) stop("post.samp is not of class mcmc.list")
  
  #coerce to matrix
  nchains = length(post.samp)
  post.samp = as.matrix(post.samp)

  # 1.) EXTRACT AND SUMMARIZE POSTERIOR SAMPLES
  if (substr(var, nchar(var), nchar(var)) == "[") { # if the parameter has subscripts
    post = post.samp[,substr(colnames(post.samp), 1, nchar(var)) == var]
    post.est = apply(post, 2, function(x) c(mean = mean(x), sd = sd(x), quantile(x, c(0.5, 0.025, 0.975))))
  } else { # if the parameter doesn't have subscripts
    post = post.samp[,var] 
    post.est = c(mean = mean(post), sd = sd(post), quantile(post, c(0.5, 0.025, 0.975)))
  } 

  # IF PLOTTING MCMC DIAGNOSTICS
  if (do.plot) {
    
    if (is.matrix(post)) { # if the extracted posterior has multiple parameters
      # number of iterations per chain
      n.iterations = nrow(post)/nchains
      
      windows(record = T)
      par(mfrow = c(4,2), mar = c(2, 2, 1.5, 1.5), oma = c(1,1,1,1))
      
      # split the posterior into chains
      chain = list()
      chain[[1]] = post[1:n.iterations,]
      for(i in 2:nchains){
        chain[[i]] = post[(((i-1) * n.iterations)+1) : (n.iterations * i),]
      }
      
      # calculate the max density for each parameter, for ylim in density plot
      max.dens = matrix(NA, nrow = nchains, ncol = ncol(post))
      for(j in 1:nchains){
        for(i in 1:ncol(post)){
          max.dens[j,i] = max(density(chain[[j]][,i])$y)
        }
      }
      max.dens = apply(max.dens, 2, max)
      
      # calculate the min and max draw for each parameter for xlim in density and ylim in trace
      min.max.pulls = matrix(NA, nrow = 2, ncol = ncol(post))
      for(i in 1:ncol(post)){
        min.max.pulls[,i] = c(min(post[,i]), max(post[,i]))
      }
      
      # plot the parameter density plots/trace plots
      # NOTE: this function deals with up to 8 chains.  To have it handle more, add more colors to the colors vector
      # to this "colors" vector
      colors = c("", "red", "green", "skyblue", "yellow", "forestgreen", "pink", "purple")
      
      for(i in 1:ncol(post)){
        plot(density(chain[[1]][,i]), type = "l", col = "Blue", xlim = min.max.pulls[,i], ylim = c(0, max.dens[i]),
             xlab = "", ylab = "Density", main = paste("Posterior of ", colnames(post)[i], sep = ""))
        for(j in 2:nchains){
          lines(density(chain[[j]][,i]), col=colors[j])
        }
        plot(chain[[1]][,i], type = "l", col="Blue", ylim=min.max.pulls[,i],
             xlab = "Iteration", ylab=" ", main=paste("Trace of ", colnames(post)[i], sep=""))
        for(j in 2:nchains){
          lines(chain[[j]][,i], col=colors[j])
        }
      }
    } else { # if the extracted posterior is for only one parameter
      # number of iterations per chain
      n.iterations = length(post)/nchains
      
      # separate posterior samples into the chains
      chain = list()
      chain[[1]] = post[1:n.iterations]
      
      for(i in 2:nchains){
        chain[[i]] = post[(((i-1) * n.iterations)+1) : (n.iterations * i)]
      }
      
      # calculate maximum density for ylim on dens plot
      max.dens = numeric(nchains)
      for(i in 1:nchains){
        max.dens[i] = max(density(chain[[i]])$y)
      }
      
      # NOTE: Same as above, if you want more than 8 chains, just add colors to this character vector
      colors = c("", "red", "green", "skyblue", "yellow", "forestgreen", "pink", "purple")
      windows(width = 7, height = 5)
      par(mfrow = c(1,2))
      
      plot(density(chain[[1]]), col = "Blue", xlab = "", xlim = c(min(post), max(post)),ylim = c(0, max(max.dens)), main = paste("Density of ", var, sep = ""))
      for(i in 2:nchains){lines(density(chain[[i]]), col = colors[i])}
      
      plot(chain[[1]], type = "l", col = "Blue",xlab = "Iteration", ylab = "", ylim = c(min(post), max(post)), main = paste("Trace of ", var, sep=""))
      for(i in 2:nchains){lines(chain[[i]], col = colors[i])}
    }
  }
  
  if (do.post) {
    list(posterior = post, summary = post.est)
  } else post.est
}

cat("Function 'post.summ()' read into workspace successfully")
