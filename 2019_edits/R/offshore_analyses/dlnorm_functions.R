# Created 10/23/2018
# Authors : David Hanisko & Adam A. Kemberling


#Inputs :
# 1. "bin_model" a glm(, family = quasibinomial(link = "logit") model using all data, response is binary success/fail
# 2. "Ln_model" a glm(, family = "gaussian") model, using the positive only subset, performed on log transformed data



dlnorm_lo_method <- function(bin_model, Ln_model) {
  
  ####  lsmeans from model 1 - Proportion Positive  ####
  # get lsmeans for proportion positive using bin_model
  qb_lsm_om <- lsmeans(bin_model, specs = "Year_f" , by = "Year_f")   #extract lsmeanas
  qb_lsm_om_df <- as.data.frame(summary(qb_lsm_om))            #save summary as dataframe
  
  
  #get lsmeans for proportion positive on response (proportional scale)
  qb_lsmt_om_df <- as.data.frame(summary(qb_lsm_om, type = "response"))
  rm(qb_lsm_om)                                                #remove intermediate to obj
  
  #add prob from response on response scale to modeled lsmeans
  qb_lsm_om_df$prob <- qb_lsmt_om_df$prob
  
  #######################################################################################
  
  ####  lsmeans from model 2 - lognormal glm  ####
  # get lsmeans on model scale
  p_lsm_om <- lsmeans(Ln_model, "Year_f",weights="proportional")
  p_lsm_om <- as.data.frame(summary(p_lsm_om))
  #p_lsm_om
  
  # create bias corrected backtransfomed to calculate correlation coefficient
  p_lsm_om$LSM_bcpos <- exp(p_lsm_om$lsmean + (p_lsm_om$SE**2/2))
  
  
  
  #######################################################################################
  
  
  ####  Bias Corrected Back Transformation
  
  
  # Estimate the annual standardized index as the product:  LSMeans ProPos * LSMeans Pos
  # apply bias correction to estimates
  
  ######## modifid by dsh to match up with MOrtiz bias correction functions and final index calculations
  
  # 1.
  # Get lsmeans from lsmeans for proportio of positieve occurrence
  LSMeansProPos <- qb_lsm_om_df[c("lsmean","SE","Year_f","prob")]
  names(LSMeansProPos) <- c("LSM_ppt","se_ppt","Year_f","LSM_prob")
  
  # 2.
  # extract lsmeans on positives to dataframe
  LSMeansPos <- p_lsm_om[c("lsmean","SE","Year_f","LSM_bcpos")]
  names(LSMeansPos) <- c("LSM_pos","se_pos","Year_f","LSM_bcpos")
  
  # 3.
  # add dispersion (residual variance) and model residual degrees of freedom (df.residual) from
  # glm model to match sas values. dispsersion and df.residual needed for bias correction function
  LSMeansPos$res_var_pos <- summary(Ln_model)$dispersion
  LSMeansPos$dfLSMeanspos <- Ln_model$df.residual
  
  
  
  
  ##### modifications performed by AK for code legibility     #####
  
  ######## modifid by dsh to match up with MOrtiz bias correction functions and final index calculations
  estimate <- merge(LSMeansPos,LSMeansProPos,by="Year_f")
  
  
  
  #Calculate all derivations using mutate rather than with() for legibility
  #Correlation estimated using pearson residuals
  estimate <- estimate %>% mutate(
    muLSMeansppt = exp(LSM_ppt)/(1+exp(LSM_ppt)),                  #Convert estimated LSMeans of binomial model to the original scale (i.e. by using inverse logit function)
    semuLSMeansppt = (exp(LSM_ppt)/(1+exp(LSM_ppt))^2)*abs(se_ppt), #Do the same for standard error estimate
    muLSMeanspos = LSM_pos,                                         #Positive model doesn't not require this if using identity link
    semuLSMeanspos = se_pos,                                        #Standard error of positive model
    cor_par = cor(LSM_bcpos, muLSMeansppt, method = "pearson")      #Estimate correlation
  )
  # changed by dsh to base correlation on back transformed with bias correction catch (LSM_bcpos)
  # and proprtion positive on proportional scale
  
  
  # Lo's bias correction formula
  tmp <- Lo_biasCorr_fun(estimate$res_var_pos, estimate$LSM_pos, (estimate$se_pos)^2, estimate$dfLSMeanspos)
  # no bias correction for the binomial model
  
  
  #Add 'var_c' and 'gc_pos' to estimate dataframe, derive more values
  estimate <- estimate %>% mutate(
    var_c = tmp[,1],                                                
    gc_pos = tmp[,2],
    var_p = semuLSMeansppt**2,                                      #calculate variance of the mean var_p to match SAS DLN
    Index = muLSMeansppt*((exp(LSM_pos)*gc_pos)),                   #Estimate index
    var_Index = var_p*(((exp(LSM_pos))*gc_pos )**2)+((var_c)*muLSMeansppt**2),
    se_Index = sqrt(var_Index),
    CV_Index = se_Index/Index,
    C = (exp(2*sqrt(log(1+CV_Index**2)))),
    LCI = Index/C,
    UCI = Index*C,
    StdIndex = Index/mean(Index),
    StdLCI = LCI/mean(Index),
    StdUCI = UCI/mean(Index)
  )
  
  
  
  return(estimate)
  
}




##########  Lo's Bias Correction  ###############
Lo_biasCorr_fun <- function(cpu_var,lcpu,c_var,mc)  # residual variance of glm model, LSM_estimate, estimated variance of each LSMean (se^2), degrees of freedom LSMean
{
  # Implementation of the Lo's Bias correction method as used in the SAS cpue delta standardization model Mortiz
  # it works in a single observation, so it need to be within an for loop type construction for each LSMean
  # output a vector with 2 items: [1] var of each LSMean [2] bias correction factor gc (see SAS code)
  nObs <- length(cpu_var)
  tc <- vector(mode="numeric",length=nObs)
  td <- vector(mode="numeric",length=nObs)
  gc <- vector(mode="numeric",length=nObs)
  gd <- vector(mode="numeric",length=nObs)
  tmp <- matrix(data=0,nrow=nObs,ncol=2)
  for (r in 1:nObs)
  {
    tc[r] <- (cpu_var[r]-c_var[r])*(mc[r]+1)/(2*mc[r])
    td[r] <- (cpu_var[r]-2*c_var[r])*(mc[r]+1)/mc[r]
    gc[r] <- 0; gd[r] <- 0; d <- 1;
    for (p1 in 0:50) {
      p <- p1
      if(p < 1) { gc[r] <- 0; gd[r] <- 0; d <-1; }   # this resets the variables gc/gd to zero, & d to 1
      d <- (mc[r]+2*p)*d;
      a <- (mc[r]^p)*(mc[r]+2*p);
      b <- (mc[r]/(mc[r]+1))^p;
      c <- gamma(p+1);
      cc <- (tc[r]^p)/c;
      cd <- (td[r]^p)/c;
      prgc <- gc[r]; prgd <- gd[r];
      gc[r] <- gc[r] + (a/d)*b*cc;
      gd[r] <- gd[r] + (a/d)*b*cd;
      if(p > 2) {
        tol <- gc[r]-prgc; tol2 <- gd[r]-prgd;
        if(abs(tol) < 0.0000001 & abs(tol2) < 0.0000001) break }
    }   # end the for loop for @ obs
    var_c <- exp(2*lcpu[r])*(gc[r]^2-gd[r])
    tmp[r,1] <- var_c; tmp[r,2] <- gc[r];
  } # end of the nobs loop
  return(tmp)
}


##########  Helper Functions  ################

ln_hist <- function(ln_model) {
  
  
  #get pearson chi-square residuals from model
  
  p_chi_resid <- data.frame(chi_resid = numeric(length(ln_model$residuals)))
  p_chi_resid$chi_resid <- residuals(ln_model, type="pearson")
  
  #use hist() to get breaks, mids etc without plotting to feed to ggplot()
  p_chi_hist <- hist(p_chi_resid$chi_resid,plot=F)
  
  #use for legend in histograms
  myscaletxt2 <- paste("Normal Curve ( Mean = ", round(mean(p_chi_resid$chi_resid),digits=4),
                       " ","Std Dev =", round(sd(p_chi_resid$chi_resid),digits=4),")", sep="")
  
  
  #Histogram of residuals
  hist_pcr <- ggplot(data=p_chi_resid, aes(x = chi_resid)) + 
    geom_histogram(aes(y =..density..),
                   breaks = p_chi_hist$breaks,
                   colour = "gray20", 
                   fill = "gray80") +
    scale_x_continuous(breaks=p_chi_hist$mids, labels=p_chi_hist$mids) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(p_chi_resid$chi_resid), 
                              sd = sd(p_chi_resid$chi_resid)),
                  aes(colour = "Normal Curve")) +
    scale_color_manual(name = '', 
                       values = c("Normal Curve" = "gray20"), 
                       labels=myscaletxt2) + 
    theme(legend.position="bottom",legend.text = element_text(size=12)) + 
    ggtitle("Distribution of Model Chi-Square Residuals") +
    theme(plot.title = element_text(hjust=0.5))
  
  
  hist_pcr
}


ln_qqplot <- function(ln_model) {
  
  #get pearson chi-square residuals from model
  p_chi_resid <- data.frame(chi_resid = numeric(length(ln_model$residuals)))
  p_chi_resid$chi_resid <- residuals(ln_model, type="pearson")
  
  #Plot them
  qqnorm(p_chi_resid$chi_resid)
  qqline(p_chi_resid$chi_resid,col="red")
  
  
}







fact_table <- function(mydata, fact_list) {
  for (i in 1:length(fact_list)) {
    
    t1 <- mydata %>%  group_by(.dots = fact_list[[1]]) %>% 
      dplyr::summarise(N_OBS = n(),
                       N_PP = sum((cpue_dat > 0) == TRUE, na.rm = T),
                       PP_OBS = N_PP/N_OBS,
                       MEAN_CPUE = mean(cpue_dat, na.rm = T))%>% 
      mutate(Factor = rep(fact_list[[1]], length(unique(fact_list[[1]])))) %>% 
      rename(Level = fact_list[[1]]) %>% 
      mutate(Level = as.character(Level)) %>%
      select(Factor, Level, everything())
    
    if(length(fact_list) == 1) break
    
    if(length(fact_list) > 1) {
      fact_2 <- mydata %>%  group_by(.dots = fact_list[[2]]) %>% 
        dplyr::summarise(N_OBS = n(),
                         N_PP = sum((cpue_dat > 0) == TRUE, na.rm = T),
                         PP_OBS = N_PP/N_OBS,
                         MEAN_CPUE = mean(cpue_dat, na.rm = T))%>% 
        mutate(Factor = rep(fact_list[[2]], length(unique(fact_list[[2]])))) %>% 
        rename(Level = fact_list[[2]]) %>% 
        mutate(Level = as.character(Level)) %>%
        select(Factor, Level, everything())
      
      t1 <- bind_rows(t1, fact_2) 
    }
    
    if(length(fact_list) == 2) break
    
    if(length(fact_list) > 2) {
      
      fact_3 <- mydata %>%  group_by(.dots = fact_list[[3]]) %>% 
        dplyr::summarise(N_OBS = n(),
                         N_PP = sum((cpue_dat > 0) == TRUE, na.rm = T),
                         PP_OBS = N_PP/N_OBS,
                         MEAN_CPUE = mean(cpue_dat, na.rm = T))%>% 
        mutate(Factor = rep(fact_list[[3]], length(unique(fact_list[[3]])))) %>% 
        rename(Level = fact_list[[3]]) %>% 
        mutate(Level = as.character(Level)) %>%
        select(Factor, Level, everything())
      
      t1 <- bind_rows(t1, fact_3)
    }
    
    if(length(fact_list) == 3) break
    
    if(length(fact_list) > 3) {
      fact_4 <-mydata %>%  group_by(.dots = fact_list[[4]]) %>% 
        dplyr::summarise(N_OBS = n(),
                         N_PP = sum((cpue_dat > 0) == TRUE, na.rm = T),
                         PP_OBS = N_PP/N_OBS,
                         MEAN_CPUE = mean(cpue_dat, na.rm = T))%>% 
        mutate(Factor = rep(fact_list[[4]], length(unique(fact_list[[4]])))) %>% 
        rename(Level = fact_list[[4]]) %>% 
        mutate(Level = as.character(Level)) %>%
        select(Factor, Level, everything())
      
      t1 <- bind_rows(t1, fact_4)
    }
    
  }
  
  t1
  
}



#####  Here are some supplemental functions for plots in the template  #####


year_samps <- function (mydata) {
  
  mydata %>% 
    group_by(Year, add=FALSE) %>% 
    summarise (N = n(),
               NominalPPOS = mean(success, na.rm = T),
               obcppos = mean(cpue_dat)) %>% 
    rename(Year_f = Year) %>% 
    as.data.frame()
  
  
}


cpue_timeline <- function (mydata) {
  
  mydata %>% 
    group_by(Year, region) %>% 
    summarise(MEAN_CPUE = mean(cpue_dat)) %>% 
    ggplot(aes(Year, MEAN_CPUE)) + 
    geom_line(aes(group = region, linetype = region)) +
    geom_point() +
    scale_x_discrete(limits = c(as.numeric(ystart:yend)), labels = ynames) + 
    xlab("Year") +
    ylab("Observed CPUE") + 
    theme(axis.text.x=element_text(angle=90, vjust=0.5))
  
}

propos_timeline <- function (mydata) {
  
  mydata %>% 
    group_by(Year, region) %>% 
    summarise(obpos = mean(success, na.rm = T)) %>% 
    ggplot(aes(Year, obpos)) + 
    geom_line(aes(group = region, linetype = region)) + 
    geom_point() +
    scale_x_discrete(limits = c(as.numeric(ystart:yend)), labels = ynames) + 
    xlab("Year") +
    ylab("Observed Proportion Positive Catch") + 
    theme(axis.text.x=element_text(angle=90, vjust=0.5))
  
}

lgcpue_freq <- function (mydata) {
  
  pos_data <- mydata %>% filter(success == 1)
  
  ggplot(pos_data, aes(x = lgcpue)) + 
    geom_histogram(aes(y = stat(density)), 
                   bins = 8, 
                   fill = "gray80", 
                   color = "gray20") +
    stat_function(
      data = pos_data,
      fun = dnorm, 
      args = list(mean = mean(mydata$lgcpue, na.rm = T), 
                  sd = sd(mydata$lgcpue, na.rm = T)),
      col = 'gray20') + 
    ylab("Percent")
  
}


year_hex <- function (mydata) {
  
  #Make Hex_Bin Plot by year
  map.world_polygon <- map_data("world")
  
  #Positive Catches
  p.list <- lapply(sort(unique(mydata$Year)), function(i) {
    ggplot(mydata[mydata$Year==i,], aes(x = sta_lon, y = sta_lat)) +
      coord_cartesian(xlim = c(-98.08, -79.95), ylim = c(24.8, 31.2)) +
      geom_polygon(data = map.world_polygon, aes(x = long, y = lat, group = group)) +
      geom_hex(bins = 600, alpha = 0.75) +
      scale_fill_distiller(palette = "Spectral") + 
      facet_wrap(~Year) +
      xlab("Longitude") +
      ylab("Latitude") +
      theme_classic() +
      theme(legend.position = "bottom")
  }
  )
  
  names(p.list) <- sort(unique(mydata$Year))
  p.list
  
}



`%notin%` = function(x,y) !(x %in% y)