#### Code for "Urbanization, climate, and species traits shape mammal communities from local to continental scales" ####

#### 1.2 Extracting and Summarizing Community Composition Estimates from the Multi-Region Community Occupancy Model ####
#### Haight, Jeffrey D. et al.


#### Setup ####
  rm(list=ls()) # clear the environment
  gc()
  set.seed(123)

# Set working directory
  #setwd("YourFilePathHere")
  # e.g.,
  setwd("C:/Users/User/Documents/GitHub/uwin-multiregioncommcomp")

# Load necessary packages
  library(dplyr)    # for working with data
  library(jagsUI)   # for fitting models in JAGS
  library(beepr)    # for making a fun sound when the model finishes (if desired)

# Load model input and output objects
  load("./data/modelinput/ModelInputData_UWIN_MRCM.RData")
  
  # this was the object output from the 'jags()' function in the 'jagsUI' package
  # the model file itself was unfortunately too large to upload to the data repository
  # this file can be reproduced by running the code in the '1_1_Analysis_FittingMultiRegionCommunityModel.R' script
  out <- readRDS("./data/modeloutput/model1output_mrcm_globalinteractionmodel_sample60k.rds")  # this also takes up a notable amount of memory to input
  
  str(tmp <- out$sims.list)   # grab MCMC samples
  length(tmp[[1]])            # number of MCMC samples
  rm(out); gc()               # remove the full model object, to free up some memory
  
  # Select a random subset of the posterior samples (make sure the seed is set above)
  # Due to computational limitations, not all 60,000 samples could be analyzed
  # Instead, a random subset of the samples
  # Approximate run time: 10-15 minutes/1000 samples with 8 GB of RAM
    # nsamp <- length(tmp[[1]])     # to use all 60,000 samples
    # nsamp <- 10000                # to use 10,000 random samples (as in manuscript)
    nsamp <- 1000                   # for illustrative purposes
    samp <- sample(1:length(tmp[[1]]), size = nsamp, replace = FALSE)
  
  # initial objects for storing the predictions
    # these have to be large enough to fit the max number of sites in a city
    # max 95 sites * 20 cities = 1900 "sites" (NAs will then be removed)
    # latent occupancy
    str(zCH <- array(NA, dim = c(nsamp, n_spp_tot, 95*20)))
    # occupancy probability of each species at each site (checked for presence in the region)
    str(psi.checked <- array(NA, dim = c(nsamp, n_spp_tot)))   
    str(hill1 <- array(NA, dim = c(nsamp, 95*20)))
    str(hill2 <- array(NA, dim = c(nsamp, 95*20)))
  

#### Extract Estimates ####
# First, the estimates of local species richness and diversity
  # In order for this to run, the original model needs to monitor phi0, phi1.s1, phi2.s2, phi3.s3, all mu.phi## parameters, and w
  (start.time <- Sys.time())
  for(k in 1:nsamp){    # randomize which samples are selected
    for(r in 1:n_region){
      for(j in 1:n_sites[r]){
        for(i in 1:n_spp_tot){
          sample <- samp[k]  # select the random sample
            
             psi <- plogis(
              tmp$phi0[sample,i,r] +
              tmp$phi1.s1[sample,i,r]*impervious[j,r]+ 
              tmp$phi2.s2[sample,i,r]*hetero[j,r] +
              tmp$phi3.s3[sample,i,r]*cropland[j,r] +
              tmp$mu.phi4.r[sample,i]*data_reg$EVI_av_std[r] + 
              tmp$mu.phi5.s1r[sample,i]*impervious[j,r]*data_reg$EVI_av_std[r]+
              tmp$mu.phi6.r[sample,i]*data_reg$mat_av_std[r] + 
              tmp$mu.phi7.s1r[sample,i]*impervious[j,r]*data_reg$mat_av_std[r]+
              tmp$mu.phi8.r[sample,i]*data_reg$urb_reg_std[r] + 
              tmp$mu.phi9.s1r[sample,i]*impervious[j,r]*data_reg$urb_reg_std[r]+
              tmp$mu.phi10.r[sample,i]*data_reg$yrs_col_std[r] + 
              tmp$mu.phi11.s1r[sample,i]*impervious[j,r]*data_reg$yrs_col_std[r]
              )
    
          # For species not present in the region, set occupancy probability to 0
          psi.checked[k, i] <- psi*tmp$w[sample,i,r]
          
          # Predict occupancy for all sites (including where presence was observed)
          zCH[k,i,(dim(ysum)[2]*(r-1)+j)] <- rbinom(1, 1, psi.checked[k, i]) 
          # (dim(ysum)[2]*(r-1)+j) is just a long-winded way of saying 'the j-th site in each region' where the max # of sites is dim(ysum)[2]
          
          # If the occurrence of a species at a site was already known (Z), check that its presence is 1
          if(Z[i,j,r] > 0 & is.na(Z[i,j,r]) == FALSE){
              zCH[k,i,(dim(ysum)[2]*(r-1)+j)] <- Z[i,j,r]
          }
       #print(i)
        
        }
      
      # Calculating Hill numbers 1 & 2
      # sum of occupancy probabilities across species
        sum.psi <- sum(psi.checked[k,])
        #print(sum.psi)
        
        sum.psi.checked <- ifelse(sum.psi == 0, 1E6, sum.psi) # avoids dividing by 0 when calculating relative psi
        #print(sum.psi.checked)
        
        # relative.psi = relative occupancy: occupancy of each species divided by the across-species sum of probabilities
        # surrogate for relative abundance in 
        relative.psi <- psi.checked/sum.psi.checked    
        
        log.relative.psi <- ifelse(relative.psi[k,]== 0,
                                   log(relative.psi[k,]+1E-6),
                                   log(relative.psi[k,])) # avoids log(0)
        #print(log.relative.psi)
        
        # Calculate Hill number 1
        hill1[k,(dim(ysum)[2]*(r-1)+j)] <- exp(-sum(relative.psi[k,]*log.relative.psi))
        
        # 
        sum.relative.psi.squared <- ifelse(sum(relative.psi[k,]^2)== 0, 
                                           1E-6, 
                                           sum(relative.psi[k,]^2))	#avoid division by zero again
        
        # Calculate Hill number 2			
        hill2[k,(dim(ysum)[2]*(r-1)+j)] <- 1 / sum.relative.psi.squared
        
       # print(j)
        
      }
      #print(r)
    }
    print(k)
  }

  (end.time <- Sys.time())
  elapsed.time <- difftime(end.time, start.time, units='mins')
  cat(paste(paste('Parameters estimated in ', elapsed.time, sep=''), ' minutes/n', sep=''))

  # Remove the NAs from zCH, hill1, and hill2
  # Identify sites that are NA (because they don't exist in a particular region)        
  sites_include <- c(
    K_tot[,1],
    K_tot[,2],
    K_tot[,3],
    K_tot[,4],
    K_tot[,5],
    K_tot[,6],
    K_tot[,7],
    K_tot[,8],
    K_tot[,9],
    K_tot[,10],
    K_tot[,11],
    K_tot[,12],
    K_tot[,13],
    K_tot[,14],
    K_tot[,15],
    K_tot[,16],
    K_tot[,17],
    K_tot[,18],
    K_tot[,19],
    K_tot[,20])  
  
  # drop the non-surveyed sites
  zCH <- zCH[,,which(is.na(sites_include)==FALSE)]
  hill1 <- hill1[,which(is.na(sites_include)==FALSE)]
  hill2 <- hill2[,which(is.na(sites_include)==FALSE)]

  # Summarize local species richness 
  # this is essentially another way to estimate hill0, but here we use an incidence-based estimation instead of occupancy-based
  SR <- apply(zCH, c(1,3), sum)

  Sys.time()
  
  
# Secondly, regional (city-level) species richness
  # While observed and possible regional species occurrence were inputs into the
  # model, the latent species presence in each region 'w' was a monitored parameter in each posterior estimate. 
  # With this, we can get each species' probability of occurring in each region (by averaging across posterior estimates) 
  # or the regional species richness predicted in each posterior estimate (by summing species occurrence values).
  # mean regional species occurrence across all posterior estimates (essentially a species-specific omega)
  pm_w <- apply(tmp$w, c(2,3), function(x)   mean(x, na.rm=TRUE))   
  
  # predicted regional species richness in each posterior sample
  RR <- apply(tmp$w, c(1,3), sum)     

#### Export Estimates ####
  #saveRDS(SR, "./data/modelsummary/model1output_hill0predicted_sample10k.rds")
  #saveRDS(hill1, "./data/modelsummary/model1output_hill1predicted_sample10k.rds")
  #saveRDS(hill2, "./data/modelsummary/model1output_hill2predicted_sample10k.rds")
  #saveRDS(pm_w, "./data/modelsummary/model1output_pm_w.rds")
  #saveRDS(RR, "./data/modelsummary/model1output_RRpredicted.rds")
  #readRDS(SR, "./data/modelsummary/model1output_hill0predicted_sample10k.rds")

#### Summarize Estimates ####
# These summarized data will be added to the site covariate data, 
# which will then be used as the input for the Bayesian meta-analysis of community composition

  pm_SR <- apply(SR, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
  pmed_SR <- apply(SR, c(2), function(x)   median(x, na.rm=TRUE))    # posterior mean
  psd_SR <- apply(SR, c(2), function(x)   sd(x, na.rm=TRUE))    # posterior standard deviation
  cri_SR <- apply(SR, c(2), function(x)   quantile(x, prob = c(0.025, 0.975, 0.05, 0.95))) # posterior quantiles
  
  pm_hill1 <- apply(hill1, 2, mean)
  pmed_hill1 <- apply(hill1, c(2), function(x)   median(x, na.rm=TRUE))    # posterior mean
  psd_hill1 <- apply(hill1, c(2), function(x)   sd(x, na.rm=TRUE))    # posterior standard deviation
  cri_hill1 <- apply(hill1, c(2), function(x)   quantile(x, prob = c(0.025, 0.975, 0.05, 0.95))) # posterior quantiles
  
  pm_hill2 <- apply(hill2, 2, mean)
  pmed_hill2 <- apply(hill2, c(2), function(x)   median(x, na.rm=TRUE))    # posterior mean
  psd_hill2 <- apply(hill2, c(2), function(x)   sd(x, na.rm=TRUE))    # posterior standard deviation
  cri_hill2 <- apply(hill2, c(2), function(x)   quantile(x, prob = c(0.025, 0.975, 0.05, 0.95))) # posterior quantiles
  
  pm_RR <- apply(RR, 2, mean)
  pmed_RR <- apply(RR, c(2), function(x)   median(x, na.rm=TRUE))    # posterior mean
  psd_RR <- apply(RR, c(2), function(x)   sd(x, na.rm=TRUE))    # posterior standard deviation
  cri_RR <- apply(RR, c(2), function(x)   quantile(x, prob = c(0.025, 0.975, 0.05, 0.95))) # posterior quantiles
  
  # add the predicted richness columns to the data_site_reg dataframe
  data_site_reg$rich_mean <- pm_SR
  data_site_reg$rich_med <- pmed_SR
  data_site_reg$rich_sd <- psd_SR
  data_site_reg$rich_2_5 <- c(t(cri_SR[1,]))
  data_site_reg$rich_97_5 <- c(t(cri_SR[2,]))
  data_site_reg$rich_5 <- c(t(cri_SR[3,]))
  data_site_reg$rich_95 <- c(t(cri_SR[4,]))
  
  data_site_reg$hill1_mean <- pm_hill1
  data_site_reg$hill1_med <- pmed_hill1
  data_site_reg$hill1_sd <- psd_hill1
  data_site_reg$hill1_2_5 <- c(t(cri_hill1[1,]))
  data_site_reg$hill1_97_5 <- c(t(cri_hill1[2,]))
  data_site_reg$hill1_5 <- c(t(cri_hill1[3,]))
  data_site_reg$hill1_95 <- c(t(cri_hill1[4,]))
  
  data_site_reg$hill2_mean <- pm_hill2
  data_site_reg$hill2_med <- pmed_hill2
  data_site_reg$hill2_sd <- psd_hill2
  data_site_reg$hill2_2_5 <- c(t(cri_hill2[1,]))
  data_site_reg$hill2_97_5 <- c(t(cri_hill2[2,]))
  data_site_reg$hill2_5 <- c(t(cri_hill2[3,]))
  data_site_reg$hill2_95 <- c(t(cri_hill2[4,]))
  
  # Add predicted regional species richness to the data_reg dataframe
  data_reg$RR_mean <- pm_RR
  data_reg$RR_med <- pmed_RR
  data_reg$RR_sd <- psd_RR
  data_reg$RR_2_5 <- c(t(cri_RR[1,]))
  data_reg$RR_97_5 <- c(t(cri_RR[2,]))
  data_reg$RR_5 <- c(t(cri_RR[3,]))
  data_reg$RR_95 <- c(t(cri_RR[4,]))

  
# Prep the data for export, 
  # dropping the columns that were no longer relevant to the final model,
  # keep the ones that were included as covariates
  colnames(data_site_reg)    # lots and lots of columns
  colnames(data_reg)
  
  
  data_site_reg <- data_site_reg[,-c(3:5)]  # remove the redundant City, Site, and Season columns

    data_site_export <- data_site_reg %>%
    dplyr::select(c(
      site_index,
      city_site,
      city,
      site,
      season,
      Long,
      Lat,
      CRS,
      rich_obs_site,
      K.days,   # rename to "K_days"
      Impervious,
      pd_undev,
      cropland,
      Kdays_std,
      imperv_std,
      pd_undev_std,
      cropland_std,
      rich_mean, rich_med, rich_sd, rich_2_5, rich_97_5, rich_5, rich_95,
      hill1_mean, hill1_med, hill1_sd, hill1_2_5, hill1_97_5, hill1_5, hill1_95,
      #hill2_mean, hill2_med, hill2_sd, hill2_2_5, hill2_97_5, hill2_5, hill2_95,  # ultimately, we did not analyze Hill #2
      reg_name,  # rename to "city_name"
      state_prov,
      survey_yr,
      rich_known_reg,
      rich_obs_reg,
      x_cent,
      y_cent,    
      EVI_av,    # for this dataset, we can stick with the regional variables used in the analysis
      mat_av,
      urb_reg,
      date_col,
      date_col_n,
      yrs_col,
      EVI_av_std,
      mat_av_std,
      urb_reg_std,
      yrs_col_std
    ))
  colnames(data_site_export)[10] <- "K_days"
  colnames(data_site_export)[39] <- "city_name"
  str(data_site_export)
  
  data_reg_export <- data_reg %>%
    dplyr::select(-c(
      mat_sd,
      temp_sm_sd,
      map_sd,
      ppt_sm_sd,
      cmd_sd,
      td_sd,
      EVI_sd,
      PET_sd,
      Forest_av,
      Forest_sd,
      log_yrs_col,
      log_mpa_u_reg,
      pd_n_reg_std,
      mpa_u_reg_std,
      crop_reg_std,
      for_reg_std, 
      cmd_av_std
    ))
  colnames(data_reg_export)[22]  <- "city_name"
  str(data_reg_export)
  
  
#### Export Summarized Estimates ####
  write.csv(data_site_export, "./data/modelsummary/data_sites_mrcmsummary.csv", row.names = FALSE)
  write.csv(data_reg_export, "./data/modelsummary/data_cities_mrcmsummary.csv", row.names = FALSE)
