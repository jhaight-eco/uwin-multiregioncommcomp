#### Code for "Urbanization, climate, and species traits shape mammal communities from local to continental scales" ####

#### 1.3 Fitting Bayesian Meta-analysis Model to Community Composition Estimates ####
#### Haight, Jeffrey D., Mason Fidino, et al.


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

#### Import Data ####
  # The main dataset containing all the site and regional covariates, indexed by site
  data_sites <- read.csv("./data/modelsummary/data_sites_mrcmsummary.csv")
  
  # Species richness and diversity were estimated across 10,0000 random draws 
  # from the posterior distribution of occupancy in the multi-region community model
  # The mean and standard deviation of each metric are what is used in the meta-analysis below
  range(data_sites$rich_mean)    # site-level species richness, mean
  range(data_sites$rich_sd)      # site-level species richness, standard deviation
  range(data_sites$hill1_mean)   # site-level species diversity, mean
  range(data_sites$hill1_sd)     # site-level species diversity, standard deviation
  

#### Fit Models ####
  data <- data_sites
  
  # get just the covariate columns we want
  to_keep <- c(
    "rich_mean", "rich_sd",
    "hill1_mean", "hill1_sd",
    "urban_std", "pd_undev_std",
    "cropland_std", "EVI_av_std",
    "mat_av_std", "urb_reg_std",
    "yrs_col_std", "City"
  )
  
  data <- data[,to_keep]
  
  # make the design matrix for the analysis
  dm_alpha <- cbind(
    1,
    data$urban_std,
    data$pd_undev_std,
    data$cropland_std,
    data$EVI_av_std,
    data$urban_std * data$EVI_av_std,
    data$mat_av_std,
    data$urban_std * data$mat_av_std,
    data$urb_reg_std,
    data$urban_std * data$urb_reg_std,
    data$yrs_col_std,
    data$urban_std * data$yrs_col_std
  )
  # and give them useful names
  colnames(dm_alpha) <- c(
    "intercept",
    "urban",
    "pd_undev",
    "cropland",
    "EVI",
    "urban x EVI",
    "mat",
    "urban x mat",
    "urb_reg",
    "urban x urb_reg",
    "yrs_col",
    "urban x yrs_col"
  )
  
#### Fit Species Richness Model ####
  # bundle the data for the analysis
  data_list_rich <- list(
    alpha_z = data$rich_mean,
    alpha_sd_known = data$rich_sd,
    design_matrix_alpha = dm_alpha,
    nsite = nrow(data),
    npar_alpha = ncol(dm_alpha)
  )
  
  # Initial values function for JAGS
  inits_rich <- function() { list(
    beta = runif(data_list_rich$npar_alpha, 0, 0.5),
    my_re = rnorm(data_list_rich$nsite),
    re_tau = dgamma(1,1,1),
    city_tau = dgamma(1,1,1)
  )
  }
  
  # MCMC parameters
  # for jagsUI, number of samples = ni - nb
  nc <- 3      # number of chains
  nt <- 3      # number to samples thin by
  # for illustrative purposes, the next three parameters are 1/10th of their value in the manuscript
  na <- 100   # number of adaptations
  nb <- 1200  # number of burn-ins
  ni <- 1800  # number of iterations
  
  # fit model in JAGS, using 'jagsUI'
  m.sr <- jags(
    model.file = "./R/3_3_jagsmodel_alpha.R",
    data = data_list_rich,
    n.chains = nc,
    parameters.to.save = c("beta", "re_sd"),
    n.adapt = na,
    n.burnin = nb,
    n.iter = ni,
    n.thin = nt,
    modules = "glm",   # log-normal model
    inits = inits_rich,
    parallel = TRUE
  )
  beep(sound = "coin")
  
  # summarize model
  m.sr.sum <- m.sr$summary
  
  # give more informative names to the parameters
  row.names(m.sr.sum) <- c(
    colnames(dm_alpha),
    "re_sd",
    "deviance"
  )
  
  # view the summary
  round(m.sr.sum, 3)
  
  # export the model files
  saveRDS(m.sr, "./data/modeloutput/model2output_logglm_hill0_sample60k.rds")
  
  # export the summary of all modeled parameters
  # these data are located in the second sheet of Supplementary Data 1 (supplementarydata1_summarytables_speciesinfo.xlsx),
  # alongside other model summaries
  write.csv(m.sr.sum, "./data/modelsummary/model2summary_logglm_hill0_sample60k.csv")
  
  
#### Fit Species Diversity Model ####
  # bundle the data for the analysis
  data_list_hill <- list(
    alpha_z = data$hill1_mean,
    alpha_sd_known = data$hill1_sd,
    design_matrix_alpha = dm_alpha,
    nsite = nrow(data),
    npar_alpha = ncol(dm_alpha)
  )
  
  # Initial values function for JAGS
  inits_hill <- function() { list(
    beta = runif(data_list_hill$npar_alpha, 0, 0.5),
    my_re = rnorm(data_list_hill$nsite),
    re_tau = dgamma(1,1,1),
    city_tau = dgamma(1,1,1)
  )
  }
  
  # MCMC parameters
  # for jagsUI, number of samples = ni - nb
  nc <- 3      # number of chains
  nt <- 3      # number to thin samples by
  
  # Exact settings used for the manuscript analysis
  #na <- 10000   # number of adaptations
  #nb <- 120000  # number of burn-ins
  #ni <- 180000  # number of iterations
  
  # Settings with samples reduced by a factor of 100, for illustrative/testing purposes
  na <- 100   # number of adaptations
  nb <- 1200  # number of burn-ins
  ni <- 1800  # number of iterations
  
  # fit model in JAGS
  m.sd <- jags(
    model.file = "./R/3_3_jagsmodel_alpha.R",
    data = data_list_hill,
    n.chains = nt,
    parameters.to.save = c("beta", "re_sd"),
    n.adapt = na,
    n.burnin = nb,
    n.iter = ni,
    n.thin = nt,
    modules = "glm",
    inits = inits_hill,
    parallel = TRUE
  )
  beep(sound = "coin")
  
  # summarize model
  m.sd.sum <- m.sd$summary
  
  # give more informative names to the parameters
  row.names(m.sd.sum) <- c(
    colnames(dm_alpha),
    "re_sd",
    "deviance"
  )
  
  # view the model summary
  round(m.sd.sum, 3)
  
  # export the model files
  saveRDS(m.sd, "./data/modeloutput/model3output_logglm_hill1_sample60k.rds")
  
  # export the summary of all modeled parameters
  # these data are located in the third sheet of Supplementary Data 1 (supplementarydata1_summarytables_speciesinfo.xlsx),
  # alongside other model summaries
  write.csv(m.sd.sum, "./data/modelsummary/model3summary_logglm_hill1_sample60k.csv")