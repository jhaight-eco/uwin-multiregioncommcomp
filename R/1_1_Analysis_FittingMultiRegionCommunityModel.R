#### Code for "Urbanization, climate, and species traits shape mammal communities from local to continental scales" ####

#### 1.1 Fitting the Multi-Region Community Occupancy Model ####
#### Haight, Jeffrey D. et al.


#### Setup ####
  rm(list=ls()) # clear the environment
  gc()
  set.seed(4321)

  # Set working directory
  #setwd("YourFilePathHere")
  # e.g.,
  setwd("C:/Users/User/Documents/GitHub/uwin-multiregioncommcomp")

  # Load necessary packages
  library(jagsUI)   # for fitting models in JAGS



#### Import and View Data ####
# Load data
load("./data/modelinput/ModelInputData_UWIN_MRCM.RData")


# Detection and Survey Data
str(yaug)    # detection array with data augmentation (all-zero species; same as 'ysum' if no data augmentation)
str(ysum)    # detection array without data augmentation
str(Z)       # naive occurence matrix (1/0 for observed/not-observed)
str(K_tot)   # number of repeat surveys at each site in each region
str(w)       # indicator variable for regional species pools



# Summary model parameters
  n_region             # number of regions (i.e., cities)
  M                    # total number of species found across all regions (no data augmentation in our analysis)
  n_spp_aug            # number of all-zero species (for if data augmentation is desired)
  n_spp_tot            # number of species with a detection (not data augmented)
  n_species            # number of species observed in each region (naive regional species richness)
  n_sites              # number of sites surveyed in each region


# Site- and Region-Level Data
  #str(data_reg)       # region data
  #str(data_site)      # site data
  #str(data_site_reg)  # site data merged with region data
  regions              # names of each metropolitan region


# Within-region Site Covariates
  # Site covariates have been standardized and indexed by region
  str(impervious) # percent impervious surfacce within 1000 m
  str(hetero)     # natural patch density within 1000 m
  str(cropland)   # agricultural land cover within 1000 m


# Species Data (from the EltonTraits database)
  #str(data_spp)     # species data, indicating known species presence in each region
  #str(elton)        # species trait data from the EltonTraits database
  species           # names of species detected and included in analysis
  # check the collinearity between species traits
  cor(elton$BodyMass.Value, pantheria$X22.2_HomeRange_Indiv_km2, use = "complete", method = "pearson")  # body mass and home range size
  cor(elton$BodyMass.Value, pantheria$X22.2_HomeRange_Indiv_km2, use = "complete", method = "spearman")
  cor(elton$logmass, pantheria$X22.2_HomeRange_Indiv_km2, use = "complete", method = "pearson")
  cor(elton$logmass, pantheria$X22.2_HomeRange_Indiv_km2, use = "complete", method = "spearman")
  cor(elton$logmass, elton$carn, use = "complete", method = "pearson") # body mass and carnivory
  cor(elton$logmass, elton$carn, use = "complete", method = "spearman")
  



#### Fit Model ####

# Bundle all the model input data
str(bugs.data <- list(M = M, 
                      n.site= n_sites, 
                      n.region = n_region, 
                      #n.spp.naive = rich_obs_reg,    # since the data was not augmented, we 
                      K = K_tot,
                      Y=yaug, 
                      cov.s1 = impervious,
                      cov.s2 = hetero,
                      cov.s3 = cropland,
                      cov.r1 = data_reg$EVI_av_std,    # cov.r1 = EVI (Enhanced Vegetation Index, as a measure of vegetation productivity)
                      cov.r2 = data_reg$mat_av_std,   # cov.r2 = Mean annual temperature
                      cov.r3 = data_reg$urb_reg_std,   # cov.r3 = Urban land cover proportion
                      cov.r4 = data_reg$yrs_col_std,   # cov.r4 = City Age
                      #cov.r5 = data_reg$pd_n_reg_std,    # cov.r5 = Fragmentation (Natural patch density)
                      cov.spp1 = elton$logmass.std,
                      cov.spp2 = elton$carn.std,
                      cov.det1 = effort,
                      Z = (yaug>0)*1, 
                      w = w))

# Initial values
# Based on results from models run with fewer samples. Parameters with estimates 
# skewing to one direction or another (positive/negative) were set to have initial 
# values closer to the mean/median of the preliminary estimates
inits <- function() { list(mu.omega=runif(1, 0, 1), 
                           slope.omega1=runif(1,-1, 0),
                           slope.omega2=runif(1, 0, 1), 
                           slope.omega3=runif(1,-1, 0), 
                           slope.omega4=runif(1,-1, 1), 
                           #slope.omega5=runif(1, 0, 0), 
                           mu.beta0=runif(1, qlogis(0.1), qlogis(0.3)), 
                           mu.alpha0=runif(1,qlogis(0.5),qlogis(0.7)), 
                           mu.tau.beta0=runif(1,1/(4),1/(0.5)), 
                           mu.tau.alpha0=runif(1,1/(3),1/(0.2)),
                           
                           mu.beta1.s1 = runif(1, -0.5, 0),
                           mu.phi1.s1 = runif(M, -0.25, 0),
                           
                           mu.beta2.s2 = runif(1, 0, 0.25),
                           mu.phi2.s2 = runif(M, 0, 0.25),
                           
                           mu.beta3.s3 = runif(1, -0.25, 0),
                           mu.phi3.s3 = runif(M, -0.25, 0),
                           
                           mu.beta4.r = runif(1, 0.25, 0.75),
                           mu.phi4.r = runif(M, 0.25, 0.75),
                           
                           mu.beta5.s1r = runif(1, -0.25, 0.25),
                           mu.phi5.s1r = runif(M, -0.25, 0.25),
                           
                           mu.phi6.r = runif(M, -0.25, 0),
                           mu.beta6.r = runif(1, -0.25, 0),
                           
                           mu.beta7.s1r = runif(1, -0.25, 0),
                           mu.phi7.s1r = runif(M, -0.25, 0),
                           
                           mu.beta8.r = runif(1, -0.5, -0.25),
                           mu.phi8.r = runif(M, -0.5, -0.25),
                           
                           mu.beta9.s1r = runif(1, -0.25, 0),
                           mu.phi9.s1r = runif(M, -0.25, 0),
                           
                           mu.beta10.r = runif(1, 0.25, 0.75),
                           mu.phi10.r = runif(M, 0.25, 0.75),
                           
                           mu.beta11.s1r = runif(1, -0.25, 0.25),
                           mu.phi11.s1r = runif(M, -0.25, 0.25),
                           
                           delta1.spp1 = runif(1, 0, 1),
                           delta2.spp1 = runif(1, -1, 0),
                           delta1.spp2 = runif(1, -1, 0),
                           delta2.spp2 = runif(1, 0, 0.5)
                           
                           
)
}


# MCMC settings
  nc <-3       # number of MCMC chains
  nt <- 3      # number to thin samples by

  # Exact settings used for the manuscript analysis
  #na <- 10000   # number of adaptations
  #nb <- 120000  # number of burn-ins
  #ni <- 180000  # number of iterations
  
  # Settings with samples reduced by a factor of 100, for illustrative/testing purposes
  # Approximate runtime (on a laptop with 12 GB of RAM): ~26 minutes
  na <- 100   # number of adaptations
  nb <- 1200  # number of burn-ins
  ni <- 1800  # number of iterations


# Parameters to monitor
parameters <- c("mu.omega","omega", "slope.omega1", "slope.omega2", "slope.omega3", "slope.omega4", 
                "mu.psi.mean","mu.sigma.psi",
                "mu.p.mean","mu.sigma.p",
                "mu.beta0",
                "mu.alpha0",
                "beta0",
                "alpha0",
                "N",
                "mu.beta1.s1",
                "mu.beta2.s2",
                "mu.beta3.s3",
                "mu.beta4.r",
                "mu.beta5.s1r",
                "mu.beta6.r",
                "mu.beta7.s1r",
                "mu.beta8.r",
                "mu.beta9.s1r",
                "mu.beta10.r",
                "mu.beta11.s1r",
                "delta1.spp1",
                "delta2.spp1",
                "delta1.spp2",
                "delta2.spp2",
                "mu.phi1.s1", 
                "mu.phi2.s2",
                "mu.phi3.s3",
                "mu.phi4.r", 
                "mu.phi5.s1r",
                "mu.phi6.r", 
                "mu.phi7.s1r",
                "mu.phi8.r", 
                "mu.phi9.s1r",
                "mu.phi10.r", 
                "mu.phi11.s1r",
                "phi0",
                "phi1.s1",
                "phi2.s2",
                "phi3.s3",
                "eta0",
                "w")

# Run the model
(start.time <- Sys.time())
out.global <- jags(bugs.data, 
                   inits = inits, 
                   parameters.to.save = parameters, 
                   model = "./R/1_1_jagsmodel_mrcm_site3_reg4_spp2_det0_regint4.R", 
                   n.adapt = na,
                   n.chains = nc, 
                   n.thin = nt, 
                   n.iter = ni, 
                   n.burnin = nb, 
                   parallel = TRUE)
(end.time <- Sys.time())
elapsed.time <- difftime(end.time, start.time, units='mins')
cat(paste(paste('Posterior computed in ', elapsed.time, sep=''), ' minutes/n', sep=''))



#### Export Model Outputs ####
  # first, export the object output from the 'jags()' function in the 'jagsUI' package
  # this model file was also too large to upload to the data repository
  saveRDS(out.global, "./data/modelsummary/model1output_mrcm_globalinteractionmodel_sample60k.rds")

  # export the summary of all modeled parameters
  # these data are located in the first sheet of Supplementary Data 1 (supplementarydata1_summarytables_speciesinfo.xlsx),
  # alongside other model summaries
  write.csv(out.global$summary, "data/modeloutput/model1summary_mrcm_globalinteractionmodel_sample60k.csv")
