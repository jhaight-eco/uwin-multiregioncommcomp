# MULTI-REGION COMMUNITY OCCUPANCY MODEL
# Based on the modeling frameworks of Sutherland et al. 2016, Tenan et al. 2017, Kery & Royle 2016, and Magle et al. 2021

# Parameter key:
# mu = averaged across regions (but not necessarily across species)
# tau = precision

# Intercepts
  # mu.phi0 = intercept for community-average occupancy in each region (mean across species)
  # phi0 = intercept for occupancy for each species in each region
  # mu.eta0 = intercept for community-average detection in each region (mean across species)
  # eta0 = intercept for detection for each species in each region
  # mu.omega = intercept for regional (relative) species richness

# Slopes
  # beta1+ = slope coefficient for covariate effects on community-average occupancy (global average across regions)
  # phi1+ = slope coefficient for covariate effects on occupancy for each species (average across regions = mu.phi1)
  # alpha1+ (not included) = slope coefficient for covariate effects on community-average detection  (global average across regions)
  # eta1+ (not included) = slope coefficient for covariate effects on detection for each species (average across regions: e.g. mu.eta1)
  # slope.omega1 = regional covariate effect on regional (relative) species richness
  # delta1 = slope coefficient for species-specific covariate effect on occupancy intercept
  # delta2 = slope coefficient for species-specific covariate effect on occupancy slope (interaction term)

  
model{
  # Priors
    # Global hyperpriors
      # 
      mu.omega ~ dnorm(0,0.01)
      mean.omega <- 1/(1+exp(-mu.omega))    # mean.omega = inverse-logit(mu.omega)
      slope.omega1 ~ dnorm(0,0.1)
      slope.omega2 ~ dnorm(0,0.1)
      slope.omega3 ~ dnorm(0,0.1)
      slope.omega4 ~ dnorm(0,0.1)  
      
      # Occupancy and detection intercept hyperpriors
      mu.beta0 ~ dnorm(0, 1 / (1.5*1.5)) 
      mu.tau.beta0 ~ dgamma(0.1, 0.1)    
      
      mu.alpha0 ~ dnorm(0, 1 / (1.5*1.5))       
      mu.tau.alpha0 ~ dgamma(0.1, 0.1)  
      
      mu.psi.mean <- 1 / (1 + exp(-1 * mu.beta0))   # mu.psi.mean = inverse-logit(mu.beta0)
    
      mu.p.mean <- 1 / (1 + exp(-1 * mu.alpha0))    # mu.p.mean = inverse-logit(mu.alpha0)
      
      mu.sigma.psi <- 1 / sqrt(mu.tau.beta0)
      mu.sigma.p <- 1 / sqrt(mu.tau.alpha0)
              
    
      # Occupancy and intercept slope hyperpriors
        # Global average covariate effects on occupancy ('beta'), averaged across all species and regions
        mu.beta1.s1 ~ dnorm(0, 0.1)
        tau.beta1.s1 ~ dgamma(0.1, 0.1)
        
        mu.beta2.s2 ~ dnorm(0, 0.1) 
        tau.beta2.s2 ~ dgamma(0.1, 0.1)
        
        mu.beta3.s3 ~ dnorm(0, 0.1)
        tau.beta3.s3 ~ dgamma(0.1, 0.1)
    
        # Regional effects on intercepts and slopes
        mu.beta4.r ~ dnorm(0, 0.1)
        tau.beta4.r ~ dgamma(0.1, 0.1)
        
        mu.beta5.s1r ~ dnorm(0, 0.1)
        tau.beta5.s1r ~ dgamma(0.1, 0.1)
        
        mu.beta6.r ~ dnorm(0, 0.1)
        tau.beta6.r ~ dgamma(0.1, 0.1)
        
        mu.beta7.s1r ~ dnorm(0, 0.1)
        tau.beta7.s1r ~ dgamma(1, 1)
        
        mu.beta8.r ~ dnorm(0, 0.1)
        tau.beta8.r ~ dgamma(0.1, 0.1)
        
        mu.beta9.s1r ~ dnorm(0, 0.1)
        tau.beta9.s1r ~ dgamma(0.1, 0.1)
        
        mu.beta10.r ~ dnorm(0, 0.1)
        tau.beta10.r ~ dgamma(0.1, 0.1)
        
        mu.beta11.s1r ~ dnorm(0, 0.1)
        tau.beta11.s1r ~ dgamma(0.1, 0.1)
    
      
        # Detection covariate effect hyperparameters and parameters
        #mu.alpha1 ~ dnorm(0, 0.1)
        #tau.alpha1 <- pow(sd.alpha1, -2)  
        #sd.alpha1 ~ dunif(0, 4)
    
    
      # Priors for species covariate effects  (added by Mason F., renamed by Jeff H.)
      delta1.spp1 ~ dnorm(0,0.1)  # Effect of spp covariate #1 on the random occupancy intercepts  
      delta2.spp1 ~ dnorm(0,0.1)  # Effect on the random slopes (Site covariate #1 x spp covariate #1 interaction)
      delta1.spp2 ~ dnorm(0,0.1)  # Effect of spp covariate #2 on the random occupancy intercepts   
      delta2.spp2 ~ dnorm(0,0.1)  # Effect on the random slopes (Site covariate #1 x spp covariate #2 interaction)
    
    # Priors indexed by species
    for (i in 1:M){
      # Intercepts 
          # Mean occupancy and detection intercepts for each region (modified to be random rather than fixed)
          # Dispersion parameters (mu.tau.phi0, mu.tau.eta0) can be left fixed at the global value (mu.tau.alpha0, mu.tau.beta0)
          mu.phi0[i] ~ dnorm(mu.beta0, mu.tau.beta0)
          mu.eta0[i] ~ dnorm(mu.alpha0, mu.tau.alpha0)
          mu.tau.phi0[i] <- mu.tau.beta0        
          mu.tau.eta0[i] <- mu.tau.alpha0
      
      # Slopes
          # Covariate effects on the occupancy of each species (phi1 through phi5), based on variation from the global-average ('slope.beta')
          # Species-specific random slopes coefficients site-level covariate on occupancy
          # Species-specific slope for the first site-level covariate varies from the global average based partly on the species trait(s)
          mu.phi1.s1[i] ~ dnorm(
            mu.beta1.s1 + delta2.spp1 * cov.spp1[i] + delta2.spp2 * cov.spp2[i],   #
            tau.beta1.s1
          )
          tau.phi1.s1[i] ~ dgamma(0.1, 0.1)      
          
          # Second site-level covariate species-specific random effects
          mu.phi2.s2[i] ~ dnorm(mu.beta2.s2, tau.beta2.s2)
          tau.phi2.s2[i] ~ dgamma(0.1, 0.1) 
          
          # Third site-level covariate species-specific random effects
          mu.phi3.s3[i] ~ dnorm(mu.beta3.s3, tau.beta3.s3)
          tau.phi3.s3[i] ~ dgamma(0.1, 0.1)
      
          # Detection covariate
          #mu.eta1[i] ~ dnorm(mu.alpha1, tau.alpha1)
          #tau.eta1[i] <- pow(sd.eta1[i], -2)  
          #sd.eta1[i] ~ dunif(0, 4) 
      
          # Species-specific random effects of regional covariates on occupancy
          mu.phi4.r[i] ~ dnorm(mu.beta4.r, tau.beta4.r)     #  average effect on each species across all regions
          mu.phi6.r[i] ~ dnorm(mu.beta6.r, tau.beta6.r)     #  average effect on each species across all regions
          mu.phi8.r[i] ~ dnorm(mu.beta8.r, tau.beta8.r)     #  average effect on each species across all regions
          mu.phi10.r[i] ~ dnorm(mu.beta10.r, tau.beta10.r)     #  average effect on each species across all regions
      
          # Species-specific site x region interaction effects
          #  = effects of the regional covariate on the effect of development on each species, averaged across all regions
          mu.phi5.s1r[i] ~ dnorm(mu.beta5.s1r, tau.beta5.s1r)    #
          mu.phi7.s1r[i] ~ dnorm(mu.beta7.s1r, tau.beta7.s1r)    #
          mu.phi9.s1r[i] ~ dnorm(mu.beta9.s1r, tau.beta9.s1r)    #
          mu.phi11.s1r[i] ~ dnorm(mu.beta11.s1r, tau.beta11.s1r)    #
      
     # Priors indexed by species and region 
      # Covariate effects on the occupancy and detection of each species in each region     
      for(r in 1:n.region){
        phi1.s1[i,r] ~ dnorm(mu.phi1.s1[i], tau.phi1.s1[i])      # Covariate 1 (site-level)
        phi2.s2[i,r] ~ dnorm(mu.phi2.s2[i], tau.phi2.s2[i])      # Covariate 2 (site-level)
        phi3.s3[i,r] ~ dnorm(mu.phi3.s3[i], tau.phi3.s3[i])      # Covariate 3 (site-level)
        #eta1[i,r] ~ dnorm(mu.eta1[i], tau.eta1[i])              # Detection covariate
      }
      
    }
  
    
  # Likelihood
  for (i in 1:M) {
    for (r in 1:n.region) {
      
      # State process
      w[i,r] ~ dbern(omega[r])
      
      # Occupancy: How common a species is in a region is set to vary based partly on their species trait(s)
      phi0[i,r] ~ dnorm(
        mu.phi0[i] + delta1.spp1 * cov.spp1[i] + delta1.spp2 * cov.spp2[i] ,     #
        mu.tau.phi0[i]
      )
      
      # Detection
      eta0[i,r] ~ dnorm(mu.eta0[i], mu.tau.eta0[i])
      
      # Linear predictor for occupancy
      for (j in 1:n.site[r]) {
        logit(psi[i,j,r]) <- phi0[i,r] + 
          phi1.s1[i,r]*cov.s1[j,r] + 
          phi2.s2[i,r]*cov.s2[j,r] +
          phi3.s3[i,r]*cov.s3[j,r] +
          mu.phi4.r[i]*cov.r1[r] + 
          mu.phi5.s1r[i]*cov.s1[j,r]*cov.r1[r]+
          mu.phi6.r[i]*cov.r2[r] + 
          mu.phi7.s1r[i]*cov.s1[j,r]*cov.r2[r]+
          mu.phi8.r[i]*cov.r3[r] + 
          mu.phi9.s1r[i]*cov.s1[j,r]*cov.r3[r]+
          mu.phi10.r[i]*cov.r4[r] + 
          mu.phi11.s1r[i]*cov.s1[j,r]*cov.r4[r]
        mu.psi[i,j,r] <- psi[i,j,r]*w[i,r]
        Z[i,j,r] ~ dbern(mu.psi[i,j,r])
        
      
        # Observation process
        logit(p[i,j,r]) <- eta0[i,r] #+ eta1[i,r]*cov.det1[j,r]
        mu.p[i,j,r] <- p[i,j,r]*Z[i,j,r]
        Y[i,j,r] ~ dbin(mu.p[i,j,r], K[j,r])
        
        
        # Alpha species diversity (local species richness = Hill Number 0)
        #hill0[j,r] <- sum(Z[,j,r]) # Number of species occurring at each site
        
      }
    }
  }
  
  
  
  # Regional species presence
  for (r in 1:n.region) {
    # the linear predictor
    logit(omega[r]) <- mu.omega  + slope.omega1*cov.r1[r] + slope.omega2*cov.r2[r] + slope.omega3*cov.r3[r] + slope.omega4*cov.r4[r] # + slope.omega5*cov.r5[r] 
    
    # regional species richness
    N[r] <- sum(w[,r])
  }
  
  

  # Code for likelihood, used to calculate CPO
  for(r in 1:n.region){
    for(j in 1:n.site[r]){
      for(i in 1:M){ 
        # Trick to calculate the binomial coefficient.
        BinCo[i,j,r] <- exp(logfact(K[j,r]) - (logfact(Y[i,j,r]) + 
                                                 logfact(K[j,r] - Y[i,j,r])))
        # the likelihood of observing what we did, given the model and the probabilities it estimated
        lik[i,j,r] <- ifelse(equals(Y[i,j,r],0), 
                             omega[r]*psi[i,j,r]*((1-p[i,j,r])^K[j,r]) + omega[r]*(1-psi[i,j,r]) + (1-omega[r]),   # likelihood of each non-observation
                             BinCo[i,j,r]*omega[r]*psi[i,j,r]*(p[i,j,r]^Y[i,j,r]) * 
                               (1-p[i,j,r])^(K[j,r]-Y[i,j,r]))                    # likelihood of each observation
      }
    }
  }
  
}