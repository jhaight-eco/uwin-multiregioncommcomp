model{
  for(i in 1:nsite){
    # Prior for site-level random effect
    my_re[i] ~ dnorm(0, re_tau)
    log(mu_alpha[i]) <- inprod(  # log-normal model. Using inprod() allows for vector multiplication (more streamlined equation)
      beta,                      # intercept and covariate slopes
      design_matrix_alpha[i,]    # design matrix of the covariate data
    ) + my_re[i]                 # site-level random effect
    
    # site-level alpha diversity estimates are random parameters with 
    # standard deviation of alpha diversity estimates carried over directly from the occupancy model
    alpha_z[i] ~ dnorm(
      mu_alpha[i],
      pow(alpha_sd_known[i], -2)
    )
  }
  
  # dispersion of random effect parameter
  re_tau ~ dgamma(1,1)
  re_sd <- 1 / sqrt(re_tau)
  # Priors for intercept and covariate slopes
  for(h in 1:npar_alpha){
    beta[h] ~ dnorm(0, 0.01)
  }
}