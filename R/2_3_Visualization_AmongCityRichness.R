#### Urbanization, climate, and species traits shape mammal communities from local to continental scales ####

#### 2.3 Visualizing Among-City Relationships among Covariates and Regional Species Richness ####
#### Haight, Jeffrey D.


#### Setup ####
rm(list=ls()) # clear the environment
gc()
set.seed(4321)

# Set working directory
setwd("YourFilePathHere")

  # Load necessary packages
  library(dplyr)
  library(ggplot2)
  library(GGally)


#### Import Data ####
  # Import dataset with regional richness and covariate data
    data.reg <- read.csv("./data/modelsummary/data_cities_mrcmsummary.csv")
    
    # Add columns useful for sorting regions by environmental variables in plotting
    data.reg <- data.reg %>% arrange(EVI_av)
    data.reg$order_EVI <- as.factor(c(1:nrow(data.reg)))
    
    data.reg <- data.reg %>% arrange(mat_av)
    data.reg$order_MAT <- as.factor(c(1:nrow(data.reg)))
    
    data.reg <- data.reg %>% arrange(urb_reg)
    data.reg$order_URB <- as.factor(c(1:nrow(data.reg)))
    
    data.reg <- data.reg %>% arrange(yrs_col)
    data.reg$order_AGE <- as.factor(c(1:nrow(data.reg)))
    
    str(data.reg)
    
  
  # Import MRCM Model Outputs (which includes parameters for calculating regional species richness)
    # object output from the 'jags()' function in the 'jagsUI' package
    # the original model file itself was unfortunately too large to upload to the data repository
    # this file can be reproduced by running the code in the '1_1_Analysis_FittingMultiRegionCommunityModel.R' script
    out <- readRDS("./data/modeloutput/model1output_mrcm_globalinteractionmodel_sample60k.rds")
    out
    
    # Regional diversity estimates
    # Mean estimates of latent species occurrence (species-specific lower-case omega)
    pm_w <- apply(out$sims.list$w, c(2,3), function(x)   mean(x, na.rm=TRUE)) 
    # Regional species richness
    RR <- apply(out$sims.list$w, c(1,3), sum) 
    
    # Total # of species
    M <- 37
  
  
  
#### Extended Data/Supplementary Figure: Relationships Between Among-City Covariates ####
    var.reg <- data.reg %>%
      dplyr::select(
        EVI_av,
        PET_av,
        mat_av,
        map_av,
        temp_sm_av,
        ppt_sm_av,
        cmd_av,
        urb_reg,
        crop_reg,
        nat_reg,
        for_reg,
        pd_n_reg,
        yrs_col,
        y_cent,
        x_cent
      )
    
    plot <- ggpairs(var.reg, title= NULL,
                    columnLabels = c("EVI", 
                                     "PET",
                                     "MAT",
                                     "MAP",
                                     "MST",
                                     "MSP",
                                     "CMD",
                                     "URB",
                                     "AGR",
                                     "NAT",
                                     "FOR",
                                     "PD",
                                     "AGE",
                                     "LAT",
                                     "LON"),
                    #mapping = ggplot2::aes(color = "blue"),
                    upper = list(continuous = wrap("cor", method = "pearson"),
                                 aes(alpha = 0.5, color = "blue4"))#,
                    #diag = NULL
    ) + 
      theme_bw() + 
      theme(
        axis.text.x = element_text(face = "bold", size = 8, angle = -90), 
        axis.text.y = element_text(face = "bold", size = 8), 
        axis.title.x = element_text(face = "bold", size = 8), 
        axis.title.y = element_text(face = "bold", size = 8)
      ) +
      scale_color_manual("blue")
    
    plot
    
    
    
#### Observed (Naive) vs. Predicted Regional Diversity ####
    # All points should fall above the 1:1 line
    # because they've been corrected for imperfect detection at the regional level 
    # capital Omega representing the probability of regional species presence
    ggplot(data.reg, aes(x = rich_obs_reg, y = RR_mean)) +
      geom_point() +
      coord_cartesian(xlim=c(7, 14), ylim=c(7, 14)) +
      geom_abline(intercept = 0, slope = 1) +
      theme_bw()  +
      theme(
        #axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(face = "bold", size = 14), 
        axis.text.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 18), 
        axis.title.y = element_text(face = "bold", size = 18)
      )+
      labs(x = "Observed Regional Richness", y = "Predicted Regional Richness")
    
    
    
#### Extended Data/Supplementary Figure: Regional (City-level) Species Richness vs. Among-City Covariates ####
  # Start by predicting values of uppercase Omega (regional species richness relative to the 
  # total number of species M) across ranges of each among-city covariate (with the others 
  # held constant): vegetation greenness (EVI), mean annual temperature (MAT), regional 
  # urbanization (URB), and city age (AGE)  
    
    # mu.omega is the logit-scale parameter of omega
    
    # Set up predictions
    tmp <- out$sims.list   # grab MCMC samples
    nsamp <- length(tmp[[1]])          # number of mcmc samples
    npred <- 200
    
    # Array for storing predicted values
    # Dimension #3 will be the number of covariate effects that you want to predict
    pred.omega.EVI <- array(NA, dim = c(nsamp, npred)) 
    pred.omega.MAT <- array(NA, dim = c(nsamp, npred)) 
    pred.omega.URB <- array(NA, dim = c(nsamp, npred)) 
    pred.omega.AGE <- array(NA, dim = c(nsamp, npred)) 
    pred.EVI <- seq(min(data.reg$EVI_av_std), max(data.reg$EVI_av_std), length.out = npred)
    pred.MAT <- seq(min(data.reg$mat_av_std), max(data.reg$mat_av_std), length.out = npred)
    pred.URB <- seq(min(data.reg$urb_reg_std), max(data.reg$urb_reg_std), length.out = npred)
    pred.AGE <- seq(min(data.reg$yrs_col_std), max(data.reg$yrs_col_std), length.out = npred)
    
    
    # predicting across EVI, with others held at the mean
    Sys.time()
    for(i in 1:npred){ 
      for(k in 1:nsamp){
        # Omega for each region
        pred.omega.EVI[k,i] <- plogis(tmp$mu.omega[k] +
                                        tmp$slope.omega1[k]*pred.EVI[i] +
                                        tmp$slope.omega2[k]*0 +
                                        tmp$slope.omega3[k]*0 +
                                        tmp$slope.omega4[k]*0)
      }
    }
    Sys.time()
    
    # predicting across MAT
    Sys.time()
    for(i in 1:npred){ 
      for(k in 1:nsamp){
        # Omega for each region
        pred.omega.MAT[k,i] <- plogis(tmp$mu.omega[k] +
                                        tmp$slope.omega1[k]*0 +
                                        tmp$slope.omega2[k]*pred.MAT[i] +
                                        tmp$slope.omega3[k]*0 +
                                        tmp$slope.omega4[k]*0)
      }
    }
    Sys.time()
    
    
    # predicting across URB
    Sys.time()
    for(i in 1:npred){ 
      for(k in 1:nsamp){
        # Omega for each region
        pred.omega.URB[k,i] <- plogis(tmp$mu.omega[k] +
                                        tmp$slope.omega1[k]*0 +
                                        tmp$slope.omega2[k]*0 +
                                        tmp$slope.omega3[k]*pred.URB[i] +
                                        tmp$slope.omega4[k]*0)
      }
    }
    Sys.time()
    
    # predicting across AGE
    Sys.time()
    for(i in 1:npred){ 
      for(k in 1:nsamp){
        # Omega for each region
        pred.omega.AGE[k,i] <- plogis(tmp$mu.omega[k] +
                                        tmp$slope.omega1[k]*0 +
                                        tmp$slope.omega2[k]*0 +
                                        tmp$slope.omega3[k]*0 +
                                        tmp$slope.omega4[k]*pred.AGE[i])
      }
    }
    Sys.time()
    
  # Then, plot the same set of predicted regional species richness (omega*M) vs. a hypothetical
  # range of each among-city covariate. Add the mean and CRI of the city-specific predictions 
  # of regional species richness
    
##### RR vs EVI #####
    # Get posterior means and 95% CRIs
    pm_omega <- apply(pred.omega.EVI, c(2), function(x)   mean(x, na.rm=TRUE))   
    pmed_omega <- apply(pred.omega.EVI, c(2), function(x)   median(x, na.rm=TRUE))  
    psd_omega <- apply(pred.omega.EVI, c(2), function(x)   sd(x, na.rm=TRUE))  
    cri_omega <- apply(pred.omega.EVI, c(2), function(x)   quantile(x, prob = c(0.025, 0.975, 0.05, 0.95)))
    
    (data.omega <- data.frame(
      "EVI_std" = pred.EVI,
      "EVI" = seq(min(data.reg$EVI_av), max(data.reg$EVI_av), length.out = length(pred.EVI)),
      "mean" = pm_omega,
      "lower95" = cri_omega[3,],
      "upper95" = cri_omega[4,]
    ))
    
    
    
    # Note: by multiplying omega by the total number of species (M), you get regional species richness
    plot1 <- ggplot() +
      theme_bw() + 
      geom_ribbon(data = data.omega, aes(x = EVI, y = mean*M, ymin = lower95*M, ymax = upper95*M), 
                  fill = "green3", alpha = 0.2) +
      geom_smooth(data = data.omega, aes(x = EVI, y = mean*M, ymin = lower95*M, ymax = upper95*M),
                  se = FALSE, color = "green4")+
      geom_point(data = data.reg, aes(x = EVI_av, y = RR_mean), 
                 color = "green4")+
      coord_cartesian(xlim=c(min(data.reg$EVI_av), max(0.35)), ylim=c(7,17.5))+
      labs(x = "Regional Greenness (EVI)", y = "Regional Species \nRichness") +
      theme(
        axis.text.x = element_text(face = "bold", size = 14), 
        axis.text.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 18), 
        axis.title.y = element_text(face = "bold", size = 18)
      )
    
    plot1
    
    
##### RR vs. MAT #####
    # Get posterior means and 95% CRIs
    pm_omega <- apply(pred.omega.MAT, c(2), function(x)   mean(x, na.rm=TRUE))   
    pmed_omega <- apply(pred.omega.MAT, c(2), function(x)   median(x, na.rm=TRUE))  
    psd_omega <- apply(pred.omega.MAT, c(2), function(x)   sd(x, na.rm=TRUE))  
    cri_omega <- apply(pred.omega.MAT, c(2), function(x)   quantile(x, prob = c(0.025, 0.975, 0.05, 0.95)))
    
    (data.omega <- data.frame(
      "MAT_std" = pred.MAT,
      "MAT" = seq(min(data.reg$mat_av), max(data.reg$mat_av), length.out = length(pred.MAT)),
      "mean" = pm_omega,
      "lower95" = cri_omega[3,],
      "upper95" = cri_omega[4,]
    ))
    
    plot2 <- ggplot() +
      theme_bw() + 
      geom_ribbon(data = data.omega, aes(x = MAT, y = mean*M, ymin = lower95*M, ymax = upper95*M), 
                  fill = "red3", alpha = 0.2) +
      geom_smooth(data = data.omega, aes(x = MAT, y = mean*M, ymin = lower95*M, ymax = upper95*M),
                  se = FALSE, color = "red4")+
      geom_point(data = data.reg, aes(x = mat_av, y = RR_mean), 
                 color = "red4")+
      coord_cartesian(xlim=c(min(data.reg$mat_av), max(data.reg$mat_av)), ylim=c(7,17.5))+
      labs(x = "Regional Temperature (?C)", y = "Regional Species \nRichness") +
      theme(
        axis.text.x = element_text(face = "bold", size = 14), 
        axis.text.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 18), 
        axis.title.y = element_text(face = "bold", size = 18)
      )
    
    plot2
    
    
##### RR vs. URB #####
    # Get posterior means and 95% CRIs
    pm_omega <- apply(pred.omega.URB, c(2), function(x)   mean(x, na.rm=TRUE))   
    pmed_omega <- apply(pred.omega.URB, c(2), function(x)   median(x, na.rm=TRUE))  
    psd_omega <- apply(pred.omega.URB, c(2), function(x)   sd(x, na.rm=TRUE))  
    cri_omega <- apply(pred.omega.URB, c(2), function(x)   quantile(x, prob = c(0.025, 0.975, 0.05, 0.95)))
    
    (data.omega <- data.frame(
      "URB_std" = pred.URB,
      "URB" = seq(min(data.reg$urb_reg), max(data.reg$urb_reg), length.out = length(pred.URB)),
      "mean" = pm_omega,
      "lower95" = cri_omega[3,],
      "upper95" = cri_omega[4,]
    ))
    
    plot3 <- ggplot() +
      theme_bw() + 
      geom_ribbon(data = data.omega, aes(x = URB, y = mean*M, ymin = lower95*M, ymax = upper95*M), 
                  fill = "purple3", alpha = 0.2) +
      geom_smooth(data = data.omega, aes(x = URB, y = mean*M, ymin = lower95*M, ymax = upper95*M),
                  se = FALSE, color = "purple4")+
      geom_point(data = data.reg, aes(x = urb_reg, y = RR_mean), 
                 color = "purple4")+
      coord_cartesian(xlim=c(min(data.reg$urb_reg), max(data.reg$urb_reg)), ylim=c(7,17.5))+
      labs(x = "Regional Urbanization (% Urban Cover)", y = "Regional Species \nRichness") +
      theme(
        axis.text.x = element_text(face = "bold", size = 14), 
        axis.text.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 18), 
        axis.title.y = element_text(face = "bold", size = 18)
      )
    
    plot3
    
    
##### RR vs. AGE #####
    # Get posterior means and 95% CRIs
    pm_omega <- apply(pred.omega.AGE, c(2), function(x)   mean(x, na.rm=TRUE))   
    pmed_omega <- apply(pred.omega.AGE, c(2), function(x)   median(x, na.rm=TRUE))  
    psd_omega <- apply(pred.omega.AGE, c(2), function(x)   sd(x, na.rm=TRUE))  
    cri_omega <- apply(pred.omega.AGE, c(2), function(x)   quantile(x, prob = c(0.025, 0.975, 0.05, 0.95)))
    
    (data.omega <- data.frame(
      "AGE_std" = pred.AGE,
      "AGE" = seq(min(data.reg$yrs_col), max(data.reg$yrs_col), length.out = length(pred.AGE)),
      "mean" = pm_omega,
      "lower95" = cri_omega[3,],
      "upper95" = cri_omega[4,]
    ))
    
    plot4 <- ggplot() +
      theme_bw() + 
      geom_ribbon(data = data.omega, aes(x = AGE, y = mean*M, ymin = lower95*M, ymax = upper95*M), 
                  fill = "blue3", alpha = 0.2) +
      geom_smooth(data = data.omega, aes(x = AGE, y = mean*M, ymin = lower95*M, ymax = upper95*M),
                  se = FALSE, color = "blue4")+
      geom_point(data = data.reg, aes(x = yrs_col, y = RR_mean), 
                 color = "blue4")+
      coord_cartesian(xlim=c(min(data.reg$yrs_col), max(data.reg$yrs_col)), ylim=c(7,17.5))+
      labs(x = "Regional City Age (Years)", y = "Regional Species \nRichness") +
      theme(
        axis.text.x = element_text(face = "bold", size = 14), 
        axis.text.y = element_text(face = "bold", size = 14), 
        axis.title.x = element_text(face = "bold", size = 18), 
        axis.title.y = element_text(face = "bold", size = 18)
      )
    
    ggsave("G:/My Drive/ASU/Jeff-Jesse-Sharon Document Sharing/UWIN/manuscript/figures/supp2_rr_vs_cityage.png",
           plot4,
           width = 6,
           height = 4,
           units = "in",
           dpi = 300)
    
    plot4