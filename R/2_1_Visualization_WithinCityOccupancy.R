#### Urbanization, climate, and species traits shape mammal communities from local to continental scales ####

#### 2.1 Visualizing Within-City Relationships between Occupancy and Covariates ####
#### Haight, Jeffrey D.


#### Setup ####
  rm(list=ls()) # clear the environment
  gc()
  set.seed(123)  # needs to be set in order for the points and error bars to 'jitter' the same
  
  # Set working directory
  #setwd("YourFilePathHere")
  # e.g.,
  setwd("C:/Users/User/Documents/GitHub/uwin-multiregioncommcomp")
  
  # Load necessary packages
  library(dplyr)
  library(reshape2)
  library(ggplot2)
  library(peRReo)   # https://github.com/jbgb13/peRReo
  library(png)
  library(scales)

  # Define color palette for figures
  calle <-  latin_palette("calle13", 9)


##### Import Data #####
  # The main datasets containing all the site and regional covariates
  data.site <- read.csv("./data/modelsummary/data_sites_mrcmsummary.csv")
  data.reg <- read.csv("./data/modelsummary/data_cities_mrcmsummary.csv")
  data.spp <- read.csv("./data/modelinput/ModelInputData_UWIN_allspecies.csv")
  
  # object output from the 'jags()' function in the 'jagsUI' package
  # the original model file itself was unfortunately too large to upload to the data repository
  # this file can be reproduced by running the code in the '1_1_Analysis_FittingMultiRegionCommunityModel.R' script
  out <- readRDS("C:/Research/urban/UWIN/analysis_MRCM/data/6_output/model1output_mrcm_globalinteractionmodel_sample60k.rds")
  # out$summary
  
  # for plotting purposes, some parameters may need to be recalculated, depending on which were monitored in the original model
  # e.g. the city-average occupancy intercept, from which the city- and species-specific intercept phi0 was estimated 
  mu.phi0 <- apply(out$sims.list$phi0, c(1,2), function(x)   mean(x, na.rm=TRUE)) 
  
  # objects for prediction
  tmp <- out$sims.list  # grab mcmc samples
  npred <- 200          # number of values to predict
  nsamp <- length(tmp[[1]])

  

# Figure 2: Urbanization vs. Occupancy for Contrasting Cities ----
      cities <- data.reg$city
      
      # community-average slopes
      mcmc.mu.beta <- cbind(tmp$mu.beta0, 
                            tmp$mu.beta1.s1, 
                            tmp$mu.beta2.s2,
                            tmp$mu.beta3.s3,
                            tmp$mu.beta4.r,
                            tmp$mu.beta5.s1r,
                            tmp$mu.beta6.r,
                            tmp$mu.beta7.s1r,
                            tmp$mu.beta8.r,
                            tmp$mu.beta9.s1r,
                            tmp$mu.beta10.r,
                            tmp$mu.beta11.s1r
      )
     
      
      r <- 2
      
      data.reg <- data.reg %>% arrange(city)
      
      # Objects for storing values from the loops below
          # Values of local, within-city covariates to predict off of. These were standardized by city
          # One option is to show relationships within specific cities, using their real values
          # using range of hypothetical values equivalent to their actual gradient of urbanization, patch density, and agriculture
          #urb.pred <- array(NA, dim = c(npred, nrow(data.reg)))
          #pd.pred <- array(NA, dim = c(npred, nrow(data.reg)))
          #ag.pred <- array(NA, dim = c(npred, nrow(data.reg)))
          #impervious <- array(NA, dim = c(npred, nrow(data.reg)))
          #for(r in 1:nrow(data.reg)){
          #  data.city <- data.site %>% filter(city == data.reg$city[r])
          #  urb.pred[,r] <- seq(min(data.city$imperv_std),
          #                      max(data.city$imperv_std), length.out = npred)
          #  pd.pred[,r] <- seq(min(data.city$pd_undev_std),
          #                     max(data.city$pd_undev_std), length.out = npred)
          #  
          #  ag.pred[,r] <- seq(min(data.city$cropland_std),
          #                     max(data.city$cropland_std), length.out = npred)
            # non-standardized values of urbanization
          #  impervious[,r] <- seq(min(data.city$Impervious),
          #                        max(data.city$Impervious), length.out = npred) 
          #}
          
          # However, to more effectively highlight the statistical effects modeld in the multi-city community occupancy model
          # we will predict effects across a range of hypothetical local urbanization values, under contrasting ranges of regional variables
          # for each regional variable, we will hold all other regional and local variables constant at their mean (zero)
          #nline <- nrow(data.reg)    
          nline <- 5
          city.num <- paste("city", 1:nline, sep = "")
          
          urban.pred <- seq(min(data.site$imperv_std), max(data.site$imperv_std), length.out = npred)
          pd.pred <- seq(min(data.site$pd_undev_std), max(data.site$pd_undev_std), length.out = npred)
          ag.pred <- seq(min(data.site$cropland_std), max(data.site$cropland_std), length.out = npred)
          pred.EVI <- seq(min(data.reg$EVI_av_std), max(data.reg$EVI_av_std), length.out = nline)
          pred.MAT <- seq(min(data.reg$mat_av_std), max(data.reg$mat_av_std), length.out = nline)
          pred.URB <- seq(min(data.reg$urb_reg_std), max(data.reg$urb_reg_std), length.out = nline)
          pred.AGE <- seq(min(data.reg$yrs_col_std), max(data.reg$yrs_col_std), length.out = nline)
          
          # objects for storing prediction summary stats
          preds.psicomm.mean <- rep(NA, npred*nline)
          preds.psicomm.med <- rep(NA, npred*nline)
          preds.psicomm.2.5 <- rep(NA, npred*nline)
          preds.psicomm.97.5 <- rep(NA, npred*nline)
     
      
      
    
      # predict across EVI gradient
      for(r in 1:nline){  # loop for each city
        # a design matrix that is remade for each city, using their specific regional covariate values
        dm.urb <- cbind(
          1,
          urban.pred,  # local urbanization
          0,   # patch density
          0,   # agriculture
          pred.EVI[r],  # EVI
          pred.EVI[r]*urban.pred,  # EVI*local urbanization
          0, #pred.MAT[r],  # MAT
          0, #pred.MAT[r]*urb.pred,  # MAT*local urbanization
          0, #pred.URB[r],  # URB
          0, #pred.URB[r]*urb.pred,  # URB*local urbanization
          0, #pred.AGE[r],  # AGE
          0  #pred.AGE[r]*urb.pred   # AGE*local urbanization
        )
        
        preds.psicomm.city <- plogis(mcmc.mu.beta %*% t(dm.urb))  # predict
        
        preds.psicomm.cri <- apply(preds.psicomm.city, 2, quantile, probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
        
        preds.psicomm.mean[(r*npred-npred+1):(r*npred)] <- apply(preds.psicomm.city, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
        preds.psicomm.2.5[(r*npred-npred+1):(r*npred)] <- preds.psicomm.cri[,1]
        preds.psicomm.med[(r*npred-npred+1):(r*npred)] <- preds.psicomm.cri[,2]
        preds.psicomm.97.5[(r*npred-npred+1):(r*npred)] <- preds.psicomm.cri[,3]
        
        
        #print(data.reg$city[r])
        print(city.num[r])
      }

      preds.urbEVI <- data.frame(
        "impervious" = rep(seq(min(data.site$Impervious), max(data.site$Impervious), length.out = npred), nline),
        "impervious_std" = rep(urban.pred, nline),
        "city" = rep(c(city.num), each = npred),
        #"EVI" = rep(data.reg$EVI_av, each = npred),
        "EVI" = rep((pred.EVI*sd(data.reg$EVI_av)+mean(data.reg$EVI_av)), each = npred),
        "psi_med" = preds.psicomm.med,
        "psi_low95" = preds.psicomm.2.5,
        "psi_upp95" = preds.psicomm.97.5
      )    
      
      # repeat predictions for other regional variables
      # MAT
      for(r in 1:nline){  # loop for each city
        # a design matrix that is remade for each city, using their specific regional covariate values
        dm.urb <- cbind(
          1,
          urban.pred,  # local urbanization
          0,   # patch density
          0,   # agriculture
          0, #pred.EVI[r],  # EVI
          0, #pred.EVI[r]*urban.pred,  # EVI*local urbanization
          pred.MAT[r],  # MAT
          pred.MAT[r]*urban.pred,  # MAT*local urbanization
          0, #pred.URB[r],  # URB
          0, #pred.URB[r]*urban.pred,  # URB*local urbanization
          0, #pred.AGE[r],  # AGE
          0  #pred.AGE[r]*urban.pred   # AGE*local urbanization
        )
        
        preds.psicomm.city <- plogis(mcmc.mu.beta %*% t(dm.urb))  # predict
        
        preds.psicomm.cri <- apply(preds.psicomm.city, 2, quantile, probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
        
        preds.psicomm.mean[(r*npred-npred+1):(r*npred)] <- apply(preds.psicomm.city, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
        preds.psicomm.2.5[(r*npred-npred+1):(r*npred)] <- preds.psicomm.cri[,1]
        preds.psicomm.med[(r*npred-npred+1):(r*npred)] <- preds.psicomm.cri[,2]
        preds.psicomm.97.5[(r*npred-npred+1):(r*npred)] <- preds.psicomm.cri[,3]
        
        
        #print(data.reg$city[r])
        print(city.num[r])
      }
      
      preds.urbMAT <- data.frame(
        "impervious" = rep(seq(min(data.site$Impervious), max(data.site$Impervious), length.out = npred), nline),
        "impervious_std" = rep(urban.pred, nline),
        "city" = rep(c(city.num), each = npred),
        #"MAT" = rep(data.reg$mat_av, each = npred),
        "MAT" = rep((pred.MAT*sd(data.reg$mat_av)+mean(data.reg$mat_av)), each = npred),
        "psi_med" = preds.psicomm.med,
        "psi_low95" = preds.psicomm.2.5,
        "psi_upp95" = preds.psicomm.97.5
      )    
      
      # URB
      for(r in 1:nline){  # loop for each city
        # a design matrix that is remade for each city, using their specific regional covariate values
        dm.urb <- cbind(
          1,
          urban.pred,  # local urbanization
          0,   # patch density
          0,   # agriculture
          0, #pred.EVI[r],  # EVI
          0, #pred.EVI[r]*urban.pred,  # EVI*local urbanization
          0, #pred.MAT[r],  # MAT
          0, #pred.MAT[r]*urban.pred,  # MAT*local urbanization
          pred.URB[r],  # URB
          pred.URB[r]*urban.pred,  # URB*local urbanization
          0, #pred.AGE[r],  # AGE
          0  #pred.AGE[r]*urban.pred   # AGE*local urbanization
        )
        
        preds.psicomm.city <- plogis(mcmc.mu.beta %*% t(dm.urb))  # predict
        
        preds.psicomm.cri <- apply(preds.psicomm.city, 2, quantile, probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
        
        preds.psicomm.mean[(r*npred-npred+1):(r*npred)] <- apply(preds.psicomm.city, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
        preds.psicomm.2.5[(r*npred-npred+1):(r*npred)] <- preds.psicomm.cri[,1]
        preds.psicomm.med[(r*npred-npred+1):(r*npred)] <- preds.psicomm.cri[,2]
        preds.psicomm.97.5[(r*npred-npred+1):(r*npred)] <- preds.psicomm.cri[,3]
        
        
        #print(data.reg$city[r])
        print(city.num[r])
      }
      
      preds.urbURB <- data.frame(
        "impervious" = rep(seq(min(data.site$Impervious), max(data.site$Impervious), length.out = npred), nline),
        "impervious_std" = rep(urban.pred, nline),
        "city" = rep(c(city.num), each = npred),
        #"URB" = rep(data.reg$urb_reg, each = npred),
        "URB" = rep((pred.URB*sd(data.reg$urb_reg)+mean(data.reg$urb_reg)), each = npred),
        "psi_med" = preds.psicomm.med,
        "psi_low95" = preds.psicomm.2.5,
        "psi_upp95" = preds.psicomm.97.5
      )    
      
      # AGE
      for(r in 1:nline){  # loop for each city
        # a design matrix that is remade for each city, using their specific regional covariate values
        dm.urb <- cbind(
          1,
          urban.pred,  # local urbanization
          0,   # patch density
          0,   # agriculture
          0, #pred.EVI[r],  # EVI
          0, #pred.EVI[r]*urban.pred,  # EVI*local urbanization
          0, #pred.MAT[r],  # MAT
          0, #pred.MAT[r]*urban.pred,  # MAT*local urbanization
          0, #pred.URB[r],  # URB
          0, #pred.URB[r]*urban.pred,  # URB*local urbanization
          pred.AGE[r],  # AGE
          pred.AGE[r]*urban.pred   # AGE*local urbanization
        )
        
        preds.psicomm.city <- plogis(mcmc.mu.beta %*% t(dm.urb))  # predict
        
        preds.psicomm.cri <- apply(preds.psicomm.city, 2, quantile, probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
        
        preds.psicomm.mean[(r*npred-npred+1):(r*npred)] <- apply(preds.psicomm.city, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
        preds.psicomm.2.5[(r*npred-npred+1):(r*npred)] <- preds.psicomm.cri[,1]
        preds.psicomm.med[(r*npred-npred+1):(r*npred)] <- preds.psicomm.cri[,2]
        preds.psicomm.97.5[(r*npred-npred+1):(r*npred)] <- preds.psicomm.cri[,3]
        
        
        #print(data.reg$city[r])
        print(city.num[r])
      }
      
      preds.urbAGE <- data.frame(
        "impervious" = rep(seq(min(data.site$Impervious), max(data.site$Impervious), length.out = npred), nline),
        "impervious_std" = rep(urban.pred, nline),
        "city" = rep(c(city.num), each = npred),
        #"AGE" = rep(data.reg$yrs_col, each = npred),
        "AGE" = rep((pred.AGE*sd(data.reg$yrs_col)+mean(data.reg$yrs_col)), each = npred),
        "psi_med" = preds.psicomm.med,
        "psi_low95" = preds.psicomm.2.5,
        "psi_upp95" = preds.psicomm.97.5
      )    
      
          
          
  

    # Plot Relationships Across Regional EVI Gradient ====
      data.plot <- preds.urbEVI
      plot.evi <- ggplot(data = data.plot, aes(x = impervious, y = psi_med, group = city)) +
        theme_bw() + 
        geom_smooth(aes(x = impervious, y = psi_med, group = city), color = "gray20", se = FALSE, lwd = 1.2)+
        geom_smooth(aes(x = impervious, y = psi_med, group = city, color = EVI), se = FALSE)+
        geom_ribbon(aes(x = impervious, y = psi_med, group = city, ymin = psi_low95, ymax = psi_upp95, fill = EVI), 
                    alpha = 0.15) + 
        #gghighlight(city %in% c("safl", "phaz", "ioio"))+
        scale_fill_distiller(palette = "BrBG", direction = 1)+
        scale_color_distiller(palette = "BrBG", direction = 1)+
        scale_y_continuous(labels = label_number(accuracy = 0.01)) +
        coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(0,0.7))+
        labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Occupancy") +
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              axis.title.x = element_text(face = "bold", size = 18), 
              axis.title.y = element_text(face = "bold", size = 18),
              legend.position = "none",
              legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt") ) 
      plot.evi
      range(data.plot$EVI)
      
      ggsave("./figures/figure2a_urbanization_vs_occupancy_EVIcontrast.png",
             plot.evi,
             width = 4,
             height = 4,
             units = "in",
             dpi = 300)
    
      # Plot Relationships Across Regional MAT Gradient ====
      data.plot <- preds.urbMAT
      plot.mat <- ggplot(data = data.plot) +
        theme_bw() + 
        geom_smooth(aes(x = impervious, y = psi_med, group = city), color = "gray20", se = FALSE, lwd = 1.2)+
        geom_smooth(aes(x = impervious, y = psi_med, group = city, color = MAT), se = FALSE)+
        geom_ribbon(aes(x = impervious, y = psi_med, group = city, ymin = psi_low95, ymax = psi_upp95, fill = MAT), 
                    alpha = 0.15) + 
        #gghighlight(city %in% c("scut", "mela"))+
        theme_bw() + 
        scale_fill_distiller(palette = "RdYlBu", direction = -1)+
        scale_color_distiller(palette = "RdYlBu", direction = -1)+
        scale_y_continuous(labels = label_number(accuracy = 0.01)) +
        coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(0,0.7))+
        labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Occupancy")  +
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              axis.title.x = element_text(face = "bold", size = 18), 
              axis.title.y = element_text(face = "bold", size = 18),
              legend.position = "none",
              legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
        ) 
      plot.mat
      
      ggsave("./figures/figure2b_urbanization_vs_occupancy_MATcontrast.png",
             plot.mat,
             width = 4,
             height = 4,
             units = "in",
             dpi = 300)
      
      
      
      # Plot Relationships Across Regional URB Gradient ====
      data.plot <- preds.urbURB
      plot.urb <- ggplot(data = data.plot) +
        theme_bw() + 
        geom_smooth(aes(x = impervious, y = psi_med, group = city), color = "gray20", se = FALSE, lwd = 1.2)+
        geom_smooth(aes(x = impervious, y = psi_med, group = city, color = URB), se = FALSE)+
        geom_ribbon(aes(x = impervious, y = psi_med, group = city, ymin = psi_low95, ymax = psi_upp95, fill = URB), 
                    alpha = 0.15) + 
        #gghighlight(city %in% c("scut", "chil"))+
        theme_bw() + 
        scale_fill_distiller(palette = "PuOr", direction = 1)+
        scale_color_distiller(palette = "PuOr", direction = 1)+
        scale_y_continuous(labels = label_number(accuracy = 0.01)) +
        coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(0,0.7))+
        labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Occupancy")  +
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              axis.title.x = element_text(face = "bold", size = 18), 
              axis.title.y = element_text(face = "bold", size = 18),
              legend.position = "none",
              legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt") 
              )
      plot.urb
      
      ggsave("./figures/figure2c_urbanization_vs_occupancy_URBcontrast.png",
             plot.urb,
             width = 4,
             height = 4,
             units = "in",
             dpi = 300)
      
      
      # Plot Relationships Across Regional AGE Gradient ====
      data.plot <- preds.urbAGE
      plot.age <- ggplot(data = data.plot) +
        theme_bw() + 
        geom_smooth(aes(x = impervious, y = psi_med, group = city), color = "gray20", se = FALSE, lwd = 1.2)+
        geom_smooth(aes(x = impervious, y = psi_med, group = city, color = AGE), se = FALSE)+
        geom_ribbon(aes(x = impervious, y = psi_med, group = city, ymin = psi_low95, ymax = psi_upp95, fill = AGE), 
                    alpha = 0.15) + 
        #gghighlight(city %in% c("inin", "wide"))+ 
        scale_fill_distiller(palette = "PRGn", direction = -1)+
        scale_color_distiller(palette = "PRGn", direction = -1)+
        scale_y_continuous(labels = label_number(accuracy = 0.01)) +
        coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(0,0.7))+
        labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Occupancy")  +
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              axis.title.x = element_text(face = "bold", size = 18), 
              axis.title.y = element_text(face = "bold", size = 18),
              legend.position = "none",
              legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt") 
        )
      plot.age
      
      ggsave("./figures/figure2d_urbanization_vs_occupancy_AGEcontrast.png",
             plot.age,
             width = 4,
             height = 4,
             units = "in",
             dpi = 300)

      
# Figure 3 and Extended Data Figure: Species Traits vs. Occupancy and Urbanization-Occupancy Relationship ----
      ###### Predict community-level occupancy vs. traits ====
      #Design matrices of covariate values to predict off of
      # sequences of hypothetical species trait values
      seq.bm <- seq(min(data.spp$logmass.std), max(data.spp$logmass.std), length.out = npred) 
      seq.ca <- seq(min(data.spp$carn.std), max(data.spp$carn.std), length.out = npred)
      # actual range of species traits
      plot.bm <- seq(min(data.spp$logmass), max(data.spp$logmass), length.out = npred)
      plot.ca <- seq(min(data.spp$carn), max(data.spp$carn), length.out = npred)
      range(plot.bm)
      range(plot.ca)
      
      # create a design matrix based on the equation: 1 = intercept
      dm.bm <- cbind(1, seq.bm, median(seq.ca))      
      dm.ca <- cbind(1, median(seq.bm), seq.ca) 
      
      # Grab the appropriate slope parameters
      mcmc.traits <- cbind(tmp$mu.beta0, tmp$delta1.spp1, tmp$delta1.spp2)
      
      # Some quick matrix math, multiplying the design matrix by the mcmc slope parameters
      pred.bm <- mcmc.traits %*% t(dm.bm)
      pred.ca <- mcmc.traits %*% t(dm.ca)
      
      # Summarize the predictions
      pred.bm <- apply(pred.bm, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>% plogis()
      pred.ca <- apply(pred.ca, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>% plogis()
      
      # data for plotting the community-level predictions
      str(data.pred.occ <- data.frame(
        "logmass_std" = seq.bm,
        "logmass" = plot.bm,
        "mass_kg" = 10^plot.bm/1000, # the range of hypothetical masses in kilograms
        "carnivory_std" = seq.ca,
        "carnivory" = plot.ca*100,
        t(pred.bm),
        t(pred.ca)
      ))
      colnames(data.pred.occ)[6:11] <- c("bm_lower95", "bm_med", "bm_upper95",
                                         "ca_lower95", "ca_med", "ca_upper95")
      
      # Data for plotting the values of the species on top of the community-level predictions
      cri <- apply(mu.phi0, 2, quantile, probs = c(0.5, 0.025, 0.975)) %>% plogis()
      
      data_muphi0 <- data.frame(
        "species" = data.spp$Species,
        "species_code" = data.spp$species_code,
        "common_name" = data.spp$name_common,
        "mass_kg" = data.spp$mass/1000,
        "logmass" = data.spp$logmass,
        "logmass_std" = data.spp$logmass.std,
        "carnivory" = data.spp$carn*100,
        "carnivory_std" = data.spp$carn.std,
        "muphi0" = c(t(cri[1,])),
        "muphi0_lower95" = c(t(cri[2,])),
        "muphi0_upper95" = c(t(cri[3,]))
      )
      
      
      ###### Plot Occupancy vs. Species Traits ====
      # drop the species that weren't very well detected from the plot
      data.plot.spp <- data_muphi0 %>% 
        filter(!species %in% c("black_bear", "cougar", "flying_squirrel_sp", "hooded_skunk", "mountain beaver","north_american_beaver", "richardson_ground_squirrel", "weasel_sp"))
      
      
      # Body Mass
      plot.mass1 <- ggplot() +
        theme_classic()+ 
        geom_ribbon(data = data.pred.occ, aes(x = logmass, y = bm_med, ymin = bm_lower95, ymax = bm_upper95), 
                    fill = "grey30", alpha = 0.2)+
        geom_smooth(data = data.pred.occ, aes(x = logmass, y = bm_med), 
                    se = FALSE, color = "grey30") +
        geom_pointrange(data = data.plot.spp, aes(x = logmass, y = muphi0, ymin = muphi0_lower95, ymax = muphi0_upper95),
                        color = "grey30") +
        labs(x = "Body Mass (kg)", y = "Species Occupancy") +
        scale_x_continuous(trans = "log10", breaks = log10(c(0.1, 1, 10, 100, 500)*1000), labels = c("0.1","1", "10", "100","500")) +
        #coord.cartesian(xlim=c(1,6), ylim=c(0, 0.6))+
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              axis.title.x = element_text(face = "bold", size = 18), 
              axis.title.y = element_text(face = "bold", size = 18),
        ) 
      plot.mass1
      
      
      ggsave("./figures/extendeddata_figure2a_speciesoccupancy_vs_mass.png",
             plot.mass1,
             width = 6,
             height = 4,
             units = "in",
             dpi = 300)
      
      
      # Carnivory
      plot.carn1 <- ggplot() +
        theme_classic()+ 
        geom_ribbon(data = data.pred.occ, aes(x = carnivory, y = ca_med, ymin = ca_lower95, ymax = ca_upper95), 
                    fill = "grey30", alpha = 0.2)+
        geom_smooth(data = data.pred.occ, aes(x = carnivory, y = ca_med), 
                    se = FALSE, color = "grey30") +
        geom_pointrange(data = data.plot.spp, aes(x = carnivory, y = muphi0, ymin = muphi0_lower95, ymax = muphi0_upper95, group = species),
                        color = "grey30", position = position_dodge(width = 2)) +
        labs(x = "Carnivory (% Vertebrate Diet)", y = "Species Occupancy") +
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              legend.position = "none",
              axis.title.x = element_text(face = "bold", size = 18), 
              axis.title.y = element_text(face = "bold", size = 18),
        )
      plot.carn1
      
      
      ggsave("./figures/extendeddata_figure2b_speciesoccupancy_vs_carnivory.png",
             plot.carn1,
             width = 6,
             height = 4,
             units = "in",
             dpi = 300)
      
      
      
      
      ###### Predict community-level occupancy-urbanization relationship vs traits ######
      
      # sequences of hypothetical trait values to predict off of
      seq.bm <- seq(min(data.spp$logmass.std), max(data.spp$logmass.std), length.out = npred) 
      seq.ca <- seq(min(data.spp$carn.std), max(data.spp$carn.std), length.out = npred)
      
      plot.bm <- seq(min(data.spp$logmass), max(data.spp$logmass), length.out = npred)
      plot.ca <- seq(min(data.spp$carn), max(data.spp$carn), length.out = npred)
      range(plot.bm)
      range(plot.ca)
      
      # Create a design matrix based on the equation: 1 = intercept, my.seq = trail values to predict off of
      dm.bm <- cbind(1, seq.bm, 0)  
      dm.ca <- cbind(1, 0, seq.ca) 
      
      # Grab the appropriate slope parameters
      mcmc.traits <- cbind(tmp$mu.beta1.s1, tmp$delta2.spp1, tmp$delta2.spp2)
      
      # Some quick matrix math, multiplying the design matrix by the mcmc slope parameters
      pred.bm <- mcmc.traits %*% t(dm.bm)   
      pred.ca <- mcmc.traits %*% t(dm.ca)
      
      # Summarize the predictions. We really only need the median and CRIs
      pred.bm <- apply(pred.bm, 2, quantile, probs = c(0.025, 0.5, 0.975)) 
      pred.ca <- apply(pred.ca, 2, quantile, probs = c(0.025, 0.5, 0.975))
      
      # organize for plotting using ggplot
      (data.pred.urbocc <- data.frame(
        "logmass_std" = seq.bm,
        "logmass" = plot.bm,
        "carnivory_std" = seq.ca,
        "carnivory" = plot.ca,
        t(pred.bm),
        t(pred.ca)
      ))
      colnames(data.pred.urbocc)[5:10] <- c("bm_lower95", "bm_med", "bm_upper95",
                                            "ca_lower95", "ca_med", "ca_upper95")
      
      
      ###### Extract distributions of each species' response to within-city covariates ######
      # community-level effects
      pred.beta1 <- tmp$mu.beta1.s1
      pred.beta2 <- tmp$mu.beta2.s2
      pred.beta3 <- tmp$mu.beta3.s3
      
      # species-level effects (city-averages)
      pred.phi1 <- array(NA, dim = c(nsamp, 37)) # blank arrays for storing the data
      pred.phi2 <- array(NA, dim = c(nsamp, 37))
      pred.phi3 <- array(NA, dim = c(nsamp, 37))
      for(i in 1:nsamp){
        for(k in 1:37){
          pred.phi1[i,k] <- tmp$mu.phi1.s1[i,k]
          pred.phi2[i,k] <- tmp$mu.phi2.s2[i,k]
          pred.phi3[i,k] <- tmp$mu.phi3.s3[i,k]
          
          
        }
      }
      
      # Summarize each, getting posterior means and credible intervals (90% and 95%)
      pm1 <- apply(pred.phi1, c(2), function(x)   mean(x, na.rm=TRUE))   
      pmed1 <- apply(pred.phi1, c(2), function(x)   mean(x, na.rm=TRUE))  
      cri1 <- apply(pred.phi1, c(2), function(x)   quantile(x, prob = c(0.025, 0.975, 0.05, 0.95)))
      
      pm2 <- apply(pred.phi2, c(2), function(x)   mean(x, na.rm=TRUE))   
      pmed2 <- apply(pred.phi2, c(2), function(x)   mean(x, na.rm=TRUE))  
      cri2 <- apply(pred.phi2, c(2), function(x)   quantile(x, prob = c(0.025, 0.975, 0.05, 0.95)))
      
      pm3 <- apply(pred.phi3, c(2), function(x)   mean(x, na.rm=TRUE))   
      pmed3 <- apply(pred.phi3, c(2), function(x)   mean(x, na.rm=TRUE))  
      cri3 <- apply(pred.phi3, c(2), function(x)   quantile(x, prob = c(0.025, 0.975, 0.05, 0.95)))
      
      data_phi1_3 <- data.frame(
        "species" = data.spp$Species,
        "species_code" = data.spp$species_code,
        "common_name" = data.spp$name_common,
        "phi1" = pmed1,
        "phi1_lower95" = c(t(cri1[3,])),
        "phi1_upper95" = c(t(cri1[4,])),
        "phi2" = pmed2,
        "phi2_lower95" = c(t(cri2[3,])),
        "phi2_upper95" = c(t(cri2[4,])),
        "phi3" = pmed3,
        "phi3_lower95" = c(t(cri3[3,])),
        "phi3_upper95" = c(t(cri3[4,])),
        "mass" = data.spp$mass/1000,
        "logmass" = data.spp$logmass,
        "carnivory" = data.spp$carn*100,   # put carnivory as a percentage
        "carn_std" = data.spp$carn.std
        
      )
      
      # Orders of increasing body mass and carnivory, for plotting purposes
      data_phi1_3 <- data_phi1_3 %>% arrange(logmass)
      data_phi1_3$order_mass <- as.factor(c(1:nrow(data_phi1_3)))
      data_phi1_3 <- data_phi1_3 %>% arrange(carnivory,logmass)
      data_phi1_3$order.carn <- as.factor(c(1:nrow(data_phi1_3)))
      data_phi1_3$fact.carn <- as.factor(round(data_phi1_3 %>% pull(carnivory), digits = 2))
      data_phi1_3 <- data_phi1_3 %>% arrange(species)
      
      ###### Plot Urbanization-Occupancy Relationships vs. Traits ######
      # Body Mass
      # drop the rarely detected species and sort by mass 
      data.plot.spp <- data_phi1_3 %>% 
        arrange(mass) %>% 
        filter(!species %in% c("black_bear", "cougar", "flying_squirrel_sp", "hooded_skunk", "mountain beaver","north_american_beaver", "richardson_ground_squirrel", "weasel_sp")) # exclude rarely detected species from plot
      
      plot.mass2 <- ggplot() +
        theme_classic()+ 
        geom_ribbon(data = data.pred.urbocc, aes(x = logmass, y = bm_med, ymin = bm_lower95, ymax = bm_upper95), 
                    fill = "grey30", alpha = 0.2)+
        geom_smooth(data = data.pred.urbocc, aes(x = logmass, y = bm_med), 
                    se = FALSE, color = "grey30") +
        geom_pointrange(data = data.plot.spp, aes(x = logmass, y = phi1, ymin = phi1_lower95, ymax = phi1_upper95),
                        color = "grey30") +
        labs(x = "Body Mass (kg)", y = "Occupancy-Urbanization \nRelationship") +
        scale_x_continuous(trans = "log10", breaks = log10(c(0.1, 1, 10, 100, 500)*1000), labels = c("0.1","1", "10", "100","500")) +
        theme(
          #axis.text.x = element_blank(),
          #axis.text.y = element_blank(),
          #axis.ticks = element_blank(),
          axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18)
        )
      plot.mass2
      
      
      ggsave("./figures/Figure3a_speciesurbanrelationship_vs_mass.png",
             plot.mass2,
             width = 6,
             height = 4,
             units = "in",
             dpi = 300)
      
      
      # Carnivory
      # drop the rarely detected species and sort by carnivory
      data.plot.spp <- data_phi1_3 %>% 
        arrange(mass) %>% 
        filter(!species %in% c("black_bear", "cougar", "flying_squirrel_sp", "hooded_skunk", "mountain beaver","north_american_beaver", "richardson_ground_squirrel", "weasel_sp")) # exclude rarely detected species from plot
      
      plot.carn2 <- ggplot() +
        theme_classic()+ 
        geom_ribbon(data = data.pred.urbocc, aes(x = carnivory*100+0.001, y = ca_med, ymin = ca_lower95, ymax = ca_upper95), 
                    fill = "grey30", alpha = 0.2)+
        geom_smooth(data = data.pred.urbocc, aes(x = carnivory*100+0.001, y = ca_med), 
                    se = FALSE, color = "grey30") +
        geom_pointrange(data = data.plot.spp, aes(x = carnivory+0.001, y = phi1, ymin = phi1_lower95, ymax = phi1_upper95, group = species),
                        color = "grey30", position = position_dodge(width = 2)) + 
        labs(x = "Carnivory (% Vertebrate Diet)", y = "Occupancy-Urbanization \nRelationship") +
        theme(
          #axis.text.x = element_blank(),
          #axis.text.y = element_blank(),
          #axis.ticks = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18)
        )
      plot.carn2
      
      ggsave("./figures/Figure3b_speciesurbanrelationship_vs_carnivory.png",
             plot.carn2,
             width = 6,
             height = 4,
             units = "in",
             dpi = 300)
      
      
# Extended Data/Supplementary Figure: Community-Averaged Occupancy vs. Within-City Covariates ----
      # Prepare to predict across gradients of each within-city covariate
      # Values of each covariate to predict off of
      # hold all other values at their mean value, for illustration
      # (should be close to 0 for standardized variables, but within-city covariates were standardized within cities)
      dm.urb <- cbind(
        1,
        seq(min(data.site$imperv_std), max(data.site$imperv_std), length.out = npred),
        0,  # patch density
        0,  # agriculture
        0,  # EVI
        0,  # EVI*local urbanization
        0,  # MAT
        0,  # MAT*local urbanization
        0,  # URB
        0,  # URB*local urbanization
        0,  # AGE
        0   # AGE*local urbanization
      )
      
      dm.pd <- cbind(
        1,  # intercept
        0,  # local urbanization
        seq(min(data.site$pd_undev_std), max(data.site$pd_undev_std), length.out = npred),  # patch density
        0,  # agriculture
        0,  # EVI
        0,  # EVI*local urbanization
        0,  # MAT
        0,  # MAT*local urbanization
        0,  # URB
        0,  # URB*local urbanization
        0,  # AGE
        0   # AGE*local urbanization
      )
      
      dm.ag <- cbind(
        1,  # intercept
        0,  # local urbanization
        0,  # patch density
        seq(min(data.site$cropland_std), max(data.site$cropland_std), length.out = npred),  # agriculture
        0,  # EVI
        0,  # EVI*local urbanization
        0,  # MAT
        0,  # MAT*local urbanization
        0,  # URB
        0,  # URB*local urbanization
        0,  # AGE
        0   # AGE*local urbanization
      )
      
      mcmc.mu.beta <- cbind(tmp$mu.beta0, 
                            tmp$mu.beta1.s1, 
                            tmp$mu.beta2.s2,
                            tmp$mu.beta3.s3,
                            tmp$mu.beta4.r,
                            tmp$mu.beta5.s1r,
                            tmp$mu.beta6.r,
                            tmp$mu.beta7.s1r,
                            tmp$mu.beta8.r,
                            tmp$mu.beta9.s1r,
                            tmp$mu.beta10.r,
                            tmp$mu.beta11.s1r
      )
      
      ###### Predict occupancy vs. urbanization #####
      # Urbanization
      pred.mu.psi.urb <- plogis(mcmc.mu.beta %*% t(dm.urb))
      
      # Patch Density
      pred.mu.psi.pd <- plogis(mcmc.mu.beta %*% t(dm.pd))
      
      # Agriculture
      pred.mu.psi.ag <- plogis(mcmc.mu.beta %*% t(dm.ag))
      
      ###### Summarize the occupancy predictions ######
      pm_psiurb <- apply(pred.mu.psi.urb, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
      cri_psiurb <- apply(pred.mu.psi.urb, c(2), function(x)   quantile(x, prob = c(0.50, 0.025, 0.975, 0.05, 0.95))) # posterior quantiles
      pm_psipd <- apply(pred.mu.psi.pd, c(2), function(x)   mean(x, na.rm=TRUE))    
      cri_psipd <- apply(pred.mu.psi.pd, c(2), function(x)   quantile(x, prob = c(0.50, 0.025, 0.975, 0.05, 0.95)))
      pm_psiag <- apply(pred.mu.psi.ag, c(2), function(x)   mean(x, na.rm=TRUE)) 
      cri_psiag <- apply(pred.mu.psi.ag, c(2), function(x)   quantile(x, prob = c(0.50, 0.025, 0.975, 0.05, 0.95)))
      
      ###### Plot Community-average Occupancy Predictions #####
      # Assemble the data into one
      data.plot <- data.frame(
        "impervious" = seq(min(data.site$Impervious), max(data.site$Impervious), length.out = npred),
        "impervious_std" = seq(min(data.site$imperv_std), max(data.site$imperv_std), length.out = npred),
        "pd" = seq(min(data.site$pd_undev), max(data.site$pd_undev), length.out = npred),
        "ag" = seq(min(data.site$cropland), max(data.site$cropland), length.out = npred),
        "psi.urb" = pm_psiurb,
        "psi_urb_med" = c(t(cri_psiurb[1,])),
        "psi.urb2.5" = c(t(cri_psiurb[2,])),
        "psi.urb97.5" = c(t(cri_psiurb[3,])),
        "psi.pd" = pm_psipd,
        "psi_pd_med" = c(t(cri_psipd[1,])),
        "psi.pd2.5" = c(t(cri_psipd[2,])),
        "psi.pd97.5" = c(t(cri_psipd[3,])),
        "psi.ag" = pm_psiag,
        "psi_ag_med" = c(t(cri_psiag[1,])),
        "psi.ag2.5" = c(t(cri_psiag[2,])),
        "psi.ag97.5" = c(t(cri_psiag[3,]))
        
      )
      
      
      plot.psi1 <- ggplot(data = data.plot, aes(x = impervious, y = psi_urb_med)) +
        geom_ribbon(aes(ymin = psi.urb2.5, ymax = psi.urb97.5), fill = calle[7], alpha = 0.5) +
        geom_smooth(se = FALSE, color = calle[8])+
        theme_bw() + 
        #geom_point(color = calle[8])+
        scale_y_continuous(labels = label_number(accuracy = 0.01)) +
        coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(0,0.4))+
        labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Occupancy") +
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              axis.title.x = element_text(face = "bold", size = 16), 
              axis.title.y = element_text(face = "bold", size = 16),
        ) 
      plot.psi1
      
      
      ggsave("./figures/extendeddata_figure1a_urbanization_vs_occupancy.png",
             plot.psi1,
             width = 4,
             height = 4,
             units = "in",
             dpi = 300)
      
      
      plot.psi2 <- ggplot(data = data.plot, aes(x = pd, y = psi_pd_med)) +
        geom_ribbon(aes(ymin = psi.pd2.5, ymax = psi.pd97.5), fill =  calle[2], alpha = 0.5) +
        geom_smooth(se = FALSE, color =  calle[5])+
        theme_bw() + 
        #geom_point(color =  calle[5])+
        scale_y_continuous(labels = label_number(accuracy = 0.01)) +
        coord_cartesian(xlim=c(0, max(data.site$pd_undev)), ylim=c(0,0.4))+
        labs(x = "Patch Density \n(#/100 ha)", y = "Species Occupancy") +
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              axis.title.x = element_text(face = "bold", size = 16), 
              axis.title.y = element_text(face = "bold", size = 16),
        ) 
      plot.psi2
      
      
      ggsave("./figures/extendeddata_figure1b_patchdensity_vs_occupancy.png",
             plot.psi2,
             width = 4,
             height = 4,
             units = "in",
             dpi = 300)
      
      
      plot.psi3 <- ggplot(data = data.plot, aes(x = ag, y = psi_ag_med)) +
        geom_ribbon(aes(ymin = psi.ag2.5, ymax = psi.ag97.5), fill =  calle[6], alpha = 0.5) +
        geom_smooth(se = FALSE, color =  calle[9])+
        theme_bw() + 
        #geom_point(color =  calle[9])+
        scale_y_continuous(labels = label_number(accuracy = 0.01)) +
        coord_cartesian(xlim=c(0, max(data.site$cropland)), ylim=c(0,0.4))+
        labs(x = "Agricultural Footprint \n(% Land Cover)", y = "Species Occupancy") +
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              axis.title.x = element_text(face = "bold", size = 16), 
              axis.title.y = element_text(face = "bold", size = 16),
        ) 
      plot.psi3
      
      
      ggsave("./figures/extendeddata_figure1c_agriculture_vs_occupancy.png",
             plot.psi3,
             width = 4,
             height = 4,
             units = "in",
             dpi = 300)