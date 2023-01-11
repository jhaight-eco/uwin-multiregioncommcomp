#### Urbanization, climate, and species traits shape mammal communities from local to continental scales #####
# Haight, Jeffrey D.
# 2.1 Visualizing Within-City Relationships between Occupancy and Covariates


#### Setup ####
rm(list=ls()) # clear the environment
gc()
set.seed(4321)

# Set working directory
setwd("G:/My Drive/ASU/Jeff-Jesse-Sharon Document Sharing/UWIN/UWIN_CrossCityManuscript_Haightetal2023")

  # Load necessary packages
  library(dplyr)
  library(ggplot2)
  library(gghighlight)
  library(viridis)
  library(RColorBrewer)
  library(peRReo)
  library(png)
  
  # Define color palette for figures
  calle <-  latin_palette("calle13", 9)


##### Import Data #####
  # The main datasets containing all the site and regional covariates
  data_site_reg <- read.csv("./suppfile_data/data3_outputsummary/data_sites_mrcmsummary.csv")
  data_reg <- read.csv("./suppfile_data/data3_outputsummary/data_cities_mrcmsummary.csv")
  data_spp <- read.csv("./suppfile_data/data1_input/ModelInputData_UWIN_allspecies.csv")
  
  # output from models run on ASU Agave computing clusters
  out <- readRDS("C:/Research/urban/UWIN/data/6_output/modeloutput_mrcm_globalinteractionmodel_sample60k.rds")
  #out
  
  # for plotting purposes, some parameters may need to be recalculated, depending on which were monitored in the mode
  # e.g. the city-average occupancy intercept, from which the city- and species-specific intercept phi0 was estimated 
  mu.phi0 <- apply(out$sims.list$phi0, c(1,2), function(x)   mean(x, na.rm=TRUE)) 

  
##### Figure 3: Species Traits vs. Occupancy and Urbanization-Occupancy Relationship #####
  ###### Predict community-level occupancy vs. traits ######
      tmp <- out$sims.list
      npred <- 200
      nsamp <- length(tmp[[1]])
      
      
      # sequences of hypothetical trait values to predict off of
      seq_bm <- seq(min(data_spp$logmass.std), max(data_spp$logmass.std), length.out = npred) 
      seq_ca <- seq(min(data_spp$carn.std), max(data_spp$carn.std), length.out = npred)
      
      plot_bm <- seq(min(data_spp$logmass), max(data_spp$logmass), length.out = npred)
      plot_ca <- seq(min(data_spp$carn), max(data_spp$carn), length.out = npred)
      range(plot_bm)
      range(plot_ca)
      
      
      # create a design matrix based on the equation: 1 = intercept, my_seq = trait values to predict off of
      dm_bm <- cbind(1, seq_bm, median(seq_ca))      
      dm_ca <- cbind(1, median(seq_bm), seq_ca) 
      
      # Grab the appropriate slope parameters
      my_mcmc <- cbind(tmp$mu.beta0, tmp$delta1.spp1, tmp$delta1.spp2)
      
      # Some quick matrix math, multiplying the design matrix by the mcmc slope parameters
      pred_bm <- my_mcmc %*% t(dm_bm)
      pred_ca <- my_mcmc %*% t(dm_ca)
      
      # Summarize the predictions
      pred_bm <- apply(pred_bm, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>% plogis()
      pred_ca <- apply(pred_ca, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>% plogis()
      
      # data for plotting the community-level predictions
      (data_pred_occ <- data.frame(
        "logmass_std" = seq_bm,
        "logmass" = plot_bm,
        "mass_kg" = plot_bm_kg,
        "carnivory_std" = seq_ca,
        "carnivory" = plot_ca*100,
        t(pred_bm),
        t(pred_ca)
      ))
      colnames(data_pred_occ)[6:11] <- c("bm_lower95", "bm_med", "bm_upper95",
                                         "ca_lower95", "ca_med", "ca_upper95")
      
      # Data for plotting the values of the species on top of the community-level predictions
      cri <- apply(mu.phi0, 2, quantile, probs = c(0.5, 0.025, 0.975)) %>% plogis()
      
      data_muphi0 <- data.frame(
        "species" = data_spp$Species,
        "species_code" = data_spp$species_code,
        "common_name" = data_spp$name_common,
        "mass_kg" = data_spp$mass/1000,
        "logmass" = data_spp$logmass,
        "logmass_std" = data_spp$logmass.std,
        "carnivory" = data_spp$carn*100,
        "carnivory_std" = data_spp$carn.std,
        "muphi0" = c(t(cri[1,])),
        "muphi0_lower95" = c(t(cri[2,])),
        "muphi0_upper95" = c(t(cri[3,]))
      )
  
      
  ###### Plot Occupancy vs. Species Traits ######
      # drop the species that weren't very well detected from the plot
      data_plot_spp <- data_muphi0 %>% 
        filter(!species %in% c("black_bear", "cougar", "flying_squirrel_sp", "hooded_skunk", "mountain beaver","north_american_beaver", "richardson_ground_squirrel", "weasel_sp"))
      
      
      # Body Mass
      plot_mass1 <- ggplot() +
        theme_classic()+ 
        geom_ribbon(data = data_pred_occ, aes(x = logmass, y = bm_med, ymin = bm_lower95, ymax = bm_upper95), 
                    fill = "grey30", alpha = 0.2)+
        geom_smooth(data = data_pred_occ, aes(x = logmass, y = bm_med), 
                    se = FALSE, color = "grey30") +
        geom_pointrange(data = data_plot_spp, aes(x = logmass, y = muphi0, ymin = muphi0_lower95, ymax = muphi0_upper95),
                        color = "grey30") +
        labs(x = "Body Mass (kg)", y = "Species Occupancy") +
        scale_x_continuous(trans = "log10", breaks = log10(c(0.1, 1, 10, 100, 500)*1000), labels = c("0.1","1", "10", "100","500")) +
        #coord_cartesian(xlim=c(1,6), ylim=c(0, 0.6))+
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              axis.title.x = element_text(face = "bold", size = 18), 
              axis.title.y = element_text(face = "bold", size = 18),
        ) 
      plot_mass1
      
      
      # Carnivory
      plot_carn1 <- ggplot() +
        theme_classic()+ 
        geom_ribbon(data = data_pred_occ, aes(x = carnivory, y = ca_med, ymin = ca_lower95, ymax = ca_upper95), 
                    fill = "grey30", alpha = 0.2)+
        geom_smooth(data = data_pred_occ, aes(x = carnivory, y = ca_med), 
                    se = FALSE, color = "grey30") +
        geom_pointrange(data = data_plot_spp, aes(x = carnivory, y = muphi0, ymin = muphi0_lower95, ymax = muphi0_upper95, group = species),
                        color = "grey30", position = position_dodge(width = 2)) +
        labs(x = "Carnivory (% Vertebrate Diet)", y = "Species Occupancy") +
        #scale_x_continuous(trans = "log10") +
        #scale_x_continuous(trans = "log10", breaks = c("0", "1","10","50"), labels = c("0","1", "10", "50")) +
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              legend.position = "none",
              axis.title.x = element_text(face = "bold", size = 18), 
              axis.title.y = element_text(face = "bold", size = 18),
        )
      plot_carn1
      
  
      
  ###### Predict community-level occupancy-urbanization relationship vs traits ######
      # similar to above
      tmp <- out$sims.list  # grab mcmc samples
      npred <- 200          # number of values to predict
      
      # sequences of hypothetical trait values to predict off of
      seq_bm <- seq(min(data_spp$logmass.std), max(data_spp$logmass.std), length.out = npred) 
      seq_ca <- seq(min(data_spp$carn.std), max(data_spp$carn.std), length.out = npred)
      
      #plot_bm <- (seq_bm + mean(data_spp$logmass))*sd(data_spp$logmass)  # back-transform that sequence to non-logged values (for plotting purposes)
      plot_bm <- seq(min(data_spp$logmass), max(data_spp$logmass), length.out = npred)
      #plot_ca <- (seq_ca + mean(data_spp$carn))*sd(data_spp$carn)
      plot_ca <- seq(min(data_spp$carn), max(data_spp$carn), length.out = npred)
      range(plot_bm)
      range(plot_ca)
      
      # Create a design matrix based on the equation: 1 = intercept, my_seq = trail values to predict off of
      dm_bm <- cbind(1, seq_bm, 0)  
      dm_ca <- cbind(1, 0, seq_ca) 
      
      # Grab the appropriate slope parameters
      my_mcmc <- cbind(tmp$mu.beta1.s1, tmp$delta2.spp1, tmp$delta2.spp2)
      
      # Some quick matrix math, multiplying the design matrix by the mcmc slope parameters
      pred_bm <- my_mcmc %*% t(dm_bm)   
      pred_ca <- my_mcmc %*% t(dm_ca)
      
      # Summarize the predictions. We really only need the median and CRIs
      pred_bm <- apply(pred_bm, 2, quantile, probs = c(0.025, 0.5, 0.975)) 
      pred_ca <- apply(pred_ca, 2, quantile, probs = c(0.025, 0.5, 0.975))
      
      # organize for plotting using ggplot
      (data_pred_urbocc <- data.frame(
        "logmass_std" = seq_bm,
        "logmass" = plot_bm,
        "carnivory_std" = seq_ca,
        "carnivory" = plot_ca,
        t(pred_bm),
        t(pred_ca)
      ))
      colnames(data_pred_urbocc)[5:10] <- c("bm_lower95", "bm_med", "bm_upper95",
                                     "ca_lower95", "ca_med", "ca_upper95")
  
      
  ###### Extract distributions of each species' response to within-city covariates ######
      tmp <- out$sims.list               # grab MCMC samples
      nsamp <- length(tmp[[1]])          # number of mcmc samples
      
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
        "species" = data_spp$Species,
        "species_code" = data_spp$species_code,
        "common_name" = data_spp$name_common,
        "phi1" = pmed1,
        "phi1_lower95" = c(t(cri1[3,])),
        "phi1_upper95" = c(t(cri1[4,])),
        "phi2" = pmed2,
        "phi2_lower95" = c(t(cri2[3,])),
        "phi2_upper95" = c(t(cri2[4,])),
        "phi3" = pmed3,
        "phi3_lower95" = c(t(cri3[3,])),
        "phi3_upper95" = c(t(cri3[4,])),
        "mass" = data_spp$mass/1000,
        "logmass" = data_spp$logmass,
        "carnivory" = data_spp$carn*100,   # put carnivory as a percentage
        "carn_std" = data_spp$carn.std
        
      )
      
      # Orders of increasing body mass and carnivory, for plotting purposes
      data_phi1_3 <- data_phi1_3 %>% arrange(logmass)
      data_phi1_3$order_mass <- as.factor(c(1:nrow(data_phi1_3)))
      data_phi1_3 <- data_phi1_3 %>% arrange(carnivory,logmass)
      data_phi1_3$order_carn <- as.factor(c(1:nrow(data_phi1_3)))
      data_phi1_3$fact_carn <- as.factor(round(data_phi1_3 %>% pull(carnivory), digits = 2))
      data_phi1_3 <- data_phi1_3 %>% arrange(species)
      
  ###### Plot Urbanization-Occupancy Relationships vs. Traits ######
      # Body Mass
      # drop the rarely detected species and sort by mass 
      data_plot_spp <- data_phi1_3 %>% 
        arrange(mass) %>% 
        filter(!species %in% c("black_bear", "cougar", "flying_squirrel_sp", "hooded_skunk", "mountain beaver","north_american_beaver", "richardson_ground_squirrel", "weasel_sp")) # exclude rarely detected species from plot
      
      plot_mass2 <- ggplot() +
        theme_classic()+ 
        geom_ribbon(data = data_pred_urbocc, aes(x = logmass, y = bm_med, ymin = bm_lower95, ymax = bm_upper95), 
                    fill = "grey30", alpha = 0.2)+
        geom_smooth(data = data_pred_urbocc, aes(x = logmass, y = bm_med), 
                    se = FALSE, color = "grey30") +
        geom_pointrange(data = data_plot_spp, aes(x = logmass, y = phi1, ymin = phi1_lower95, ymax = phi1_upper95),
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
      plot_mass2
      
      # Carnivory
      # drop the rarely detected species and sort by carnivory
      data_plot_spp <- data_phi1_3 %>% 
        arrange(mass) %>% 
        filter(!species %in% c("black_bear", "cougar", "flying_squirrel_sp", "hooded_skunk", "mountain beaver","north_american_beaver", "richardson_ground_squirrel", "weasel_sp")) # exclude rarely detected species from plot
      
      set.seed(123)  # needs to be set in order for the points and error bars to jitter the same
      
      plot_carn2 <- ggplot() +
        theme_classic()+ 
        geom_ribbon(data = data_pred_urbocc, aes(x = carnivory*100+0.001, y = ca_med, ymin = ca_lower95, ymax = ca_upper95), 
                    fill = "grey30", alpha = 0.2)+
        geom_smooth(data = data_pred_urbocc, aes(x = carnivory*100+0.001, y = ca_med), 
                    se = FALSE, color = "grey30") +
        geom_pointrange(data = data_plot_spp, aes(x = carnivory+0.001, y = phi1, ymin = phi1_lower95, ymax = phi1_upper95, group = species),
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
      plot_carn2
      
      
      
      
##### Supplementary Figure: Community-Averaged Occupancy vs. Within-City Covariates #####
  # Prepare to predict across gradients of each within-city covariate
      # Set up predictions
      str(tmp <- out$sims.list)           # grab MCMC samples
      #nsamp <- length(tmp[[1]])          # for using full number of mcmc samples
      nsamp <- 1000           # for using a subset of samples
      npred <- 500    # the number of values across which to predict
      
      
      # Select a random subset of the posterior samples
      #set.seed(123)
      #samp <- sample(1:length(tmp[[1]]), size = nsamp, replace = FALSE)
      
      # initial objects for storing the predictions
      str(mu.psi.urb <- array(NA, dim = c(nsamp, npred)))
      str(mu.psi.pd <- array(NA, dim = c(nsamp, npred)))
      str(mu.psi.ag <- array(NA, dim = c(nsamp, npred)))
      
      # Values of each covariate to predict off of
      pred.urb <- seq(min(data_site_reg$imperv_std), max(data_site_reg$imperv_std), length.out = 500)
      pred.pd <- seq(min(data_site_reg$pd_undev_std), max(data_site_reg$pd_undev_std), length.out = 500)
      pred.ag <- seq(min(data_site_reg$cropland_std), max(data_site_reg$cropland_std), length.out = 500)
      
  ###### Predict occupancy vs. urbanization #####
      # hold all other values at their mean value, for illustration
      # (should be close to 0 for standardized variables, but within-city covariates were standardized within cities)
      
      # Urbanization
      for(i in 1:npred){
        for(k in 1:nsamp)
          
          mu.psi.urb[k,i] <- plogis(
            tmp$mu.beta0[k] + 
              tmp$mu.beta1.s1[k]*pred.urb[i] +
              tmp$mu.beta2.s2[k]*mean(data_site_reg$pd_undev_std) +
              tmp$mu.beta3.s3[k]*mean(data_site_reg$cropland_std) +
              tmp$mu.beta4.r[k]*mean(data_reg$EVI_av_std) +
              tmp$mu.beta5.s1r[k]*pred.urb[i]*mean(data_reg$EVI_av_std) +
              tmp$mu.beta6.r[k]*mean(data_reg$mat_av_std) +
              tmp$mu.beta7.s1r[k]*pred.urb[i]*mean(data_reg$mat_av_std) +
              tmp$mu.beta8.r[k]*mean(data_reg$urb_reg_std) +
              tmp$mu.beta9.s1r[k]*pred.urb[i]*mean(data_reg$urb_reg_std) +
              tmp$mu.beta10.r[k]*mean(data_reg$yrs_col_std) +
              tmp$mu.beta11.s1r[k]*pred.urb[i]*mean(data_reg$yrs_col_std)
          )
        
        
      }
      # Patch Density
      for(i in 1:npred){
        for(k in 1:nsamp){
          mu.psi.pd[k,i] <- plogis(
            tmp$mu.beta0[k] + 
              tmp$mu.beta1.s1[k]*mean(data_site_reg$imperv_std) +
              tmp$mu.beta2.s2[k]*pred.pd[i] +
              tmp$mu.beta3.s3[k]*mean(data_site_reg$cropland_std) +
              tmp$mu.beta4.r[k]*mean(data_reg$EVI_av_std) +
              tmp$mu.beta5.s1r[k]*mean(data_site_reg$imperv_std)*mean(data_reg$EVI_av_std) +
              tmp$mu.beta6.r[k]*mean(data_reg$mat_av_std) +
              tmp$mu.beta7.s1r[k]*mean(data_site_reg$imperv_std)*mean(data_reg$mat_av_std) +
              tmp$mu.beta8.r[k]*mean(data_reg$urb_reg_std) +
              tmp$mu.beta9.s1r[k]*mean(data_site_reg$imperv_std)*mean(data_reg$urb_reg_std) +
              tmp$mu.beta10.r[k]*mean(data_reg$yrs_col_std) +
              tmp$mu.beta11.s1r[k]*mean(data_site_reg$imperv_std)*mean(data_reg$yrs_col_std)
          ) 
        }
      }
      # Agriculture
      for(i in 1:npred){
        for(k in 1:nsamp){
          mu.psi.ag[k,i] <- plogis(
            tmp$mu.beta0[k] + 
              tmp$mu.beta1.s1[k]*mean(data_site_reg$imperv_std) +
              tmp$mu.beta2.s2[k]*mean(data_site_reg$pd_undev_std) +
              tmp$mu.beta3.s3[k]*pred.ag[i] +
              tmp$mu.beta4.r[k]*mean(data_reg$EVI_av_std) +
              tmp$mu.beta5.s1r[k]*mean(data_site_reg$imperv_std)*mean(data_reg$EVI_av_std) +
              tmp$mu.beta6.r[k]*mean(data_reg$mat_av_std) +
              tmp$mu.beta7.s1r[k]*mean(data_site_reg$imperv_std)*mean(data_reg$mat_av_std) +
              tmp$mu.beta8.r[k]*mean(data_reg$urb_reg_std) +
              tmp$mu.beta9.s1r[k]*mean(data_site_reg$imperv_std)*mean(data_reg$urb_reg_std) +
              tmp$mu.beta10.r[k]*mean(data_reg$yrs_col_std) +
              tmp$mu.beta11.s1r[k]*mean(data_site_reg$imperv_std)*mean(data_reg$yrs_col_std)
          )
        }
      }

  ###### Summarize the occupancy predictions ######
      pm_psiurb <- apply(mu.psi.urb, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
      cri_psiurb <- apply(mu.psi.urb, c(2), function(x)   quantile(x, prob = c(0.50, 0.025, 0.975, 0.05, 0.95))) # posterior quantiles
      pm_psipd <- apply(mu.psi.pd, c(2), function(x)   mean(x, na.rm=TRUE))    
      cri_psipd <- apply(mu.psi.pd, c(2), function(x)   quantile(x, prob = c(0.50, 0.025, 0.975, 0.05, 0.95)))
      pm_psiag <- apply(mu.psi.ag, c(2), function(x)   mean(x, na.rm=TRUE)) 
      cri_psiag <- apply(mu.psi.ag, c(2), function(x)   quantile(x, prob = c(0.50, 0.025, 0.975, 0.05, 0.95)))
      
  ###### Plot Community-average Occupancy Predictions #####
      # Assemble the data into one
      data_plot <- data.frame(
        "impervious" = seq(min(data_site_reg$Impervious), max(data_site_reg$Impervious), length.out = 500),
        "impervious_std" = seq(min(data_site_reg$imperv_std), max(data_site_reg$imperv_std), length.out = 500),
        "pd" = seq(min(data_site_reg$pd_undev), max(data_site_reg$pd_undev), length.out = 500),
        "ag" = seq(min(data_site_reg$cropland), max(data_site_reg$cropland), length.out = 500),
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
      
      
      plot_psi1 <- ggplot(data = data_plot, aes(x = impervious, y = psi_urb_med)) +
        geom_ribbon(aes(ymin = psi.urb2.5, ymax = psi.urb97.5), fill = calle[7], alpha = 0.5) +
        geom_smooth(se = FALSE, color = calle[8])+
        theme_bw() + 
        geom_point(color = calle[8])+
        scale_y_continuous(limits = c(0,0.4)) +
        labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Occupancy") +
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              axis.title.x = element_text(face = "bold", size = 18), 
              axis.title.y = element_text(face = "bold", size = 18),
        ) 
      plot_psi1
      
      
      plot_psi2 <- ggplot(data = data_plot, aes(x = pd, y = psi_pd_med)) +
        geom_ribbon(aes(ymin = psi.pd2.5, ymax = psi.pd97.5), fill =  calle[2], alpha = 0.5) +
        geom_smooth(se = FALSE, color =  calle[5])+
        theme_bw() + 
        geom_point(color =  calle[5])+
        scale_y_continuous(limits = c(0,0.4)) +
        labs(x = "Patch Density \n(#/100 ha)", y = "Species Occupancy") +
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              axis.title.x = element_text(face = "bold", size = 18), 
              axis.title.y = element_text(face = "bold", size = 18),
        ) 
      plot_psi2
      
      
      plot_psi3 <- ggplot(data = data_plot, aes(x = ag, y = psi_ag_med)) +
        geom_ribbon(aes(ymin = psi.ag2.5, ymax = psi.ag97.5), fill =  calle[6], alpha = 0.5) +
        geom_smooth(se = FALSE, color =  calle[9])+
        theme_bw() + 
        geom_point(color =  calle[9])+
        scale_y_continuous(limits = c(0,0.4)) +
        labs(x = "Agricultural Footprint \n(% Land Cover)", y = "Species Occupancy") +
        theme(axis.text.x = element_text(face = "bold", size = 14), 
              axis.text.y = element_text(face = "bold", size = 14), 
              axis.title.x = element_text(face = "bold", size = 18), 
              axis.title.y = element_text(face = "bold", size = 18),
        ) 
      plot_psi3
      