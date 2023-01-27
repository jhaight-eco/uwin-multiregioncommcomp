#### Urbanization, climate, and species traits shape mammal communities from local to continental scales ####

#### 2.2 Visualizing Within-City Relationships between Community Composition and Covariates ####
#### Haight, Jeffrey D.


#### Setup ####
  rm(list=ls()) # clear the environment
  gc()
  set.seed(4321)
  
  # Set working directory
  setwd("YourFilePathHere")

  # Load necessary packages
  library(dplyr)
  library(jagsUI)  # modeling package
  library(ggplot2) # visualization packages
  library(peRReo)
  library(png)
  library(GGally)
  
  # Define color palette for figures
  calle <-  latin_palette("calle13", 9)


##### Import Data #####
  # The main dataset containing all the site and regional covariates 
  # and summarized community composition parameters
  data_site <- read.csv("./suppfile_data/data3_outputsummary/data_sites_mrcmsummary.csv")
  
  # import the meta-analysis models and their parameter summaries
  m.sr <- readRDS("./modeloutput/model2output_logglm_hill0_sample60k.rds")
  m.sd <- readRDS("./modeloutput/model3output_logglm_hill1_sample60k.rds")
  
  m.sr.sum <- read.csv("./data/modelsummary/model2summary_logglm_hill0_sample60k.csv")
  m.sd.sum <- read.csv("./data/modelsummary/model3summary_logglm_hill1_sample60k.csv")
  
  # get the mcmc steps all in one matrix
  m.sr.mcmc <- do.call(
    "cbind",
    m.sr$sims.list
  )
  
  colnames(m.sr.mcmc) <- row.names(m.sr.sum)
  
  m.sd.mcmc <- do.call(
    "cbind",
    m.sd$sims.list
  )
  
  colnames(m.sd.mcmc) <- row.names(m.sd.sum)
  
  
  colnames(m.sr.mcmc) <- row.names(m.sr.sum)
  colnames(m.sd.mcmc) <- row.names(m.sd.sum)
  
  colnames(m.sr.sum)[1] <- c("Predictor")
  colnames(m.sd.sum)[1] <- c("Predictor")
  
  m.sr.sum
  m.sd.sum

#### Extended Data/Supplementary Figure: Relationships Among Within-City Covariates ####
  var.loc <- data_site %>%
    dplyr::select(
      Impervious,
      pd_undev,
      cropland
    )
  
  plot.corrloc <- ggpairs(var.loc, title= NULL,
                  columnLabels = c("Urban \nIntensity", 
                                   "Natural Patch \nDensity",
                                   "Agricultural \nIntensity"),
                  upper = list(continuous = wrap("cor", method = "pearson")),
    ) + 
    theme_bw() + 
    theme(
      axis.text.x = element_text(face = "bold", size = 9), 
      axis.text.y = element_text(face = "bold", size = 9), 
      axis.title.x = element_text(face = "bold", size = 9), 
      axis.title.y = element_text(face = "bold", size = 9)
    )
 
  plot.corrloc
  

#### Figure 2: Within-City Covariates vs. Species Richness and Diversity ####
# 1) Predict across Urbanization Gradient
  npred <- 200    # how many values to predict across
  
  cov1 <- data_site$Impervious
  cov1.std <- data_site$imperv_std
  
  # Values to predict off of
  # Hold all other covariates constant at their mean (0 since they are standardized)
  pred_urb <- cbind(
    1,# intercept
    seq(min(cov1.std),max(cov1.std), length.out = npred), #urban
    0, #pd_undev
    0, # cropland
    0, # EVI
    0, #urb * EVI
    0, #mat,
    0, # urb * mat,
    0,
    0,
    0,
    0 
  )
  
  preds_sr_urb <- m.sr.mcmc[,1:12] %*% t(pred_urb)
  preds_sr_urb <- exp(preds_sr_urb)  # exponentiate, because that's the reverse of the log-link
  preds_sr_urb <- apply(
    preds_sr_urb,
    2,
    quantile, 
    probs = c(0.025,0.5,0.975)
  ) %>% t() %>% data.frame()
  colnames(preds_sr_urb)[1:3] <- c("lower95", "median", "upper95")
  
  # add a column for the covariate
  preds_sr_urb$cov1 <- seq(min(cov1), max(cov1), length.out = npred)
  preds_sr_urb$cov1.std <- seq(min(cov1.std), max(cov1.std), length.out = npred)
  str(preds_sr_urb)
  
  
  # repeat for species diversity
  preds_sd_urb <- m.sd.mcmc[,1:12] %*% t(pred_urb)
  preds_sd_urb <- exp(preds_sd_urb)  # exponentiate, because that's the reverse of the log-link
  preds_sd_urb <- apply(
    preds_sd_urb,
    2,
    quantile, 
    probs = c(0.025,0.5,0.975)
  ) %>% t() %>% data.frame()
  colnames(preds_sd_urb)[1:3] <- c("lower95", "median", "upper95")
  preds_sd_urb$cov1 <- seq(min(cov1), max(cov1), length.out = npred)
  preds_sd_urb$cov1.std <- seq(min(cov1.std), max(cov1.std), length.out = npred)
  str(preds_sd_urb)
  
# 2) Predict across Patch Density Gradient
  npred <- 200    # how many values to predict across
  
  cov1 <- data_site$pd_undev
  cov1.std <- data_site$pd_undev_std
  
  # Values to predict off of
  # Hold all other covariates constant at their mean (0 since they are standardized)
  pred_pd <- cbind(
    1,# intercept
    0, #urban
    seq(min(cov1.std),max(cov1.std), length.out = npred), #pd_undev
    0, # cropland
    0, # EVI
    0, #urb * EVI
    0, #mat,
    0, # urb * mat,
    0,
    0,
    0,
    0 
  )
  
  preds_sr_pd <- m.sr.mcmc[,1:12] %*% t(pred_pd)
  preds_sr_pd <- exp(preds_sr_pd)  # exponentiate, because that's the reverse of the log-link
  preds_sr_pd <- apply(
    preds_sr_pd,
    2,
    quantile, 
    probs = c(0.025,0.5,0.975)
  ) %>% t() %>% data.frame()
  colnames(preds_sr_pd)[1:3] <- c("lower95", "median", "upper95")
  
  # add a column for the covariate
  preds_sr_pd$cov1 <- seq(min(cov1), max(cov1), length.out = npred)
  preds_sr_pd$cov1.std <- seq(min(cov1.std), max(cov1.std), length.out = npred)
  str(preds_sr_pd)
  
  
  # repeat for species diversity
  preds_sd_pd <- m.sd.mcmc[,1:12] %*% t(pred_pd)
  preds_sd_pd <- exp(preds_sd_pd)  # exponentiate, because that's the reverse of the log-link
  preds_sd_pd <- apply(
    preds_sd_pd,
    2,
    quantile, 
    probs = c(0.025,0.5,0.975)
  ) %>% t() %>% data.frame()
  colnames(preds_sd_pd)[1:3] <- c("lower95", "median", "upper95")
  preds_sd_pd$cov1 <- seq(min(cov1), max(cov1), length.out = npred)
  preds_sd_pd$cov1.std <- seq(min(cov1.std), max(cov1.std), length.out = npred)
  str(preds_sd_pd)

# 3) Predict across Agricultural Footprint Gradient
  npred <- 200    # how many values to predict across
  
  cov1 <- data_site$cropland
  cov1.std <- data_site$cropland_std
  
  # Values to predict off of
  # Hold all other covariates constant at their mean (0 since they are standardized)
  pred_ag <- cbind(
    1,# intercept
    0, #urban
    0, #pd_undev
    seq(min(cov1.std),max(cov1.std), length.out = npred), # cropland
    0, # EVI
    0, #urb * EVI
    0, #mat,
    0, # urb * mat,
    0,
    0,
    0,
    0 
  )
  
  preds_sr_ag <- m.sr.mcmc[,1:12] %*% t(pred_ag)
  preds_sr_ag <- exp(preds_sr_ag)  # exponentiate, because that's the reverse of the log-link
  preds_sr_ag <- apply(
    preds_sr_ag,
    2,
    quantile, 
    probs = c(0.025,0.5,0.975)
  ) %>% t() %>% data.frame()
  colnames(preds_sr_ag)[1:3] <- c("lower95", "median", "upper95")
  
  # add a column for the covariate
  preds_sr_ag$cov1 <- seq(min(cov1), max(cov1), length.out = npred)
  preds_sr_ag$cov1.std <- seq(min(cov1.std), max(cov1.std), length.out = npred)
  str(preds_sr_ag)
  
  
  # repeat for species diversity
  preds_sd_ag <- m.sd.mcmc[,1:12] %*% t(pred_ag)
  preds_sd_ag <- exp(preds_sd_ag)  # exponentiate, because that's the reverse of the log-link
  preds_sd_ag <- apply(
    preds_sd_ag,
    2,
    quantile, 
    probs = c(0.025,0.5,0.975)
  ) %>% t() %>% data.frame()
  colnames(preds_sd_ag)[1:3] <- c("lower95", "median", "upper95")
  preds_sd_ag$cov1 <- seq(min(cov1), max(cov1), length.out = npred)
  preds_sd_ag$cov1.std <- seq(min(cov1.std), max(cov1.std), length.out = npred)
  str(preds_sd_ag)

  
  
##### Figure 2a: Urbanization vs. Richness #####
  plot1 <- ggplot() +
    geom_errorbar(data = data_site, aes(x = Impervious, y = rich_mean, ymin = rich_2_5, ymax = rich_97_5), 
                  lwd = 0.3, color = calle[7], alpha = 0.3)+
    geom_point(data = data_site, aes(x = Impervious, y = rich_mean, ymin = rich_5, ymax = rich_95),
               col = calle[8])+
    geom_ribbon(data = preds_sr_urb, aes(x = cov1, y = median, ymin = lower95, ymax = upper95), 
                fill = calle[7], alpha = 0.5) +
    geom_smooth(data = preds_sr_urb, aes(x = cov1, y = median, ymin = lower95, ymax = upper95),
                se = FALSE, color = calle[8])+
    theme_bw() + 
    scale_x_continuous(limits = c(-5, 150)) +
    scale_y_continuous(limits = c(-3, 15)) +
    coord_cartesian(xlim=c(0, max(data_site$Impervious)), ylim=c(0,11.5))+
    labs(x = "Local Urbanization (% Impervious Surface)", y = "Species Richness")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
    ) 
  
  plot1

  
  
##### Figure 2b: Urbanization vs. Diversity #####
  plot2 <- ggplot() +
    geom_errorbar(data = data_site, aes(x = Impervious, y = rich_mean, ymin = hill1_2_5, ymax = hill1_97_5), 
                  lwd = 0.3, color = calle[7], alpha = 0.3)+
    geom_point(data = data_site, aes(x = Impervious, y = hill1_mean, ymin = hill1_2_5, ymax = hill1_97_5),
               col = calle[8])+
    geom_ribbon(data = preds_sd_urb, aes(x = cov1, y = median, ymin = lower95, ymax = upper95), 
                fill = calle[7], alpha = 0.5) +
    geom_smooth(data = preds_sd_urb, aes(x = cov1, y = median, ymin = lower95, ymax = upper95),
                se = FALSE, color = calle[8])+
    theme_bw() + 
    scale_x_continuous(limits = c(-5, 150)) +
    scale_y_continuous(limits = c(-3, 15)) +
    coord_cartesian(xlim=c(0, max(data_site$Impervious)), ylim=c(0,11.5))+
    labs(x = "Local Urbanization (% Impervious Surface)", y = "Species Diversity")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
    ) 
  
  plot2
  
  
  
  
##### Figure 2c: Patch Density vs. Richness #####
  plot1 <- ggplot() +
    geom_errorbar(data = data_site, aes(x = pd_undev, y = rich_mean, ymin = rich_2_5, ymax = rich_97_5), 
                  lwd = 0.3, color = calle[5], alpha = 0.3)+
    geom_point(data = data_site, aes(x = pd_undev, y = rich_mean, ymin = rich_2_5, ymax = rich_97_5),
               col = calle[5])+
    geom_ribbon(data = preds_sr_pd, aes(x = cov1, y = median, ymin = lower95, ymax = upper95), 
                fill = calle[2], alpha = 0.5) +
    geom_smooth(data = preds_sr_pd, aes(x = cov1, y = median, ymin = lower95, ymax = upper95),
                se = FALSE, color = calle[5])+
    theme_bw() + 
    scale_x_continuous(limits = c(-5, 150)) +
    scale_y_continuous(limits = c(-3, 15)) +
    coord_cartesian(xlim=c(0, max(data_site$pd_undev)), ylim=c(0,11.5))+
    labs(x = "Local Patch Density (#/100 ha)", y = "Species Richness")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
    ) 
  
  plot1
  
  
  
##### Figure 2d: Patch Density vs. Diversity #####
  plot2 <- ggplot() +
    geom_errorbar(data = data_site, aes(x = pd_undev, y = hill1_mean, ymin = hill1_2_5, ymax = hill1_97_5), 
                  lwd = 0.3, color = calle[5], alpha = 0.3)+
    geom_point(data = data_site, aes(x = pd_undev, y = hill1_mean, ymin = hill1_2_5, ymax = hill1_97_5),
               col = calle[5])+
    geom_ribbon(data = preds_sd_pd, aes(x = cov1, y = median, ymin = lower95, ymax = upper95), 
                fill = calle[2], alpha = 0.5) +
    geom_smooth(data = preds_sd_pd, aes(x = cov1, y = median, ymin = lower95, ymax = upper95),
                se = FALSE, color = calle[5])+
    theme_bw() + 
    scale_x_continuous(limits = c(-5, 150)) +
    scale_y_continuous(limits = c(-3, 15)) +
    coord_cartesian(xlim=c(0, max(data_site$pd_undev)), ylim=c(0,11.5))+
    labs(x = "Local Patch Density (#/100 ha)", y = "Species Diversity")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
    ) 

  plot2
  
  
  
##### Figure 2e: Agriculture vs. Richness #####
  plot1 <- ggplot() +
    geom_errorbar(data = data_site, aes(x = (cropland+0.0000000001)*100, y = rich_mean, ymin = rich_2_5, ymax = rich_97_5), 
                  lwd = 0.3, color = calle[6], alpha = 0.3)+
    geom_point(data = data_site, aes(x = (cropland+0.0000000001)*100, y = rich_mean, ymin = rich_2_5, ymax = rich_97_5),
               col = calle[9], position = "jitter")+
    geom_ribbon(data = preds_sr_ag, aes(x = cov1*100, y = median, ymin = lower95, ymax = upper95), 
                fill = calle[6], alpha = 0.5) +
    geom_smooth(data = preds_sr_ag, aes(x = cov1*100, y = median, ymin = lower95, ymax = upper95),
                se = FALSE, color = calle[9])+
    theme_bw() + 
    scale_x_continuous(limits = c(-5, 110)) +
    scale_y_continuous(limits = c(-3, 15)) +
    coord_cartesian(xlim=c(0, max(data_site$cropland*100)), ylim=c(0,11.5)) +
    labs(x = "Local Agricultural Footprint (% Land Cover)", y = "Species Richness") +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18)
    ) 
  
  plot1
  
  
  
##### Figure 2f: Agriculture vs. Diversity #####
  plot2 <- ggplot() +
    geom_errorbar(data = data_site, aes(x = (cropland+0.0000000001)*100, 
                                        y = hill1_mean, ymin = hill1_2_5, ymax = hill1_97_5), 
                  lwd = 0.3, color = calle[6], alpha = 0.3)+
    geom_point(data = data_site, aes(x = (cropland+0.0000000001)*100, y = hill1_mean, ymin = hill1_2_5, ymax = hill1_97_5),
               col = calle[9], position = "jitter")+
    geom_ribbon(data = preds_sd_ag, aes(x = cov1*100, y = median, ymin = lower95, ymax = upper95), 
                fill = calle[6], alpha = 0.5) +
    geom_smooth(data = preds_sd_ag, aes(x = cov1*100, y = median, ymin = lower95, ymax = upper95),
                se = FALSE, color = calle[9])+
    theme_bw() + 
    scale_x_continuous(limits = c(-5, 110)) +
    scale_y_continuous(limits = c(-3, 15)) +
    coord_cartesian(xlim=c(0, max(data_site$cropland*100)), ylim=c(0,11.5)) +
    labs(x = "Local Agricultural Footprint (% Land Cover)", y = "Species Diversity") +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18)
    ) 

  plot2
