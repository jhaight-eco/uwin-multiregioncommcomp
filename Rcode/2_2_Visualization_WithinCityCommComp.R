#### Urbanization, climate, and species traits shape mammal communities from local to continental scales ####

#### 2.2 Visualizing Within-City Relationships between Community Composition and Covariates ####
#### Haight, Jeffrey D.


#### Setup ####
  rm(list=ls()) # clear the environment
  gc()
  set.seed(4321)
  
  # Set working directory
  #setwd("YourFilePathHere")
  # e.g.,
  setwd("C:/Users/User/Documents/GitHub/uwin-multiregioncommcomp")

  # Load necessary packages
  library(dplyr)
  library(jagsUI)  # modeling package
  library(ggplot2) # visualization packages
  library(peRReo)  # https://github.com/jbgb13/peRReo
  library(png)
  library(gghighlight)
  library(GGally)
  library(scales)
  
  # Define color palette for figures
  calle <-  latin_palette("calle13", 9)


##### Import Data #####
  # The main datasets containing all the site and regional covariates 
  # and summarized community composition parameters
  data.site <- read.csv("./data/modelsummary/data_sites_mrcmsummary.csv")
  data.reg <- read.csv("./data/modelsummary/data_cities_mrcmsummary.csv") 
  
  # import the meta-analysis models and their parameter summaries
  m.sr <- readRDS("./data/modeloutput/model2output_logglm_hill0_sample60k.rds")
  m.sd <- readRDS("./data/modeloutput/model3output_logglm_hill1_sample60k.rds")
  
  m.sr.sum <- m.sr$summary
  m.sd.sum <- m.sd$summary
  meta.names <- c("intercept",
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
    "urban x yrs_col",
    "re_sd",
    "deviance")
  rownames(m.sr.sum) <- meta.names
  rownames(m.sd.sum) <- meta.names
  
  # get the mcmc steps all in one matrix
  m.sr.mcmc <- do.call(
    "cbind",
    m.sr$sims.list
  )
  
  m.sd.mcmc <- do.call(
    "cbind",
    m.sd$sims.list
  )
  
  colnames(m.sr.mcmc) <- row.names(m.sr.sum)
  colnames(m.sd.mcmc) <- row.names(m.sd.sum)
  
  colnames(m.sr.sum)[1] <- c("Predictor")
  colnames(m.sd.sum)[1] <- c("Predictor")
  
  m.sr.sum
  m.sd.sum

  # objects for prediction
  tmp <- out$sims.list  # grab mcmc samples
  npred <- 200          # number of values to predict
  nsamp <- length(tmp[[1]])
  
  
#### Extended Data/Supplementary Figure: Relationships Among Within-City Covariates ----
  var.loc <- data.site %>%
    dplyr::select(
      Impervious,
      pd_undev,
      cropland
    )
  
  plot.corrloc <- ggpairs(var.loc, title= NULL,
                  columnLabels = c("Local \nUrbanization", 
                                   "Local \nPatch Density",
                                   "Local \nAgricultural Footprint"),
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
  
  ggsave("./figures/ExtendedDataFigure_collinearity_localvariables.png",
         plot.corrloc,
         width = 6,
         height = 5,
         units = "in",
         dpi = 300)
  
  # Figure 2: Urbanization vs. Richness/Diversity Trends Under Contrasting Regional Variables ----
  # First, we will use slopes and intercepts from the Bayesian meta-analysis
  # to predict richness and diversity across ranges of covariate values under multiple regional conditions
  
  # Option A. Base among-region predictions on hypothetical gradients of regional covariates
  # for each set of predictions, we will hold all other regional and local covariates constant at their mean (zero)
  
  # Option B. Base it on each city's actual sequence of values # Values of local, within-city covariates to predict off of. These were standardized by city
  # Since we are showing the relationships within specific cities, it makes more sense to predict off of their 
  # a range of hypothetical values equivalent to their actual gradient of urbanization (with patch density and agriculture held constant)
  
  # For Option B, please see 'Magnitude of Relationships within Contrasting Cities' section below
  
  
  # Here, we will go with Option A, holding all other variables constant, as it makes for a clearer visual 
  # that more effectively conveys the statistical relationships between local urbanization and regional covariates
  cities <- data.reg$city
  #nline <- length(cities)        # how many different lines you're wanting to plot
  nline <- 5
  
  
  city.num <- paste("city", 1:nline, sep = "")    
  
  urban.pred <- seq(min(data.site$imperv_std), max(data.site$imperv_std), length.out = npred)
  pd.pred <- seq(min(data.site$pd_undev_std), max(data.site$pd_undev_std), length.out = npred)
  ag.pred <- seq(min(data.site$cropland_std), max(data.site$cropland_std), length.out = npred)
  
  # predicting across moderate ranges of regional variables (excluding unrealistic combinations of greenness and temperature)
  data.reg %>% select(city, EVI_av, mat_av, urb_reg, yrs_col)
  pred.EVI <- seq((0.2-mean(data.reg$EVI_av))/sd(data.reg$EVI_av), (0.29-mean(data.reg$EVI_av))/sd(data.reg$EVI_av), length.out = nline)
  pred.MAT <- seq((10-mean(data.reg$mat_av))/sd(data.reg$mat_av), (15-mean(data.reg$mat_av))/sd(data.reg$mat_av), length.out = nline) # from 10 to 15 degrees
  pred.URB <- seq((0.4-mean(data.reg$urb_reg))/sd(data.reg$urb_reg), (0.8-mean(data.reg$urb_reg))/sd(data.reg$urb_reg), length.out = nline)
  pred.AGE <- seq((150-mean(data.reg$yrs_col))/sd(data.reg$yrs_col), (250-mean(data.reg$yrs_col))/sd(data.reg$yrs_col), length.out = nline)
  # predicting across ranges that includes the extreme values (not as realistic)
  #pred.EVI <- seq(min(data.reg$EVI_av_std), max(data.reg$EVI_av_std), length.out = nline)
  #pred.MAT <- seq(min(data.reg$mat_av_std), max(data.reg$mat_av_std), length.out = nline)
  #pred.URB <- seq(min(data.reg$urb_reg_std), max(data.reg$urb_reg_std), length.out = nline)
  #pred.AGE <- seq(min(data.reg$yrs_col_std), max(data.reg$yrs_col_std), length.out = nline)
  
  # objects for storing prediction summary stats
  preds.sr.mean <- rep(NA, npred*nline)
  preds.sr.med <- rep(NA, npred*nline)
  preds.sr.2.5 <- rep(NA, npred*nline)
  preds.sr.97.5 <- rep(NA, npred*nline)
  preds.sd.mean <- rep(NA, npred*nline)
  preds.sd.med <- rep(NA, npred*nline)
  preds.sd.2.5 <- rep(NA, npred*nline)
  preds.sd.97.5 <- rep(NA, npred*nline)
  
  # predict across the EVI gradient
  for(r in 1:nline){  # loop for each line 
    # a design matrix that is remade for each line, using their specific regional covariate values
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
    
    preds.sr.city <- m.sr.mcmc[,1:12] %*% t(dm.urb) %>% exp()  # predict and exponentiate, because that's the reverse of the log-link
    preds.sd.city <- m.sd.mcmc[,1:12] %*% t(dm.urb) %>% exp()
    preds.sr.cri <- apply(preds.sr.city, 2, quantile, probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
    preds.sd.cri <- apply(preds.sd.city, 2, quantile,  probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
    
    preds.sr.mean[(r*npred-npred+1):(r*npred)] <- apply(preds.sr.city, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
    preds.sr.2.5[(r*npred-npred+1):(r*npred)] <- preds.sr.cri[,1]
    preds.sr.med[(r*npred-npred+1):(r*npred)] <- preds.sr.cri[,2]
    preds.sr.97.5[(r*npred-npred+1):(r*npred)] <- preds.sr.cri[,3]
    preds.sd.mean[(r*npred-npred+1):(r*npred)] <- apply(preds.sd.city, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
    preds.sd.2.5[(r*npred-npred+1):(r*npred)] <- preds.sd.cri[,1]
    preds.sd.med[(r*npred-npred+1):(r*npred)] <- preds.sd.cri[,2]
    preds.sd.97.5[(r*npred-npred+1):(r*npred)] <- preds.sd.cri[,3]
    
    print(city.num[r])
  }
  
  preds.urbEVI <- data.frame(
    "impervious" = rep(seq(min(data.site$Impervious), max(data.site$Impervious), length.out = npred), nline),
    "impervious_std" = rep(urban.pred, nline),
    "city" = rep(c(cities), each = npred),
    #"EVI" = rep(data.reg$EVI_av, each = npred),
    "EVI" = rep((pred.EVI*sd(data.reg$EVI_av)+mean(data.reg$EVI_av)), each = npred),
    "sr_med" = preds.sr.med,
    "sr_low95" = preds.sr.2.5,
    "sr_upp95" = preds.sr.97.5,
    "sd_med" = preds.sd.med,
    "sd_low95" = preds.sd.2.5,
    "sd_upp95" = preds.sd.97.5
  )
  
  # repeat predictions for other regional covariates
  # MAT
  for(r in 1:nline){  # loop for each line 
    # a design matrix that is remade for each line, using their specific regional covariate values
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
    
    preds.sr.city <- m.sr.mcmc[,1:12] %*% t(dm.urb) %>% exp()  # predict and exponentiate, because that's the reverse of the log-link
    preds.sd.city <- m.sd.mcmc[,1:12] %*% t(dm.urb) %>% exp()
    preds.sr.cri <- apply(preds.sr.city, 2, quantile, probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
    preds.sd.cri <- apply(preds.sd.city, 2, quantile, probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
    
    preds.sr.mean[(r*npred-npred+1):(r*npred)] <- apply(preds.sr.city, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
    preds.sr.2.5[(r*npred-npred+1):(r*npred)] <- preds.sr.cri[,1]
    preds.sr.med[(r*npred-npred+1):(r*npred)] <- preds.sr.cri[,2]
    preds.sr.97.5[(r*npred-npred+1):(r*npred)] <- preds.sr.cri[,3]
    preds.sd.mean[(r*npred-npred+1):(r*npred)] <- apply(preds.sd.city, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
    preds.sd.2.5[(r*npred-npred+1):(r*npred)] <- preds.sd.cri[,1]
    preds.sd.med[(r*npred-npred+1):(r*npred)] <- preds.sd.cri[,2]
    preds.sd.97.5[(r*npred-npred+1):(r*npred)] <- preds.sd.cri[,3]
    
    print(city.num[r])
  }
  
  preds.urbMAT <- data.frame(
    "impervious" = rep(seq(min(data.site$Impervious), max(data.site$Impervious), length.out = npred), nline),
    "impervious_std" = rep(urban.pred, nline),
    "city" = rep(c(cities), each = npred),
    #"MAT" = rep(data.reg$mat_av, each = npred),
    "MAT" = rep((pred.MAT*sd(data.reg$mat_av)+mean(data.reg$mat_av)), each = npred),
    "sr_med" = preds.sr.med,
    "sr_low95" = preds.sr.2.5,
    "sr_upp95" = preds.sr.97.5,
    "sd_med" = preds.sd.med,
    "sd_low95" = preds.sd.2.5,
    "sd_upp95" = preds.sd.97.5
  )
  
  # URB
  for(r in 1:nline){  # loop for each line 
    # a design matrix that is remade for each line, using their specific regional covariate values
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
    
    preds.sr.city <- m.sr.mcmc[,1:12] %*% t(dm.urb) %>% exp()  # predict and exponentiate, because that's the reverse of the log-link
    preds.sd.city <- m.sd.mcmc[,1:12] %*% t(dm.urb) %>% exp()
    preds.sr.cri <- apply(preds.sr.city, 2, quantile, probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
    preds.sd.cri <- apply(preds.sd.city, 2, quantile, probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
    
    preds.sr.mean[(r*npred-npred+1):(r*npred)] <- apply(preds.sr.city, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
    preds.sr.2.5[(r*npred-npred+1):(r*npred)] <- preds.sr.cri[,1]
    preds.sr.med[(r*npred-npred+1):(r*npred)] <- preds.sr.cri[,2]
    preds.sr.97.5[(r*npred-npred+1):(r*npred)] <- preds.sr.cri[,3]
    preds.sd.mean[(r*npred-npred+1):(r*npred)] <- apply(preds.sd.city, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
    preds.sd.2.5[(r*npred-npred+1):(r*npred)] <- preds.sd.cri[,1]
    preds.sd.med[(r*npred-npred+1):(r*npred)] <- preds.sd.cri[,2]
    preds.sd.97.5[(r*npred-npred+1):(r*npred)] <- preds.sd.cri[,3]
    
    print(city.num[r])
  }
  
  preds.urbURB <- data.frame(
    "impervious" = rep(seq(min(data.site$Impervious), max(data.site$Impervious), length.out = npred), nline),
    "impervious_std" = rep(urban.pred, nline),
    "city" = rep(c(cities), each = npred),
    #"URB" = rep(data.reg$urb_reg, each = npred),
    "URB" = rep((pred.URB*sd(data.reg$urb_reg)+mean(data.reg$urb_reg)), each = npred),
    "sr_med" = preds.sr.med,
    "sr_low95" = preds.sr.2.5,
    "sr_upp95" = preds.sr.97.5,
    "sd_med" = preds.sd.med,
    "sd_low95" = preds.sd.2.5,
    "sd_upp95" = preds.sd.97.5
  )
  
  # AGE
  for(r in 1:nline){  # loop for each line 
    # a design matrix that is remade for each line, using their specific regional covariate values
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
    
    preds.sr.city <- m.sr.mcmc[,1:12] %*% t(dm.urb) %>% exp()  # predict and exponentiate, because that's the reverse of the log-link
    preds.sd.city <- m.sd.mcmc[,1:12] %*% t(dm.urb) %>% exp()
    preds.sr.cri <- apply(preds.sr.city, 2, quantile, probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
    preds.sd.cri <- apply(preds.sd.city, 2, quantile, probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
    
    preds.sr.mean[(r*npred-npred+1):(r*npred)] <- apply(preds.sr.city, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
    preds.sr.2.5[(r*npred-npred+1):(r*npred)] <- preds.sr.cri[,1]
    preds.sr.med[(r*npred-npred+1):(r*npred)] <- preds.sr.cri[,2]
    preds.sr.97.5[(r*npred-npred+1):(r*npred)] <- preds.sr.cri[,3]
    preds.sd.mean[(r*npred-npred+1):(r*npred)] <- apply(preds.sd.city, c(2), function(x)   mean(x, na.rm=TRUE))    # posterior mean
    preds.sd.2.5[(r*npred-npred+1):(r*npred)] <- preds.sd.cri[,1]
    preds.sd.med[(r*npred-npred+1):(r*npred)] <- preds.sd.cri[,2]
    preds.sd.97.5[(r*npred-npred+1):(r*npred)] <- preds.sd.cri[,3]
    
    print(city.num[r])
  }
  
  preds.urbAGE <- data.frame(
    "impervious" = rep(seq(min(data.site$Impervious), max(data.site$Impervious), length.out = npred), nline),
    "impervious_std" = rep(urban.pred, nline),
    "city" = rep(c(cities), each = npred),
    #"AGE" = rep(data.reg$yrs_col, each = npred),
    "AGE" = rep((pred.AGE*sd(data.reg$yrs_col)+mean(data.reg$yrs_col)), each = npred),
    "sr_med" = preds.sr.med,
    "sr_low95" = preds.sr.2.5,
    "sr_upp95" = preds.sr.97.5,
    "sd_med" = preds.sd.med,
    "sd_low95" = preds.sd.2.5,
    "sd_upp95" = preds.sd.97.5
  )
  
  # add columns for the covariates
  #preds.sr.city$impervious <- impervious
  #preds.sr.city$impervious.std <- urb.pred
  #preds.sr.city$city <- rep(c(cities), each = npred)
  #preds.sr.city$EVI <- rep(data.reg$EVI_av, each = npred)
  #preds.sr.city$MAT <- rep(data.reg$mat_av, each = npred)
  #preds.sr.city$URB <- rep(data.reg$urb_reg, each = npred)
  #preds.sr.city$AGE <- rep(data.reg$yrs_col, each = npred)
  #colnames(preds.sr.city)[1:3] <- c("lower95", "median", "upper95")
  #preds.sr.city
  
  
  # Plot Relationships Across Regional EVI Gradient ====
  # To provide maximum contrast, compare a low EVI city (Phoenix) to high EVI city (Sanford) with similar temperatures
  data.plot <- preds.urbEVI
  
  plot.sr <- ggplot() +
    theme_bw() + 
    geom_smooth(data = data.plot, aes(x = impervious, y = sr_med, group = city), se = FALSE, lwd = 1.2, color = "gray20") +
    geom_smooth(data = data.plot, aes(x = impervious, y = sr_med, group = city, color = EVI), se = FALSE) +
    geom_ribbon(data = data.plot, aes(x = impervious, y = sr_med, group = city, ymin = sr_low95, ymax = sr_upp95, fill = EVI), alpha = 0.075) + 
    #gghighlight(city %in% c("safl", "phaz"))+
    scale_fill_distiller(palette = "BrBG", direction = 1)+
    scale_color_distiller(palette = "BrBG", direction = 1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(2, 6.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Richness")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  plot.sr
  
  #data.plot <- preds.sd.city
  plot.sd <- ggplot() +
    theme_bw() + 
    geom_smooth(data = data.plot, aes(x = impervious, y = sd_med, group = city), 
                se = FALSE, lwd = 1.2, color = "gray20") +
    geom_smooth(data = data.plot, aes(x = impervious, y = sd_med, group = city, color = EVI), 
                se = FALSE) +
    geom_ribbon(data = data.plot, aes(x = impervious, y = sd_med, group = city, ymin = sd_low95, ymax = sd_upp95, fill = EVI), 
                alpha = 0.075) + 
    #gghighlight(city %in% c("safl", "phaz"))+
    scale_fill_distiller(palette = "BrBG", direction = 1)+
    scale_color_distiller(palette = "BrBG", direction = 1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(5, 9.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Diversity")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    )
  plot.sd
  
  range(data.plot$EVI)
  
  
  ggsave("./figures/figure2e_urbanization_vs_richness_EVIcontrast.png",
         plot.sr,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  
  ggsave("./figures/figure2i_urbanization_vs_diversity_EVIcontrast.png",
         plot.sd,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)       
  
  
  
  # Plot Relationships Across Regional MAT Gradient ====
  # compare a low  MAT city (Salt Lake City) to high MAT city (Metro LA) with similar average EVI (0.19)
  data.plot <- preds.urbMAT
  
  plot.sr <- ggplot() +
    theme_bw() + 
    geom_smooth(data = data.plot, aes(x = impervious, y = sr_med, group = city), 
                se = FALSE, lwd = 1.2, color = "gray20") +
    geom_smooth(data = data.plot, aes(x = impervious, y = sr_med, group = city, color = MAT), 
                se = FALSE) +
    geom_ribbon(data = data.plot, aes(x = impervious, y = sr_med, group = city, ymin = sr_low95, ymax = sr_upp95, fill = MAT), 
                alpha = 0.075) + 
    #gghighlight(city %in% c("scut", "mela"))+
    scale_fill_distiller(palette = "RdYlBu", direction = -1)+
    scale_color_distiller(palette = "RdYlBu", direction = -1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(2, 6.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Richness")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  plot.sr
  
  plot.sd <- ggplot() +
    theme_bw() + 
    geom_smooth(data = data.plot, aes(x = impervious, y = sd_med, group = city), 
                se = FALSE, lwd = 1.2, color = "gray20") +
    geom_smooth(data = data.plot, aes(x = impervious, y = sd_med, group = city, color = MAT), 
                se = FALSE) +
    geom_ribbon(data = data.plot, aes(x = impervious, y = sd_med, group = city, ymin = sd_low95, ymax = sd_upp95, fill = MAT), 
                alpha = 0.075) + 
    #gghighlight(city %in% c("scut", "mela"))+
    scale_fill_distiller(palette = "RdYlBu", direction = -1)+
    scale_color_distiller(palette = "RdYlBu", direction = -1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(5, 9.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Diversity")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  plot.sd
  
  range(data.plot$MAT)
  
  ggsave("./figures/figure2f_urbanization_vs_richness_MATcontrast.png",
         plot.sr,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  
  ggsave("./figures/figure2j_urbanization_vs_diversity_MATcontrast.png",
         plot.sd,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  
  
  # Plot Relationships Across Regional URB Gradient ====
  # This is a little trickier to see, as there wasn't a substantial relationship
  # But, two cities with low URB (Salt Lake) and high URB (Chicago), but similar other covariates
  data.plot <- preds.urbURB
  
  plot.sr <- ggplot() +
    theme_bw() + 
    geom_smooth(data = data.plot, aes(x = impervious, y = sr_med, group = city), 
                se = FALSE, lwd = 1.2, color = "gray20") +
    geom_smooth(data = data.plot, aes(x = impervious, y = sr_med, group = city, color = URB), 
                se = FALSE) +
    geom_ribbon(data = data.plot, aes(x = impervious, y = sr_med, group = city, ymin = sr_low95, ymax = sr_upp95, fill = URB), 
                alpha = 0.075) + 
    #gghighlight(city %in% c("scut", "chil"))+
    scale_fill_distiller(palette = "PuOr", direction = 1)+
    scale_color_distiller(palette = "PuOr", direction = 1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(2, 6.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Richness")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  
  plot.sd <- ggplot() +
    theme_bw() + 
    geom_smooth(data = data.plot, aes(x = impervious, y = sd_med, group = city), 
                se = FALSE, lwd = 1.2, color = "gray20") +
    geom_smooth(data = data.plot, aes(x = impervious, y = sd_med, group = city, color = URB), 
                se = FALSE) +
    geom_ribbon(data = data.plot, aes(x = impervious, y = sd_med, group = city, ymin = sd_low95, ymax = sd_upp95, fill = URB), 
                alpha = 0.075) + 
    #gghighlight(city %in% c("scut", "chil"))+
    scale_fill_distiller(palette = "PuOr", direction = 1)+
    scale_color_distiller(palette = "PuOr", direction = 1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(5, 9.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Diversity")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  range(data.plot$URB)
  plot.sr
  plot.sd
  
  ggsave("./figures/figure2g_urbanization_vs_richness_URBcontrast.png",
         plot.sr,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  
  ggsave("./figures/figure2k_urbanization_vs_diversity_URBcontrast.png",
         plot.sd,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  
  # Plot Relationships Across Regional AGE Gradient ====
  # Again, not a substantial difference, even for the two cities that should have a maximum contrast
  # Newer city (Indianapolis) and older city (Wilmington) have similar EVI, MAT (though Indianapolis is slighly more urbanized)
  # there are cities with larger age differences, but they end up having very different EVI and MAT as well
  data.plot <- preds.urbAGE
  
  plot.sr <- ggplot() +
    theme_bw() + 
    geom_smooth(data = data.plot, aes(x = impervious, y = sr_med, group = city), 
                se = FALSE, lwd = 1.2, color = "gray20") +
    geom_smooth(data = data.plot, aes(x = impervious, y = sr_med, group = city, color = AGE), 
                se = FALSE) +
    geom_ribbon(data = data.plot, aes(x = impervious, y = sr_med, group = city, ymin = sr_low95, ymax = sr_upp95, fill = AGE), 
                alpha = 0.075) + 
    #gghighlight(city %in% c("inin", "wide"))+ 
    scale_fill_distiller(palette = "PRGn", direction = -1)+
    scale_color_distiller(palette = "PRGn", direction = -1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(2, 6.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Richness")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  
  plot.sd <- ggplot() +
    theme_bw() + 
    geom_smooth(data = data.plot, aes(x = impervious, y = sd_med, group = city), 
                se = FALSE, lwd = 1.2, color = "gray20") +
    geom_smooth(data = data.plot, aes(x = impervious, y = sd_med, group = city, color = AGE), 
                se = FALSE) +
    geom_ribbon(data = data.plot, aes(x = impervious, y = sd_med, group = city, ymin = sd_low95, ymax = sd_upp95, fill = AGE), 
                alpha = 0.075) + 
    #gghighlight(city %in% c("inin", "wide"))+
    scale_fill_distiller(palette = "PRGn", direction = -1)+
    scale_color_distiller(palette = "PRGn", direction = -1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(5, 9.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Diversity")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  range(data.plot$AGE)
  plot.sr
  plot.sd
  
  ggsave("./figures/figure2h_urbanization_vs_richness_AGEcontrast.png",
         plot.sr,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  
  ggsave("./figures/figure2l_urbanization_vs_diversity_AGEcontrast.png",
         plot.sd,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  
  
# Extended Data Figure: Within-City Covariates vs. Species Richness and Diversity ----
# 1) Predict across Urbanization Gradient
  npred <- 200    # how many values to predict across
  
  cov1 <- data.site$Impervious
  cov1.std <- data.site$imperv_std
  
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
  
  cov1 <- data.site$pd_undev
  cov1.std <- data.site$pd_undev_std
  
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
  
  cov1 <- data.site$cropland
  cov1.std <- data.site$cropland_std
  
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
    geom_errorbar(data = data.site, aes(x = Impervious, y = rich_mean, ymin = rich_2_5, ymax = rich_97_5), 
                  lwd = 0.3, color = calle[7], alpha = 0.3)+
    geom_point(data = data.site, aes(x = Impervious, y = rich_mean, ymin = rich_5, ymax = rich_95),
               col = calle[8])+
    geom_ribbon(data = preds_sr_urb, aes(x = cov1, y = median, ymin = lower95, ymax = upper95), 
                fill = calle[7], alpha = 0.5) +
    geom_smooth(data = preds_sr_urb, aes(x = cov1, y = median, ymin = lower95, ymax = upper95),
                se = FALSE, color = calle[8])+
    theme_bw() + 
    scale_y_continuous(labels = label_number(accuracy = 0.1), breaks = c(0, 3, 6, 9, 12)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(0, 12))+
    labs(x = "Local Urbanization \n(% Impervious Surface)", y = "Species Richness")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 16), 
          axis.title.y = element_text(face = "bold", size = 16),
    ) 
  
  plot1

  ggsave("./figures/extendeddata_figure1d_urbanization_vs_richness.png",
         plot1,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  
##### Figure 2b: Urbanization vs. Diversity #####
  plot2 <- ggplot() +
    geom_errorbar(data = data.site, aes(x = Impervious, y = rich_mean, ymin = hill1_2_5, ymax = hill1_97_5), 
                  lwd = 0.3, color = calle[7], alpha = 0.3)+
    geom_point(data = data.site, aes(x = Impervious, y = hill1_mean, ymin = hill1_2_5, ymax = hill1_97_5),
               col = calle[8])+
    geom_ribbon(data = preds_sd_urb, aes(x = cov1, y = median, ymin = lower95, ymax = upper95), 
                fill = calle[7], alpha = 0.5) +
    geom_smooth(data = preds_sd_urb, aes(x = cov1, y = median, ymin = lower95, ymax = upper95),
                se = FALSE, color = calle[8])+
    theme_bw() + 
    scale_y_continuous(labels = label_number(accuracy = 0.1), breaks = c(2, 4, 6, 8, 10, 12)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(3, 12))+
    labs(x = "Local Urbanization \n(% Impervious Surface)", y = "Species Diversity")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 16), 
          axis.title.y = element_text(face = "bold", size = 16),
    ) 
  
  plot2
  
  ggsave("./figures/extendeddata_figure1g_urbanization_vs_diversity.png",
         plot2,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  
  
  
  
##### Figure 2c: Patch Density vs. Richness #####
  plot1 <- ggplot() +
    geom_errorbar(data = data.site, aes(x = pd_undev, y = rich_mean, ymin = rich_2_5, ymax = rich_97_5), 
                  lwd = 0.3, color = calle[5], alpha = 0.3)+
    geom_point(data = data.site, aes(x = pd_undev, y = rich_mean, ymin = rich_2_5, ymax = rich_97_5),
               col = calle[5])+
    geom_ribbon(data = preds_sr_pd, aes(x = cov1, y = median, ymin = lower95, ymax = upper95), 
                fill = calle[2], alpha = 0.5) +
    geom_smooth(data = preds_sr_pd, aes(x = cov1, y = median, ymin = lower95, ymax = upper95),
                se = FALSE, color = calle[5])+
    theme_bw() + 
    scale_y_continuous(labels = label_number(accuracy = 0.1), breaks = c(0, 3, 6, 9, 12)) +
    coord_cartesian(xlim=c(0, max(data.site$pd_undev)), ylim=c(0, 12))+
    labs(x = "Local Patch Density \n(#/100 ha)", y = "Species Richness")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 16), 
          axis.title.y = element_text(face = "bold", size = 16),
    ) 
  
  plot1
  
  ggsave("./figures/extendeddata_figure1e_patchdensity_vs_richness.png",
         plot1,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  
  
  
##### Figure 2d: Patch Density vs. Diversity #####
  plot2 <- ggplot() +
    geom_errorbar(data = data.site, aes(x = pd_undev, y = hill1_mean, ymin = hill1_2_5, ymax = hill1_97_5), 
                  lwd = 0.3, color = calle[5], alpha = 0.3)+
    geom_point(data = data.site, aes(x = pd_undev, y = hill1_mean, ymin = hill1_2_5, ymax = hill1_97_5),
               col = calle[5])+
    geom_ribbon(data = preds_sd_pd, aes(x = cov1, y = median, ymin = lower95, ymax = upper95), 
                fill = calle[2], alpha = 0.5) +
    geom_smooth(data = preds_sd_pd, aes(x = cov1, y = median, ymin = lower95, ymax = upper95),
                se = FALSE, color = calle[5])+
    theme_bw() + 
    scale_y_continuous(labels = label_number(accuracy = 0.1), breaks = c(2, 4, 6, 8, 10, 12)) +
    coord_cartesian(xlim=c(0, max(data.site$pd_undev)), ylim=c(3, 12))+
    labs(x = "Local Patch Density \n(#/100 ha)", y = "Species Diversity")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 16), 
          axis.title.y = element_text(face = "bold", size = 16),
    ) 

  plot2
  
  ggsave("./figures/extendeddata_figure1h_patchdensity_vs_diversity.png",
         plot2,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  
  
  
##### Figure 2e: Agriculture vs. Richness #####
  plot1 <- ggplot() +
    geom_errorbar(data = data.site, aes(x = (cropland+0.0000000001)*100, y = rich_mean, ymin = rich_2_5, ymax = rich_97_5), 
                  lwd = 0.3, color = calle[6], alpha = 0.3)+
    geom_point(data = data.site, aes(x = (cropland+0.0000000001)*100, y = rich_mean, ymin = rich_2_5, ymax = rich_97_5),
               col = calle[9], position = "jitter")+
    geom_ribbon(data = preds_sr_ag, aes(x = cov1*100, y = median, ymin = lower95, ymax = upper95), 
                fill = calle[6], alpha = 0.5) +
    geom_smooth(data = preds_sr_ag, aes(x = cov1*100, y = median, ymin = lower95, ymax = upper95),
                se = FALSE, color = calle[9])+
    theme_bw() + 
    scale_y_continuous(labels = label_number(accuracy = 0.1), breaks = c(0, 3, 6, 9, 12)) +
    coord_cartesian(xlim=c(0, max(data.site$cropland*100)), ylim=c(0, 12)) +
    labs(x = "Local Agricultural Footprint \n(% Land Cover)", y = "Species Richness") +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 16), 
          axis.title.y = element_text(face = "bold", size = 16),
    ) 
  
  plot1
  
  ggsave("./figures/extendeddata_figure1f_agriculture_vs_richness.png",
         plot1,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  
  
  
##### Figure 2f: Agriculture vs. Diversity #####
  plot2 <- ggplot() +
    geom_errorbar(data = data.site, aes(x = (cropland+0.0000000001)*100, 
                                        y = hill1_mean, ymin = hill1_2_5, ymax = hill1_97_5), 
                  lwd = 0.3, color = calle[6], alpha = 0.3)+
    geom_point(data = data.site, aes(x = (cropland+0.0000000001)*100, y = hill1_mean, ymin = hill1_2_5, ymax = hill1_97_5),
               col = calle[9], position = "jitter")+
    geom_ribbon(data = preds_sd_ag, aes(x = cov1*100, y = median, ymin = lower95, ymax = upper95), 
                fill = calle[6], alpha = 0.5) +
    geom_smooth(data = preds_sd_ag, aes(x = cov1*100, y = median, ymin = lower95, ymax = upper95),
                se = FALSE, color = calle[9])+
    theme_bw() + 
    scale_y_continuous(labels = label_number(accuracy = 0.1), breaks = c(2, 4, 6, 8, 10, 12)) +
    coord_cartesian(xlim=c(0, max(data.site$cropland*100)), ylim=c(3, 12)) +
    labs(x = "Local Agricultural Footprint \n(% Land Cover)", y = "Species Diversity") +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 16), 
          axis.title.y = element_text(face = "bold", size = 16),
    )
  plot2
  
  ggsave("./figures/extendeddata_figure1i_agriculture_vs_diversity.png",
         plot2,
         width = 4,
         height = 4,
         units = "in",
         dpi = 300)
  

# Magnitude of Relationships with Within-City Covariates ----
  # In writing the results, it may be helpful to be able to make statements about the 
  # magnitude of the covariate effects, but written in real terms
  # e.g. the global-mean effect of urbanization on richness = -0.09, on diversity = -0.07
  mean(m.sr$sims.list$beta[,2])
  mean(m.sd$sims.list$beta[,2])
  # this is the negativity of the relationship when all other variables are held at their mean (zero)
  # what does that mean in terms of impervious surface?
  # what then, is the difference in occupancy at 0% impervious surface vs. 10% vs. 50%
  preds_sr_urb[1,]
  preds_sd_urb[1,]
  preds_sr_urb[200,]
  preds_sd_urb[200,]
  # percent change
  (preds_sr_urb$median[200] - preds_sr_urb$median[1])/preds_sr_urb$median[1]*100
  (preds_sd_urb$median[200] - preds_sd_urb$median[1])/preds_sd_urb$median[1]*100
  
  # How about patch density?
  mean(m.sr$sims.list$beta[,3])
  mean(m.sd$sims.list$beta[,3])
    preds_sr_pd[1,]
    preds_sd_pd[1,]
    preds_sr_pd[200,]
    preds_sd_pd[200,]
    #
    (preds_sr_pd$median[200] - preds_sr_pd$median[1])/preds_sr_pd$median[1]*100 # on average, <1 (0.85) additional species across the whole gradient
    (preds_sd_pd$median[200] - preds_sd_pd$median[1])/preds_sd_pd$median[1]*100
  
  # How about agricultural footprint?
    mean(m.sr$sims.list$beta[,4])
    mean(m.sd$sims.list$beta[,4])
    preds_sr_ag[1,]
    preds_sd_ag[1,]
    preds_sr_ag[200,]
    preds_sd_ag[200,]
    #
    (preds_sr_ag$median[200] - preds_sr_ag$median[1])/preds_sr_ag$median[1]*100 # on average, <1 (0.85) additional species across the whole gradient
    (preds_sd_ag$median[200] - preds_sd_ag$median[1])/preds_sd_ag$median[1]*100
  
   
# Magnitude of Relationships within Contrasting Cities ----
    # We also want to be able to make statements about trends wihin different cities
    # For instance, to back up statements such as 
    # "Species occupancy and diversity were most negatively related to urbanization in the warmest, least vegetated cities"
    # it may helpful to be able to say that the effect of urbanization was XX times greater in the least vegetated city (Phoenix) than in one of the greenest city with comparable temperature (Sanford)
    
    # For this, it may be helpful to predict across across ranges of actual values for each city
    nline <- length(cities)  # one line for each city
    npred <- 200               # in this case, we can predict across fewer values, for simplicity sace
    impervious <- rep(NA, npred*nline)
    urb.pred <- rep(NA, npred*nline)
    pd.pred <- rep(NA, npred*nline)
    ag.pred <- rep(NA, npred*nline)
    
    # Use standardized covariate values from each city
    for(i in 1:nline){
      city.select <- cities[i]
      r <- data.site %>%
        filter(city == city.select)
      impervious[(i*npred-npred+1):(i*npred)] <- seq(min(r$Impervious), max(r$Impervious), length.out = npred)
      urb.pred[(i*npred-npred+1):(i*npred)] <- seq(min(r$imperv_std), max(r$imperv_std), length.out = npred)
      pd.pred[(i*npred-npred+1):(i*npred)] <- rep(median(r$pd_undev_std), length.out = npred)
      ag.pred[(i*npred-npred+1):(i*npred)] <- rep(median(r$cropland_std), length.out = npred)
    }
    
    # a design matrix of the standardized covariate values
    dm.city.real <- cbind(
      1,# intercept
      urb.pred, #urban
      # to use real values of patch density and agriculture
      #pd.pred, # pd_undev
      #ag.pred, # cropland
      # to hold patch density and agriculture constant at their mean
      0,
      0,
      rep(data.reg$EVI_av_std, each = npred), # EVI
      urb.pred*rep(data.reg$EVI_av_std, each = npred),  #urb*EVI
      rep(data.reg$mat_av_std, each = npred), #mat,
      urb.pred*rep(data.reg$mat_av_std, each = npred), # urb*mat,
      rep(data.reg$urb_reg_std, each = npred), # URB,
      urb.pred*rep(data.reg$urb_reg_std, each = npred), # urb*URB
      rep(data.reg$yrs_col_std, each = npred), # AGE,
      urb.pred*rep(data.reg$yrs_col_std, each = npred) # urb*AGE
    )
    
    preds.sr.city <- m.sr.mcmc[,1:12] %*% t(dm.city.real) %>% exp()  # predict and exponentiate, because that's the reverse of the log-link
    preds.sd.city <- m.sd.mcmc[,1:12] %*% t(dm.city.real) %>% exp()
    preds.sr.cri <- apply(preds.sr.city, 2, quantile, probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
    preds.sd.cri <- apply(preds.sd.city, 2, quantile,  probs = c(0.025,0.5,0.975)) %>% t() %>% data.frame()
    
    
    
    data.plot.sr.city <- data.frame(
      "city" = rep(data.reg$city, each = npred),
      "EVI" = rep(data.reg$EVI_av, each = npred),
      "MAT" = rep(data.reg$mat_av, each = npred),
      "URB" = rep(data.reg$urb_reg, each = npred),
      "AGE" = rep(data.reg$yrs_col, each = npred),
     "impervious" = impervious,
     "mean" = apply(preds.sr.city, c(2), function(x)   mean(x, na.rm=TRUE)) ,   # posterior mean
     "lower95" = preds.sr.cri[,1],
     "median" = preds.sr.cri[,2],
     "upper95" = preds.sr.cri[,3]
    )
    
    data.plot.sd.city <- data.frame(
      "city" = rep(data.reg$city, each = npred),
      "EVI" = rep(data.reg$EVI_av, each = npred),
      "MAT" = rep(data.reg$mat_av, each = npred),
      "URB" = rep(data.reg$urb_reg, each = npred),
      "AGE" = rep(data.reg$yrs_col, each = npred),
      "impervious" = impervious,
      "mean" = apply(preds.sd.city, c(2), function(x)   mean(x, na.rm=TRUE)) ,   # posterior mean
      "lower95" = preds.sd.cri[,1],
      "median" = preds.sd.cri[,2],
      "upper95" = preds.sd.cri[,3]
    )
    
    # now that values have been predicted, let's compare the trends (see plots for these in the 'Spare Code' below)
    # we can do this simply by comparing the slope between the predicted values at minimum and maximum levels of impervious surface

    # First, compare a low EVI city (Phoenix) to high EVI city (Sanford) with similar temperatures
    t <- data.plot.sr.city %>% filter(city %in% c("safl")) %>% select(impervious, median)
    slope1 <- (t$median[npred]-t$median[1])/(t$impervious[npred]-t$impervious[1])   # rise over run
    t <- data.plot.sr.city %>% filter(city %in% c("phaz")) %>% select(impervious, median)
    slope2 <- (t$median[npred]-t$median[1])/(t$impervious[npred]-t$impervious[1])
    slope2/slope1
    
    t <- data.plot.sd.city %>% filter(city %in% c("safl")) %>% select(impervious, median)
    slope1 <- (t$median[npred]-t$median[1])/(t$impervious[npred]-t$impervious[1])
    t <- data.plot.sd.city %>% filter(city %in% c("phaz")) %>% select(impervious, median)
    slope2 <- (t$median[npred]-t$median[1])/(t$impervious[npred]-t$impervious[1])
    slope2/slope1
    
    
    # Temperature
    # compare a low  MAT city (Salt Lake City) to high MAT city (Metro LA) with similar average EVI (0.19)
    t <- data.plot.sr.city %>% filter(city %in% c("scut")) %>% select(impervious, median)
    (slope1 <- (t$median[npred]-t$median[1])/(t$impervious[npred]-t$impervious[1]))
    t <- data.plot.sr.city %>% filter(city %in% c("mela")) %>% select(impervious, median)
    (slope2 <- (t$median[npred]-t$median[1])/(t$impervious[npred]-t$impervious[1]))
    slope2/slope1
    
    t <- data.plot.sd.city %>% filter(city %in% c("scut")) %>% select(impervious, median)
    (slope1 <- (t$median[npred]-t$median[1])/(t$impervious[npred]-t$impervious[1]))
    t <- data.plot.sd.city %>% filter(city %in% c("mela")) %>% select(impervious, median)
    (slope2 <- (t$median[npred]-t$median[1])/(t$impervious[npred]-t$impervious[1]))
    slope2/slope1
    
    
    
# Spare Code ----
  #
  ##### Plot Cities with Contrasting EVI ====
  # To provide maximum contrast, compare a low EVI city (Phoenix) to high EVI city (Sanford) with similar temperatures
  data.plot <- data.plot.sr.city
  plot.sr <- ggplot() +
    theme_bw() + 
    geom_ribbon(data = data.plot, aes(x = impervious, y = median, group = city, ymin = lower95, ymax = upper95, fill = EVI), 
                alpha = 0.3) + 
    geom_smooth(data = data.plot, aes(x = impervious, y = median, group = city, color = EVI), 
                se = FALSE) +
    gghighlight(city %in% c("safl", "phaz"))+
    scale_fill_distiller(palette = "BrBG", direction = 1)+
    scale_color_distiller(palette = "BrBG", direction = 1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(5, 9.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Richness")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  plot.sr
  
  data.plot <- data.plot.sd.city
  plot.sd <- ggplot() +
    theme_bw() + 
    geom_ribbon(data = data.plot, aes(x = impervious, y = median, group = city, ymin = lower95, ymax = upper95, fill = EVI), 
                alpha = 0.3) + 
    geom_smooth(data = data.plot, aes(x = impervious, y = median, group = city, color = EVI), 
                se = FALSE) +
    gghighlight(city %in% c("safl", "phaz"))+
    scale_fill_distiller(palette = "BrBG", direction = 1)+
    scale_color_distiller(palette = "BrBG", direction = 1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(5, 9.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Diversity")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    )
  
  plot.sd
  
  
  ##### Plot Cities with Contrasting MAT ====
  # compare a low  MAT city (Salt Lake City) to high MAT city (Metro LA) with similar average EVI (0.19)
  data.plot <- data.plot.sr.city
  plot.sr <- ggplot() +
    theme_bw() + 
    geom_ribbon(data = data.plot, aes(x = impervious, y = median, group = city, ymin = lower95, ymax = upper95, fill = MAT), 
                alpha = 0.3) + 
    #geom_smooth(data = data.plot, aes(x = urban, y = median, group = city),se = FALSE, color = "grey70", lwd = 1.2) +
    geom_smooth(data = data.plot, aes(x = impervious, y = median, group = city, color = MAT), 
                se = FALSE) +
    gghighlight(city %in% c("edal", "mela"))+
    scale_fill_distiller(palette = "RdYlBu", direction = -1)+
    scale_color_distiller(palette = "RdYlBu", direction = -1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(5, 9.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Richness")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  plot.sr
  
  data.plot <- data.plot.sd.city
  plot.sd <- ggplot() +
    theme_bw() + 
    geom_ribbon(data = data.plot, aes(x = impervious, y = median, group = city, ymin = lower95, ymax = upper95, fill = MAT), 
                alpha = 0.3) + 
    geom_smooth(data = data.plot, aes(x = impervious, y = median, group = city, color = MAT), 
                se = FALSE) +
    gghighlight(city %in% c("edal", "mela"))+
    scale_fill_distiller(palette = "RdYlBu", direction = -1)+
    scale_color_distiller(palette = "RdYlBu", direction = -1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(5, 9.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Diversity")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  
  plot.sd
  

  
  ##### Plot Cities with Contrasting URB ====
  # This is a little trickier to see, as there wasn't a substantial relationship
  # But, two cities with low URB (Salt Lake) and high URB (Chicago), but similar other covariates
  data.plot <- data.plot.sr.city
  plot.sr <- ggplot() +
    theme_bw() + 
    geom_ribbon(data = data.plot, aes(x = impervious, y = median, group = city, ymin = lower95, ymax = upper95, fill = URB), 
                alpha = 0.3) + 
    geom_smooth(data = data.plot, aes(x = impervious, y = median, group = city, color = URB), 
                se = FALSE) +
    gghighlight(city %in% c("scut", "chil"))+
    scale_fill_distiller(palette = "PuOr", direction = 1)+
    scale_color_distiller(palette = "PuOr", direction = 1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(5, 9.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Richness")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  
  data.plot <- data.plot.sd.city
  plot.sd <- ggplot() +
    theme_bw() + 
    geom_ribbon(data = data.plot, aes(x = impervious, y = median, group = city, ymin = lower95, ymax = upper95, fill = URB), 
                alpha = 0.3) + 
    geom_smooth(data = data.plot, aes(x = impervious, y = median, group = city, color = URB), 
                se = FALSE) +
    gghighlight(city %in% c("scut", "chil"))+
    scale_fill_distiller(palette = "PuOr", direction = 1)+
    scale_color_distiller(palette = "PuOr", direction = 1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(5, 9.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Diversity")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  
  plot.sr
  plot.sd

  
  ##### Plot Cities with Contrasting AGE ====
  # Again, not a substantial difference, even for the two cities that should have a maximum contrast
  # Newer city (Indianapolis) and older city (Wilmington) have similar EVI, MAT (though Indianapolis is slighly more urbanized)
  # there are cities with larger age differences, but they end up having very different EVI and MAT as well
  data.plot <- data.plot.sr.city
  plot.sr <- ggplot() +
    theme_bw() + 
    geom_ribbon(data = data.plot, aes(x = impervious, y = median, group = city, ymin = lower95, ymax = upper95, fill = AGE), 
                alpha = 0.3) + 
    geom_smooth(data = data.plot, aes(x = impervious, y = median, group = city, color = AGE), 
                se = FALSE) +
    gghighlight(city %in% c("inin", "wide"))+ 
    scale_fill_distiller(palette = "PRGn", direction = -1)+
    scale_color_distiller(palette = "PRGn", direction = -1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(5, 9.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Richness")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  
  data.plot <- data.plot.sd.city
  plot.sd <- ggplot() +
    theme_bw() + 
    geom_ribbon(data = data.plot, aes(x = impervious, y = median, group = city, ymin = lower95, ymax = upper95, fill = AGE), 
                alpha = 0.3) + 
    geom_smooth(data = data.plot, aes(x = impervious, y = median, group = city, color = AGE), 
                se = FALSE) +
    gghighlight(city %in% c("inin", "wide"))+
    scale_fill_distiller(palette = "PRGn", direction = -1)+
    scale_color_distiller(palette = "PRGn", direction = -1)+
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    coord_cartesian(xlim=c(0, max(data.site$Impervious)), ylim=c(5, 9.5))+
    labs(x = "Urbanization \n(% Impervious Surface)", y = "Species Diversity")  +
    theme(axis.text.x = element_text(face = "bold", size = 14), 
          axis.text.y = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(face = "bold", size = 18), 
          axis.title.y = element_text(face = "bold", size = 18),
          legend.position = "none",
          legend.margin = margin(t = 5, r = 74, b = 5, l = 5, unit = "pt")  #top, right, bottom, left
    ) 
  
  plot.sr
  plot.sd

  
  
