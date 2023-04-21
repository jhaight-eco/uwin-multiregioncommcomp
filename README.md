# uwin-multiregioncommcomp

A repository that contains code and data associated with the submitted manuscript "Urbanization, climate, and species traits shape mammal communities from local to continental scales" (In Review)

This `README` file

Please direct all questions to Jeffrey Haight jdhaight.eco(at)gmail.com

Full author list:
Haight, J. D.,; S. J. Hall; M. Fidino; S. A. Adalsteinsson; A. A. Ahlers; J. Angstman; W. J. B. Anthonysamy; E. Biro; M. K. Collins; B. Dugelby; T. Gallo; A. M. Green; L. Hartley; M. J. Jordan; C. A. M. Kay; E. W. Lehrer; R. A. Long; B. MacDougall; S. B. Magle; D. E. Minier; C. Mowry; M. Murray; K. Nininger; M. E. Pendergast; K. R. Remine; T. Ryan; C. Salsbury; C. J. Schell; Ç. Șekercioğlu; C. J. Shier; K. C. Simon; C. C. St. Clair; T. Stankowich; C. J. Stevenson; L. Wayne; D. Will; J. Williamson; L. Wilson; A. J. Zellmer; J. S. Lewis



---
<div align="center"> <h3>data</h3> </div>

This contains three subfolders with all the data files used for conducting the analyses and producing the `figures`


**./data/modelinput/ModelInputData_UWIN_allspecies.csv**


**./data/modelinput/ModelInputData_UWIN_MRCM.RData**
Contains all the cleaned datasets necessary for fitting the Bayesian multi-city community occupancy model, including the following R objects:

| Object Name	| Description   |
|---------------------------|--------|
| ysum		| An array of by-day detections of each species with the dimensions of `species X maximum number of sites X city`	|
| yaug		| An array of by-day detections with the dimensions of `species X maximum number of sites X city`, including additional all-zero species for data augmentation. Since no data augmented species were included in final analysis, this is identical to ysum	|
| Z			| Naive species occurence matrix (1 = observed; 2 = not observed)	|
| w			| A matrix incicating membership of each species within each city's regional species pool.  |
| K_tot         	| A matrix including the number of repeat surveys at each site within each region (1 = species is  known to occur within the city and was detected during survey period; 0 = species is not known to occur within the city; NA = species is known to occur within the city, but was not detected during the survey period)	|
| n_sites		| A vector with the number of sites surveyed in each city	|
| n_region		| The number of regions (i.e., cities)	|
| n_species		| The number of species observed in each region (i.e., naive regional species richness)	|
| M			| The overall number of species detected across all cities, equal to the sum of all uniquely identifiable species. No non-detected species (augmented data) were included in our analysis) |
| impervious	| A matrix of values of the covariate `local urbanization` (percent impervious surface cover), grouped by city and standardized around each city's mean |
| hetero     	| A matrix of values of the covariate `local urbanization` (percent impervious surface cover), grouped by city and standardized around each city's mean |
| cropland		| A matrix of values of the covariate `local urbanization` (percent impervious surface cover), grouped by city and standardized around each city's mean |
| elton		| A dataframe of traits for the 37 study species, derived from the EltonTraits database (Wilman et al. 2016)	|
| pantheria		| A dataframe of traits for the 37 study species, derived from the PanTHERIA database (Jones et al. 2009)		|

Additional explanation of each R object is provided as comments within the script for fitting the model (./Rcode/1_1_Analysis_FittingMultiRegionCommunityModel.R)

---


---
<div align="center"> <h3>figures</h3> </div>

This contains the component figures produced using the R scripts below, which were combined to create the figures in the published. 


---
---
<div align="center"> <h3>Rcode</h3> </div>

This file contains all R script files used for conducting the analyses and producing the `figures`. There are a total of eight R scripts, numbered by the order in which they are to be run. Scripts 1_1 through 1_3 are for conducting the analyses, while 2_1 through 2_3 are for producing the figures components.

**./Rcode/1_1_Analysis_FittingMultiRegionCommunityModel.R**  
Script for using JAGS to fit the Bayesian multi-city, community occupancy model within R, using the R package `jagsUI`  

**./Rcode/1_1_jagsmodel_mrcm_site3_reg4_spp2_det0_regint4.R**  
The Bayesian multi-city, community occupancy model that we fit to the species occurrence data  

**./Rcode/1_2_Analysis_CommunityCompositionEstimation.R**  
Script for extracting posterior estimates of species presence from the multi-city community occupancy model and producing posterior estimates of community composition (species richness and diversity)  

**./Rcode/1_3_Analysis_FittingMetaanalysisModel.R**  
Script for using JAGS to fit the Bayesian log-normal model meta-analysis of community compostion (species richness and diversity) within R, using the R package `jagsUI`  

**./Rcode/1_3_jagsmodel_alpha.R**  
The log-normal model used for the Bayesian meta-analysis of community compostion estimates  

**./Rcode/2_1_Visualization_WithinCityOccupancy.R**  
Script for creating figures visualizing within-city relationships between community-average occupancy and th variables of local urbanization, local patch density, local agricultural footprint, as well as among-city variation in within-city composition-urbanization relationships  

**./Rcode/2_2_Visualization_WithinCityCommComp.R**  
Script for creating figures visualizing within-city relationships between local community composition (species richness and diversity) and the variables of local urbanization, local patch density, local agricultural footprint, as well as correlations between each pair of local variables and among-city variation in within-city composition-urbanization relationships  

**./Rcode/2_3_Visualization_AmongCityRichness.R**  
Script for creating figures visualizing relationships between among-city variation in regional species richness, as well as correlations between each pair of among-city variables  


---