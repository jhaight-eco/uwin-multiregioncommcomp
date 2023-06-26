# uwin-multiregioncommcomp

A repository that contains code and data associated with the manuscript "Urbanization, climate, and species traits shape mammal communities from local to continental scales"

This `README` file includes information on the various scripts and datasets used for this analysis, though not every data source is saved in this repository (e.g., Bayesian multi-city, community occupancy model output; GIS data). The manuscript includes citation of all locations from which spatially-linked within-city and among-city covariates were derived.

Please direct all questions to Jeffrey Haight jdhaight.eco(at)gmail.com

Full author list:
Haight, J. D.,; S. J. Hall; M. Fidino; S. A. Adalsteinsson; A. A. Ahlers; J. Angstman; W. J. B. Anthonysamy; E. Biro; M. K. Collins; B. Dugelby; T. Gallo; A. M. Green; L. Hartley; M. J. Jordan; C. A. M. Kay; E. W. Lehrer; R. A. Long; B. MacDougall; S. B. Magle; D. E. Minier; C. Mowry; M. Murray; K. Nininger; M. E. Pendergast; K. R. Remine; T. Ryan; C. Salsbury; C. J. Schell; Ç. Șekercioğlu; C. J. Shier; K. C. Simon; C. C. St. Clair; T. Stankowich; C. J. Stevenson; L. Wayne; D. Will; J. Williamson; L. Wilson; A. J. Zellmer; J. S. Lewis



---
<div align="center"> <h3>data</h3> </div>

This contains three subfolders with all the data files used for conducting the analyses and producing the `figures`:  
**`modelinput`**  
**`modeloutput`**   
**`modelsummary`**  

Within **`modelinput`**, there are two files:  
**./data/modelinput/ModelInputData_UWIN_allspecies.csv**  
Contains data of species-level attributes, including each species' names, taxonomic groupings, and species traits (body mass, log-transformed as 'logmass'; carnivory). Species traits were derived from the EltonTraits 1.0 database (https://doi.org/10.1890/13-1917.1). Columns 'atga' through 'wide' represent each species' known membership in each city's regional species pool, with 1 indicating that the city is part of that species' geographic range. The column 'city_occ' indicates the number of regional species pools each species is a member of. 


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
| impervious	| A matrix of values of the covariate `local urbanization` (mean percent impervious surface cover), grouped by city and standardized around each city's mean |
| hetero     	| A matrix of values of the covariate `local patch density` (density of non-urban, non-agricultural land cover patches), grouped by city and standardized around each city's mean |
| cropland		| A matrix of values of the covariate `local urbanization` (proportional coverage of agricultural land cover patches), grouped by city and standardized around each city's mean |
| data_reg		| A dataframe containing city-level attributes, including the standardized among-city covariates `regional greenness` (EVI_av_std), `regional temperature` (mat_av_std), `regional urbanization` (urb_reg_std), and `regional city age` (yrs_col_std) |
| elton		| A dataframe of traits for the 37 study species, derived from the EltonTraits database (Wilman et al. 2016)	|
| pantheria		| A dataframe of traits for the 37 study species, derived from the PanTHERIA database (Jones et al. 2009)		|

Additional explanation of each R object is provided as comments within the script for fitting the model (**./Rcode/1_1_Analysis_FittingMultiRegionCommunityModel.R**).  
  
  
  
Within **`modeloutput`**, there are two files:  
**model2output_logglm_hill0_sample60k.rds**  
**model3output_logglm_hill1_sample60k.rds**  
Contain outputs of the Bayesian meta-analysis models of community composition (species richness and diversity), in the form of R objects output by the 'jags()' function.run in the R script **./Rcode/1_3_Analysis_FittingMetaanalysisModel**. Note that the model output file **model1output_mrcm_globalinteractionmodel_sample60k.rds** is not included in this repository, as the file is too large. This file can be replicated by fitting the Bayesian multi-city community occupancy model code using the R script **./Rcode/1_1_Analysis_FittingMultiRegionCommunityModel.R** and can be shared by the lead author via an email request to jdhaight.eco(at)gmail.com.  
  
  
  
Within **`modelsummary`**, there are five files:  

**data_sites_mrcmsummary.csv** and **data_sites_metadata.xlsx**  
A .csv file containing all site-level data used in analyses, accompanied by an .xlsx metadata spreadsheet describing all column names

**supplementarydata1_summarytables_speciesinfo.xlsx**  
A copy of Supplementary Data 1, an .xlsx spreadsheet depicign full lists of modeled species- and community-level effect parameters and species trait information, with effect parameters  estimated using a Bayesian multi-region community occupancy model (MRCM) and meta-analysis models of community composition (species richness and diversity).

**data_cities_mrcmsummary.csv** and **data_cities_metadata.xlsx**  
A .csv file containing all cit-level data used in analyses, accompanied by an .xlsx metadata spreadsheet describing all column names


---


---
<div align="center"> <h3>figures</h3> </div>

This folder contains all images utilized in the production of the manuscript figures. This folder includes partial produced using the R scripts below, which were subsequently combined within Inkscape (https://inkscape.org/) to create the figures in the published manuscript. 

This folder also includes the subfolder **./figures/mammalgraphics** which contains image files used to represent mammals species in Figure 1 of the manuscript. All mammal graphics were sourced from PhyloPic (https://www.phylopic.org/) and were utilized as part of the public domain (https://creativecommons.org/publicdomain/zero/1.0/) or under the Creative Commons Attribution 3.0 license (https://creativecommons.org/licenses/by/3.0/). Mammal graphics are accompanied by the text file **./figures/mammalgraphics/imageattributions.txt**, which specifies each image's source and provides attribution for each image.


---
---
<div align="center"> <h3>Rcode</h3> </div>

This folder contains all R script files used for conducting the analyses that produce the `modeloutput` and `modelsummary` data and the `figures`. There are a total of eight R scripts, numbered by the order in which they are to be run. Scripts 1_1 through 1_3 are for conducting the analyses, while 2_1 through 2_3 are for producing the figures components.

**./Rcode/1_1_Analysis_FittingMultiRegionCommunityModel.R**  
Script for using JAGS to fit the Bayesian multi-city, community occupancy model within R, using the R package `jagsUI`  

**./Rcode/1_1_jagsmodel_mrcm_site3_reg4_spp2_det0_regint4.R**  
The Bayesian multi-city, community occupancy model that we fit to the species occurrence data  

**./Rcode/1_2_Analysis_CommunityCompositionEstimation.R**  
Script for extracting posterior estimates of species presence from the multi-city community occupancy model and producing posterior estimates of community composition (species richness and diversity)  

**./Rcode/1_3_Analysis_FittingMetaanalysisModel.R**  
Script for using JAGS to fit the Bayesian log-normal meta-analysis models of community compostion (species richness and diversity) within R, using the R package `jagsUI`  

**./Rcode/1_3_jagsmodel_alpha.R**  
The log-normal model used for the Bayesian meta-analysis of community compostion estimates  

**./Rcode/2_1_Visualization_WithinCityOccupancy.R**  
Script for creating figures visualizing within-city relationships between community-average occupancy and th variables of local urbanization, local patch density, local agricultural footprint, as well as among-city variation in within-city composition-urbanization relationships  

**./Rcode/2_2_Visualization_WithinCityCommComp.R**  
Script for creating figures visualizing within-city relationships between local community composition (species richness and diversity) and the variables of local urbanization, local patch density, local agricultural footprint, as well as correlations between each pair of local variables and among-city variation in within-city composition-urbanization relationships  

**./Rcode/2_3_Visualization_AmongCityRichness.R**  
Script for creating figures visualizing relationships between among-city variation in regional species richness, as well as correlations between each pair of among-city variables  


---
