# uwin-multiregioncommcomp

A repository that contains code and data associated with the submitted manuscript "Urbanization, climate, and species traits shape mammal communities from local to continental scales" (In Review)

This `README` file

Please direct all questions to Jeffrey Haight jdhaight.eco(at)gmail.com

Full author list:
Haight, J. D.,; S. J. Hall; M. Fidino; S. A. Adalsteinsson; A. A. Ahlers; J. Angstman; W. J. B. Anthonysamy; E. Biro; M. K. Collins; B. Dugelby; T. Gallo; A. M. Green; L. Hartley; M. J. Jordan; C. A. M. Kay; E. W. Lehrer; R. A. Long; B. MacDougall; S. B. Magle; D. E. Minier; C. Mowry; M. Murray; K. Nininger; M. E. Pendergast; K. R. Remine; T. Ryan; C. Salsbury; C. J. Schell; Ç. Șekercioğlu; C. J. Shier; K. C. Simon; C. C. St. Clair; T. Stankowich; C. J. Stevenson; L. Wayne; D. Will; J. Williamson; L. Wilson; A. J. Zellmer; J. S. Lewis



---
<div align="center"> <h3>data</h3> </div>

---


---
<div align="center"> <h3>figures</h3> </div>
---
This contains the component figures produced using the R scripts below, which were combined to create the figures in the published. 



---
<div align="center"> <h3>R</h3> </div>
---
This file contains all R code used for conducting the analyses and producing the figures. There are a total of eight R scripts, numbered by the order in which they are to be run. Scripts 1_1 through 1_3 are for conducting the analyses, while 2_1 through 2_3 are for producing the figures components.

`1_1_Analysis_FittingMultiRegionCommunityModel.R`
The code for fitting the Bayesian multi-city, community occupancy model with JAGS, using the R package `jagsUI`

`1_1_jagsmodel_mrcm_site3_reg4_spp2_det0_regint4.R`
The Bayesian multi-city, community occupancy model that we fit to the data

`1_2_Analysis_CommunityCompositionEstimation.R`


`1_3_Analysis_FittingMetaanalysisModel.R`


`1_3_jagsmodel_alpha.R`


`2_1_Visualization_WithinCityOccupancy.R`


`2_2_Visualization_WithinCityCommComp.R`


`2_3_Visualization_AmongCityRichness.R`



---