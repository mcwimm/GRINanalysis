
# GRINanalysis (Grafted Root Interaction networks) - A VWStifftung funded project -

This repository contains data and analyses of the manuscript entitled "Roots of cooperation: Can root graft networks benefit trees under stress?", currently under Review at Nature-plants 2020.

Co-Authored by: AG Vovides, MC Wimmler, F Schrewe, T Balke, M Zwanzig, C Piou, E Delay, J López-Portillo and U Berger

The following lines briefly describe the files included in this repository.

The *setup.R* file installs all required packages at once.

### 1) Data

This folder contains the raw data (csv-files) and processed data (rda-files) to run the analysis and create the figures.

**Raw data**

- Networks.rda: graft data, contains only information of grafted trees in a from-to format, where trees in column "Tree1" in grafted to trees in column "Tree2" 

**Processed data**

  - LMtrees.rda: contains data (e.g. unique ID, coordinates, species, DBH, graft-status, …) for each tree in the study area, resulting from merging raw data bases of tree attributes (publicly available at https://doi.org/10.5525/gla.researchdata.657) and field mapping of root connections through root grafts (Networks.rda)
  - LMavis.rda: is a subset of LMtrees.rda containing only Avicennia germinans trees, but also additional information as the salinity group of the study plot and the cumulated grafting probability of each tree.
  - LMlinks.rda: contains all grafted pairs of trees with additional information like the distance between them or the group they belong to.
  - LMgroups.rda: contains all groups of grafted trees with additional information as the number of trees and the number of links within each group.
  - AlldistLines.rda and Plot_data.rda: contains node degree destribution 
  - data_gam.rda: contains gam model results


### 2) Analysis

This folder contains

- The multilevel logistic regression model described in the extended data section (*glmer_grafting.R*)
- The computations to extract the indices of competition (Hegyi) and neighborhood asymmetry (*neighborhood_gamm_slenderness.R*). The latter used on the Generalized additive mixed effects model (GAMM). This script also contains the necessary code to reproduce extended data Figures 3 and 4, as well as Supplementary information Figures 1 and 2.
- The comparison between power-law distribution and other random-process distribution functions (*PowerLawDist.R*).
- Descriptive statistics and simple correlations (*numbers.R*)

### 3) Figures

This folder contains the scripts needed to reproduce figures present in the manuscript (including those in Extended Data and Supplementary Information Files).  
Only the scripts to build Fig. 1 of the Main document are not yet included to date (17th July 2020), these was combined from 3 different R objects (Mexico's country map, Study site GIS image, and grafted tree maps) and were further combined using GIMP V.2.10. We are working on uploading such scripts and will be updated as soon as possible
