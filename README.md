# GRINanalysis

This repository contains data and analyses of the manuscript entitled "Roots of cooperation: Can root graft networks benefit trees under stress?", currently under Review at Nature-plants 2020.

The following lines detail de information included in this repository

### 1) Data

This folder contains the raw data (csv-files) and processed data (rda-files) to run the analysis and create the figures.

**Raw data**

- FullData2019clean.csv: tree data
- links2019.csv: graft data

**Processed data**

  - LMtrees.rda: contains data (e.g. unique ID, coordinates, species, DBH, graft-status, …) for each tree in the study area. The dataframe is based on _FullData2019.csv_ and   _GraftsComplete2019.csv_
  - LMavis.rda: is a subset of LMtrees.rda containing only Avicennia germinans trees, but also additional information as the salinity group of the study plot and the cumulated grafting probability of each tree.
  - LMlinks.rda: contains all grafted pairs of trees with additional information like the distance between them or the group they belong to.
  - LMgroups.rda: contains all groups of grafted trees with additional information as the number of trees and the number of links within each group.


### 2) Analysis

This folder contains

- The multilevel logistic regression model described in the extended data section (_glmer_grafting.R_)
- The computations to extract the indices of competition (Hegyi) and neighbourhood asymmetri, the latter used on the Generalised additive mixed effects model (GAMM). This script also contains the necesary code to repoduce Figures 2B from the Main document, Extended data Figures 3 and 4, as well as Supplementary information Figures 1 and 2 

### 3) Figures

This folder contains the scripts needed to reproduce all figures.
