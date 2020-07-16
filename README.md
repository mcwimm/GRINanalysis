# GRINanalysis

This repository contains all data and analyses of the manuscript entitled "Roots of cooperation...", in Review at Nature-plants 2020:

### 1) Data

This folder contains the raw data (csv-files) and processed data (rda-files) to run the analysis and create the figures.

**Raw data**

- FullData2019.csv
- GraftsComplete2019.csv

**Processed data**

  - LMtrees.rda: contains data (e.g. unique ID, coordinates, species, DBH, graft-status, …) for each tree in the study area. The dataframe is based on _FullData2019.csv_ and   _GraftsComplete2019.csv_
  - LMavis.rda: is a subset of LMtrees.rda containing only Avicennia germinans trees, but also additional information as the salinity group of the study plot and the cumulated grafting probability of each tree.
  - LMlinks.rda: contains all grafted pairs of trees with additional information like the distance between them or the group they belong to.
  - LMgroups.rda: contains all groups of grafted trees with additional information as the number of trees and the number of links within each group.


### 2) Analysis



### 3) Figures

This folder contains the scripts needed to reproduce all figures.
