# Required packages
packages = c(
   # data handling
   "tidyverse", "janitor", "sjmisc", "sjlabelled",
   # data visualization
   "ggplot2", "ggpubr", "ggforce", "ggcorrplot", "sjPlot", "qqplotr",
   # network packages
   "igraph", "poweRlaw",  
   # formatting packages
   "BBmisc", "scales", "signs", 
   # non-parametric tests (significance letters)
   "agricolae", 
   # gam building and plotting
   "mgcv", "gratia")
install.packages(packages)
lapply(packages, require, character.only = TRUE)
