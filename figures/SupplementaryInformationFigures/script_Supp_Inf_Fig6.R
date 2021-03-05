#### Required packages ####
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggcorrplot")) install.packages("ggcorrplot")

#### Required data ####
load("./data/LMavis.Rda") 

#### Prepare data ####
# Select desired rows (location, densities, salinity) from data frame
d = LM.avis %>% 
   distinct(LOC, tot.dens, avi.dens, salinity) %>% 
   mutate(r_avi = avi.dens / tot.dens)

# Compute correlation matrix
correlations <- cor(d[, -1],
                    use = "complete.obs")

# Calculate p-values
res1 <- cor_pmat(d[, -1])


#### SI Figure 6 ####
Supp_Inf_Fig6 <- ggcorrplot(correlations, 
           method = "circle", type = "upper",
           colors = c("#6D9EC1", "white", "#E46726"),
           legend.title = "Correlation",
           outline.color = "#A9A9A9",
           lab = F,
           p.mat = res1) +
   scale_x_discrete(labels = c("Total stand density",
                               expression(paste("Total ",
                                                italic(" A. g."), " density")),
                               "Salinity")) +
   scale_y_discrete(labels = c(expression(paste("Total ",
                                                italic(" A. g."), " density")),
                               "Salinity",
                               expression(paste("Relative ",
                                                italic(" A. g."), " density")))) +
   theme(panel.grid.major = element_line(size = 0.5, 
                                         linetype = 'solid',
                                         colour = "#A9A9A9"))

#### Save file ####
tiff("figures/Sup_Info_Figure6.tiff", width = 2000, height = 2000, res=300)
Supp_Inf_Fig6
dev.off()
