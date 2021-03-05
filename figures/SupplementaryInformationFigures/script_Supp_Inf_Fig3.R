#### Required packages ####
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("BBmisc")) install.packages("BBmisc")

#### Required data ####
load("./data/LMavis.Rda") 


#### SI Figure 3  #####
Supp_Inf_Fig3 = LM.avis %>%
   group_by(LOC) %>% 
   mutate(DBHnorm = normalize(DBH, method = "range", range = c(0, 1))) %>% 
   ggplot(.) +
   geom_line(aes(x = DBHnorm, y = obs.rf, 
                 col = avi.dens, group = LOC),
             size = 1., alpha = 1) +
   geom_point(aes(x = DBHnorm, y = obs.rf, 
                  shape = sal.group, fill = avi.dens),
              col = "black",
              alpha = 0.6, size = 2.8) +
   labs(x = "Normalized stem diameter (-)",
        y = "Cumulative frequency of grafted trees",
        shape = "Salinity (ppt)",
        fill = "Stand density \n(trees per hectare)",
        col = "Stand density \n(trees per hectare)") +
   scale_shape_manual(values = c(21, 22, 24)) +
   scale_color_gradient(low = "chartreuse3", high = "black",
                        breaks = c(300, 611, 900)) +
   scale_fill_gradient(low = "chartreuse3", high = "black",
                       breaks = c(300, 611, 900)) +
   theme(panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                    size=12, colour = "black" ),
         axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                    size=12, colour = "black"),
         axis.title.x = element_text(size=14, colour = "black"),
         axis.title.y = element_text(size=14, colour = "black"),
         axis.ticks.length=unit(-1.5, "mm"),
         axis.ticks = element_line(size = .5),
         axis.line = element_line(colour = 'black', size = 1),
         legend.text = element_text(size = 12),
         legend.spacing.y = unit(0.45,"cm"),
         legend.title = element_text(size = 14),
         legend.margin = margin(t = 1, unit = "cm"),
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA)) +
   guides(
      linetype=guide_legend(keywidth = 2.5, keyheight = 1),
      size = guide_legend(ncol = 2, byrow = F))


#### Save file ####
tiff("figures/Sup_Info_Figure3.tiff", width = 1700, height = 2100, res=300)
Supp_Inf_Fig3
dev.off()
