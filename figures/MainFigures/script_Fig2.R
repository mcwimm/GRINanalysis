#### Required packages ####
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggpubr")) install.packages("ggpubr")   # ggarrange

#### Required data ####
load("./data/LMavis.Rda") 

fillcolors <- c("#260C7D", "#007D06")


#### Fig 2a ####
Fig2A = LM.avis[!is.na(LM.avis$fm.gmc.probs),] %>% 
   ggplot(.) +
   geom_line(aes(x = DBH, y = fm.gmc.probs, 
                 col = avi.dens, group = LOC),
             size = 1., alpha = 1) +
   geom_point(aes(x = DBH, y = fm.gmc.probs, 
                  shape = sal.group, fill = avi.dens),
              col = "black",
              alpha = 0.6, size = 2.8) +
   labs(x = "Stem diameter (cm)",
        y = "P(grafting)",
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
         legend.title = element_text(size = 14),
         legend.margin = margin(t = 1, unit = "cm"),
         legend.spacing.y = unit(0.45,"cm"),
         
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA)) +
   guides(linetype=guide_legend(keywidth = 2.5, keyheight = 1),
          size = guide_legend(ncol = 2, byrow = F))

#### Fig 2b ####
Fig2B <- LM.avis %>% 
   filter(complete.cases(AsymmNeighDBH)) %>% 
   ggplot(., aes(x = DBH, y = height, 
                 colour = groupBin, 
                 # shift between d2 (all trees) and d5, Trees with asymmetry
                 linetype = groupBin)) + 
   geom_point(aes(size=AsymmNeighDBH), alpha=0.5)+
   ylim(-1,32)+
   xlim(0,68)+
   xlab("Stem diameter (cm)") +
   ylab("Tree height (m)") +
   stat_smooth(method = 'nls', geom="line", alpha=0.9, size=1, span=0.7,   
               #Fit nls model
               method.args = list( formula = 'y~H*(1-exp(-a*x))',  
                                   start=c(H=20, a=0.2)), se=FALSE)+
   scale_linetype_manual(name="Condition", 
                         values = c("solid", "longdash" ),
                         labels=c("Grafted", "Non-grafted"))+
   scale_size_continuous(name=expression(atop(bold("Neighbourhood"), 
                                              paste(bold("asymmetry")))))+
   scale_color_manual(name="Condition",
                      values= c("#260C7D", "#007D06"),
                      breaks= c("grafted", "non-grafted"),
                      labels=c("Grafted", "Non-grafted"))+
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
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA))+
   
   guides(fill = guide_legend(keywidth = 1, keyheight = 1),
          linetype=guide_legend(keywidth = 2.5, keyheight = 1),
          colour=guide_legend(keywidth = 2.5, keyheight = 2),
          size = guide_legend(ncol = 2, byrow = F))

#### Fig 2c ####
Fig2C <- LM.avis %>% 
   filter(complete.cases(AsymmNeighDBH)) %>% 
   ggplot(., aes(x = DBH, y = slenderness, 
                 linetype = groupBin,
                 col = groupBin, fill = groupBin,
                 size = AsymmNeighDBH)) + 
   geom_point(shape = 21, alpha = 0.5)+
   labs(x = "Stem diameter (cm)", y = "Slenderness")+
   geom_smooth(method = "lm", se=F)+
   scale_color_manual(values = fillcolors) +
   scale_fill_manual(values = fillcolors)+
   scale_x_sqrt(breaks = c(2,5,10,15,25,35,45,60))+ 
   scale_y_sqrt()+
   scale_linetype_manual(name="Condition", 
                         values = c("solid", "longdash" ), 
                         labels=c("Grafted", "Non-grafted"))+
   labs(y = "Slenderness",
        x = "Stem diameter (cm)",
        fill = "Condition",
        col = "Condition") +
   
   theme(panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                    size=12, colour = "black" ),
         axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                    size=12, colour = "black"),
         axis.title.x = element_text(size=14, colour = "black"),
         axis.title.y = element_text(size=14, colour = "black"),
         axis.ticks.length=unit(-1.5, "mm"),
         axis.line = element_line(colour = 'black', size = 1),
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA))+
   guides(colour=guide_legend(keywidth = 2.5, keyheight = 1),
      size = guide_legend(ncol = 2, byrow = F))



#### Merge figures ####
Fig2BC <- ggarrange(Fig2B, Fig2C, widths=c(1,1.1), heights=1,
                    common.legend = TRUE, labels = c("b)","c)"),
                    legend = "bottom")

Fig2 <- ggarrange(Fig2A, Fig2BC, nrow = 2,
                  heights=c(1,1), labels="a)")

#### Save file ####
tiff("figures/Fig2.tiff", width = 2000, height = 3000, res=300)
annotate_figure(Fig2,
                top = text_grob(paste0("Fig. 2: Probability of rafting and allometric differences\nbetween grafted and non-grafted trees\n"),
                                color = "black", face = "bold", 
                                size = 14, hjust = 0, x=0.005,just="left"))
dev.off()
