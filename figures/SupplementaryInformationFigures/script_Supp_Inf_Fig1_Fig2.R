#### Required packages ####
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("tidyverse")) install.packages("tidyverse")

#### Required data ####
load("./data/LMavis.Rda") 

#### SI Figure 1 #####
SI_Fig1A <- ggplot(data = LM.avis, aes(x = Hegyi, y = DBH, 
                                      color = Condition, size=Hegyi)) +
   geom_point(alpha=0.5) +
   xlab("Stem diameter (cm)") +
   ylab("Tree height (m)")+
   scale_x_continuous(expression(paste( ~italic(CI))),
                      limits = c(0, 30))+
   scale_y_continuous(expression(paste("Stem diameter", " (cm)")), 
                      limits = c(0, 60))+
   scale_color_manual(name="Condition",
                      values=c("#008080","#800080"),
                      breaks= c("grafted", "non-grafted"),
                      labels=c("Grafted", "Non-grafted"))+
   scale_size(expression(paste( ~italic(CI))),
              breaks = c(0, 10, 20, 30,40))+
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


SI_Fig1B <- ggplot(data = LM.avis, aes(x =DBH, y = height, 
                                     color = Condition,size=Hegyi)) +
   geom_point(alpha=0.5) +
   xlab("Stem diameter (cm)") +
   ylab("Tree height (m)")+
   scale_color_manual(name="Condition",
                      values=c("#008080","#800080"),
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

#### Save file ####
tiff("figures/Sup_Info_Figure1.tiff", height = 1500, width = 2000, res=300)
ggarrange(SI_Fig1A, SI_Fig1B, 
          common.legend = TRUE, legend = "bottom",
          labels= c("a)","b)"))
dev.off()

# ggsave(filename = "SupFig1.pdf", width = 10, height = 6)


#### SI Figure 2 ####
# shift between d2 (all trees) and d5, Trees with asymmetry
SI_Fig2A <- ggplot(LM.avis, aes(x = AsymmNeighDBH, y = DBH, colour=Condition)) + 
   geom_point(aes(size=AsymmNeighDBH), alpha=0.5)+
   xlab("Neighbourhood asymmetry index") +
   ylab("Stem diameter (cm)") +
   scale_size_continuous(name=expression(atop(bold("Neighbourhood"), 
                                              paste(bold("asymmetry")))))+
   scale_color_manual(name="Condition",
                      values=c("#008080","#800080"),
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

SI_Fig2B <- ggplot(data = LM.avis, aes(x =DBH, y = height, 
                                       color = Condition, 
                                       size = AsymmNeighDBH)) +
   geom_point(alpha=0.5) +
   xlab("Stem diameter (cm)") +
   ylab("Tree height (m)")+
   scale_size_continuous(name=expression(atop(bold("Neighbourhood"),
                                              paste(bold("asymmetry")))))+
   scale_color_manual(name="Condition",
                      values=c("#008080","#800080"),
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

#### Merge figures ####
Sup_Info_Figure2 <- ggarrange(SI_Fig2A, SI_Fig2B, 
                              widths = c(0.53, 0.47),
                              common.legend = TRUE, legend = "bottom",
                              labels = c("a)","b)"),
                              heights = 1)

#### Save file ####
tiff("figures/Sup_Info_Figure2.tiff", width=2000, height=1500, res=300)
Sup_Info_Figure2
dev.off()

# ggsave(filename = "SupFig2.pdf", width = 10, height = 6)
