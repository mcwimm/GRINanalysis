# required packages
library(tidyverse)
library(BBmisc)
library(ggpubr)

# required data
load("./data/LMtrees.Rda") 
load("./data/LMavis.Rda") 
load("d4_data_fig2b.Rda")


######## Fig 2 A ########

fig2A = LM.avis %>% 
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
   guides(
          linetype=guide_legend(keywidth = 2.5, keyheight = 1),
          size = guide_legend(ncol = 2, byrow = F))

######## Fig 2 B ########

Fig2B <- ggplot(d4, aes(x = DBH, y = height,
                        fill = Condition, # colour
                        linetype = Condition)) + #shift between d2 (all trees) and d5, Trees with asymmetry
   geom_point(aes(size=AsymmNeighDBH), alpha=0.85, shape = 21)+
   ylim(-1,32)+
   xlab("Stem diameter (cm)") +
   ylab("Tree height (m)") +
   stat_smooth(method = 'nls',   aes(col = Condition),
               geom ="line", alpha=0.9, size = 1, span=0.7 ,                #Fit nls model
               method.args = list( formula = 'y~H*(1-exp(-a*x))',  
                                   start=c(H=20, a=0.2)), se=FALSE)+
   scale_linetype_manual(name="Condition", 
                         values = c("solid", "longdash" ), labels=c("Grafted", "Non-grafted"))+
   scale_size_continuous(name=expression(atop(bold("Neighbourhood"), paste(bold("asymmetry")))))+
   scale_fill_manual(name="Condition",
                     values = c("#d8b365", "#5ab4ac"),
                      breaks= c("Grafted", "Non-Grafted"),
                      labels=c("Grafted", "Non-grafted"))+
   scale_color_manual(name="Condition",
                     values = c("#d8b365", "#5ab4ac"),
                     breaks= c("Grafted", "Non-Grafted"),
                     labels=c("Grafted", "Non-grafted"))+
   theme(panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),size=12, colour = "black" ),
         axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),size=12, colour = "black"),
         axis.title.x = element_text(size=14, colour = "black"),
         axis.title.y = element_text(size=14, colour = "black"),
         axis.ticks.length=unit(-1.5, "mm"),
         axis.ticks = element_line(size = .5),
         axis.line = element_line(colour = 'black', size = 1),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.spacing.y = unit(0.45,"cm"),
         
         legend.margin = margin(t = 1, unit = "cm"),
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA)) +
   
   guides(fill = guide_legend(keywidth = 1, keyheight = 1),
          linetype=guide_legend(keywidth = 2.5, keyheight = 1),
          colour=guide_legend(keywidth = 2.5, keyheight = 2),
          size = guide_legend(ncol = 2, byrow = F))



######## MERGE ########

x11()

ggarrange(fig2A, Fig2B, widths = c(1.15, 1),
          labels = c("A", "B"),
          font.label = list(size = 16, color = "black"))


######## SAVE ########

ggsave(filename = "Fig2.svg")
ggsave(filename = "Fig2.pdf", device=cairo_pdf)






