# required packages
library(tidyverse)
library(ggpubr)

# required data
load("./data/LMtrees.Rda") 
load("./data/LMavis.Rda") 

fillcolors<-c("#260C7D", "#007D06")


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
Fig2B<-ggplot(LM.avis, aes(x = DBH, y = height, colour=Condition, 
                           linetype=Condition)) + #shift between d2 (all trees) and d5, Trees with asymmetry
   geom_point(aes(size=AsymmNeighDBH), alpha=0.5)+
   ylim(-1,32)+
   xlim(0,60)+
   xlab("Stem diameter (cm)") +
   ylab("Tree height (m)") +
   stat_smooth(method = 'nls',   geom="line", alpha=0.9, size=1, span=0.7 ,                #Fit nls model
               method.args = list( formula = 'y~H*(1-exp(-a*x))',  
                                   start=c(H=20, a=0.2)), se=FALSE)+
   scale_linetype_manual(name="Condition", values = c("solid", "longdash" ), labels=c("Grafted", "Non-grafted"))+
   scale_size_continuous(name=expression(atop(bold("Neighbourhood"), paste(bold("asymmetry")))))+
   scale_color_manual(name="Condition",
                      values= c("#260C7D", "#007D06"),
                      breaks= c("grafted", "non-grafted"),
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
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA))+
   
   guides(fill = guide_legend(keywidth = 1, keyheight = 1),
          linetype=guide_legend(keywidth = 2.5, keyheight = 1),
          colour=guide_legend(keywidth = 2.5, keyheight = 2),
          size = guide_legend(ncol = 2, byrow = F))

######## Fig 2 C ########

Fig2C<-ggplot(LM.avis, aes(x=DBH, y=slenderness,fill = Condition,
                           linetype=Condition,
                      col = Condition, size=AsymmNeighDBH)) + 
   geom_point(shape = 21, alpha = 0.5)+
   labs(x = "Stem diameter (cm)", y = "Slenderness")+
   geom_smooth(method = "lm", se=F)+
   scale_color_manual(values = fillcolors) +
   scale_fill_manual(values = fillcolors)+
   scale_x_sqrt(breaks = c(2,5,10,15,25,35,45,60))+ 
   scale_y_sqrt()+
   scale_linetype_manual(name="Condition", values = c("solid", "longdash" ), labels=c("Grafted", "Non-grafted"))+
   labs(y = "Slenderness",
        x = "Stem diameter (cm)",
        fill = "Condition",
        col = "Condition")+
   ### The equation annotations are based on the model (sqrt(Slenderness~Condition*sqrt(dbh))), they are the same as the authomatically computed by ggplot's "lm" function
   #annotate("text", x=43.3,y=180, label=paste("y=",round(coef(Smodel3)[1]+coef(Smodel3)[2]),round(SlopeNG,digits=2),"x, p < 0.01" ), col="#007D06")+ # non-grafted
   #annotate("text", x=45,y=160, label=paste("y=",round(coef(Smodel3)[1]),round(coef(Smodel3)[3],digits=2),"x, p < 0.001" ), col="#260C7D")+ # non-grafted
   theme(panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),size=12, colour = "black" ),
         axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),size=12, colour = "black"),
         axis.title.x = element_text(size=14, colour = "black"),
         axis.title.y = element_text(size=14, colour = "black"),
         axis.ticks.length=unit(-1.5, "mm"),
         axis.line = element_line(colour = 'black', size = 1),
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA))+
   
   guides(#fill = guide_legend(keywidth = 1, keyheight = 1),
      #linetype=guide_legend(keywidth = 2.5, keyheight = 1),
      colour=guide_legend(keywidth = 2.5, keyheight = 1),
      size = guide_legend(ncol = 2, byrow = F))

######## MERGE ########

Fig2BC<-ggarrange(Fig2B, Fig2C, widths=1, heights=1,
                  common.legend = TRUE, labels = c("B","C"),
                  legend = "bottom")

#Fig2<-ggarrange(fig2A,Fig2BC, nrow = 2, heights=c(1,1), labels="A")

#tiff("Fig2.tiff", width = 2000, height = 3000, res=300)
#Fig2
#dev.off()

x11()

ggarrange(fig2A,Fig2BC, nrow = 2, heights=c(1,1), labels="A")


######## SAVE ########

# ggsave(filename = "Fig2.svg")
ggsave(filename = "Fig2.pdf", device=cairo_pdf,
       width = 10, height = 7.5)






