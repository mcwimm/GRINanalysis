library(lme4)
library(tidyverse)
library(BBmisc)
library(ggpubr)

load("./data/LMtrees.Rda") 

LM.avis = LM.trees %>% 
   group_by(LOC) %>%
   mutate(tot.dens = n() / (30*30) * 10000) %>% 
   filter(Sp == "A") %>% 
   mutate(avi.dens = n() / (30*30) * 10000,
          r_avi = avi.dens / tot.dens)

LM.avis = LM.avis %>% 
   mutate(DBH.gmc = (DBH - mean(LM.avis$DBH)) / sd(LM.avis$DBH),
          salinity.gmc = (salinity - mean(LM.avis$salinity)) /
             sd(LM.avis$salinity),
          avi.dens.gmc = (avi.dens - mean(LM.avis$avi.dens)) /
             sd(LM.avis$avi.dens),
          tot.dens.gmc = (tot.dens - mean(LM.avis$tot.dens)) /
             sd(LM.avis$tot.dens),
          r_avi.gmc = (r_avi - mean(LM.avis$r_avi)) /
             sd(LM.avis$r_avi)) %>% 
   group_by(LOC) %>% 
   mutate(obs.grafted = sum(grouped)) %>% 
   arrange(DBH) %>% 
   mutate(obs.rg = cumsum(grouped),
          obs.rf = obs.rg/obs.grafted) 


LM.avis$sal.group <- ifelse(LM.avis$salinity < 45, "\u003c 45",
                            ifelse(LM.avis$salinity >= 55, 
                                   "\u2265 55", # kleiner-gleich  & \u2264 60
                                   "45 \u2212 54")) #gr??er-gleich \u2265 

LM.avis$sal.group <- factor(LM.avis$sal.group,
                            levels = c("\u003c 45", 
                                       "45 \u2212 54", "\u2265 55"))


######### Model #############

fm.gmc = glmer(grouped ~ DBH.gmc + tot.dens.gmc + salinity.gmc +
                  tot.dens.gmc * salinity.gmc +
                  DBH.gmc : tot.dens.gmc + DBH.gmc : salinity.gmc +
                  (DBH.gmc || LOC), 
               data = LM.avis, family = "binomial")
glm.probs <- predict(fm.gmc, newdata = LM.avis, type = "response",
                     allow.new.levels = TRUE)
LM.avis$fm.gmc.probs <- glm.probs
LM.avis$fm.gmc.pred <- ifelse(glm.probs > 0.5, 1, 0)

######## Ale's data ####

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
        # fill = expression(atop(bold("Neighbourhood"), paste(bold("asymmetry"))))
        fill = "Stand density \n(trees per hectare)",
        col = "Stand density \n(trees per hectare)") +
   # scale_shape_manual(values = c(0, 1, 2)) +
   
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
      # fill = guide_legend(keywidth = 1, keyheight = 1),
          linetype=guide_legend(keywidth = 2.5, keyheight = 1),
          # colour=guide_legend(keywidth = 2.5, keyheight = 2),
          size = guide_legend(ncol = 2, byrow = F))


Fig2B <- ggplot(d4, aes(x = DBH, y = height,
                        fill = Condition, # colour
                        linetype = Condition)) + #shift between d2 (all trees) and d5, Trees with asymmetry
   geom_point(aes(size=AsymmNeighDBH), alpha=0.85, shape = 21)+
   #ggtitle("Single-Stem trees Asymm Neighbourhoods")+
   ylim(-1,32)+
   xlab("Stem diameter (cm)") +
   ylab("Tree height (m)") +
   #stat_smooth(method = 'nls', se=FALSE)+
   stat_smooth(method = 'nls',   aes(col = Condition),
               geom ="line", alpha=0.9, size = 1, span=0.7 ,                #Fit nls model
               method.args = list( formula = 'y~H*(1-exp(-a*x))',  
                                   start=c(H=20, a=0.2)), se=FALSE)+
   scale_linetype_manual(name="Condition", 
                         values = c("solid", "longdash" ), labels=c("Grafted", "Non-grafted"))+
   #theme_bw()+
   scale_size_continuous(name=expression(atop(bold("Neighbourhood"), paste(bold("asymmetry")))))+
   scale_fill_manual(name="Condition",
                      # values = c("#FC6F00", "#009CB0"),#B04C00    
                      # values = c("#FC5800", "#00B0A9"),#B04C00     
                      values = c("#d8b365", "#5ab4ac"),
                      # values=c("#FF7F50","#BC8F8F"),
                      breaks= c("Grafted", "Non-Grafted"),
                      labels=c("Grafted", "Non-grafted"))+
   scale_color_manual(name="Condition",
                     # values = c("#FC6F00", "#009CB0"),#B04C00    
                     # values = c("#FC5800", "#00B0A9"),#B04C00     
                     values = c("#d8b365", "#5ab4ac"),
                     # values=c("#FF7F50","#BC8F8F"),
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


x11()

tiff("Fig2_2.tiff", width= 30, height= 12, res= 900, units = "cm") 

ggarrange(fig2A, Fig2B, widths = c(1.15, 1),
          labels = c("A", "B"),
          font.label = list(size = 16, color = "black"))

dev.off()   

ggsave(filename = "Fig2_2.svg")
ggsave(filename = "Fig2.pdf", device=cairo_pdf)

#################

fig2Supp = LM.avis %>%
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
      # fill = guide_legend(keywidth = 1, keyheight = 1),
      linetype=guide_legend(keywidth = 2.5, keyheight = 1),
      # colour=guide_legend(keywidth = 2.5, keyheight = 2),
      size = guide_legend(ncol = 2, byrow = F))


x11()
fig2Supp
ggarrange(fig2Supp, ggplot(), widths = c(1.15, 1),
          # labels = c("A", "B"),
          font.label = list(size = 16, color = "black"))

ggsave(filename = "Fig1Extended.pdf", device=cairo_pdf)
ggsave(filename = "Fig1Extended.svg")



############ CORRELATION MATRIX #############
library(ggcorrplot)

d = LM.avis %>% 
   distinct(LOC, tot.dens, avi.dens, salinity) %>% 
   mutate(r_avi = avi.dens / tot.dens)# %>% 
   rename("Salinity" = "salinity",
          "Total stand density" = "tot.dens",
          "Total A. g. density" = "avi.dens",
          "Relativ A. g. density" = "r_avi")
# d$
correlations <- cor(d[, -1],
                    use = "complete.obs")
res1 <- cor_pmat(d[, -1])

x11()
ggcorrplot(correlations, 
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


ggsave(filename = "CorrelationMatrix.pdf", device=cairo_pdf)
ggsave(filename = "CorrelationMatrix.svg")
