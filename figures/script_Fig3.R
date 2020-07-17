# required packages
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(scales)  # trans_breaks

# required data
load("./data/LMgroups.Rda") 
load("./data/LMtrees.Rda") 
load("./data/LMavis.Rda") 
load("./data/Plot_data.Rda")
load("./data/AlldistLines.Rda")

# set color scheme
fillcolors = c("#260C7D", "#007D06", "#7D410C")
fillalpha = 0.65

# calculate plot characteristics (total density, avicennia density, salinity, node degree)
LOCs = LM.trees %>%
   group_by(LOC) %>%
   mutate(tot.dens = n() / (30*30) * 10000) %>% 
   filter(Sp == "A") %>% 
   mutate(avi.dens = n() / (30*30) * 10000,
          r_avi = avi.dens / tot.dens) %>% 
   distinct(LOC, tot.dens, avi.dens, salinity,
            node.degree = mean(netDeg))


# calculate group density and mean group size to plot characteristics
groups = LM.groups %>% 
   group_by(LOC) %>% 
   distinct(LOC,
            groupsHa = n() / 30 / 30 * 10000,
            meanGS = mean(no.memb))
   
# merge plot data + add salinity groups
LOCs = merge(LOCs, groups, by.x = "LOC", by.y = "LOC") 

LOCs$sal.group <- ifelse(LOCs$salinity < 45, "\u003c 45",
                            ifelse(LOCs$salinity >= 55, 
                                   "\u2265 55", # kleiner-gleich  & \u2264 60
                                   "45 \u2212 54")) #gr??er-gleich \u2265 

LOCs$sal.group <- factor(LOCs$sal.group,
                            levels = c("\u003c 45", 
                                       "45 \u2212 54", "\u2265 55"))

# calculate node degree (total and per plot)
nd.tot = LM.avis %>%
   ungroup() %>% 
   mutate(N = n()) %>% 
   group_by(netDeg) %>% 
   distinct(netDeg, N, rfT = n()/N)
   
nd.LOC = LM.avis %>% 
   group_by(LOC) %>% 
   mutate(N = n()) %>% 
   group_by(LOC, netDeg) %>% 
   distinct(LOC, netDeg, N, rfS = n()/N)


nd.LOC = merge(LOCs[, c(1, 4, 6)], nd.LOC, by.x = "LOC", by.y = "LOC")

############ Figure A #####################

#fig3A = Plot_data %>%
fig3A<-ggplot(Plot_data)+
   geom_point(aes(x=x, y=y)) + labs(x="Node degree", y="CDF") + 
   geom_line(data = AlldistLines, aes(x=x, y=y, colour=distribution, linetype=distribution), size=1.0)+
   annotate(geom="text", x=2.8, y=0.34, label="\u03B3 = 4.47",
            color="black")+
   scale_y_continuous(trans="log",breaks = trans_breaks("log", function(x) round(exp(x),2)))+
   scale_x_continuous(trans="log",breaks = trans_breaks("log", function(x) round(exp(x))))+
   scale_linetype_manual("Distribution \nfunction", values=c("solid","twodash", "dashed","dotted","logndash"))+
   scale_color_manual("Distribution \nfunction", values=c("#8B008B","#3CB371", "#4682B4","black","blue"))+
   
   labs(x = "Node degree", y = "log(CDF)") + #Cumulative distribution function",
   
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
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.margin = margin(t = 1, unit = "cm"),
         legend.direction = "vertical",
         legend.spacing.y = unit(0.45,"cm"),
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA))


############ Figure B #####################

fig3B<- ggplot(LOCs) +
   geom_point(aes(y = node.degree, x = avi.dens,
                          size = meanGS,
                          fill = sal.group, shape = sal.group), 
              alpha = fillalpha) +
   scale_shape_manual(values = c(21, 22, 24)) +
   scale_fill_manual(values = fillcolors) +
   labs(y = "Average \nnode degree", x = "",
        size = "No. of trees \nper group",
        fill = "Salinity (ppt)", shape = "Salinity (ppt)") +
   theme(panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.text.x=element_blank(),
         axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                    size=12, colour = "black"),
         axis.title.y = element_text(size=14, colour = "black"),
         axis.ticks.length=unit(-1.5, "mm"),
         
         axis.ticks = element_line(size = .5),
         axis.line = element_line(colour = 'black', size = 1),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.margin = margin(t = 1, unit = "cm"),
         legend.spacing.y = unit(0.45,"cm"),
         legend.direction = "vertical",
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA)) +
   theme(plot.margin = unit(c(0.6,3,0,1), "lines")) 


############ Figure C #####################

C <- ggplot(LOCs) +
   geom_point(aes(x = avi.dens, y = groupsHa,
                  fill = sal.group, shape = sal.group,
                  size = meanGS), 
              alpha = fillalpha) +
   stat_smooth(aes(x = avi.dens,
                   y = groupsHa),
               method = "lm", formula = y~x, col = "#070F36",
               geom ="line", alpha=0.9, size=1, span=0.7,
               se = F) + 
   stat_regline_equation(
      aes(x = avi.dens,
          y = groupsHa,
         label =  paste("Groups~per~ha:", ..adj.rr.label.., sep = "~~~~~~")),
      label.x.npc = 0., label.y.npc = 1.0, size = 4,
      formula = y~x) +
   
   stat_regline_equation(
      aes(x = avi.dens,
          y = meanGS,
          label =  paste("Group~members:", ..adj.rr.label.., sep = "~~~")),
      label.x.npc = 0., label.y.npc = 0.85, size = 4,
      formula = y~x) +
   
   scale_shape_manual(values = c(21, 22, 24)) +
   scale_fill_manual(values = fillcolors) +
   scale_size_continuous(breaks = c(2.5, 3.5, 4.5))  #+

fig3C <- C +
   labs(x = "Stand density (trees per hectare)",
        y = "Group density \n(groups per hectare)",
        size = "No. of trees \nper group",
        
        fill = "Salinity (ppt)", shape = "Salinity (ppt)") +
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
         legend.direction = "vertical",
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA)) +
   theme(plot.margin = unit(c(0.6,3,0,1), "lines"))
   

################## MERGE ##############

r <- ggarrange(fig3B, fig3C, #
              ncol = 1, nrow = 2,
              legend = "right",
              labels = c("B", "C"),
              common.legend = T, 
              heights = c(0.7, 1.5),
              font.label = list(size = 16, color = "black"))

ggarrange(fig3A, r, labels = c("A", ""))

ggsave(filename = "Fig3.pdf", device=cairo_pdf)
ggsave(filename = "Fig3.svg")

