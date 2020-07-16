library(ggpubr)
library(ggplot2)
library(tidyverse)
library(scales)  # trans_breaks

load("./data/LMgroups.Rda") 
load("./data/LMtrees.Rda") 

fillcolors = c("#260C7D", "#007D06", "#7D410C")
fillalpha = 0.65

fillcolors = c("#d8b365", "#f5f5f5", "#5ab4ac")
fillalpha = 0.85

LM.avis = LM.trees %>% 
   group_by(LOC) %>%
   mutate(tot.dens = n() / (30*30) * 10000) %>% 
   filter(Sp == "A") %>% 
   mutate(avi.dens = n() / (30*30) * 10000,
          r_avi = avi.dens / tot.dens)

LOCs = LM.trees %>%
   group_by(LOC) %>%
   mutate(tot.dens = n() / (30*30) * 10000) %>% 
   filter(Sp == "A") %>% 
   mutate(avi.dens = n() / (30*30) * 10000,
          r_avi = avi.dens / tot.dens) %>% 
   # filter(netDeg != 0) %>% 
   distinct(LOC, tot.dens, avi.dens, salinity,
            node.degree = mean(netDeg))

LM.avis$sal.group <- ifelse(LM.avis$salinity < 45, "\u003c 45",
                            ifelse(LM.avis$salinity >= 55, 
                                   "\u2265 55", # kleiner-gleich  & \u2264 60
                                   "45 \u2212 54")) #gr??er-gleich \u2265 

LM.avis$sal.group <- factor(LM.avis$sal.group,
                            levels = c("\u003c 45", 
                                       "45 \u2212 54", "\u2265 55"))


groups = LM.groups %>% 
   group_by(LOC) %>% 
   distinct(LOC,
            groupsHa = n() / 30 / 30 * 10000,
            meanGS = mean(no.memb))
   
LOCs = merge(LOCs, groups, by.x = "LOC", by.y = "LOC") 

LOCs$sal.group <- ifelse(LOCs$salinity < 45, "\u003c 45",
                            ifelse(LOCs$salinity >= 55, 
                                   "\u2265 55", # kleiner-gleich  & \u2264 60
                                   "45 \u2212 54")) #gr??er-gleich \u2265 

LOCs$sal.group <- factor(LOCs$sal.group,
                            levels = c("\u003c 45", 
                                       "45 \u2212 54", "\u2265 55"))

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

fig3A = nd.tot %>%
   ggplot(.) +
   geom_line(aes(x = netDeg, y = rfT), col = "darkred", size = 1.2) +
   geom_point(LOCs, mapping=aes(x = node.degree, y = avi.dens/ 1800,
                                fill = sal.group, shape = sal.group), 
              size = 3.5, alpha = fillalpha) +
   scale_y_continuous("Relative frequency",
                      sec.axis = sec_axis(~ . * 1800 , 
                                          name = "Stand density \n(trees per hectare)"),
                      limits = c(0, 0.6)) +
   scale_shape_manual(values = c(21, 22, 24)) +
   scale_fill_manual(values = fillcolors) +
   # scale_color_gradient(low = "chartreuse3", high = "black",
   #                     breaks = c(300, 611, 900)) +
   labs(x = "Node degree", 
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
         legend.key = element_rect(fill = NA, color = NA)) #+
   theme(plot.margin = unit(c(0.8, 3, 0, 0), "lines"))



############ Figure B #####################
load("Plot_data.Rda")
load("AlldistLines.Rda")

fig3B = Plot_data %>%
   ggplot(.) +
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
         # axis.ticks = element_line(size = .5),
         axis.line = element_line(colour = 'black', size = 1),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.margin = margin(t = 1, unit = "cm"),
         legend.direction = "vertical",
         legend.spacing.y = unit(0.45,"cm"),
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA))


############ Figure C #####################
C = LOCs %>% 
   ggplot(.) +
   # geom_point(aes(x = avi.dens, y = groupsHa,
   #                shape = sal.group, #fill = avi.dens, 
   #                size = meanGS)) +
   geom_point(aes(x = avi.dens, y = groupsHa,
                  fill = sal.group, shape = sal.group,
                  size = meanGS), 
              alpha = fillalpha) +
   stat_smooth(aes(x = avi.dens,
                   y = groupsHa),
               method = "lm", formula = y~x, col = "#070F36",
               geom ="line", alpha=0.9, size=1, span=0.7,
               se = F) + #, fill = "lightgrey") +
   stat_regline_equation(
      aes(x = avi.dens,
          y = groupsHa,
         label =  paste("Groups~per~ha:", ..adj.rr.label.., sep = "~~~~~~")),
      #label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
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

fig3C = C +
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
         # axis.ticks = element_line(size = .5),
         axis.line = element_line(colour = 'black', size = 1),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.direction = "vertical",
         legend.box = "vertical",
         legend.spacing.y = unit(0.45,"cm"),
         legend.margin = margin(t = 1, unit = "cm"),
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA)) #+
   theme(plot.margin = unit(c(0.5,3,0,0), "lines")) +
   #       legend.box ="vertical") +
   guides(shape = FALSE, fill = FALSE)
   

################## MERGE ##############
x11()
ggarrange(fig3B, fig3A, fig3C, #
          ncol = 2, nrow = 2,
          # legend = "bottom",
          labels = c("A", "B", "C"), #, "C"),
          common.legend = F, 
          widths = c(0.8, 1, 1),
          font.label = list(size = 16, color = "black"))

ggsave(filename = "Fig3.pdf", device=cairo_pdf)
ggsave(filename = "Fig3.svg")


### ALTERNATIVE #############

fig3Aa = Plot_data %>%
   ggplot(.) +
   geom_line(aes(x = x, y = y), col = "darkred", size = 0.8,
             alpha = 0.5) +
   
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
         # axis.ticks = element_line(size = .5),
         axis.line = element_line(colour = 'black', size = 1),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.margin = margin(t = 1, unit = "cm"),
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA)) +
   theme(plot.margin = unit(c(0.6,3,0,1), "lines"))


fig3Ca = C +
   theme(panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                    size=12, colour = "black" ),
         axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
                                    size=12, colour = "black"),
         axis.title.x = element_text(size=14, colour = "black"),
         axis.title.y = element_text(size=14, colour = "black"),
         axis.ticks.length=unit(-1.5, "mm"),
         # axis.ticks = element_line(size = .5),
         axis.line = element_line(colour = 'black', size = 1),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.margin = margin(t = 1, unit = "cm"),
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA)) +
   theme(plot.margin = unit(c(0.6,3,0,1), "lines"),
         legend.box ="vertical") +
   guides(shape = FALSE, fill = FALSE) 


fig3Ca2 = ggplot(LOCs) +
   geom_point(mapping=aes(x = node.degree, y = avi.dens,
                                fill = sal.group, shape = sal.group), 
              size = 3.5, alpha = fillalpha) +
   scale_shape_manual(values = c(21, 22, 24)) +
   scale_fill_manual(values = fillcolors) +
   labs(x = "Node degree", y = "Stand density (trees per hectare)",
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






x11()
ggarrange(fig3B, fig3Ca2, fig3C, #
          ncol = 3, 
          legend = "none",
          labels = c("A", "B", "C"),
          common.legend = F, 
          widths = c(0.7, 0.7, 0.8),
          font.label = list(size = 16, color = "black"))

ggsave(filename = "Fig3_2.pdf", device=cairo_pdf)



#######
fig3Ca3 = ggplot(LOCs) +
   geom_point(mapping=aes(y = node.degree, x = avi.dens,
                          size = meanGS,
                          fill = sal.group, shape = sal.group), 
              alpha = fillalpha) +
   scale_shape_manual(values = c(21, 22, 24)) +
   scale_fill_manual(values = fillcolors) +
   labs(y = "Average \nnode degree", x = "",
        size = "No. of trees \nper group",#"Stand density (trees per hectare)",
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

fig3C = C +
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

r = ggarrange(fig3Ca3, fig3C, #
          ncol = 1, nrow = 2,
          legend = "right",
          labels = c("B", "C"),
          common.legend = T, 
          heights = c(0.7, 1.5),
          font.label = list(size = 16, color = "black"))

ggarrange(fig3B, r, labels = c("A", ""))

ggsave(filename = "Fig3_4.pdf", device=cairo_pdf)


###########

edges <- LM.links[which(LM.links$LOC == 12), ]
vertices <- LM.trees[which(LM.trees$LOC == 12), -c(1, 2)]

net <- graph_from_data_frame(d = edges, vertices = vertices, directed = FALSE)
edge_connectivity(net, source = NULL, target = NULL, checks = TRUE)
strength(net
         )

# https://transportgeography.org/?page_id=5981
x11()
LM.groups %>% 
   # filter(no.memb > 2) %>% 
   group_by(no.memb) %>% 
   mutate(N = n()) %>% 
   ungroup() %>% 
   mutate(beta = no.links/no.memb) %>% 
   ggplot(.) +
   geom_hline(yintercept = 1, col = "darkgray") +
   # geom_histogram(aes(x = beta)) +
   geom_point(aes(x = no.memb, y = beta, size = N)) +
   annotate(geom = "segment", x = 10, y = 0.5, xend = 10, yend = 1.2, 
      size = 3, col = "darkblue", alpha = 0.8,
      arrow = arrow()) +
   annotate(geom = "text", x = 9.5, y = 0.85, 
      label = "Network complexity", color = "darkblue", alpha = 0.8,
      angle = 90, size = 5) +
   labs(y = "Beta", x = "Group size", size = "Frequency",
        title = "Level of connectivity",
        subtitle = "beta = number of links over the number of nodes") +
   theme_classic() 
ggsave("beta_networkComplexity.svg",
       width = 6, height = 4)

netG <- graph_from_data_frame(d = edges, 
                              vertices = vertices[vertices$netDeg > 0, ], 
                              directed = FALSE)

edge_density(netG, loops = F)

ID = "9_20"
grou = graph_from_data_frame(d = LM.links[LM.links$groupID == ID, ], 
                             vertices = LM.trees[LM.trees$groupID == ID, -c(1,2)], 
                             directed = FALSE)
plot(grou)
edge_density(grou, loops = F)


LM.groups %>% 
   mutate(tot = n()) %>% 
   # filter(no.memb > 2) %>% 
   group_by(no.memb) %>% 
   mutate(N = n(), fre = n()/tot) %>%
   distinct(no.memb, tot, N, fre)
