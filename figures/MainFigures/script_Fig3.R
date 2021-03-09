#### Required packages ####
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("scales")) install.packages("scales")

#### Required data ####
load("./data/LMavis.Rda") 
load("./data/LMgroups.Rda") 
load("./data/Plot_data.Rda")
load("./data/AlldistLines.Rda")

#### Define color scheme ####
fillcolors = c("#260C7D", "#007D06", "#7D410C")
fillalpha = 0.65

#### Prepare data ####
# Create a dataframe with plot-related information
# (plot number, total density, avicennia density, salinity, node degree)

LOCs = LM.avis %>%
   group_by(LOC) %>%
   mutate(avi.trees = n()) %>% 
   mutate(avgNodeDegreeAll = mean(netDeg)) %>% 
   filter(netDeg != 0) %>% 
   mutate(pGrafted = round(n() / avi.trees * 100, 1)) %>%
   distinct(LOC, tot.dens, avi.dens, salinity, avgNodeDegreeAll,
            avgNodeDegree = mean(netDeg), pGrafted)

# Group plots by salinity
LM.avis$sal.group <- ifelse(LM.avis$salinity < 45, "\u003c 45",
                            ifelse(LM.avis$salinity >= 55, 
                                   "\u2265 55", 
                                   "45 \u2212 54")) 

LM.avis$sal.group <- factor(LM.avis$sal.group,
                            levels = c("\u003c 45", 
                                       "45 \u2212 54", "\u2265 55"))

# Add group density (per ha) and average group size to data frame
groups = LM.groups %>% 
   group_by(LOC) %>% 
   distinct(LOC,
            groupsHa = n() / 30 / 30 * 10000,
            meanGS = mean(no.memb))

# Merge plot and group data
LOCs = merge(LOCs, groups, by.x = "LOC", by.y = "LOC") 

# Add salinity groups to plot dataa
LOCs$sal.group <- ifelse(LOCs$salinity < 45, "\u003c 45",
                            ifelse(LOCs$salinity >= 55, 
                                   "\u2265 55", 
                                   "45 \u2212 54")) 

LOCs$sal.group <- factor(LOCs$sal.group,
                            levels = c("\u003c 45", 
                                       "45 \u2212 54", "\u2265 55"))

# Calculate total node degree
nd.tot = LM.avis %>%
   ungroup() %>% 
   mutate(N = n()) %>% 
   group_by(netDeg) %>% 
   distinct(netDeg, N, rfT = n()/N)

# Calculate node degree for each plot
nd.LOC = LM.avis %>% 
   group_by(LOC) %>% 
   mutate(N = n()) %>% 
   group_by(LOC, netDeg) %>% 
   distinct(LOC, netDeg, N, rfS = n()/N)

# Merge node degree data
nd.LOC = merge(LOCs[, c(1, 4, 6)], nd.LOC, by.x = "LOC", by.y = "LOC")

#### Figure 3a ####
fig3A = Plot_data %>%
   ggplot(.) +
   geom_point(aes(x=x, y=y)) + labs(x="Node degree", y="CDF") + 
   geom_line(data = AlldistLines, aes(x=x, y=y, 
                                      colour=distribution, 
                                      linetype=distribution), size=1.0)+
   annotate(geom="text", x=2.8, y=0.34, label="\u03B3 = 4.47",
            color="black")+
   scale_y_continuous(trans="log",
                      breaks = trans_breaks("log", function(x) round(exp(x),2))) +
   scale_x_continuous(trans="log",
                      breaks = trans_breaks("log", function(x) round(exp(x)))) +
   scale_linetype_manual("Distribution \nfunction", 
                         values=c("solid","twodash", "dashed",
                                  "dotted","logndash"))+
   scale_color_manual("Distribution \nfunction", 
                      values=c("#8B008B","#3CB371", "#4682B4",
                               "black","blue"))+
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
         legend.position = "right",
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 14),
         legend.margin = margin(t = 1, unit = "cm"),
         legend.direction = "vertical",
         legend.spacing.y = unit(0.45,"cm"),
         legend.background = element_rect(fill = NA),
         legend.key = element_rect(fill = NA, color = NA))

#### Figure 3b ####

fig3B = ggplot(LOCs) +
   geom_point(mapping=aes(y = avgNodeDegreeAll, x = avi.dens,
                          size = pGrafted,
                          fill = sal.group, shape = sal.group), 
              alpha = fillalpha) +
   scale_shape_manual(values = c(21, 22, 24)) +
   scale_fill_manual(values = fillcolors) +
   stat_smooth(aes(x = avi.dens,
                   y = avgNodeDegreeAll),
               method = "lm", formula = y~x, col = "#070F36",
               geom ="line", alpha = 0.9, size=1, span=0.7,
               se = F) + 
   stat_regline_equation(
      aes(x = avi.dens, y = avgNodeDegreeAll,
          label =  paste("Average~node~'degree':~", 
                         ..adj.rr.label.., sep = "~~~")),
      label.x.npc = 0, label.y = 1.15, size = 4,
      formula = y~x) +
   labs(y = "Average \nnode degree", x = "",
        size = "Grafting \nfrequency (%)",
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
   theme(plot.margin = unit(c(0.6,3,0,1), "lines")) +
   guides(fill = F,
          shape = F)

#### Figure 3c ####

C = LOCs %>% 
   ggplot(.) +
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
          label =  paste("Groups~per~ha:", 
                         ..adj.rr.label.., sep = "~~~~~~")),
      label.x.npc = 0., label.y.npc = 1.0, size = 4,
      formula = y~x) +
   
   stat_regline_equation(
      aes(x = avi.dens,
          y = meanGS,
          label =  paste("Group~members:", 
                         ..adj.rr.label.., sep = "~~~")),
      label.x.npc = 0., label.y.npc = 0.90, size = 4,
      formula = y~x) +
   
   scale_shape_manual(values = c(21, 22, 24)) +
   scale_fill_manual(values = fillcolors) +
   scale_size_continuous(breaks = c(2.5, 3.5, 4.5))  

fig3C = C +
   labs(x = "Stand density (trees per hectare)",
        y = "Group density \n(groups per hectare)",
        size = "Group \nmembers",
        fill = "Salinity (ppt)", 
        shape = "Salinity (ppt)") +
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


#### Merge figures ####
r = ggarrange(fig3B, fig3C,
              ncol = 1, nrow = 2,
              legend = "right",
              labels = c("b)", "c)"),
              align = "v",
              heights = c(1, 1),
              font.label = list(size = 16, color = "black"))


Fig3 <- ggarrange(fig3A, r, labels = c("a)", ""))


#### Save file ####
tiff("figures/Fig3.tiff", width = 3700, height = 2300, res=300)
annotate_figure(Fig3,
                top = text_grob(paste0("Fig. 3: Root graft network attributes\n"),
                                color = "black", face = "bold", 
                                size = 14, hjust = 0, x=0.005,just="left"))
dev.off()

