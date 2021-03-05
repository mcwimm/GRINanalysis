#### Required libraries
#devtools::install_github("diegovalle/mxmaps") # Install mxmaps from github
library("mxmaps")
library(ggplot2)

#### render the map ####

data(mxstate.map)

MexMap<-ggplot(mxstate.map, aes(long, lat, group=group)) +
  geom_polygon(fill = "#8A8E92", color = "#8A8E92", size = .2) +
  coord_map()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())+
  geom_polygon(data= mxstate.map[mxstate.map$id==30,], aes(long, lat))

#ggsave(file="figures/MexMap.svg", plot=MexMap, width=12, height=12)


