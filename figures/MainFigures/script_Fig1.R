#### Required packages ####
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggpubr")) install.packages("ggpubr")    # ggarrange
if (!require("ggforce")) install.packages("ggforce")  # geom_circle
if (!require("png")) install.packages("png")          # readPNG


#### Import the La Mancha map (Fig 1a) ####
# Files used to construct StudySiteMexico.png are contained withing the folder "Files_MapStudySiteMexico". The folder includes plot coordinates, a geoTIFF and a qgz to open in Qgis (open source)

img <- readPNG("figures/MainFigures/StudySiteMexico.png")
Fig1A <- ggplot() + 
  theme_void()+
  background_image(img)


#### Top view of selected study sites (Fig 1 B-D) ####
# Required La Mancha data
load("./data/LMtrees.Rda") 
load("./data/LMlinks.Rda") 
# low stress = LOC 1
# mid stress = LOC 9
# high stress = LOC 12

#### Figure 1b ####
# subset data
l = LM.links %>% 
  filter(LOC %in% c(12, 9, 1)) %>% 
  mutate(LOC = factor(LOC, levels = c(12, 9, 1)))
t = LM.trees %>% 
  filter(LOC %in% c(12, 9, 1)) %>% 
  mutate(alpha = ifelse(groupBin == "group", 0.5, 0.2)) %>% 
  mutate(LOC = factor(LOC, levels = c(12, 9, 1)))

labs = data.frame(label = c("b)", "c)", "d)"),
                  LOC = factor(c(12, 9, 1), levels = c(12, 9, 1)))
Fig1bcd = t %>% 
  ggplot(.) + 
  coord_fixed(clip = 'off', xlim = c(-2, 32)) +
  geom_circle(mapping = aes(x0=x, y0=y, r = CR, 
                            alpha = alpha, fill = Sp, color=NA),
              size=0.1) +
  geom_segment(l, mapping = aes(x=x1, y=y1, xend=x2, yend=y2), 
               size = 0.8) +
  geom_point(t[t$netDeg != 0, ], mapping=aes(x=x, y=y),
             shape = 16, col = "khaki1", alpha = 0.9, size = 0.8) +
  facet_wrap(~ LOC, ncol = 1) +
  geom_text(labs, mapping=aes(x = -14, y = 32.7, label = label),
            hjust = -0.1, vjust = 0, size = 14, fontface = 'bold') +
  scale_alpha_continuous(range = c(0.2, 0.5),
                         guide = "none") +
  scale_fill_manual(values = c("chartreuse4", "#56B4E9", "#E69F00"),
                    labels = c("A. germinans", "L. racemosa",
                               "R. mangle"),
                    name = "Species",
                    guide = guide_legend(
                      label.theme = element_text(face = "italic", 
                                                 size = 14),
                      title.theme = element_text(face = "bold", 
                                                 size = 14))) +
  scale_color_manual(values = c("chartreuse4", "#56B4E9", "#E69F00"),
                     labels = c("A. germinans", "L. racemosa",
                                "R. mangle"),
                     name = "Species",
                     guide = guide_legend(
                       label.theme = element_text(face = "italic", 
                                                  size = 20),
                       title.theme = element_text(face = "bold", 
                                                  size = 20))) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.3),
                             label.theme = element_text(face = "italic",
                                                        size = 20),
                             title.theme = element_text(face = "bold", 
                                                        size = 20))) +
  labs(x=NULL, y=NULL) +
  theme_classic() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "bottom")



#### Arrange figures and save file ####
tiff("figures/Fig1.tiff", width = 9000, height = 5000, res=300)
annotate_figure(ggarrange(Fig1A, Fig1bcd, 
                          widths = c(3.5,1.2), labels=c("a)",""),
                          font.label = list(size=40, color="black") ),
                top = text_grob(paste0("Fig. 1: Study site and root network maps located on the central coast of the Gulf of Mexico\n"),
                                color = "black", face = "bold", size = 38,
                                hjust = 0, x=0.005,just="left"))
dev.off()
