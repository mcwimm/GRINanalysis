# required packages
library(tidyverse)
library(BBmisc)
library(ggpubr)
library(ggforce) # geom_circle


# required data
load("./data/LMtrees.Rda") 
load("./data/LMlinks.Rda") 


# subset data
# low stress = LOC 1
# mid stress = LOC 9
# high stress = LOC 12
trees = LM.trees[LM.trees$LOC == 1, ] # 9, 12
links = LM.links[LM.links$LOC == 1, ] # 9, 12

# add alpha value for visualization of grafted and non-grafted trees
trees$alpha <- ifelse(trees$groupBin == "group", 0.5, 0.2)

# set LOC-variable as factor
links$LOC = factor(links$LOC)

# create figure
x11()
ggplot() + 
   coord_fixed() +
   geom_circle(trees, mapping = aes(x0=x, y0=y, r = CR, 
                                    alpha = alpha, fill = Sp), size=0.1) +
   geom_segment(links, mapping = aes(x=x1, y=y1, xend=x2, yend=y2), size = 0.8) +
   geom_point(trees[trees$netDeg != 0, ], mapping=aes(x=x, y=y),
              shape = 16, col = "khaki1", alpha = 0.9, size = 0.8) +
   
   scale_alpha_continuous(range = c(0.2, 0.5),
                          guide = "none") +
   scale_fill_manual(values = c("chartreuse4", "#56B4E9", "#E69F00"),
                     labels = c("Avicennia germinans", "Laguncularia racemosa",
                                "Rhizophora mangle"),
                     name = "Species",
                     guide = guide_legend(
                        label.theme = element_text(face = "italic", size = 10),
                        title.theme = element_text(face = "bold", size = 10))) +   
   guides(fill = guide_legend(override.aes = list(alpha = 0.3),
                              label.theme = element_text(face = "italic", size = 10),
                              title.theme = element_text(face = "bold", size = 10))) +
   labs(x="", y="") +
   theme_classic() +
   theme(legend.position="right",
         text = element_text(size = 12))


# save figure
ggsave("Fig1_LOC1_low.svg")
# ggsave("Fig1_LOC9_mid.svg")
# ggsave("Fig1_LOC12_high.svg")
