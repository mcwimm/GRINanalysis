load("./data/LMtrees.Rda") 
load("./data/LMlinks.Rda") 

library(ggforce) # geom_circle

# select 3 study sites
trees = LM.trees %>%  
   filter(LOC == 1 | LOC == 9 | LOC == 12)
links = LM.links %>%  
   filter(LOC == 1 | LOC == 9 | LOC == 12)

trees$alpha <- ifelse(trees$groupBin == "group", 0.5, 0.2)

links$LOC = factor(links$LOC)

labeller_names <- c("1" = "Benign environment",
                    "9" = "Mid point environment",
                    "12" = "Harsh environment")


labeller_names2 <- c("1" = "Low",
                    "9" = "Medium",
                    "12" = "High")

x11()
ggplot() + 
   coord_fixed() +
   geom_circle(trees, mapping = aes(x0=x, y0=y, r = CR, 
                                    alpha = alpha, fill = Sp), size=0.1) +
   geom_segment(links, mapping = aes(x=x1, y=y1, xend=x2, yend=y2), size = 0.8) +
   geom_point(trees[trees$netDeg != 0, ], mapping=aes(x=x, y=y),
              shape = 16, col = "khaki1", alpha = 0.9, size = 0.8) +
   
   facet_wrap(~ LOC,
              ncol = 1, 
              labeller = as_labeller(labeller_names2)) +
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

ggsave("Fig1_network_2.svg")


######## SINGLE
trees = LM.trees[LM.trees$LOC == 12, ] # 9, 12
links = LM.links[LM.links$LOC == 12, ] # 9, 12

trees$alpha <- ifelse(trees$groupBin == "group", 0.5, 0.2)
links$LOC = factor(links$LOC)

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

ggsave("Fig1_LOC12_high.svg")
