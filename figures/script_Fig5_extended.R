# library(agricolae)

load("./data/LMavis.Rda") 

fillcolors = c("#260C7D", "#007D06", "#7D410C")

# create group variable with grafting condition and salinity group
LM.avis$group = with(LM.avis, paste0(Condition, " | ", sal.group))
unique(LM.avis$group)

# get significance letters of kruskal-walis trest
krusLetters = kruskal(LM.avis$slenderness, LM.avis$group, 
                      group = TRUE, alpha = 0.05,
                      p.adj="bonferroni")$groups
krusLetters$group = rownames(krusLetters)
krusLetters = merge(krusLetters, 
                    LM.avis[, c("group", "Condition", "sal.group")],
                    by = "group", all.x = T) %>% 
   distinct() 

krusLetters = krusLetters %>% 
   rename("Salinity" = "sal.group")

p = LM.avis %>% 
   rename("Salinity" = "sal.group") %>% 
   ggplot(.) +
   geom_boxplot(aes(x = Condition, y = slenderness, col = Condition)) +
   geom_jitter(aes(x = Condition, y = slenderness, col = Condition),
               alpha = 0.2) +
   geom_text(krusLetters, mapping=aes(x = Condition,
                                      y = 150,
                                      label = groups)) +
   facet_wrap(~ Salinity, labeller = label_both) +
   scale_color_manual(values = fillcolors) +
   labs(y = "Slenderness-Index",
        x = "Condition") +#,
   # x = "Mean plot salinity (ppt)") +
   theme_classic() +
   theme(legend.position = "none")

x11()
p
ggsave(filename = "Fig5Extended.pdf", width = 7, height = 5)
