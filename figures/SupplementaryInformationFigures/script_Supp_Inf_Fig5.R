#### Required packages ####
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gratia")) install.packages("gratia")
if (!require("signs")) install.packages("signs")   # signs_format

#### Required data ####
load("./data/GAMM.Rda") 


#### SI Figure 5 ####

plt1 <- qq_plot(Mod, method="simulate",  point_col="steelblue",
                point_alpha = 0.7, alpha=0.0)+
   labs(title = NULL, subtitle = NULL)+
   scale_y_continuous(limits= c(-0.8, 0.8), 
                      labels = signs_format(accuracy = .1))+
   scale_x_continuous(labels = signs_format(accuracy = .1))+
   theme(panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black", size=1),
         axis.text = element_text(colour = "black", size=12),
         axis.title = element_text(colour = "black", size = 14),
   )

df <- data.frame(log_fitted= fitted(Mod), 
                 residuals=resid(Mod, type= "deviance"))
# head(df)


plt2 <- ggplot(df, aes(x=log_fitted, y=residuals)) +
   geom_point(alpha=0.7, colour="steelblue") +
   labs(x="Linear predictor", y="Deviance residuals") +
   geom_hline(yintercept=0, linetype="solid", color="red", size=0.5) +
   scale_y_continuous(limits= c(-0.8, 0.6), 
                      labels = signs_format(accuracy = .1)) +
   theme(panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black", size=1),
         axis.text = element_text(colour = "black", size=12),
         axis.title = element_text(colour = "black",size = 14),
   )




#### Merge figures ####
SuppInfFig5 <- ggarrange(plt1, plt2, labels = c("a)","b)"))

#### Save file ####
tiff("figures/Supp_Inf_Fig5.tiff", height = 1400, width =2100, res=300)
annotate_figure(SuppInfFig5,
                top = text_grob(paste0("Supplementary Figure 5. Generalized additive mixed effects model residuals\n"),  
                                color = "black", face = "bold", 
                                size = 12, hjust = 0, x=0.01,just="left"))
dev.off()