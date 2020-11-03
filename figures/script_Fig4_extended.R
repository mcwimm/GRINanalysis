load("./data/data_gam.rda") # LM.trees


#### Extended data Figure 4 ####

plt1<-qq_plot(Mod, method="simulate",  point_col="steelblue",point_alpha = 0.7, alpha=0.0)+
   labs(title = NULL, subtitle = NULL)+
   scale_y_continuous(limits= c(-0.8, 0.8), labels = signs_format(accuracy = .1))+
   scale_x_continuous(labels = signs_format(accuracy = .1))+
   theme(panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black", size=1),
         axis.text = element_text(colour = "black", size=12),
         axis.title = element_text(colour = "black", size = 14),
   )

df<-data.frame(log_fitted= fitted(Mod), 
               residuals=resid(Mod, type= "deviance"))
head(df)


plt2<-ggplot(df, aes(x=log_fitted, y=residuals))+
   geom_point(alpha=0.7, colour="steelblue")+
   labs(x="Linear predictor", y="Deviance residuals")+
   geom_hline(yintercept=0, linetype="solid", color="red", size=0.5)+
   scale_y_continuous(limits= c(-0.8, 0.6), labels = signs_format(accuracy = .1))+
   theme(panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black", size=1),
         axis.text = element_text(colour = "black", size=12),
         axis.title = element_text(colour = "black",size = 14),
   )



ExtDatFig4<-ggarrange(plt1,plt2, labels = c("A","B"))

#tiff("GAMM_Resid.tiff", height = 1400, width =2100, res=300)
ExtDatFig4
#dev.off()