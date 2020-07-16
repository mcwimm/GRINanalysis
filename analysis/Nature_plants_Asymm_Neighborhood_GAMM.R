
#Required packages
library(ggplot2)
library(janitor)
library(ggpubr)
library(gratia)
library(signs)
library(mgcv)
#### Load data ####
d<- read.table("FullData2019clean.csv", header = TRUE, sep = ",")

#Create unique tree ID taggs based on tree ID and plot ID, the ourpose is to accuartley identify grefted tree form the graft links data set
d$uniqueID<-paste(d$LOC, d$ID)


#grfat connections data ("from to" format)
links<-read.csv("links2019.csv", header=TRUE)
#Also create unique ID's for tree from (Tree 1) and to (Tree2), then merge rows and create vectro with unique values (remove dipplicates)
uniqueT1<-paste(links$LOC, links$Tree1); uniqueT2<-paste(links$LOC, links$Tree2); UnID<-c(uniqueT1, uniqueT2)

IdsGraft<-unique(UnID) # these vector contains the unique ID's of grafted trees for this study
Matches<-match(d$uniqueID, IdsGraft)
Matches[is.na(Matches)]<-0
#### Ad to data base a "Condition" columns pinning "Grafted" and "non-Grafted" trees on the data frame
d$Condition<- ifelse(Matches!=0, "Grafted","Non-Grafted")

ggplot(d, aes(x = DBH, y = height, colour=Condition)) + #shift between d2 (all trees) and d5, Trees with asymmetry
  geom_point(aes(shape = GEO), alpha=0.8)



d$Ymax <- d$Xmax <- ifelse(d$LOC %in% c("6","Lr 3"), 20, 30)

# Compute crown metrics  using stem position as reference point in a complex plane such that X,y position are expressed as complex numbers
r <- function(a) outer(1:nrow(d), a, function(x,y) y)  # function to create a matrix with nrow(d) rows all equal to a

# Define stem position 
stempos <- d$X+d$Y*1i


#### Compute neighborhood asymmetry of each tree (aggregate vector connecting neighbors within 5 m distance to the tree) ####
maxNNDistance_m <- 5 # define the perimeter of the neighborhood around a focal tree
asymmetry <- rowSums(ifelse(
	r(d$LOC) == d$LOC & r(d$ID) != d$ID #pull data of neighbors inside the neighbohood
	& maxNNDistance_m < Re(stempos) & Re(stempos) < d$Xmax - maxNNDistance_m
	& maxNNDistance_m < Im(stempos) & Im(stempos) < d$Ymax - maxNNDistance_m
	& r(d$height) >= d$height  # --> include this line to consider only neighbors larger than the focal tree
	& abs(r(stempos)-stempos) <= maxNNDistance_m,
	(r(stempos)-stempos) * r(d$DBH) / abs(r(stempos)-stempos)^2, 0))# cahnge crownarea for sd$DBH to estimate asymmetry as a function of neighbours stem diameter
asymmetry[!(maxNNDistance_m < Re(stempos) & Re(stempos) < d$Xmax - maxNNDistance_m #here we drop all constellations falling out of the plot limits
	& maxNNDistance_m < Im(stempos) & Im(stempos) < d$Ymax - maxNNDistance_m)] <- NA


#### Compute hegyi index for each target tree  #### 
Hegyi <- rowSums(ifelse(
  r(d$LOC) == d$LOC & r(d$ID) != d$ID #pull data of neighbors inside the neighbohood
  & maxNNDistance_m < Re(stempos) & Re(stempos) < d$Xmax - maxNNDistance_m
  & maxNNDistance_m < Im(stempos) & Im(stempos) < d$Ymax - maxNNDistance_m
  & r(d$height) >= d$height  # --> include this line to consider only neighbors larger than the focal tree
  & abs(r(stempos)-stempos) <= maxNNDistance_m,
  (r(d$DBH) /(d$DBH))/ abs(r(stempos)-stempos), 0))# cahnge crownarea for sd$DBH to estimate asymmetry as a function of neighbours stem diameter
Hegyi[!(maxNNDistance_m < Re(stempos) & Re(stempos) < d$Xmax - maxNNDistance_m #here we drop all constellations falling out of the plot limits
            & maxNNDistance_m < Im(stempos) & Im(stempos) < d$Ymax - maxNNDistance_m)] <- NA


## add Hegyi and asymmetric neighboruhood indices to the data frame
d$AsymmNeighDBH<-Mod(asymmetry) # competition strength (or asymmetric neighbourhood) computed with stem diameters of neighbours
d$Hegyi<-Hegyi

###################################


# Function to remove rows without values on a specific column
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Fliter only trees with an asymmetric neighboirhood value
d2<-completeFun(d, "AsymmNeighDBH") #data sets excluding trees with no computed asymmetric neigbhourhood based on DBH
# Remove multiple-stem trees abd trees with nos tem infomration
d3<-d2[d2$Stems==1 ,]  #remove multi-stem trees, one tree has no stem info, next line removes it from the data set

d4<-d3 %>% remove_empty("rows")
#curves change when removing multi-stem trees, which overestimate dbh in relation to Height, this confirms the
#need to have one dbh and one height value for each stem of multi-stem trees ans some how treat them as individuals in future studies

Fig2B<-ggplot(d4, aes(x = DBH, y = height, colour=Condition, linetype=Condition)) + #shift between d2 (all trees) and d5, Trees with asymmetry
  geom_point(aes(size=AsymmNeighDBH), alpha=0.5)+
  ylim(-1,32)+
  xlab("Stem diameter (cm)") +
  ylab("Tree height (m)") +
  stat_smooth(method = 'nls',   geom="line", alpha=0.9, size=1, span=0.7 ,                #Fit nls model
              method.args = list( formula = 'y~H*(1-exp(-a*x))',  
                                  start=c(H=20, a=0.2)), se=FALSE)+
  scale_linetype_manual(name="Condition", values = c("solid", "longdash" ), labels=c("Grafted", "Non-grafted"))+
  #theme_bw()+
  scale_size_continuous(name=expression(atop(bold("Neighbourhood"), paste(bold("asymmetry")))))+
  scale_color_manual(name="Condition",
                     values=c("#FF7F50","#BC8F8F"),
                     breaks= c("Grafted", "Non-Grafted"),
                     labels=c("Grafted", "Non-grafted"))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),size=12, colour = "black" ),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),size=12, colour = "black"),
        axis.title.x = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, colour = "black"),
        axis.ticks.length=unit(-1.5, "mm"),
        axis.ticks = element_line(size = .5),
        axis.line = element_line(colour = 'black', size = 1),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA, color = NA))+
       
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 2.5, keyheight = 1),
         colour=guide_legend(keywidth = 2.5, keyheight = 2),
         size = guide_legend(ncol = 2, byrow = F))


#tiff("Height_dbh_allometry_Nature.tiff", width=3000, height=4000, res=400)
Fig2B
#dev.off()



####       GENERALIZED ADDITIVE MIXED EFFECTS MODELS ####
#AS A WAY TO EXPLAIN HEIGHT IN RESPOONSE TO GRAFTING, ASYMMETRIC NEIGHBOURHOOD AND STEM DIAMETER

#####################################
d32<-d4[d4$DBH<55,]# Remove outliers
d32<-d32[!is.na(d32$DBH),]
d32$Condition<-as.factor(d32$Condition)
d32$LOC<-as.factor(d32$LOC)

#### tested models ####
Mod3<-gam(height~s(AsymmNeighDBH, by=Condition)+s(DBH, by=Condition)+ Condition, data=d32,random=~(1|LOC))

Mod4<-gam(height~s(AsymmNeighDBH, by=Condition)+s(DBH, by=Condition)+ Condition, data=d32,random=~(1|LOC), family= gaussian, REML=T)

Mod5<-gam(height~s(AsymmNeighDBH, by=Condition,  bs="cc")+s(DBH, by=Condition,  bs="ds"), data=d32,random=~(1|LOC), family= gaussian, REML=T)

d32$ppt<-as.factor(d32$MeanSal)
Mod6<-gam(height~s(AsymmNeighDBH, by=Condition,  bs="cc")+s(DBH, by=Condition,  bs="ds")+  Condition, data=d32,random=~(1|LOC), family= Gamma("identity"), REML=T)

Mod7<-gam(height~s(AsymmNeighDBH, by=Condition,  bs="cc")+s(DBH, by=Condition,  bs="ds")+  Condition +  ppt, data=d32,random=~(1|LOC), family= Gamma("identity"), REML=T)

d4$ppt<-as.factor(d4$MeanSal)
d4$LOC<-as.factor(d4$LOC)
d4$Condition<-as.factor(d4$Condition)
Mod8<-gam(height~s(AsymmNeighDBH, by=Condition,  bs="cc")+s(DBH, by=Condition,  bs="ds")+  Condition +  ppt, data=d4,random=~(1|LOC), family= Gamma("identity"), REML=T)

anova(Mod3,Mod4, Mod5,Mod6, Mod7)
AIC(Mod3,Mod4, Mod5,Mod6, Mod7)
summary(Mod7)
summary(Mod8)

AIC(Mod7, Mod8)

qqnorm(residuals(Mod7))
qqline(residuals(Mod7))
plot(fitted(Mod7), residuals(Mod7))
abline(h=0)



#### Plot GAMM model results ####


#### Extended data Figure 3 ####
#tiff("Ext_Dat_Fig3.tiff", height = 4000, width = 7000, res=600)
draw(Mod7)
#dev.off()


#other diagnosis figures
appraise(Mod7)



#### Extended data Figure 4 ####

plt1<-qq_plot(Mod7, method="simulate",  point_col="steelblue",point_alpha = 0.7, alpha=0.0)+
  labs(title = NULL, subtitle = NULL)+
  scale_y_continuous(limits= c(-0.8, 0.6), labels = signs_format(accuracy = .1))+
  scale_x_continuous(labels = signs_format(accuracy = .1))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black", size=1),
        axis.text = element_text(colour = "black", size=12),
        axis.title = element_text(colour = "black", size = 14),
        )
  
df<-data.frame(log_fitted= fitted(Mod7), residuals=resid(Mod7, type= "deviance"))
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


###### Supplementary Information Fifures ####

SI_Fig1A<-ggplot(data = d4, aes(x = Hegyi, y = DBH, color = Condition,size=Hegyi)) +
  geom_point( alpha=0.5) +
  xlab("Stem diameter (cm)") +
  ylab("Tree height (m)")+
  scale_x_continuous(expression(paste( ~italic(CI))),limits = c(0, 30))+
  scale_y_continuous(expression(paste("Stem diameter", " (cm)")), limits = c(0, 60))+
  scale_color_manual(name="Condition",
                     values=c("#008080","#800080"),
                     breaks= c("Grafted", "Non-Grafted"),
                     labels=c("Grafted", "Non-grafted"))+
  scale_size(expression(paste( ~italic(CI))),
             breaks = c(0, 10, 20, 30,40))+
  #labels = expression(15^2, 17^2, 19^2, 21^2))
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),size=12, colour = "black" ),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),size=12, colour = "black"),
        axis.title.x = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, colour = "black"),
        axis.ticks.length=unit(-1.5, "mm"),
        axis.ticks = element_line(size = .5),
        axis.line = element_line(colour = 'black', size = 1),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA, color = NA))+
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 2.5, keyheight = 1),
         colour=guide_legend(keywidth = 2.5, keyheight = 2),
         size = guide_legend(ncol = 2, byrow = F))


SI_Fig1B<-ggplot(data = d4, aes(x =DBH, y = height, color = Condition,size=Hegyi)) +
  geom_point(alpha=0.5) +
  xlab("Stem diameter (cm)") +
  ylab("Tree height (m)")+
  scale_color_manual(name="Condition",
                     values=c("#008080","#800080"),
                     breaks= c("Grafted", "Non-Grafted"),
                     labels=c("Grafted", "Non-grafted"))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),size=12, colour = "black" ),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),size=12, colour = "black"),
        axis.title.x = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, colour = "black"),
        axis.ticks.length=unit(-1.5, "mm"),
        axis.ticks = element_line(size = .5),
        axis.line = element_line(colour = 'black', size = 1),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA, color = NA))+
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 2.5, keyheight = 1),
         colour=guide_legend(keywidth = 2.5, keyheight = 2),
         size = guide_legend(ncol = 2, byrow = F))

#tiff("Supp_Info_Fig1.tiff", height = 1500, width = 2000, res=300)
ggarrange(SI_Fig1A, SI_Fig1B,common.legend = TRUE, labels= c("A","B"))
#dev.off()

SI_Fig2A<-ggplot(d4, aes(x = AsymmNeighDBH, y = DBH, colour=Condition)) + #shift between d2 (all trees) and d5, Trees with asymmetry
  geom_point(aes(size=AsymmNeighDBH), alpha=0.8)+
  xlab("Neighbourhood asymmetry index") +
  ylab("Stem diameter (cm)") +
  theme_bw()+
  scale_size_continuous(name=expression(atop(bold("Neighbourhood"), paste(bold("asymmetry")))))+
  scale_color_manual(name="Condition",
                     values=c("#008080","#800080"),
                     breaks= c("Grafted", "Non-Grafted"),
                     labels=c("Grafted", "Non-grafted"))+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size=11),
        legend.title = element_text(size=12),
        legend.position = "right")+
  guides(size = guide_legend(nrow = 2, byrow = T)) 

SI_Fig2B<-ggplot(data = d4, aes(x =DBH, y = height, color = Condition,size=AsymmNeighDBH)) +
  geom_point(alpha=0.5) +
  xlab("Stem diameter (cm)") +
  ylab("Tree height (m)")+
  scale_size_continuous(name=expression(atop(bold("Neighbourhood"), paste(bold("asymmetry")))))+
  scale_color_manual(name="Condition",
                     values=c("#008080","#800080"),
                     breaks= c("Grafted", "Non-Grafted"),
                     labels=c("Grafted", "Non-grafted"))+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),size=12, colour = "black" ),
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"),size=12, colour = "black"),
        axis.title.x = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, colour = "black"),
        axis.ticks.length=unit(-1.5, "mm"),
        axis.ticks = element_line(size = .5),
        axis.line = element_line(colour = 'black', size = 1),
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA, color = NA))+
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 2.5, keyheight = 1),
         colour=guide_legend(keywidth = 2.5, keyheight = 2),
         size = guide_legend(ncol = 2, byrow = F))


Sup_Info_Figure2<-ggarrange(SI_Fig2A,SI_Fig2B, common.legend = TRUE, labels = c("A","B"),widths = 1, heights = 1)
#tiff("NeighAsymm_dbh.tiff", width=2000, height=2000, res=300)
Sup_Info_Figure2
#dev.off()

