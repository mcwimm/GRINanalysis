# Required packages
packages = c("ggplot2", "ggpubr",
             "janitor", "gratia", "signs", "mgcv",
             "sjPlot", "sjmisc", "sjlabelled", "qqplotr")
lapply(packages, require, character.only = TRUE)

# Load data
load("./data/LMtrees.rda") # LM.trees

d <- LM.trees

# Rename grouBin column to "Condition
colnames(d)[12] <- "Condition"

# define limits of plots
d$Ymax <- d$Xmax <- ifelse(d$LOC %in% c("6","Lr 3"), 20, 30) 

#########################
# Neighbourhood metrics #
#########################

# Compute neighbourhood metrics  using stem position as reference point 
# in a complex plane such that x,y positions are expressed as complex 
# numbers
r <- function(a) outer(1:nrow(d), a, function(x,y) y)  # function to create a matrix with nrow(d) rows all equal to a

# Define stem position in a complex plain
stempos <- d$x+d$y*1i


# Compute neighborhood asymmetry of each tree (aggregate vector 
# connecting neighbors within 5 m distance to the tree)
maxNNDistance_m <- 5 # define the perimeter of the neighborhood around a focal tree
asymmetry <- rowSums(ifelse(
   r(d$LOC) == d$LOC & r(d$treeID) != d$treeID #pull data of neighbors inside the neighbohood
   & maxNNDistance_m < Re(stempos) & Re(stempos) < d$Xmax - maxNNDistance_m
   & maxNNDistance_m < Im(stempos) & Im(stempos) < d$Ymax - maxNNDistance_m
   & r(d$height) >= d$height  # --> include this line to consider only neighbors larger than the focal tree
   & abs(r(stempos)-stempos) <= maxNNDistance_m,
   (r(stempos)-stempos) * r(d$DBH) / abs(r(stempos)-stempos)^2, 0))# cahnge crownarea for sd$DBH to estimate asymmetry as a function of neighbours stem diameter
asymmetry[!(maxNNDistance_m < Re(stempos) & Re(stempos) < d$Xmax - maxNNDistance_m #here we drop all constellations falling out of the plot limits
            & maxNNDistance_m < Im(stempos) & Im(stempos) < d$Ymax - maxNNDistance_m)] <- NA


#### Compute hegyi index for each target tree  #### 
Hegyi <- rowSums(ifelse(
   r(d$LOC) == d$LOC & r(d$treeID) != d$treeID #pull data of neighbors inside the neighbohood
   & maxNNDistance_m < Re(stempos) & Re(stempos) < d$Xmax - maxNNDistance_m
   & maxNNDistance_m < Im(stempos) & Im(stempos) < d$Ymax - maxNNDistance_m
   & r(d$height) >= d$height  # --> include this line to consider only neighbors larger than the focal tree
   & abs(r(stempos)-stempos) <= maxNNDistance_m,
   (r(d$DBH) /(d$DBH))/ abs(r(stempos)-stempos), 0))
Hegyi[!(maxNNDistance_m < Re(stempos) & Re(stempos) < 
           d$Xmax - maxNNDistance_m # here we drop all constellations falling out of the plot limits
        & maxNNDistance_m < Im(stempos) & Im(stempos) < 
           d$Ymax - maxNNDistance_m)] <- NA


## add Hegyi and asymmetric neighboruhood indices to the data frame
d$AsymmNeighDBH<-Mod(asymmetry) # competition strength (or asymmetric neighbourhood) computed with stem diameters of neighbours
d$Hegyi<-Hegyi


# Function to remove rows without values on a specific column
completeFun <- function(data, desiredCols) {
   completeVec <- complete.cases(data[, desiredCols])
   return(data[completeVec, ])
}

# Filter only trees with an asymmetric neighbourhood value
d2<-completeFun(d, "AsymmNeighDBH") #data sets excluding trees with no computed asymmetric neigbhourhood based on DBH
# Remove multiple-stem trees and trees with no stem information
d3<-d2[d2$stems==1 ,]  #remove multi-stem trees, one tree has no stem info, next line removes it from the data set

d4<-d3 %>% remove_empty("rows")

# Filter only trees of Avicennia g. species
d4<-d4[d4$Sp=="A",]

########
# GAMM #
########
# GENERALIZED ADDITIVE MIXED EFFECTS MODELS
# AS A WAY TO EXPLAIN HEIGHT IN RESPOONSE TO GRAFTING, 
# ASYMMETRIC NEIGHBOURHOOD AND STEM DIAMETER


d4<-d4[!is.na(d4$DBH),]
d4$Condition<-as.factor(d4$Condition)
d4$LOC<-as.factor(d4$LOC)
d4$ppt<-as.factor(d4$salinity)

# Model

Mod <- gam(height ~ s(AsymmNeighDBH, by=Condition, bs="cc") +
              s(DBH, by=Condition,  bs="ds") +
              Condition +  ppt, 
           data=d4,
           random=~(1|LOC), 
           family= Gamma("identity"), REML=T)

summary(Mod)
AIC(Mod)

# Plot GAMM model results - Extended data Figure 3

#tiff("Ext_Dat_Fig3.tiff", height = 4000, width = 7000, res=600)
draw(Mod)
#dev.off()

#other diagnosis figures
appraise(Mod)

# save(Mod, file = "./data/data_gam.rda")



###############
# Slenderness #
###############

d4$slenderness<-d4$height/(d4$DBH / 100)
# remove 1 tree with missing stem number attribute

d4$SlendernessSqrt<-sqrt(d4$slenderness)
d4$dbhSqrt<-sqrt(d4$DBH)
ModS1<-lm(slenderness~DBH*Condition*AsymmNeighDBH, 
          data=d4)

ModS2<-lm(SlendernessSqrt~dbhSqrt*Condition*AsymmNeighDBH, 
          data=d4)
summary(ModS2)

AIC(ModS1,ModS2)
tab_model(ModS2)

# Plot Slenderness linear model diagnostics
lmResid<-as.data.frame(qqnorm(residuals(ModS2), plot.it = FALSE))

plt1_S<-ggplot(lmResid, mapping = aes(sample = y)) +
   stat_qq_band(bandType="pointwise", mapping = NULL, alpha=0.5) +
   stat_qq_line(col="red") +
   stat_qq_point(col="steelblue", alpha=0.5) +
   labs(x = "Theoretical Quantiles", y = "Sample Quantiles")+
   scale_x_continuous(labels = signs_format(accuracy = .1))+
   scale_y_continuous(labels = signs_format(accuracy = .1))+
   theme(panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black", size=1),
         axis.text = element_text(colour = "black", size=12),
         axis.title = element_text(colour = "black", size = 14),
   )

df_S <- data.frame(log_fitted= fitted(ModS2), 
                   residuals=resid(ModS2, type= "deviance"))
head(df_S)


plt2_S<-ggplot(df_S, aes(x=log_fitted, y=residuals))+
   geom_point(alpha=0.7, colour="steelblue")+
   labs(x="Linear predictor", y="Deviance residuals")+
   geom_hline(yintercept=0, linetype="solid", color="red", size=0.5)+
   # scale_y_continuous(limits= c(-0.8, 0.6),
                     # labels = signs_format(accuracy = .1))+
   theme(panel.grid = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black", size=1),
         axis.text = element_text(colour = "black", size=12),
         axis.title = element_text(colour = "black",size = 14),
   )


# Model diagnostics plot for the multiple linear regression explaining slenderness as a reponse to grafting condition, stem diameter and asymmetric neighbourhood
ggarrange(plt1_S,plt2_S, labels = c("A","B"))



#############
# Save data #
#############

#### add results to empirical data ####
# load("./data/LMavis.Rda")

##### ACHTUNG!!! ####
# Use match() instead of merge(). Merge will exclude from data set all empty rows
# add Hegyi, AsymmNeighDBH, Slenderness and DBH to LMavis.rda

# LM.avis_bis = merge(LM.avis[, c(1:32)], d4[, c(3, 19:21)], by = "treeID")
# length(LM.avis_bis$treeID)

#### USE MATCH() ####

# LM.avis$AsymmNeighDBH <- d4$AsymmNeighDBH[match(LM.avis$treeID,  d4$treeID)]
# LM.avis$Hegyi <- d4$Hegyi[match(LM.avis$treeID,  d4$treeID)]
# LM.avis$slenderness <- d4$slenderness[match(LM.avis$treeID,  d4$treeID)]


#### rearrange data frame (perhaps not necessary) ####

# library(stringr)
# PloTree<-str_split_fixed(LM.avis$treeID, "_", 2)

# colnames(PloTree) <- c("plot", "tree")
# PloTree <- as.data.frame(PloTree)


# LM.avis <- cbind(LM.avis, PloTree)

# LM.avis$plot <-as.numeric(as.character(test1$plot))
# LM.avis$tree <-as.numeric(as.character(test1$tree))

# LM.avis <- LM.avis[with(LM.avis, order(plot, tree)), ]
# LM.avis <- LM.avis[, 1:37]


#### Save updated LMavis.Rda file #### 
# save(LM.avis, file = "./data/LMavis.Rda")



