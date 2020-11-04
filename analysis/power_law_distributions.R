# library(igraph)

load("./data/LMlinks.Rda") 

LM.links$from = as.character(LM.links$from)
LM.links$to = as.character(LM.links$to)

# Create network
g<-graph(c(LM.links$from, LM.links$to))

a_data <- degree(g)
#a_data <- a_data[a_data>0]

data.dist <- data.frame(k=0:max(a_data),p_k=degree_distribution(g))
data.dist <- data.dist[data.dist$p_k>0,]


ggplot(data.dist) + geom_point(aes(x=k, y=p_k)) + theme_bw()

m_pl <- displ$new(a_data)


est_pl <- estimate_xmin(m_pl)

est_pl$xmin #k_min
est_pl$pars #gamma
est_pl$gof #D


data.s <- unique(a_data)

d_est <- data.frame(K_min=sort(data.s)[1:(length(data.s)-2)], 
                    gamma=rep(0,length(data.s)-2), 
                    D=rep(0,length(data.s)-2))

for (i in d_est$K_min){
   d_est[which(d_est$K_min == i),2] <- estimate_xmin(m_pl, xmins = i)$pars
   d_est[which(d_est$K_min == i),3] <- estimate_xmin(m_pl, xmins = i)$gof
}

K.min_D.min <- d_est[which.min(d_est$D), 1]


ggplot(data=d_est, aes(x=K_min, y=D)) + 
   geom_line() + theme_bw() + 
   geom_vline(xintercept=K.min_D.min, colour="red") + 
   annotate("text", x=K.min_D.min, y=max(d_est$D)/3*2, 
            label=K.min_D.min)


ggplot(data=d_est, aes(x=K_min, y=gamma)) + 
   geom_line() + theme_bw() + 
   geom_vline(xintercept=K.min_D.min, colour="red") + 
   annotate("text", x=K.min_D.min, y=max(d_est$gamma)/3*2, 
            label=K.min_D.min)


m_pl$setXmin(est_pl)
plot.data <- plot(m_pl, draw = F)
fit.data <- lines(m_pl, draw = F)
ggplot(plot.data) + 
   geom_point(aes(x=log(x), y=log(y))) + 
   labs(x="log(k)", y="log(CDF)") + theme_bw() + 
   geom_line(data=fit.data, aes(x=log(x), y=log(y)), colour="red")  

#Investigate goodness of fit
a_bs_pl <- bootstrap_p(m_pl, no_of_sims=800, threads=2, seed = 123)
#threads=core number of processor that used by function
#parallel::detectCores() determines how many cores in your computer and adjust threads if needed

plot(a_bs_pl)
hist(a_bs_pl$bootstraps[,2], breaks="fd", xlab = "K_min")
hist(a_bs_pl$bootstraps[,3], breaks="fd",xlab = "gamma")
plot(a_bs_pl$bootstraps[,2], a_bs_pl$bootstraps[,3],
     xlab = "K_min",ylab = "gamma")

######
a_df_bs_pl <- a_bs_pl$bootstraps
ggplot(data=a_df_bs_pl, aes(pars)) + geom_histogram() + labs(x="gamma", y="frequency") + theme_bw()

ggplot(data=a_df_bs_pl, aes(xmin)) + geom_histogram() + labs(x="K_min", y="frequency") + theme_bw()

gamma_D.min <- d_est[which.min(d_est$D), 2]

allPlot1<-ggplot(data=a_df_bs_pl, aes(x=xmin, y=pars)) +
   labs(x="K_min", y="gamma") + theme_bw() + 
   geom_point(shape=21, colour="black", fill="red", size=0.5, stroke=2, 
              position = position_jitter(), alpha=0.6) +
   geom_vline(xintercept=K.min_D.min, colour="blue") +
   geom_hline(yintercept=gamma_D.min, colour="blue") +
   annotate("text", x=K.min_D.min, y=min(a_df_bs_pl$pars), 
            label=K.min_D.min, col="blue") +
   annotate("text", x=min(a_df_bs_pl$xmin), y=gamma_D.min, 
            label=round(gamma_D.min, digits=2), col="blue")


D.min <- d_est[which.min(d_est$D), 3]

ggplot(data=a_df_bs_pl, aes(gof)) + 
   geom_histogram() + 
   labs(x="D", y="frequency") + 
   geom_vline(xintercept=D.min, colour="red") + 
   theme_bw()

a_bs_pl$p #p value


#########    Fitting real distribution
#generate kmin & kmax pairs
pairs <- as.data.frame(t(combn(sort(data.s), 2)))
pairs$D <- rep(0, length(pairs$V1))
pairs$gamma <- rep(0, length(pairs$V1))

#scan D for all kmin-kmax pairs
for (i in 1:length(pairs$D)){
   m_pl$setXmin(pairs[i,1])
   pairs[i,3]<- estimate_xmin(m_pl, xmin = pairs[i,1], 
                              xmax = pairs[i,2], distance = "ks")$gof
   pairs[i,4]<- estimate_xmin(m_pl, xmin = pairs[i,1], 
                              xmax = pairs[i,2], distance = "ks")$pars
}

a_bs_pl_sat_cut <- bootstrap_p(m_pl, 
                               xmins = pairs[which.min(pairs$D), 1], 
                               xmax = pairs[which.min(pairs$D), 2], 
                               no_of_sims = 5000, threads = 2)

pairs[which.min(pairs$D), 1] #k_{sat}


pairs[which.min(pairs$D), 2] #k_{cut}

#in this range
pairs[which.min(pairs$D), 3] #D

pairs[which.min(pairs$D), 4] #gamma
a_bs_pl_sat_cut$p #p-value


pairs[which.min(pairs$D), 1] -> k_sat
pairs[which.min(pairs$D), 2] -> k_cut
pairs[which.min(pairs$D), 4] -> gamma_a

m_pl$getXmin()


#powerlaw
# To compare distributions Xmin should be the same that estimated for the Power law dist.
m_pl = displ$new(a_data)
est_pl <- estimate_xmin(m_pl, xmins = k_sat, xmax = k_cut, 
                        distance = "ks")
m_pl$setXmin(est_pl)
m_pl$setXmin(2)

#a_bs_pl <- bootstrap_p(m_pl, no_of_sims=1000, threads=8, seed = 123)
#a_bs_pl$p

#lognormal
m_ln = dislnorm$new(a_data)
est_ln <- estimate_xmin(m_ln)
m_ln$setXmin(est_ln)
m_ln$setXmin(2)
#a_bs_ln <- bootstrap_p(m_ln, no_of_sims=1000, threads=8, seed = 123)#error produced
#a_bs_ln$p
#exponential
m_exp = disexp$new(a_data)
est_exp <- estimate_xmin(m_exp)
m_exp$setXmin(est_exp)
m_exp$setXmin(2)
a_bs_exp <- bootstrap_p(m_exp, no_of_sims=1000, threads=8, seed = 123)#error produced
a_bs_exp$p
#poisson
m_poi = dispois$new(a_data)
est_poi <- estimate_xmin(m_poi)
m_poi$setXmin(est_poi)
m_poi$setXmin(2)
#a_bs_poi <- bootstrap_p(m_poi, no_of_sims=1000, threads=8, seed = 123)#error produced
#a_bs_poi$p

acomp_pl_ln = compare_distributions(m_pl, m_ln)
acomp_pl_exp = compare_distributions(m_pl, m_exp)
acomp_pl_poi= compare_distributions(m_pl, m_poi)
acomp_poi_exp = compare_distributions(m_poi, m_exp)

acomp_pl_ln$p_one_sided; acomp_pl_ln$p_two_sided
acomp_pl_exp$p_one_sided; acomp_pl_exp$p_two_sided
acomp_pl_poi$p_one_sided; acomp_pl_poi$p_two_sided
acomp_poi_exp$p_one_sided; acomp_poi_exp$p_two_sided


Plot_data<-plot(m_pl); Plot_data
a_pl<-lines(m_pl, col="red") #power law
a_ln<-lines(m_ln, col="green") #log normal
a_ps<-lines(m_poi, col="blue") # Poisson
a_ex<-lines(m_exp, col="magenta") #Exponen

a_pl$distribution<-rep("Power law",3)
a_ln$distribution<-rep("Log normal",3)
a_ps$distribution<-rep("Poisson",3)
a_ex$distribution<-rep("Exponential",3)

AlldistLines<-rbind(a_pl,a_ln,a_ps,a_ex)
#save(AlldistLines, file="AlldistLines.rda")
gamma_a

## plot compared distribution functions with X-min set to 2 (the xmin estimated fro the power law dstribution)
Fig3A<-ggplot(Plot_data) + 
   geom_point(aes(x=x, y=y)) + labs(x="Node degree", y="CDF") + 
   #ggtitle("All plots")+
   geom_line(data=AlldistLines, aes(x=x, y=y, colour=distribution, linetype=distribution), size=1.0)+
   annotate(geom="text", x=2.8, y=0.34, label="\u03B3 = 4.47",
            color="black")+
   scale_y_continuous(trans="log",breaks = trans_breaks("log", function(x) round(exp(x),2)))+
   scale_x_continuous(trans="log",breaks = trans_breaks("log", function(x) round(exp(x))))+
   scale_linetype_manual("Distribution function", values=c("solid","twodash", "dashed","dotted","logndash"))+
   scale_color_manual("Distribution function", values=c("#8B008B","#3CB371", "#4682B4","black","blue"))+
   theme(axis.ticks = element_line(size = .5),
         axis.line = element_line(colour = 'black', size = 0.5),
         axis.title.x = element_text(size=14, colour = "black"),
         axis.title.y = element_text(size=14, colour = "black"),
         text = element_text(size = 13),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         strip.background = element_rect(fill=NA),
         legend.key = element_rect(color = NA, fill = NA)
   )+
   guides(fill = guide_legend(keywidth = 1, keyheight = 1),
          linetype=guide_legend(keywidth = 2, keyheight = 1),
          colour=guide_legend(keywidth = 2, keyheight = 1))

# tiff("Fig.3B v.1 GRIN_Nature.tiff",width=3500,height = 2800, res=300)
# Fig3A
# dev.off()  


# Plot distribution functions with their own estimated xmin
m_pl$setXmin(2)
m_ln$setXmin(1)
m_exp$setXmin(1)
m_poi$setXmin(1)

Plot_data2<-plot(m_pl); Plot_data2
a_pl<-lines(m_pl, col="red") #power law
a_ln<-lines(m_ln, col="green") #log normal
a_ps<-lines(m_poi, col="blue") # Poisson
a_ex<-lines(m_exp, col="magenta") #Exponen

a_pl$distribution<-rep("Power law",3)
a_ln$distribution<-rep("Log normal",4)
a_ps$distribution<-rep("Poisson",4)
a_ex$distribution<-rep("Exponential",4)

AlldistLines2<-rbind(a_pl,a_ln,a_ps,a_ex)

Fig3b_2<-ggplot(Plot_data2) + 
   geom_point(aes(x=x, y=y)) + labs(x="Node degree", y="CDF") + 
   #ggtitle("All plots")+
   geom_line(data=AlldistLines2, aes(x=x, y=y, colour=distribution, linetype=distribution), size=1.0)+
   annotate(geom="text", x=2.8, y=0.34, label="\u03B3 = 4.47",
            color="black")+
   scale_y_continuous(trans="log",breaks = trans_breaks("log", function(x) round(exp(x),2)))+
   scale_x_continuous(trans="log",breaks = trans_breaks("log", function(x) round(exp(x))))+
   scale_linetype_manual("Distribution function", values=c("solid","twodash", "dashed","dotted","logndash"))+
   scale_color_manual("Distribution function", values=c("#8B008B","#3CB371", "#4682B4","black","blue"))+
   theme(axis.ticks = element_line(size = .5),
         axis.line = element_line(colour = 'black', size = 0.5),
         axis.title.x = element_text(size=14, colour = "black"),
         axis.title.y = element_text(size=14, colour = "black"),
         text = element_text(size = 13),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         strip.background = element_rect(fill=NA),
         legend.key = element_rect(color = NA, fill = NA)
   )+
   guides(fill = guide_legend(keywidth = 1, keyheight = 1),
          linetype=guide_legend(keywidth = 2, keyheight = 1),
          colour=guide_legend(keywidth = 2, keyheight = 1))


#### call figures
# Fig3A
# Fig3b_2
# ggarrange(Fig3A, Fig3b_2, ncol = 2)
