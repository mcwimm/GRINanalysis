# Multilevel logistic regression model
# Extended data

# required packages
library(lme4)
library(DHARMa)
library(tidyverse) 

# required data
load("./data/LMtrees.Rda") 

#### Prepare data ####
# calculate total and avicennia density + 
# select avicennia trees for further calculations
LM.avis = LM.trees %>% 
   group_by(LOC) %>%
   mutate(tot.dens = n() / (30*30) * 10000) %>% 
   filter(Sp == "A") %>% 
   mutate(avi.dens = n() / (30*30) * 10000,
          r_avi = avi.dens / tot.dens)

# z-transform data (grand-mean centering (gmc) > over all study plots)
# + add observed probabaility of grafting to dataframe
LM.avis = LM.avis %>% 
   mutate(DBH.gmc = (DBH - mean(LM.avis$DBH)) / sd(LM.avis$DBH),
          salinity.gmc = (salinity - mean(LM.avis$salinity)) /
             sd(LM.avis$salinity),
          avi.dens.gmc = (avi.dens - mean(LM.avis$avi.dens)) /
             sd(LM.avis$avi.dens),
          tot.dens.gmc = (tot.dens - mean(LM.avis$tot.dens)) /
             sd(LM.avis$tot.dens),
          r_avi.gmc = (r_avi - mean(LM.avis$r_avi)) /
             sd(LM.avis$r_avi)) %>% 
   group_by(LOC) %>% 
   mutate(obs.grafted = sum(grouped)) %>% 
   arrange(DBH) %>% 
   mutate(obs.rg = cumsum(grouped),
          obs.rf = obs.rg/obs.grafted) 

# add salinity groups
LM.avis$sal.group <- ifelse(LM.avis$salinity < 45, "\u003c 45",
                            ifelse(LM.avis$salinity >= 55, 
                                   "\u2265 55",
                                   "45 \u2212 54")) 

LM.avis$sal.group <- factor(LM.avis$sal.group,
                            levels = c("\u003c 45", 
                                       "45 \u2212 54", "\u2265 55"))

#### Build generalised mixed model ####
# 'grouped' indicates whether a tree is grafted (1) or non-grafted (0)

# remove multistem trees
LM.avis_S <- LM.avis %>% 
   filter(stems == 1)
   
fm.gmc = glmer(grouped ~ DBH.gmc + tot.dens.gmc + salinity.gmc +
                  tot.dens.gmc * salinity.gmc +
                  DBH.gmc : tot.dens.gmc + DBH.gmc : salinity.gmc +
                  (DBH.gmc || LOC), 
               data = LM.avis_S, family = "binomial")

# predict grafting probability
glm.probs <- predict(fm.gmc, newdata = LM.avis_S, type = "response",
                     allow.new.levels = TRUE)

# add predictions to empirical data
LM.avis_S$fm.gmc.probs <- glm.probs
LM.avis_S$fm.gmc.pred <- ifelse(glm.probs > 0.5, 1, 0)

# model accuracy ~ 70.4%
m0 = round(nrow(LM.avis_S[LM.avis_S$grouped == LM.avis_S$fm.gmc.pred, ])/
              nrow(LM.avis_S), 3) * 100

# Residuen-Plots
simulationOutput <- simulateResiduals(fittedModel = fm.gmc, n = 500)
x11()
plot(simulationOutput)

#### Estimate ODDs ratio ####
sd.DBH.gmc <- sd(LM.avis$DBH.gmc) # standard deviation of z-transf. DBH
LM.avis$DBH_m1SD <- LM.avis$DBH.gmc + sd.DBH.gmc
LM.avis$DBH_p1SD <- LM.avis$DBH.gmc - sd.DBH.gmc

# create a model for the smallest and biggest trees, respectively
FM_m1SD <- glmer(grouped ~ DBH_m1SD + tot.dens.gmc + salinity.gmc +
                    tot.dens.gmc * salinity.gmc +
                    DBH_m1SD : tot.dens.gmc + DBH_m1SD : salinity.gmc +
                    (DBH.gmc || LOC), 
                 data = LM.avis[LM.avis$stems==1,], family = "binomial")

FM_p1SD <- glmer(grouped ~ DBH_p1SD + tot.dens.gmc + salinity.gmc +
                    tot.dens.gmc * salinity.gmc +
                    DBH_p1SD : tot.dens.gmc + DBH_p1SD : salinity.gmc +
                    (DBH.gmc || LOC), 
                 data = LM.avis[LM.avis$stems==1,], family = "binomial")

# extract p values
P_fm.gmc <- coef(summary(fm.gmc))
P_FM_m1SD <- coef(summary(FM_m1SD))
P_FM_p1SD <- coef(summary(FM_p1SD))

p_values <- c(P_fm.gmc[1:7, 4], P_FM_m1SD[3:4, 4], P_FM_p1SD[3:4, 4])
length(p_values)

# calculate odds ratios and confidence intervals
# needs a few minutes
# less precise, faster with confint argument method="Wald"
OR.FM1 <- exp(fixef(fm.gmc))
CI.FM1 <- exp(confint(fm.gmc, parm = "beta_"))

OR_m1SD.FM1 <- exp(fixef(FM_m1SD))
CI_m1SD.FM1 <- exp(confint(FM_m1SD, parm = "beta_"))


OR_p1SD.FM1 <- exp(fixef(FM_p1SD))
CI_p1SD.FM1 <- exp(confint(FM_p1SD, parm = "beta_"))

OR_m1SD.FM1[3]
OR_p1SD.FM1[3]


# create a table
OR.CI.FM1 <- rbind(cbind(OR.FM1, CI.FM1), 
                   cbind(OR_m1SD.FM1, CI_m1SD.FM1)[3,],
                   cbind(OR_m1SD.FM1, CI_m1SD.FM1)[4,],
                   #cbind(OR_m1SD.FM1, CI_m1SD.FM1)[5,],
                   #cbind(OR_m1SD.FM1, CI_m1SD.FM1)[6,],
                   #cbind(OR_m1SD.FM1, CI_m1SD.FM1)[7,],
                   cbind(OR_p1SD.FM1, CI_p1SD.FM1)[3,],
                   cbind(OR_p1SD.FM1, CI_p1SD.FM1)[4,])
                   #cbind(OR_p1SD.FM1, CI_p1SD.FM1)[5,],
                   #cbind(OR_p1SD.FM1, CI_p1SD.FM1)[6,],
                   #cbind(OR_p1SD.FM1, CI_p1SD.FM1)[7,])

rownames(OR.CI.FM1) <- c(rownames(cbind(OR.FM1, CI.FM1)), 
                         "tot.dens_m1SD",
                         "Salinity_m1SD",
                        # "salinity*tot.dens_m1SD",
                        # "dbh*tot_dens_m1SD",
                        # "dbh*salinity_m1SD",
                         "tot.dens_p1SD",
                         "Salinity_p1SD")
                        # "salinity*tot_dens_p1SD",
                        # "dbh*tot_dens_p1SD",
                        # "dbh*salinity_p1SD")
OR.CI.FM1 <- data.frame(OR.CI.FM1)
OR.CI.FM1 <- cbind(OR.CI.FM1, p_values)
OR.CI.FM1$sig <- ifelse((OR.CI.FM1$OR > 1 & OR.CI.FM1$X2.5.. > 1 
                         & OR.CI.FM1$X97.5.. > 1) |
                           (OR.CI.FM1$OR < 1 & OR.CI.FM1$X2.5.. < 1 
                            & OR.CI.FM1$X97.5.. < 1),
                        "*", "-")
colnames(OR.CI.FM1) <- c("Odd Ratios", "CI.low", "CI.upp","p_values","sig")

# save table as csv
write.csv(OR.CI.FM1, file = "OR-CI-FM1.csv")

# ... or as xlsx
# library("xlsx")
# write.xlsx(OR.CI.FM1, file = "data/Supp_Table_1.xlsx",
#            sheetName = "lm.Coeffs", append = FALSE)

# add predictions from LM.avis_S subset to full empirical data set
LM.avis$fm.gmc.probs <- LM.avis_S$fm.gmc.probs[match(LM.avis$treeID,  
                                                     LM.avis_S$treeID)]
LM.avis$fm.gmc.pred <- LM.avis_S$fm.gmc.pred[match(LM.avis$treeID,  
                                                   LM.avis_S$treeID)]

#### Save LM.avis in a Rda file ####
# save(LM.avis, file = "./data/LMavis.Rda")
# save(LM.avis_S, file = "./data/LMavis_single.Rda")
