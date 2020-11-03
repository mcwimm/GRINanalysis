# Multilevel logistic regression model
# Extended data

# required packages
library(lme4)
library(DHARMa)
library(tidyverse) 

# required data
load("./data/LMtrees.Rda") 

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

# build generalised mixed model 
# 'grouped' indicates whether a tree is grafted (1) or non-grafted (0)
fm.gmc = glmer(grouped ~ DBH.gmc + tot.dens.gmc + salinity.gmc +
                  tot.dens.gmc * salinity.gmc +
                  DBH.gmc : tot.dens.gmc + DBH.gmc : salinity.gmc +
                  (DBH.gmc || LOC), 
               data = LM.avis, family = "binomial")

# predict grafting probability
glm.probs <- predict(fm.gmc, newdata = LM.avis, type = "response",
                     allow.new.levels = TRUE)

# add predictions to empirical data
LM.avis$fm.gmc.probs <- glm.probs
LM.avis$fm.gmc.pred <- ifelse(glm.probs > 0.5, 1, 0)

# model accuracy ~ 69%
m0 = round(nrow(LM.avis[LM.avis$grouped == LM.avis$fm.gmc.pred, ])/
              nrow(LM.avis), 3) * 100

# add salinity groups
LM.avis$sal.group <- ifelse(LM.avis$salinity < 45, "\u003c 45",
                            ifelse(LM.avis$salinity >= 55, 
                                   "\u2265 55", # kleiner-gleich  & \u2264 60
                                   "45 \u2212 54")) #gr??er-gleich \u2265 

LM.avis$sal.group <- factor(LM.avis$sal.group,
                            levels = c("\u003c 45", 
                                       "45 \u2212 54", "\u2265 55"))

# save(LM.avis, file = "./data/LMavis.Rda")

# Residuen-Plots
simulationOutput <- simulateResiduals(fittedModel = fm.gmc, n = 500)
x11()
plot(simulationOutput)

# ODDs ratio
sd.DBH.gmc <- sd(LM.avis$DBH.gmc) # standard deviation of z-transf. DBH
LM.avis$DBH_m1SD <- LM.avis$DBH.gmc + sd.DBH.gmc
LM.avis$DBH_p1SD <- LM.avis$DBH.gmc - sd.DBH.gmc

# create a model for the smallest and biggest trees, respectively
FM_m1SD <- glmer(grouped ~ DBH_m1SD + tot.dens.gmc + salinity.gmc +
                    tot.dens.gmc * salinity.gmc +
                    DBH_m1SD : tot.dens.gmc + DBH_m1SD : salinity.gmc +
                    (DBH.gmc || LOC), 
                 data = LM.avis, family = "binomial")

FM_p1SD <- glmer(grouped ~ DBH_p1SD + tot.dens.gmc + salinity.gmc +
                    tot.dens.gmc * salinity.gmc +
                    DBH_p1SD : tot.dens.gmc + DBH_p1SD : salinity.gmc +
                    (DBH.gmc || LOC), 
                 data = LM.avis, family = "binomial")

# calculate odds ratios and confidence intervals
OR.FM1 <- exp(fixef(fm.gmc))
CI.FM1 <- exp(confint(fm.gmc, parm = "beta_")) # needs a few minutes

OR_m1SD.FM1 <- exp(fixef(FM_m1SD))
CI_m1SD.FM1 <- exp(confint(FM_m1SD, parm = "beta_"))

OR_p1SD.FM1 <- exp(fixef(FM_p1SD))
CI_p1SD.FM1 <- exp(confint(FM_p1SD, parm = "beta_"))

# create a table
OR.CI.FM1 <- rbind(cbind(OR.FM1, CI.FM1), 
                   cbind(OR_m1SD.FM1, CI_m1SD.FM1)[3,],
                   cbind(OR_p1SD.FM1, CI_p1SD.FM1)[3,])

rownames(OR.CI.FM1) <- c(rownames(cbind(OR.FM1, CI.FM1)), 
                         "salinity*tot.dens_m1SD", 
                         "salinity*tot.dens_p1SD")
OR.CI.FM1 <- data.frame(OR.CI.FM1)
OR.CI.FM1$sig <- ifelse((OR.CI.FM1$OR > 1 & OR.CI.FM1$X2.5.. > 1 
                         & OR.CI.FM1$X97.5.. > 1) |
                           (OR.CI.FM1$OR < 1 & OR.CI.FM1$X2.5.. < 1 
                            & OR.CI.FM1$X97.5.. < 1),
                        "*", "-")

# save table as csv
write.csv(OR.CI.FM1, file = "OR-CI-FM1.csv")