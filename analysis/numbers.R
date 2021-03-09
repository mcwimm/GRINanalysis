#################################################################################
# MS: Roots of cooperation: can root graft networks benefit trees under stress? #
#################################################################################

# Descriptive statistics and simple linear models 

# load data
load("./data/LMtrees.Rda") # dataframe containing tree characteristics
load("./data/LMavis.Rda") # dataframe containing a.g. tree characteristics
load("./data/LMgroups.Rda") # dataframe containing group characteristics

###########################################################
# Drivers of root grafting and implications for tree size #
###########################################################

# ranges: stand density (total and avicennia), 
# grafting frequency, salinity

d = LM.trees %>% 
   group_by(LOC) %>% 
   mutate(tot.dens = n() / (30*30) * 10000) %>% 
   filter(Sp == "A") %>%  # filter only Avicennia g. trees
   mutate(avi.trees = n(),
          avi.dens = avi.trees / (30*30) * 10000,
          avgNodeDegreeAll = round(mean(netDeg), 3)) %>% 
   filter(netDeg != 0) %>% 
   mutate(pGrafted = round(n() / avi.trees * 100, 1),
          avgNodeDegree = round(mean(netDeg), 3)) %>% 
   distinct(LOC, tot.dens = round(tot.dens, 0 ), 
            avi.dens = round(avi.dens, 0 ),
            avi.trees, pGrafted, salinity, avgNodeDegreeAll,
            avgNodeDegree) %>% 
   ungroup() %>% 
   mutate(mean_pGrafted = round(mean(pGrafted), 1)) 
d %>% 
   View()


# frequency of root grafting of top height trees
LM.trees %>% 
   filter(Sp == "A") %>% 
   ungroup() %>%
   group_by(LOC) %>%
   mutate(totalTrees = n()) %>% 
   arrange(DBH) %>% 
   mutate(r = 1:n()) %>%
   filter(r >= 0.8 * n()) %>% 
   mutate(topTrees = n()) %>% 
   filter(netDeg != 0) %>% 
   mutate(graftedTop = n()) %>% 
   mutate(pTopGrafted = round(graftedTop / topTrees * 100, 1)) %>%
   distinct(totalTrees, topTrees, graftedTop, pTopGrafted) %>%
   ungroup() %>% 
   mutate(mean_pTopGrafted = round(mean(pTopGrafted), 1)) %>% 
   View()

#####################
# NETWORK FORMATION #
#####################

# Frequency of trees with 1 partner             
nrow(LM.avis[LM.avis$netDeg == 1, ]) / nrow(LM.avis[LM.avis$netDeg != 0, ]) * 100 

# Frequency of trees with 2 partners            
nrow(LM.avis[LM.avis$netDeg == 2, ]) / nrow(LM.avis[LM.avis$netDeg != 0, ]) * 100 

# Frequency of trees with 4 partners            
nrow(LM.avis[LM.avis$netDeg == 4, ]) / nrow(LM.avis[LM.avis$netDeg != 0, ]) * 100 


# correlation node degree (average per plot) and grafting frequency
# data frame with average group size and group density (groups/ha)
g = LM.groups %>% 
   group_by(LOC) %>% 
   mutate(meanGS = mean(no.memb)) %>% 
   distinct(meanGS, groupID) %>% 
   distinct(meanGS, groupHa = n() / (30 * 30 ) * 10000)


d = merge(d, g, by = "LOC")
View(d)

# linear models: text and figure 3

## average node degree (all A.g. trees) predicted by grafting frequency
summary(lm(avgNodeDegreeAll ~ pGrafted, data = d))    
summary(lm(avgNodeDegreeAll ~ avi.dens, data = d))       
summary(lm(avgNodeDegreeAll ~ avi.dens, data = d %>% 
              filter(LOC != 3)))       

## group size predicted by avicennia density
summary(lm(meanGS ~ avi.dens, data = d))
summary(lm(groupHa ~ avi.dens, data = d))



# group size (no. of trees per group)
LM.groups %>% 
   mutate(groupsTot = n()) %>% 
   group_by(no.memb) %>% 
   distinct(groupsTot, groups = n()) %>% 
   arrange(no.memb) %>% 
   ungroup() %>% 
   mutate(cumSumGroups = cumsum(groups)) %>% 
   mutate(cumFreq = round(cumSumGroups / groupsTot * 100, 1)) %>% 
   View()


# percentage of groups containing the least required 
# number of connections to form a group
1 - LM.groups %>% 
   filter(no.memb > 2) %>% 
   mutate(N = n()) %>% 
   mutate(x = no.links / (no.memb - 1)) %>% 
   filter(x > 1) %>% 
   mutate(p = n()/N) %>% 
   distinct(p)
