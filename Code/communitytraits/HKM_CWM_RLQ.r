setwd("C:/Work/ProtPelTrait/Kleyer/CapePointK")

##Some functions
source("scripts/Inference_modelset.r")
source("scripts/Inference_compute.r")
source("scripts/corratio.R")
source("scripts/calinski.R")
source("scripts/VarScoreOMI.r")
source("scripts/doublerda.R")

##Some packages
library(ade4)
library(MASS)
library(vegan) 
library(ecodist) 
library(maptools) 
library(rpart)
library(splines)
library(gam) 
library(pgirmess) 
library(utils)
library(combinat) 
library(mvpart) 
library(cluster)
library(fpc) 
library(clusterSim)
library(lmtest)
library(Hmisc) 
library(gplots)

##Read data
setwd("C:/Work/ProtPelTrait/MultivariateAnalysis")
traits=read.csv("Data/Q.csv", row.names=1)
spe=read.csv("Data/L.csv", row.names=1)
env=read.csv("Data/R.csv",row.names=1)


# Baseline: species responses to env gradients
coa1 <- dudi.coa(spe, scannf = F)
cca1 <- pcaiv(coa1, env, scannf = F)

# percentage in variation in species composition
100 * sum(cca1$eig) / sum(coa1$eig)

#########################################################
######################## CWM-RDA ########################


#sp x trait table, matrix mult
cwm.tab <- prop.table(as.matrix(spe),1)%*%as.matrix(scale(traits))
#redundancy analysis
pca.cwm <- dudi.pca(cwm.tab,scannf=FALSE)
rda.cwm <- pcaiv(pca.cwm,env, scannf=FALSE)

# percentage of variation in community traits explained by env
100 * sum(rda.cwm$eig) / sum(pca.cwm$eig)
# percentages of variation assigned to each axis
100 * rda.cwm$eig / sum(rda.cwm$eig)

barplot(100 * rda.cwm$eig / sum(rda.cwm$eig), main="Percent variation assigned to each axis", xlab="RCA Eigenvector",ylab="% Variation")

# relationships between the trait and the environmental variables
windows()
par(mar = c(0.1, 0.1, 0.1, 0.1))
s.arrow(rda.cwm$c1, xlim=c(-1.5,1), boxes = FALSE)
s.label(rda.cwm$cor[-1,], add.plot=T, clab=1.5)

#########################################################
########################## RLQ ##########################

#Prior to the analysis, the table L must be analysed by correspondence 
#analysis. Species and sites weights computed in these analysis are 
#then used in the analyses of species traits (Q) and environmental 
#variables (R)
pca.traits <- dudi.pca(traits, row.w = coa1$cw, scannf = FALSE)
pca.env <- dudi.pca(env, row.w = coa1$lw, scannf = FALSE)


#RLQ analysis
rlq1 <- rlq(pca.env, coa1, pca.traits, scannf = FALSE)
summary(rlq1)

#plot
windows()
plot(rlq1)

## Percentage of co-Inertia for each axis
100*rlq1$eig/sum(rlq1$eig)
barplot(100*rlq1$eig/sum(rlq1$eig), main="Percentage of Environment/Trait Costructure (Co-Inertia)", xlab="RLQ Eigenvector",ylab="% variaion in Co-Inertia")

## weighted correlations axes / env.
t(pca.env$tab)%*%(diag(pca.env$lw))%*%as.matrix(rlq1$mR)
## weighted correlations axes / traits.
t(pca.traits$tab)%*%(diag(pca.traits$lw))%*%as.matrix(rlq1$mQ)
## correlations traits / env.
rlq1$tab

#biplot
windows()
s.arrow(rlq1$c1, xlim=c(-1,1.5), boxes = FALSE)
s.label(rlq1$li, add.plot=T, clab=.75)

#species scores
s.label(rlq1$lQ, clabel = 0)
#pointLabel(rlq1$lQ,row.names(rlq1$lQ), cex=0.7)

#classification using these scores to obtain functional groups
hc2 <- hclust(dist(rlq1$lQ), method = "ward")
plot(hc2)

#use the Calinsky-Harabasz criteria to find the best partition 
#(try between 2 and 20 groups) :
ntest <- 20
res <- rep(0,ntest - 1)
for (i in 2:ntest){
  fac <- cutree(hc2, k = i)
  res[i-1] <- calinski(tab=rlq1$lQ, fac = fac)[1]
}
par(mfrow=c(1,2))
plot(2:ntest, res, type='b', pch=20, xlab="Number of groups", ylab = "C-H index")
plot(3:ntest, diff(res), type='b', pch=20, xlab="Number of groups", ylab = "Diff in C-H index")

#### Best partition is for 6 groups (this will change for each--check!)

spe.group2 <- as.factor(cutree(hc2, k = which.max(res) +1))
levels(spe.group2) <- c(letters)
spe.group2 <- factor(spe.group2, levels=c(letters)) # change depending on how many
windows()
s.class(rlq1$lQ, spe.group2, col= 1:nlevels(spe.group2))
#s.arrow(rlq1$c1, add.plot = T,clab=0.8)
s.arrow(rlq1$c1*6, add.plot = T,clab=0.8)

#drop unused factor levels
spe.group2=factor(spe.group2)

#interpret partition in terms of traits
eta2 <- cor.ratio(traits[,-1], data.frame(spe.group2), weights = rep(1, length(spe.group2)))
windows()
par(mfrow=n2mfrow(ncol(traits)))
for(i in 2:ncol(traits)){
  label <- paste(names(traits)[i], "(cor.ratio =", round(eta2[i-1],3), ")")
  plot(traits[,i]~spe.group2, main = label, border = 1:nlevels(spe.group2))
}

##########################################################
####################### double CCA #######################

#Double CCA is based on the correspondence analysis of the 
#species-by-sites table. In this analysis, the ordination of 
#sites and species is constrained by both species traits and 
#environment:
dbcca1 <- dbrda(coa1,env, traits, scannf = FALSE)

