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

rm(list = ls())

setwd("C:/Users/hajira/Desktop/DATA_11_03_13/analysis with hayley/CapePoint")
source("scripts/Inference_modelset.r")
source("scripts/Inference_compute.r")
source("scripts/corratio.R")
source("scripts/calinski.R")
source("scripts/VarScoreOMI.r")
source("scripts/doublerda.R")

traits=read.csv("Q.csv", row.names=1)
spe=read.csv("L.csv", row.names=1)
env=read.csv("R.csv",row.names=1)


#######replace NAs with mean######
#traits$LMA[is.na(traits$LMA)]=mean(traits$LMA, na.rm=T)
#traits$FWC[is.na(traits$FWC)]=mean(traits$FWC, na.rm=T)
#env$ratio[is.na(env$ratio)]=mean(env$ratio, na.rm=T)
#env$sdii[is.na(env$sdii)]=mean(env$sdii, na.rm=T)
#remove zero columns from site-species and adjust trait matrix accordingly
#z=which(colSums(spe[,2:340])!=0)
#spe=spe[,z+1]
#traits=traits[z,]
#rownames(traits)<-as.character(traits$SpeciesID)
#traits<-(traits[,2:5])


pca.env<-dudi.pca(env, scannf=FALSE)
windows()
scatter(pca.env)  #fig1
100 * pca.env$eig/sum(pca.env$eig)


coa1 <- dudi.coa(spe)
cca1 <- pcaiv(coa1, env)
100 * sum(cca1$eig) / sum(coa1$eig)

windows()
s.label(cca1$c1, clabel = 0)
par(mar = c(0.1, 0.1, 0.1, 0.1))
pointLabel(cca1$c1,row.names(cca1$c1), cex=0.7)
s.arrow(cca1$cor[-1,], add.plot=TRUE)

######################################################################################
######################################OMI-GAM#########################################
######################################################################################
?dudi.pca
windows()
pca.env<-dudi.pca(env, scannf=FALSE)
scatter(pca.env)     
100 * pca.env$eig/sum(pca.env$eig)
pca.env$lw

str(pca.env)

#analyse the distribution of species on environmental gradients using
#the OMI analysis
omi1<-niche(pca.env, spe, scannf=FALSE)
windows()
plot(omi1)  #fig2


100 * omi1$eig/sum(omi1$eig)

windows()
par(mar = c(0.10,0.10, 0.10,0.10))
s.arrow(omi1$c1, clab = 0.8, xlim=c(-2.5,2.5))
s.label(omi1$li, xax = 1,yax = 2, clabel=0,add.plot = TRUE)
par(mar = c(0.1, 0.1, 0.1, 0.1))
pointLabel(omi1$li, rownames(omi1$li), cex=0.5) #fig3

windows()
par(mfrow=c(1,2))
sco.distri(omi1$ls[,1],spe,clab=0)
sco.distri(omi1$ls[,2],spe,clab=0)    #fig4
?sco.distri

####GAM####
str(traits)
modelset<-Inference_modelset(Explanatory=traits)
inf.axis1 <- Inference_compute(Fam="gaussian", combin=modelset[[1]], Mat=modelset[[2]],
                               Response=omi1$li[,1], Explanatory=traits, Average = TRUE)
inf.axis2 <- Inference_compute(Fam="gaussian", combin=modelset[[1]], Mat=modelset[[2]],
                                 Response=omi1$li[,2], Explanatory=traits, Average = TRUE)

str(traits)
dd.names <- c('CanopyArea', 'FWC', 'Height', 'LeafThick',
                'LMA', 'LWR')
dd.names.2 <- sapply(dd.names, function(x) gsub("\\s", "\\\n", x))
windows()
barplot(inf.axis1$Var.importance[,1], names.arg=dd.names.2)

windows()
#Limits <- apply(inf.axis1$Plot.response[, seq(2, 12, by=2)], 2, range)
#lim <- c(min(Limits[1,]), max(Limits[2,]))
par(mfrow=c(3,2))
plot(inf.axis1$Plot.response[,1], inf.axis1$Plot.response[,2],
      type="l", xlab="LMA", ylab="Species position axis 1" ) #ylim=lim ,)
plot(inf.axis1$Plot.response[,3], inf.axis1$Plot.response[,4], #ylim=lim,
       type="l", xlab="CanopyArea", ylab="Species position on OMI axis 1")
plot(inf.axis1$Plot.response[,5], inf.axis1$Plot.response[,6], #ylim=lim,
       type="l", xlab="FWC", ylab="Species position on OMI axis 1")
plot(inf.axis1$Plot.response[,7], inf.axis1$Plot.response[,8], #ylim=lim,
       type="l", xlab="LWR", ylab="Species position on OMI axis 1")
plot(inf.axis1$Plot.response[,9], inf.axis1$Plot.response[,10], #ylim=lim,
     type="l", xlab="FWC", ylab="Species position on OMI axis 1")
plot(inf.axis1$Plot.response[,11], inf.axis1$Plot.response[,12], #ylim=lim,
     type="l", xlab="LWR", ylab="Species position on OMI axis 1")

Averaged.Pred.1.2<-cbind(inf.axis1$Averaged.Pred, inf.axis2$Averaged.Pred)
hc1 <- hclust(dist(Averaged.Pred.1.2), method = "ward")
windows()
plot(hc1, cex=0.2)

ntest <- 12
res <- rep(0,ntest - 1)
for (i in 2:ntest){
  fac <- cutree(hc1, k = i)
  res[i-1] <- calinski(tab=Averaged.Pred.1.2, fac = fac)[1]
}
windows()
par(mfrow=c(1,2))
plot(2:ntest, res, type='b', pch=20, xlab="Number of groups", ylab = "C-H index")
plot(3:ntest, diff(res), type='b', pch=20, 
     xlab="Number of groups", ylab = "Diff in C-H index")

windows()
nbgroup <- 6
spe.group <- as.factor(cutree(hc1, k = nbgroup))
spe.group <- as.factor(spe.group)
s.class(Averaged.Pred.1.2, spe.group, col= 1:nlevels(spe.group))
s.arrow(omi1$c1, xax=1, yax=2, csub = 1, clab = 0.8, add.plot=T)

eta2 <- cor.ratio(traits, data.frame(spe.group), weights = rep(1, length(spe.group)))
windows()
par(mfrow=n2mfrow(ncol(traits)))
#plot(table(spe.group,traits[,1]), main =names(traits)[1])
for(i in 2:ncol(traits)){
  label <- paste(names(traits)[i], "(cor.ratio =", round(eta2[i-1],3), ")")
    plot(traits[,i]~spe.group, main = label, border = 1:nlevels(spe.group))
}

########################################################################################
##############################F-"RLQ": RLQ Analysis#####################################
########################################################################################
head(traits)
rownames(traits)<-as.character(traits$SpeciesID)
str(env)
pca.traits <- dudi.pca(traits[2:5], row.w = coa1$cw, scannf = FALSE)
pca.env <- dudi.pca(env[,2:6], row.w = coa1$lw, scannf = FALSE)


rlq1 <- rlq(pca.env, coa1, pca.traits, scannf = FALSE)
summary(rlq1)
windows()
plot(rlq1)

windows()
hc2 <- hclust(dist(rlq1$lQ), method = "ward")
plot(hc2)

## Percentage of co-Inertia for each axis
100*rlq1$eig/sum(rlq1$eig)

## weighted correlations axes / env.
t(pca.env$tab)%*%(diag(pca.env$lw))%*%as.matrix(rlq1$mR)

## weighted correlations axes / traits.
t(pca.traits$tab)%*%(diag(pca.traits$lw))%*%as.matrix(rlq1$mQ)

## correlations traits / env.
rlq1$tab

s.arrow(rlq1$c1, xlim=c(-1,1), boxes = FALSE)
s.label(rlq1$li, add.plot=T, clab=1.5)

windows()
s.label(rlq1$lQ, clabel = 0)
par(mar = c(0.1, 0.1, 0.1, 0.1))
pointLabel(rlq1$lQ,row.names(rlq1$lQ), cex=0.7)


ntest <- 6
res <- rep(0,ntest - 1)
for (i in 2:ntest){
  fac <- cutree(hc3, k = i)
  res[i-1] <- calinski(tab=dbcca1$co, fac = fac)[1]
}
par(mfrow=c(1,2))
plot(2:ntest, res, type='b', pch=20, xlab="Number of groups", ylab = "C-H index")
plot(3:ntest, diff(res), type='b', pch=20, xlab="Number of groups", ylab = 
       "Diff in C-H index")


nbgroup <- ifelse((which.max(res) + 1) == ntest, nlevels(spe.group2), which.max(res) + 1)
spe.group3 <- as.factor(cutree(hc3, k = nbgroup))
levels(spe.group3) <- c("B","C","D","A")
spe.group3 <- factor(spe.group3, levels=c("A","B","C","D"))
s.class(dbcca1$co, spe.group3, col = 1:nlevels(spe.group3))
s.arrow(dbcca1$corZ[-1,],add.plot=T,clab=0.8)

#not all of code. get from pdf...

########################################################################################
##############################B-"CLUS-MOD": Cluster Regression Analysis#################
########################################################################################
#CLUS-MOD firstly builds species groups from their traits (component 3), then
#searches for the trait combination with the best response to the environmental
#variables (component 1 and 2).

max.traits<-6
min.cophenetic.corr<-0.7
max.no.groups<-10
noisecut.value<-5
min.prop<-0.9
no.boot<-20
mean.jac<-0.7
name.output.table<-"result.cluster.boot.txt"
traits.to.consider<-c('CanopyArea', 'FWC', 'Height', 'LeafThick',
                      'LMA', 'LWR')


#traits<-(traits)
#str(traits)
#cor(traits)
source("scripts/script1_cluster_analysis_report.r")
clus.boot<-read.table("result.cluster.boot.txt",sep="\t",header=TRUE)
head(clus.boot[,c(1:6,13:14)])


max.var<-3
r2.min<-0
min.cum.cov<-0.5
min.prev<-0.1


#############################################################problem#####
source("scripts/script2_group_models_report.r")

name.outputfile.1<-"modelling.output.groupwise.txt"
name.outputfile.2<-"modelling.output.clusterwise.txt"
source("scripts/script3_output_tables_report.r")
mod.groupwise<-read.table(name.outputfile.1,sep="\t",header=TRUE)
mod.clusterwise<-read.table(name.outputfile.2,sep="\t",header=TRUE)
head(mod.clusterwise[order(-mod.clusterwise$no.clusters,
                           -mod.clusterwise$r2.av),c(1:9,17)])
head(mod.groupwise[,1:3])


combis.to.plot<-c(25)
min.weight<-10
mycol<-palette()
plot.points<-0
name.table<-"spec.groups.txt"
source("scripts/script4_group_plots_report.r")

name.table<-"spec.groups.txt" #name of output file
#combis.to.compare<-as.vector(output.table.2[,"no.combi"])
combis.to.compare<-c(25,24,44,26,12,17)
source("scripts/script5_group_assignements_report.r")
spec.in.groups<-read.table("spec.groups.txt",sep="\t",header=TRUE)
head(spec.in.groups)


########################################################################################
########C-"RDA-sRTA"and D-"RDA-mRTA": redundancy analysis and regression tree###########
########################################################################################
str(spe)
env$UID<-as.numeric(env$UID)
str(env)
#?rda
rda.map <- rda(X=spe, Y=env$MAP, Z=env$MATmax, scale = TRUE)
rda.matmax <- rda(X=spe, Y=env$MATmax, Z=env$MAP, scale = TRUE)
rda.both <- rda(X=spe, Y=env[,c("MATmax","MAP")], scale = TRUE)
100 * rda.map$CCA$tot.chi/rda.map$tot.chi
100 * rda.matmax$CCA$tot.chi/rda.matmax$tot.chi

cor(env$MATmax,env$MAP)

cor(rda.map$CCA$v,rda.matmax$CCA$v)

rda.map$CCA$biplot
rda.matmax$CCA$biplot


str(traits)
traits<-traits[,2:5]
df.map <- cbind(RDA1=rda.map$CCA$v[,1],traits)
df.matmax <- cbind(RDA1=rda.matmax$CCA$v[,1],traits)
rta.map<-rpart::rpart(RDA1~.,data=df.map, xval = 100, minbucket = 3)
rta.matmax<-rpart::rpart(RDA1~.,data=df.matmax, xval = 100, minbucket = 3)


#####MAP#####
rta.map
windows()
plotcp(rta.map)
windows()
plotcp(rta.matmax)

idx.min <- which.min(rta.map$cptable[,4])
rta.map.pruned <- prune(rta.map, cp = 1e-6+rta.matmax$cptable[,1][idx.min])
windows()
plot(rta.map.pruned, ylim = c(0.28,1.1))
text(rta.map.pruned, use.n=T, cex=0.7)
arrows(0.9, 0.28, 2, 0.28, length = 0.1)
text(2, 0.26, "Species score on RDA axis (MAP)", cex = 1)


table(rta.map.pruned$where)

100 * (1-rta.map$cptable[2,3])


#####MATmax#####
rta.matmax
windows()
plotcp(rta.matmax)
cptab <- rta.matmax$cptable

idx.min <- which.min(cptab[,4])
rta.matmax.pruned <- prune(rta.matmax, cp = 1e-6+cptab[,1][idx.min])
windows()
par(mar = c(0.1, 0.1, 0.1, 0.1))
plot(rta.matmax.pruned, )#ylim = c(0.33,1.1))
text(rta.matmax.pruned, use.n=T)
arrows(0.9, 0.33, 4.1, 0.33, length = 0.1)
text(2.5, 0.31, "Species score on RDA axis (MATmax)", cex = 1)

######################
####D-RTA analysis####
######################
rda.both <- rda(X=spe, Y=env[,c("MATmax","MAP")], scale = TRUE)
100 * rda.both$CCA$tot.chi/rda.both$tot.chi
100 * rda.both$CCA$eig/rda.both$CCA$tot.chi
plot(rda.both, display=c("bp","sp"))


df.both <- cbind(RDA1=rda.both$CCA$v.eig[,1],RDA2=rda.both$CCA$v.eig[,2],traits)
windows()
par(mar = c(1, 1, 1, 1))
rta.both<-mvpart(data.matrix(df.both[,1:2])~CanopyArea+FWC+Height
                   +LeafThick+LMA+LWR, data=df.both, xval = 100, xv="1se")