library(FD)
library(AICcmodavg)

####################### Code from "community aggregate traits.r ###############

setwd("C:/Work/Dimensions/Data")
library(reshape)
library(lattice)

##Read in data

tr1=read.csv("Data/SpeciesTraits_ErrorsExcluded.csv")
pl1=read.csv("Data/ReleveQuadrat2010_NamesCorrected.csv")

head(tr1)
head(pl1)

pl1$NewSpeciesID=paste(pl1$NewGenus, pl1$NewSpecies)

###Two ways to normalize cover--by plot & by quadrat
pl1[is.na(pl1)==T]=0
head(pl1)

##by plot

#Add total cover (across all 10 quadrats)
pl1$TotCov=(pl1$A_PercCov+pl1$B_PercCov+pl1$C_PercCov+pl1$D_PercCov+pl1$E_PercCov+pl1$F_PercCov+pl1$G_PercCov+pl1$H_PercCov+pl1$I_PercCov+pl1$J_PercCov)/10
head(pl1)

#normalize cover
plottotcov=matrix(nrow=length(unique(pl1$Plot)), ncol=1)
for (i in 1:length(unique(pl1$Plot))){
  plottotcov[i]=sum(pl1$TotCov[pl1$Plot==(unique(pl1$Plot)[i])])
  }
summary(plottotcov)

plottotcov1=cbind(unique(pl1$Plot), plottotcov)
colnames(plottotcov1)=c("Plot","TotalCover")
head(plottotcov1)
str(plottotcov1)
plottotcov1=as.data.frame(plottotcov1)
str(plottotcov1)

#species x plot matrix
spabun_pn=cbind(pl1[,c("Plot","NewSpeciesID","TotCov")])
for (i in 1:nrow(spabun_pn)){
  spabun_pn$PlotTotCov[i]=plottotcov1$TotalCover[plottotcov1$Plot==spabun_pn$Plot[i]]
  }

spabun_pn$WghtedCov=spabun_pn$TotCov/spabun_pn$PlotTotCov

head(spabun_pn)
summary(spabun_pn)

spabun_plot=cast(spabun_pn, Plot~NewSpeciesID, value="WghtedCov")
head(spabun_plot)
summary(spabun_plot)

# check
table(rowSums(spabun_plot, na.rm=T)) #should all be 1




#### Percent of plot made up of species with no trait data
sp_p=colnames(spabun_plot[2:length(colnames(spabun_plot))])
sp_p
tr1$SpeciesID=paste(tr1$Genus, tr1$Species)
sp_t=as.character(tr1$SpeciesID)
sp_t

allsp=matrix(nrow=length(unique(c(sp_p, sp_t))), ncol=3)
colnames(allsp)=c("Species", "Plots", "Traits")
allsp=as.data.frame(allsp)
allsp$Species=unique(c(sp_p, sp_t))
allsp$Plots=allsp$Species%in%sp_p  # Is species in 2010 releve data- T/F?
allsp$Traits=allsp$Species%in%sp_t # Is species in trait data- T/F?
head(allsp)
summary(allsp)

#noplot=allsp$Species[allsp$Plots==F]
notrait=allsp$Species[allsp$Traits==F]
notrait

#Percentage of each plot that is species with no trait data
nodataperc=subset(spabun_plot, select=notrait)
nodataperc1=rowSums(nodataperc, na.rm=T)
nodataperc1
summary(nodataperc1)
hist(nodataperc1)
nodataperc2=cbind(spabun_plot$Plot, nodataperc1)
nodataperc2
nodataperc2=nodataperc2[order(nodataperc2[,2]),]


##### Plot x Species Matrix - Renormalized to Exclude Species w/o Trait Data
colnames(nodataperc2)=c("Plot", "PercNA")
dim(nodataperc2)
dim(spabun_plot)

spabun_plot1=merge(spabun_plot, nodataperc2, by="Plot")
head(spabun_plot1)


# Exlude plots with more than 10% of cover from species w/no trait data   <- This is 8 plots out of 67
# (If we excluded plots with more than 20% of cover from species which we don't have trait data for, it would be 1 plot out of 67
spa=spabun_plot1[spabun_plot1$PercNA<0.10,]
dim(spa)
head(spa)

# Remove species with no trait data & re-normalize
spa=subset(spa, select=c("Plot", c(colnames(spa)[colnames(spa)%in%sp_t==T])))
head(spa)
dim(spa)
PlotTotal=rowSums(spa[2:ncol(spa)], na.rm=T)
spa[,2:ncol(spa)]=spa[,2:ncol(spa)]/PlotTotal
rowSums(spa[2:ncol(spa)], na.rm=T)  #should all be 1

# Get trait data ready for matrix multiplication
head(tr1)
sp_spa=as.character(tr1$SpeciesID[tr1$SpeciesID%in%colnames(spa)==T])
sp_spa=unique(sp_spa)
sp_spa
length(sp_spa)
dim(spa)
tr1$SpeciesID=as.character(tr1$SpeciesID)
tr2=tr1[tr1$SpeciesID%in%sp_spa==T,]
head(tr2)
tr2=tr2[order(tr2$SpeciesID),]
head(tr2)

# Select traits to be multiplied
tr3=subset(tr2, select=c("SpeciesID", "LeafLength_cm_Mean", "AvgLeafWidth_cm_Mean", "MaxLeafWidth_cm_Mean",
          "LeafThickness_mm_Mean", "SLA_Mean", "LeafSucculence_Mean", "TwigSucculence_Mean", "Height_cm_Max",
          "CanopyX_cm_Max", "CanopyY_cm_Max", "PercN", "PercC", "C.N_ratio"))
head(tr3)
dim(tr3)
head(spa)
dim(spa)


####################### New code ###############

#Exclude species which don't occur in any community
colS=colSums(spa[,2:ncol(spa)], na.rm=T)
spzero=names(colS[colS==0])

spzero=c(spzero, "Ficinia oligantha") #exclude F. oligantha because it causes an error

spa=spa[,colnames(spa) %in% spzero==F]
tr3=tr3[tr3$SpeciesID %in% spzero==F,]

#standardize traits


#Create species-by-species distance matrix computed from functional traits
dmatrix=scale(tr3[,2:ncol(tr3)])
rownames(dmatrix)=tr3[,1]
spdist=dist(dmatrix)      # Use gowdis if there are other kinds of variables: factors, etc.

#Find NAs in distance matrix & fix error
#spdistmtx=as.matrix(spdist)
#which(is.na(spdistmtx)==T, arr.ind=T)

#spdistmtx[112,108]
#spdistmtx[108,112]

#tr3[tr3$SpeciesID=="Ficinia trichodes",]
#tr3[tr3$SpeciesID=="Ficinia oligantha",]


#Change species abundance dataframe to matrix
abunsp= as.matrix(spa[,2:ncol(spa)], )


#Functional Dispersion
fd=fdisp(spdist, abunsp)


#Calculate all distance-based indices
dist.ind=dbFD(dmatrix, abunsp, corr="none")

func.ind=cbind(spa$Plot, dist.ind$FRic, dist.ind$FEve, dist.ind$FDiv, dist.ind$FDis)
colnames(func.ind)=c("Plot","FRic","FEve","FDiv","FDis")

write.csv(func.ind, "Data/plotfunctionalindices.csv", row.names=F)

###############################################################################
############ Regressions of plot traits vs. indices ###########################
###############################################################################

func.ind=read.csv("Data/plotfunctionalindices.csv")

### Create plot description file

env=read.csv("PreprocessedData/PlotDescription.csv")
head(env)

#Subset plot env columns; combine center depth measurements
env1=env[,c(1:5)]
head(env1)
env1$Depth_Center_m[is.na(env1$Depth_Center_m)==T]=env$Depth_Center_cm[is.na(env1$Depth_Center_m)==T]/100
#Create slope/aspect metrics
env1$eastwestness=sin(env1$Slope*pi/180)*cos(env1$Aspect*pi/180)
env1$northsouthness=sin(env1$Slope*pi/180)*sin(env1$Aspect*pi/180)
env1$slopetransformed=sin(env1$Slope*pi/180)

# Subset of plots included in analysis
env2=env1[env1$Plot %in% func.ind$Plot==T,]

#normalize
envraw=env2 #untransformed copy

env2$Moisture=as.factor(env2$Moisture)
env2$Depth_Center_m=scale(env2$Depth_Center_m)
env2$eastwestness=scale(env2$eastwestness)
env2$northsouthness=scale(env2$northsouthness)
env2$slopetransformed=scale(env2$slopetransformed)

#Combine
func.reg=merge(func.ind, env2, by="Plot")

### Regressions

lm.FRic=lm(log(FRic) ~ Moisture + Depth_Center_m + eastwestness + northsouthness + slopetransformed, data=func.reg) 
lm.FEve=lm(FEve ~ Moisture + Depth_Center_m + eastwestness + northsouthness + slopetransformed, data=func.reg)
lm.FDiv=lm(FDiv ~ Moisture + Depth_Center_m + eastwestness + northsouthness + slopetransformed, data=func.reg)
lm.FDis=lm(FDis ~ Moisture + Depth_Center_m + eastwestness + northsouthness + slopetransformed, data=func.reg)

summary(lm.FRic)
summary(lm.FEve)
summary(lm.FDiv)
summary(lm.FDis)


###FRic
lm.FRic1=lm(log(FRic) ~ Depth_Center_m + eastwestness + northsouthness + slopetransformed, data=func.reg)
lm.FRic2=lm(log(FRic) ~ Moisture + Depth_Center_m + northsouthness + slopetransformed, data=func.reg)
lm.FRic3=lm(log(FRic) ~ Depth_Center_m + northsouthness + slopetransformed, data=func.reg)
lm.FRic4=lm(log(FRic) ~ Moisture + Depth_Center_m + northsouthness, data=func.reg)
lm.FRic5=lm(log(FRic) ~ Depth_Center_m + slopetransformed, data=func.reg)
lm.FRic6=lm(log(FRic) ~ Depth_Center_m + northsouthness, data=func.reg)
lm.FRic7=lm(log(FRic) ~ Depth_Center_m, data=func.reg)

lm.FRicNS=lm(log(FRic) ~ northsouthness, data=func.reg)
lm.FRicST=lm(log(FRic) ~ slopetransformed, data=func.reg)

# FRic AIC table
FRicReg=list()
FRicReg[[1]]=lm.FRic
FRicReg[[2]]=lm.FRic1
FRicReg[[3]]=lm.FRic2
FRicReg[[4]]=lm.FRic3
FRicReg[[5]]=lm.FRic4
FRicReg[[6]]=lm.FRic5
FRicReg[[7]]=lm.FRic6
FRicReg[[8]]=lm.FRic7


aictab(cand.set=FRicReg, modnames=paste("Reg", 0:7))

# Models with Depth at Center, northsouthness, & slopetransformed get best AIC values

summary(lm.FRic3)

plot(func.reg$Depth_Center_m, log(func.reg$FRic))
plot(func.reg$northsouthness, log(func.reg$FRic))
plot(func.reg$slopetransformed, log(func.reg$FRic))

###FEve

lm.FEve1=lm(FEve ~ Moisture + Depth_Center_m + northsouthness + slopetransformed, data=func.reg)
lm.FEve2=lm(FEve ~ Moisture + Depth_Center_m + slopetransformed, data=func.reg)
lm.FEve3=lm(FEve ~ Moisture + Depth_Center_m, data=func.reg)
lm.FEve4=lm(FEve ~ Moisture, data=func.reg)
lm.FEve5=lm(FEve ~ Depth_Center_m, data=func.reg)

FEveReg=list()
FEveReg[[1]]=lm.FEve
FEveReg[[2]]=lm.FEve1
FEveReg[[3]]=lm.FEve2
FEveReg[[4]]=lm.FEve3
FEveReg[[5]]=lm.FEve4
FEveReg[[6]]=lm.FEve5

aictab(cand.set=FEveReg, modnames=paste("Reg", 0:5))

# Depth at Center OR Moisture

plot(func.reg$Depth_Center_m, func.reg$FEve)
plot(func.reg$Moisture, func.reg$FEve)

###FDiv
lm.FDiv1=lm(FDiv ~ Moisture + eastwestness + northsouthness + slopetransformed, data=func.reg)
lm.FDiv2=lm(FDiv ~ Moisture + northsouthness + slopetransformed, data=func.reg)
lm.FDiv3=lm(FDiv ~ Moisture + slopetransformed, data=func.reg)
lm.FDiv4=lm(FDiv ~ slopetransformed, data=func.reg)
lm.FDiv5=lm(FDiv ~ Moisture, data=func.reg)

FDivReg=list()
FDivReg[[1]]=lm.FDiv
FDivReg[[2]]=lm.FDiv1
FDivReg[[3]]=lm.FDiv2
FDivReg[[4]]=lm.FDiv3
FDivReg[[5]]=lm.FDiv4
FDivReg[[6]]=lm.FDiv5

aictab(cand.set=FDivReg, modnames=paste("Reg", 0:5))

##FDis
lm.FDis1=lm(FDis ~ Moisture + Depth_Center_m + eastwestness + slopetransformed, data=func.reg)
lm.FDis2=lm(FDis ~ Depth_Center_m + eastwestness + slopetransformed, data=func.reg)
lm.FDis3=lm(FDis ~ Depth_Center_m + slopetransformed, data=func.reg)
lm.FDis4=lm(FDis ~ slopetransformed, data=func.reg)

FDisReg=list()
FDisReg[[1]]=lm.FDis
FDisReg[[2]]=lm.FDis1
FDisReg[[3]]=lm.FDis2
FDisReg[[4]]=lm.FDis3
FDisReg[[5]]=lm.FDis4

aictab(cand.set=FDisReg, modnames=paste("Reg", 0:4))

###### All plots

#Func Rich
x=envraw$Depth_Center_m
y=log(func.reg$FRic)
plot(x, y)
#lines(loess.smooth(x, y), col="red")
lmplot=lm(y~x)
abline(lmplot, col="blue")

x=envraw$northsouthness
y=log(func.reg$FRic)
plot(x, y)
#lines(loess.smooth(x, y), col="red")
lmplot=lm(y~x)
abline(lmplot, col="blue")

#plot(envraw$eastwestness, log(func.reg$FRic))
#lines(loess.smooth(envraw$eastwestness, log(func.reg$FRic)))

x=envraw$slopetransformed
y=log(func.reg$FRic)
plot(x, y)
#lines(loess.smooth(x, y), col="red")
lmplot=lm(y~x)
abline(lmplot, col="blue")

#plot(func.reg$Moisture, log(func.reg$FRic))

#Func Even
x=envraw$Depth_Center_m
y=func.reg$FEve
plot(x, y)
#lines(loess.smooth(x, y), col="red")
lmplot=lm(y~x)
abline(lmplot, col="blue")

plot(func.reg$Moisture, func.reg$FEve)

#FDiv
x=envraw$slopetransformed
y=func.reg$FDiv
plot(x, y)
#lines(loess.smooth(x, y), col="red")
lmplot=lm(y~x)
abline(lmplot, col="blue")

plot(func.reg$Moisture, func.reg$FDiv)

x=envraw$northsouthness
y=func.reg$FDiv
plot(x, y)
#lines(loess.smooth(x, y), col="red")
lmplot=lm(y~x)
abline(lmplot, col="blue")

#FDis
x=envraw$slopetransformed
y=func.reg$FDis
plot(x, y)
#lines(loess.smooth(x, y), col="red")
lmplot=lm(y~x)
abline(lmplot, col="blue")

##### Old plots
plot(func.reg$northsouthness, func.reg$FEve)
lines(loess.smooth(func.reg$northsouthness, func.reg$FEve))
plot(func.reg$eastwestness, func.reg$FEve)
lines(loess.smooth(func.reg$eastwestness, func.reg$FEve))
plot(func.reg$slopetransformed, func.reg$FEve)
lines(loess.smooth(func.reg$slopetransformed, func.reg$FEve))

plot(func.reg$Depth_Center_m, func.reg$FDiv)
plot(func.reg$Moisture, func.reg$FDiv)
plot(func.reg$northsouthness, func.reg$FDiv)
plot(func.reg$eastwestness, func.reg$FDiv)
plot(func.reg$slopetransformed, func.reg$FDiv)

plot(func.reg$Depth_Center_m, func.reg$FDis)
plot(func.reg$Moisture, func.reg$FDis)
plot(func.reg$northsouthness, func.reg$FDis)
plot(func.reg$eastwestness, func.reg$FDis)
plot(func.reg$slopetransformed, func.reg$FDis)