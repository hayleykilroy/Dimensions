setwd("C:/Work/Dimensions/Data")
library(reshape)
library(lattice)

##Read in data

tr1=read.csv("Data/SpeciesTraits_ErrorsExcluded.csv")
pl1=read.csv("Data/ReleveQuadrat2010_NamesCorrected.csv")

head(tr1)
head(pl1)

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
spabun_pn=cbind(pl1[,c("Plot","SpeciesID","TotCov")])
for (i in 1:nrow(spabun_pn)){
  spabun_pn$PlotTotCov[i]=plottotcov1$TotalCover[plottotcov1$Plot==spabun_pn$Plot[i]]
  }

spabun_pn$WghtedCov=spabun_pn$TotCov/spabun_pn$PlotTotCov

head(spabun_pn)
summary(spabun_pn)

spabun_plot=cast(spabun_pn, Plot~SpeciesID, value="WghtedCov")
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

############## Resume editing here: #############################

# Exlude plots with more than 10% of cover of species w/no trait data   <- This is half the plots??
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
sp_spa=as.character(tr1$NewSpeciesID[tr1$NewSpeciesID%in%colnames(spa)==T])
sp_spa=unique(sp_spa)
sp_spa
length(sp_spa)
dim(spa)
tr1$NewSpeciesID=as.character(tr1$NewSpeciesID)
tr2=tr1[tr1$NewSpeciesID%in%sp_spa==T,]
head(tr2)
tr2=tr2[order(tr2$NewSpeciesID),]
head(tr2)

mult=table(tr2$NewSpeciesID)
mult=mult[mult>1]
mult
mult=as.matrix(mult)

colnames(tr2)[5:8]=paste(colnames(tr2)[5:8],"_Mean", sep="")

tr2[tr2$NewSpeciesID=="Coleonema album",]
tr2[tr2$NewSpeciesID=="Disparago ericoides",]
tr2[tr2$NewSpeciesID=="Erica labialis",]
tr2[tr2$NewSpeciesID=="Syncarpha speciosissima",]
tr2[tr2$NewSpeciesID=="Erica hispidula",]
tr2[tr2$NewSpeciesID=="Zygophyllum spinosum",]

trm=tr2[tr2$NewSpeciesID%in%row.names(mult)==T,]
trm

#arbitrarily pick a row for each duplicate...

#trm2=trm[c(1,3,6,8,10,12),]
#trm2

#edit duplicates in excel
write.csv(trm2, file="duplicates.csv", row.names=F)

tr3=tr2[tr2$NewSpeciesID%in%c("Coleonema album","Disparago ericoides","Erica abietina","Syncarpha speciosissima","Erica hispidula","Zygophyllum spinosum")==F,]
tr3=rbind(tr3,trm2)
head(tr3)
dim(tr3)
dim(spa)
tr3=tr3[order(tr3$NewSpeciesID),]

head(spa)
head(tr3)

spa2=spa
spa2[is.na(spa2)==T]=0

trcat=tr3[,10:24]
head(trcat)
dim(trcat)
str(trcat)

spacat=spa2[,2:ncol(spa2)]
head(spacat)
dim(spacat)
str(spacat)

trcat=as.matrix(trcat)
head(trcat)
dim(trcat)
str(trcat)

#Excluding NAs--this is problematic!!! NAs can't just be zeros
trcat[is.na(trcat)==T]=0

spacat=as.matrix(spacat)

catm=spacat%*%trcat
head(catm)
dim(catm)
summary(catm)

catm=as.data.frame(catm)

hist(catm$SLA_Mean)

catm2=catm[,c(2:6, 9:10, 13:15)]
head(catm2)

catm2=cbind(spa$Plot, catm2)
colnames(catm2)[1]="Plot"

##### By Plot Env Factors
env=read.csv("Env/PlotDescription.csv")
head(env)

env1=env[,c(1:4)]
head(env1)

catm2=merge(catm2, env1, by="Plot")
head(catm2)

par(mfrow=c(2,3))

catm2$Moisture=as.factor(catm2$Moisture)

boxplot(catm2$SLA_Mean~catm2$Moisture, varwidth=T, notch=T)
boxplot(catm2$Moisture, catm2$LeafArea_cm2_Mean)
boxplot(catm2$Moisture, catm2$MaxLeafWidth_cm_Mean)
boxplot(catm2$Moisture, catm2$LeafThickness_mm_Mean)
boxplot(catm2$Moisture, catm2$TwigSucculence_Mean)
boxplot(catm2$Moisture, catm2$LeafSucculence_Mean)

plot(catm2$Slope, catm2$SLA_Mean)
plot(catm2$Slope, catm2$LeafArea_cm2_Mean)
plot(catm2$Slope, catm2$MaxLeafWidth_cm_Mean)
plot(catm2$Slope, catm2$LeafThickness_mm_Mean)
plot(catm2$Slope, catm2$TwigSucculence_Mean)
plot(catm2$Slope, catm2$LeafSucculence_Mean)


catl=melt(catm2, id.vars=c("Plot","Moisture","Slope", "Aspect"))

pdf(file="C:/Work/Cape Point Data/2010survey/Rcode/Plots/commagg.pdf", paper="letter")

bwplot(value~as.factor(Moisture)|variable, data=catl, varwidth=T, notch=T, scales=list(y=list(relation="free")), xlab="Plot Moisture", ylab="Community Aggregate Traits", layout=c(4,3))

xyplot(value~Slope|variable, data=catl, varwidth=T, notch=T, scales=list(y=list(relation="free")), ylab="Community Aggregate Traits", xlab="Plot Slope", layout=c(4,3))

xyplot(value~Aspect|variable, data=catl, varwidth=T, notch=T, scales=list(y=list(relation="free")), ylab="Community Aggregate Traits", xlab="Plot Aspect", layout=c(4,3))