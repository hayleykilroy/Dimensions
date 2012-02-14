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
dim(spa) # Number of columns should be one more than number of rows in tr3

# Replace NAs in species abundance matrix with 0s
spa2=spa
spa2[is.na(spa2)==T]=0

trcat=tr3[,2:ncol(tr3)]
trcat=as.matrix(trcat)
head(trcat)
dim(trcat)
str(trcat)

spacat=spa2[,2:ncol(spa2)]
spacat=as.matrix(spacat)

#Excluding NAs--this is problematic!!! NAs can't just be zeros - should re-normalize species abundance to exclude NAs for each variable
trcat[is.na(trcat)==T]=0


catm=spacat%*%trcat
head(catm)
dim(catm)
summary(catm)

catm=as.data.frame(catm)
catm2=cbind(spa$Plot, catm)
colnames(catm2)[1]="Plot"

# Look at distribution of traits across plots
hist(catm2$LeafLength_cm_Mean)  # slightly right skewed
hist(catm2$AvgLeafWidth_cm_Mean) # right skewed, potential outlier
hist(catm2$MaxLeafWidth_cm_Mean)  #slightly right skewed
hist(catm2$LeafThickness_mm_Mean)  #right skewed
hist(catm2$SLA_Mean)               #looks normalish
hist(catm2$LeafSucculence_Mean)  #left skewed
hist(catm2$TwigSucculence_Mean)   #normal
hist(catm2$Height_cm_Max)      #right skewed
hist(catm2$CanopyX_cm_Max)    # right skewed, potential outlier
hist(catm2$CanopyY_cm_Max)    # heavily right skewed (Poisson)
hist(catm2$PercN)           #right skewed
hist(catm2$PercC)          # left skewed, potential outlier
hist(catm2$C.N_ratio)      #normal

write.csv(catm2, "Data/CAT_byPlot.csv", row.names=F)


##### By Plot Env Factors
catm2=read.csv("Data/CAT_byPlot.csv")
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


catm2=merge(catm2, env1, by="Plot")
head(catm2)

catm2$Moisture=as.factor(catm2$Moisture)


## Plots

# Plots for each variable in a pdf
par(mfrow=c(2,2))

pdf(file="C:/Work/Dimensions/Code/HayleyAnalysis/Figures/commagg.pdf", paper="letter")
par(mfrow=c(2,2))

boxplot(LeafLength_cm_Mean~Moisture, data=catm2, xlab="Plot Moisture Category", ylab="Abundance-Weighted Mean Leaf Length (cm)")
#plot(catm2$Slope,catm2$LeafLength_cm_Mean, xlab="Slope of Plot", ylab="Abundance-Weighted Mean Leaf Length (cm)") 
#plot(catm2$Aspect,catm2$LeafLength_cm_Mean, xlab="Aspect of Plot", ylab="Abundance-Weighted Mean Leaf Length (cm)")
plot(catm2$eastwestness, catm2$LeafLength_cm_Mean, xlab="East-Westness", ylab="Abundance-Weighted Mean Leaf Length (cm)") 
plot(catm2$northsouthness, catm2$LeafLength_cm_Mean, xlab="North-Southness", ylab="Abundance-Weighted Mean Leaf Length (cm)")
plot(catm2$slopetransformed, catm2$LeafLength_cm_Mean, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean Leaf Length (cm)")
plot(catm2$Depth_Center_m,catm2$LeafLength_cm_Mean, xlab="Soil Depth in Center of Plot (m)", ylab="Abundance-Weighted Mean Leaf Length (cm)")

boxplot(AvgLeafWidth_cm_Mean~Moisture, data=catm2, xlab="Plot Moisture Category",ylab="Abundance-Weighted Mean Avg. Leaf Width (cm)")
#plot(catm2$Slope,catm2$AvgLeafWidth_cm_Mean, xlab="Slope of Plot",ylab="Abundance-Weighted Mean Avg. Leaf Width (cm)") 
#plot(catm2$Aspect,catm2$AvgLeafWidth_cm_Mean, xlab="Aspect of Plot",ylab="Abundance-Weighted Mean Avg. Leaf Width (cm)")
plot(catm2$eastwestness, catm2$AvgLeafWidth_cm_Mean, xlab="East-Westness", ylab="Abundance-Weighted Mean Leaf Avg. Width (cm)") 
plot(catm2$northsouthness, catm2$AvgLeafWidth_cm_Mean, xlab="North-Southness", ylab="Abundance-Weighted Mean Avg. Leaf Width (cm)")
plot(catm2$slopetransformed, catm2$AvgLeafWidth_cm_Mean, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean Avg. Leaf Width (cm)")
plot(catm2$Depth_Center_m,catm2$AvgLeafWidth_cm_Mean, xlab="Soil Depth in Center of Plot (m)",ylab="Abundance-Weighted Mean Avg. Leaf Width (cm)")

boxplot(MaxLeafWidth_cm_Mean~Moisture, data=catm2, xlab="Plot Moisture Category",ylab="Abundance-Weighted Mean Max Leaf Width (cm)")
#plot(catm2$Slope,catm2$MaxLeafWidth_cm_Mean, xlab="Slope of Plot",ylab="Abundance-Weighted Mean Max Leaf Width (cm)") 
#plot(catm2$Aspect,catm2$MaxLeafWidth_cm_Mean, xlab="Aspect of Plot",ylab="Abundance-Weighted Mean Max Leaf Width (cm)")
plot(catm2$eastwestness, catm2$MaxLeafWidth_cm_Mean, xlab="East-Westness", ylab="Abundance-Weighted Mean Max Leaf Width (cm)") 
plot(catm2$northsouthness, catm2$MaxLeafWidth_cm_Mean, xlab="North-Southness", ylab="Abundance-Weighted Mean Max Leaf Width (cm)")
plot(catm2$slopetransformed, catm2$MaxLeafWidth_cm_Mean, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean Max Leaf Width (cm)")
plot(catm2$Depth_Center_m,catm2$MaxLeafWidth_cm_Mean, xlab="Soil Depth in Center of Plot (m)",ylab="Abundance-Weighted Mean Max Leaf Width (cm)")

boxplot(LeafThickness_mm_Mean~Moisture, data=catm2, xlab="Plot Moisture Category",ylab="Abundance-Weighted Mean Leaf Thickness (mm)")
#plot(catm2$Slope,catm2$LeafThickness_mm_Mean, xlab="Slope of Plot",ylab="Abundance-Weighted Mean Leaf Thickness (mm)") 
#plot(catm2$Aspect,catm2$LeafThickness_mm_Mean, xlab="Aspect of Plot",ylab="Abundance-Weighted Mean Leaf Thickness (mm)")
plot(catm2$eastwestness, catm2$LeafThickness_mm_Mean, xlab="East-Westness", ylab="Abundance-Weighted Mean Leaf Thickness (mm)") 
plot(catm2$northsouthness, catm2$LeafThickness_mm_Mean, xlab="North-Southness", ylab="Abundance-Weighted Mean Leaf Thickness (mm)")
plot(catm2$slopetransformed, catm2$LeafThickness_mm_Mean, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean Leaf Thickness (mm)")
plot(catm2$Depth_Center_m,catm2$LeafThickness_mm_Mean, xlab="Soil Depth in Center of Plot (m)",ylab="Abundance-Weighted Mean Leaf Thickness (mm)")

boxplot(SLA_Mean~Moisture, data=catm2, xlab="Plot Moisture Category",ylab="Abundance-Weighted Mean SLA")
#plot(catm2$Slope,catm2$SLA_Mean, xlab="Slope of Plot",ylab="Abundance-Weighted Mean SLA") 
#plot(catm2$Aspect,catm2$SLA_Mean, xlab="Aspect of Plot",ylab="Abundance-Weighted Mean SLA")
plot(catm2$eastwestness, catm2$SLA_Mean, xlab="East-Westness", ylab="Abundance-Weighted Mean SLA") 
plot(catm2$northsouthness, catm2$SLA_Mean, xlab="North-Southness", ylab="Abundance-Weighted Mean SLA")
plot(catm2$slopetransformed, catm2$SLA_Mean, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean SLA")
plot(catm2$Depth_Center_m,catm2$SLA_Mean, xlab="Soil Depth in Center of Plot (m)",ylab="Abundance-Weighted Mean SLA")

boxplot(LeafSucculence_Mean~Moisture, data=catm2, xlab="Plot Moisture Category",ylab="Abundance-Weighted Mean Leaf Succulence")
#plot(catm2$Slope,catm2$LeafSucculence_Mean, xlab="Slope of Plot",ylab="Abundance-Weighted Mean Leaf Succulence") 
#plot(catm2$Aspect,catm2$LeafSucculence_Mean, xlab="Aspect of Plot",ylab="Abundance-Weighted Mean Leaf Succulence")
plot(catm2$eastwestness, catm2$LeafSucculence_Mean, xlab="East-Westness", ylab="Abundance-Weighted Mean Leaf Succulence") 
plot(catm2$northsouthness, catm2$LeafSucculence_Mean, xlab="North-Southness", ylab="Abundance-Weighted Mean Leaf Succulence")
plot(catm2$slopetransformed, catm2$LeafSucculence_Mean, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean Leaf Succulence")
plot(catm2$Depth_Center_m,catm2$LeafSucculence_Mean, xlab="Soil Depth in Center of Plot (m)",ylab="Abundance-Weighted Mean Leaf Succulence")

boxplot(TwigSucculence_Mean~Moisture, data=catm2, xlab="Plot Moisture Category",ylab="Abundance-Weighted Mean Twig Succulence")
#plot(catm2$Slope,catm2$TwigSucculence_Mean, xlab="Slope of Plot",ylab="Abundance-Weighted Mean Twig Succulence") 
#plot(catm2$Aspect,catm2$TwigSucculence_Mean, xlab="Aspect of Plot",ylab="Abundance-Weighted Mean Twig Succulence")
plot(catm2$eastwestness, catm2$TwigSucculence_Mean, xlab="East-Westness", ylab="Abundance-Weighted Mean Twig Succulence") 
plot(catm2$northsouthness, catm2$TwigSucculence_Mean, xlab="North-Southness", ylab="Abundance-Weighted Mean Twig Succulence")
plot(catm2$slopetransformed, catm2$TwigSucculence_Mean, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean Twig Succulence")
plot(catm2$Depth_Center_m,catm2$TwigSucculence_Mean, xlab="Soil Depth in Center of Plot (m)",ylab="Abundance-Weighted Mean Twig Succulence")

boxplot(Height_cm_Max~Moisture, data=catm2, xlab="Plot Moisture Category",ylab="Abundance-Weighted Mean Plant Height")
#plot(catm2$Slope,catm2$Height_cm_Max, xlab="Slope of Plot",ylab="Abundance-Weighted Mean Plant Height") 
#plot(catm2$Aspect,catm2$Height_cm_Max, xlab="Aspect of Plot",ylab="Abundance-Weighted Mean Plant Height")
plot(catm2$eastwestness, catm2$Height_cm_Max, xlab="East-Westness", ylab="Abundance-Weighted Mean Plant Height") 
plot(catm2$northsouthness, catm2$Height_cm_Max, xlab="North-Southness", ylab="Abundance-Weighted Mean Plant Height")
plot(catm2$slopetransformed, catm2$Height_cm_Max, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean Plant Height")
plot(catm2$Depth_Center_m,catm2$Height_cm_Max, xlab="Soil Depth in Center of Plot (m)",ylab="Abundance-Weighted Mean Plant Height")

boxplot(CanopyX_cm_Max~Moisture, data=catm2, xlab="Plot Moisture Category",ylab="Abundance-Weighted Mean Canopy X Axis (cm)")
#plot(catm2$Slope,catm2$CanopyX_cm_Max, xlab="Slope of Plot",ylab="Abundance-Weighted Mean Canopy X Axis (cm)") 
#plot(catm2$Aspect,catm2$CanopyX_cm_Max, xlab="Aspect of Plot",ylab="Abundance-Weighted Mean Canopy X Axis (cm)")
plot(catm2$eastwestness, catm2$CanopyX_cm_Max, xlab="East-Westness", ylab="Abundance-Weighted Mean Canopy X Axis (cm)") 
plot(catm2$northsouthness, catm2$CanopyX_cm_Max, xlab="North-Southness", ylab="Abundance-Weighted Mean Canopy X Axis (cm)")
plot(catm2$slopetransformed, catm2$CanopyX_cm_Max, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean Canopy X Axis (cm)")
plot(catm2$Depth_Center_m,catm2$CanopyX_cm_Max, xlab="Soil Depth in Center of Plot (m)",ylab="Abundance-Weighted Mean Canopy X Axis (cm)")

boxplot(CanopyY_cm_Max~Moisture, data=catm2, xlab="Plot Moisture Category",ylab="Abundance-Weighted Mean Canopy Y Axis (cm)")
#plot(catm2$Slope,catm2$CanopyY_cm_Max, xlab="Slope of Plot",ylab="Abundance-Weighted Mean Canopy Y Axis (cm)") 
#plot(catm2$Aspect,catm2$CanopyY_cm_Max, xlab="Aspect of Plot",ylab="Abundance-Weighted Mean Canopy Y Axis (cm)")
plot(catm2$eastwestness, catm2$CanopyY_cm_Max, xlab="East-Westness", ylab="Abundance-Weighted Mean Canopy Y Axis (cm)") 
plot(catm2$northsouthness, catm2$CanopyY_cm_Max, xlab="North-Southness", ylab="Abundance-Weighted Mean Canopy Y Axis (cm)")
plot(catm2$slopetransformed, catm2$CanopyY_cm_Max, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean Canopy Y Axis (cm)")
plot(catm2$Depth_Center_m,catm2$CanopyY_cm_Max, xlab="Soil Depth in Center of Plot (m)",ylab="Abundance-Weighted Mean Canopy Y Axis (cm)")

boxplot(PercN~Moisture, data=catm2, xlab="Plot Moisture Category",ylab="Abundance-Weighted Mean %Nitrogen")
#plot(catm2$Slope,catm2$PercN, xlab="Slope of Plot",ylab="Abundance-Weighted Mean %Nitrogen") 
#plot(catm2$Aspect,catm2$PercN, xlab="Aspect of Plot",ylab="Abundance-Weighted Mean %Nitrogen")
plot(catm2$eastwestness, catm2$PercN, xlab="East-Westness", ylab="Abundance-Weighted Mean %Nitrogen") 
plot(catm2$northsouthness, catm2$PercN, xlab="North-Southness", ylab="Abundance-Weighted Mean %Nitrogen")
plot(catm2$slopetransformed, catm2$PercN, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean %Nitrogen")
plot(catm2$Depth_Center_m,catm2$PercN, xlab="Soil Depth in Center of Plot (m)",ylab="Abundance-Weighted Mean %Nitrogen")

boxplot(PercC~Moisture, data=catm2, xlab="Plot Moisture Category",ylab="Abundance-Weighted Mean %Carbon")
#plot(catm2$Slope,catm2$PercC, xlab="Slope of Plot",ylab="Abundance-Weighted Mean %Carbon") 
#plot(catm2$Aspect,catm2$PercC, xlab="Aspect of Plot",ylab="Abundance-Weighted Mean %Carbon")
plot(catm2$eastwestness, catm2$PercC, xlab="East-Westness", ylab="Abundance-Weighted Mean %Carbon") 
plot(catm2$northsouthness, catm2$PercC, xlab="North-Southness", ylab="Abundance-Weighted Mean %Carbon")
plot(catm2$slopetransformed, catm2$PercC, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean %Carbon")
plot(catm2$Depth_Center_m,catm2$PercC, xlab="Soil Depth in Center of Plot (m)",ylab="Abundance-Weighted Mean %Carbon")

boxplot(C.N_ratio~Moisture, data=catm2, xlab="Plot Moisture Category",ylab="Abundance-Weighted Mean C/N Ratio")
#plot(catm2$Slope,catm2$C.N_ratio, xlab="Slope of Plot",ylab="Abundance-Weighted Mean C/N Ratio") 
#plot(catm2$Aspect,catm2$C.N_ratio, xlab="Aspect of Plot",ylab="Abundance-Weighted Mean C/N Ratio")
plot(catm2$eastwestness, catm2$C.N_ratio, xlab="East-Westness", ylab="Abundance-Weighted Mean C/N Ratio") 
plot(catm2$northsouthness, catm2$C.N_ratio, xlab="North-Southness", ylab="Abundance-Weighted Mean C/N Ratio")
plot(catm2$slopetransformed, catm2$C.N_ratio, xlab="Sin(Slope)", ylab="Abundance-Weighted Mean C/N Ratio")
plot(catm2$Depth_Center_m,catm2$C.N_ratio, xlab="Soil Depth in Center of Plot (m)",ylab="Abundance-Weighted Mean C/N Ratio")

dev.off()







######## PCA Trait analysis
pc.tr=princomp(catm)
summary(pc.tr)
loadings(pc.tr)

# 1st axis - height, canopy x & y
# 2nd axis - ?

# linear regression with 1st axis
pc.axis1=pc.tr$scores[,1]
pc.axis1=as.numeric(pc.axis1)

pcreg=lm(pc.axis1 ~ catm2$Moisture + catm2$Slope + catm2$Aspect + catm2$Depth_Center_m)
summary(pcreg) # Nothing significant

pcreg1=lm(pc.axis1 ~ catm2$Slope + catm2$Depth_Center_m)
pcreg2=lm(pc.axis1 ~ catm2$Slope)

summary(pcreg2) # Most significant model - not less than 0.05 though

# PCA of leaf traits only
pc.lf=princomp(subset(catm2, select=c("LeafLength_cm_Mean", "AvgLeafWidth_cm_Mean", "MaxLeafWidth_cm_Mean",
                                "LeafThickness_mm_Mean", "SLA_Mean", "LeafSucculence_Mean")))
summary(pc.lf)
loadings(pc.lf)                                

# 1st axis - Leaf Length & SLA

lfax1=pc.lf$scores[,1]
lfax1=as.numeric(lfax1)
lfreg=lm(lfax1 ~ catm2$Moisture + catm2$Slope + catm2$Aspect + catm2$Depth_Center_m)
summary(lfreg)

lfreg1=lm(lfax1 ~ catm2$Moisture + catm2$Aspect + catm2$Depth_Center_m)
summary(lfreg1)

lfreg2=lm(lfax1 ~ catm2$Moisture + catm2$Aspect)
summary(lfreg2)

lfreg3=lm(lfax1 ~ catm2$Moisture)
summary(lfreg3)

# Low moisture sites -> longer leaves, lower SLA