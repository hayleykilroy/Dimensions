setwd("C:/Work/Cape Point Data/2010survey/FinalData/Traits")
library(lattice)
library(reshape)

ld=read.csv("LabData.csv")
head(ld)
summary(ld)
ld$SpeciesID=paste(ld$Family, ld$Genus, ld$Species, sep=' ')

ld2=melt(ld, id.vars=c("SpeciesID", "Family", "Genus"), measure.vars=c("NumLeaves", "LeafArea_cm2","LeafLength_cm",
    "AvgLeafWidth_cm", "MaxLeafWidth_cm", "LeafThickness_mm", "LeafFresh_g", "LeafDry_g",
    "TwigFresh_g","TwigDry_g","SLA", "LeafSucculence", "TwigSucculence"))

head(ld2)

length(unique(ld2$SpeciesID))

#bwplot(SpeciesID~value|variable, data=ld2, horizontal=T, scales=list(x=list(relation="free"),y=list(cex=.25))) #layout-rows, columns, pages

#####XY plots by Species
##open pdf device, write to, close device
pdf(file="C:/Work/Cape Point Data/2010survey/Rcode/Plots/labdata_xyplots.pdf", paper="letter")
#plot
xyplot(as.factor(SpeciesID)~value|variable, data=ld2, scales=list(x=list(relation="free"),y=list(cex=.25)), pch=15, cex=0.2, layout=c(1,1))
dev.off()

#####XY plots by Genus
##open pdf device, write to, close device
pdf(file="C:/Work/Cape Point Data/2010survey/Rcode/Plots/labdata_genusplots.pdf", paper="letter")
#plot
xyplot(as.factor(Genus)~value|variable, data=ld2, scales=list(x=list(relation="free"),y=list(cex=.25)), pch=15, cex=0.2, layout=c(1,1))
dev.off()

#####XY plots by Family
##open pdf device, write to, close device
pdf(file="C:/Work/Cape Point Data/2010survey/Rcode/Plots/labdata_familyplots.pdf", paper="letter")
#plot
xyplot(as.factor(Family)~value|variable, data=ld2, scales=list(x=list(relation="free"),y=list(cex=.75)), pch=15, cex=0.2, layout=c(1,1))
dev.off()

##outliers
ld[ld$AvgLeafWidth_cm==max(ld$AvgLeafWidth_cm,na.rm=T),]
ld[ld$SpeciesID=="CYPERACEAE Tetraria thermalis",c(16,29)]

ld[ld$LeafThickness_mm==max(ld$LeafThickness_mm,na.rm=T),]
ld[ld$SpeciesID=="ASPARAGACEAE Protasparagus capensis",c(18,29)]

##other weird things

#summary(ld$LeafFresh_g)
#summary(ld$LeafDry_g)
summary(ld$LeafSucculence)
summary((ld$LeafFresh_g-ld$LeafDry_g)/ld$LeafFresh_g)

#ld[(ld$LeafDry_g>ld$LeafFresh_g)==T,c(27:29)]
#ld[c(331, 1007, 1061, 1581),]

ls=ld[ld$LeafSucculence>=70 & !is.na(ld$LeafSucculence),] 
ls[order(ls$LeafSucculence),]

ts1=ld[ld$TwigSucculence>=70 & !is.na(ld$TwigSucculence),] 
ts1[order(ts1$TwigSucculence),]

ts=ld[ld$TwigSucculence>=100,c(25,21,22,29)]
ts=ts[is.na(ts[,1])==F,]
ts

#closer looks at species with weird leaf succulence values
ls10=ld[ld$SpeciesID=="ERICACEAE Erica imbricata",]
ls10

ls1=ld2[ld2$SpeciesID=="ERICACEAE Erica imbricata",]
bwplot(SpeciesID~value|variable, data=ls1, horizontal=T, scales=list(x=list(relation="free")))

#ls2=ls10[c(1:2,4:9),]
#ls2=melt(ls2, id.vars=c("SpeciesID"), measure.vars=c("NumLeaves", "LeafArea_cm2","LeafLength_cm",
#    "AvgLeafWidth_cm", "MaxLeafWidth_cm", "LeafThickness_mm", "LeafFresh_g", "LeafDry_g",
#    "TwigFresh_g","TwigDry_g","SLA", "LeafSucculence", "TwigSucculence"))
#bwplot(SpeciesID~value|variable, data=ls2, horizontal=T, scales=list(x=list(relation="free")))

write.csv(ls10, "outliertables.csv", row.names=F)

#closer looks at species with weird twig succulence values


###look at ranges of succulence values for each species--see if these differ greatly


maxls=matrix(nrow=length(unique(ld$SpeciesID)), ncol=4)
maxls=as.data.frame(maxls)
colnames(maxls)=c("SpeciesID", "MaxLS", "MinLS", "Range")
maxls$SpeciesID=unique(ld$SpeciesID)
maxls$MaxLS=ld$LeafSucculence[ld$SpeciesID==maxls$SpeciesID & ld$LeafSucculence==max(ld$LeafSucculence, na.rm=T)]

ld$LeafThickness_mm==max(ld$LeafThickness_mm,na.rm=T)

###see if difference btwn dry & fresh is within scale's margin of error
# repeatability (st. dev.)=0.003     2sd=0.006
# 

histogram(ld$RawLeafDry_g, scales=list(x=list(log=T))
#SLAs based on leafdry around 0.003 g could be sketchy
#add flag column for possible problems/errors
  # - raw measurement for anything is below 0.006
  # - diff btwn dry & fresh is within 0.006
  
  
lde=ld[is.na(ld$Error)==F,]
lde

write.csv(lde, "C:/Work/Cape Point Data/2010survey/Rcode/Plots/LabData_ErrorFlags.csv")


#####XY plots-Errors excluded
ld3=ld[is.na(ld$Error)==T,]
dim(ld3)

ld4=melt(ld3, id.vars=c("SpeciesID", "Family", "Genus"), measure.vars=c("NumLeaves", "LeafArea_cm2","LeafLength_cm",
    "AvgLeafWidth_cm", "MaxLeafWidth_cm", "LeafThickness_mm", "LeafFresh_g", "LeafDry_g",
    "TwigFresh_g","TwigDry_g","SLA", "LeafSucculence", "TwigSucculence"))



##open pdf device, write to, close device
pdf(file="C:/Work/Cape Point Data/2010survey/Rcode/Plots/labdata_xyplots_flagrem.pdf", paper="letter")
#plot
xyplot(as.factor(SpeciesID)~value|variable, data=ld4, scales=list(x=list(relation="free"),y=list(cex=.25)), pch=15, cex=0.2, layout=c(1,1))
dev.off()

pdf(file="C:/Work/Cape Point Data/2010survey/Rcode/Plots/labdata_genusplots_flagrem.pdf", paper="letter")
#plot
xyplot(as.factor(Genus)~value|variable, data=ld4, scales=list(x=list(relation="free"),y=list(cex=.25)), pch=15, cex=0.2, layout=c(1,1))
dev.off()

pdf(file="C:/Work/Cape Point Data/2010survey/Rcode/Plots/labdata_familyplots_flagrem.pdf", paper="letter")
#plot
xyplot(as.factor(Family)~value|variable, data=ld4, scales=list(x=list(relation="free"),y=list(cex=.25)), pch=15, cex=0.2, layout=c(1,1))
dev.off()