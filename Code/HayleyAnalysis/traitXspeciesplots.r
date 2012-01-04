setwd("C:/Work/Dimensions/Data")
library(reshape)
library(lattice)

tr1=read.csv("Data/SpeciesTraits_ErrorsExcluded.csv")
tr1$SpeciesID=paste(tr1$Genus, tr1$Species)

tr3=subset(tr1, select=c("SpeciesID", "LeafLength_cm_Mean", "AvgLeafWidth_cm_Mean", "MaxLeafWidth_cm_Mean",
          "LeafThickness_mm_Mean", "SLA_Mean", "LeafSucculence_Mean", "TwigSucculence_Mean"))
          
trsd=subset(tr1, select=c("SpeciesID", "LeafLength_cm_SD", "AvgLeafWidth_cm_SD", "MaxLeafWidth_cm_SD",
          "LeafThickness_mm_SD", "SLA_SD", "LeafSucculence_SD", "TwigSucculence_SD"))

          
trm=melt(tr3, id.vars="SpeciesID")
head(trm)
trsdm=melt(trsd, id.vars="SpeciesID")
head(trsdm)

colnames(trsdm)[3]="SD"

trm=cbind(trm, trsdm$SD)
colnames(trm)[4]="SD"
head(trm)
trm$SpeciesID=as.factor(trm$SpeciesID)
bwplot(SpeciesID ~value | variable, data=trm, scales=list(x=list(relation="free"),y=list(cex=.1)),horizontal=T,cex=".")

#Add to this: full set of data points
# Sort by something (abundance, # of plots, etc)

xyplot(SpeciesID ~value | variable, data=trm, scales=list(x=list(relation="free"),y=list(cex=.1)),panel=function(x,y,subscripts){
    td=trm[subscripts,]
       panel.segments(td$value-td$SD,td$SpeciesID,td$value+td$SD,td$SpeciesID,col="grey")
       panel.xyplot(td$value,td$SpeciesID,col="red",pch=16,cex=.3)
        #panel.xyplot(tapply(td$value,td$SpeciesID,mean,na.rm=T),td$SpeciesID,col="red",pch=16,cex=.3)  #something like this to manually calc means after reading in all the daata points
        },layout=c(1,1))
       