## Run this script if you want to flag errors in the lab data.  This should be run
## instead of the last part of "Step 3" in the CapePointDataCorrectionCombining script
library(gregmisc)

setwd("C:/Work/Dimensions/Data")

########### Flagging Errors ############
########################################

# Read in lab data
ld=read.csv("Data/LabData.csv", header=T)
head(ld)
summary(ld)
dim(ld)
str(ld)

#flag as an error if a weight measurement is below 0.006 (2 SD of scale's repeatability)
ld[which(ld$RawLeafFresh_g<=0.006),]
ld$Error[ld$RawLeafFresh_g<=0.006]="RawLeafFresh below scale threshold"
summary(as.factor(ld$Error))

length(which(ld$RawLeafDry_g<=0.006))
ld$Error[which(ld$RawLeafDry_g<=0.006 & is.na(ld$Error)==F)]=paste(ld$Error[which(ld$RawLeafDry_g<=0.006 & is.na(ld$Error)==F)], "RawLeafDry below scale threshold", sep=", ")
ld$Error[which(ld$RawLeafDry_g<=0.006 & is.na(ld$Error)==T)]="RawLeafDry below scale threshold"
summary(as.factor(ld$Error))

ld[which(ld$TwigFresh_g<=0.006),]
ld$Error[which(ld$TwigFresh_g<=0.006 & is.na(ld$Error)==F)]=paste(ld$Error[which(ld$TwigFresh_g<=0.006 & is.na(ld$Error)==F)], "TwigFresh below scale threshold", sep=", ")
ld$Error[which(ld$TwigFresh_g<=0.006 & is.na(ld$Error)==T)]="TwigFresh below scale threshold"
summary(as.factor(ld$Error))

ld[which(ld$TwigDry_g<=0.006),]
ld$Error[which(ld$TwigDry_g<=0.006 & is.na(ld$Error)==F)]=paste(ld$Error[which(ld$TwigDry_g<=0.006 & is.na(ld$Error)==F)], "TwigDry below scale threshold", sep=", ")
ld$Error[which(ld$TwigDry_g<=0.006 & is.na(ld$Error)==T)]="TwigDry below scale threshold"
summary(as.factor(ld$Error))

#difference btwn dry & fresh measurements are within 0.006
length(which(abs(ld$RawLeafFresh_g-ld$RawLeafDry_g)<=0.006))
ld$Error[which(abs(ld$RawLeafFresh_g-ld$RawLeafDry_g)<=0.006 & is.na(ld$Error)==F)]=paste(ld$Error[which(abs(ld$RawLeafFresh_g-ld$RawLeafDry_g)<=0.006 & is.na(ld$Error)==F)], "Leaf Difference below scale threshold", sep=", ")
ld$Error[which(abs(ld$RawLeafFresh_g-ld$RawLeafDry_g)<=0.006 & is.na(ld$Error)==T)]="Leaf Difference below scale threshold"
summary(as.factor(ld$Error))

ld[which(abs(ld$TwigFresh_g-ld$TwigDry_g)<=0.006),]
ld$Error[which(abs(ld$TwigFresh_g-ld$TwigDry_g)<=0.006 & is.na(ld$Error)==F)]=paste(ld$Error[which(abs(ld$TwigFresh_g-ld$TwigDry_g)<=0.006 & is.na(ld$Error)==F)], "Twig Difference below scale threshold", sep=", ")
ld$Error[which(abs(ld$TwigFresh_g-ld$TwigDry_g)<=0.006 & is.na(ld$Error)==T)]="Twig Difference below scale threshold"
summary(as.factor(ld$Error))

#Succulence greater than 100%
ld[which(ld$LeafSucculence>=100),]
ld$Error[which(ld$LeafSucculence>=100 & is.na(ld$Error)==F)]=paste(ld$Error[which(ld$LeafSucculence>=100 & is.na(ld$Error)==F)], "LeafSucculence greater than 100%", sep=", ")
ld$Error[which(ld$LeafSucculence>=100 & is.na(ld$Error)==T)]="LeafSucculence greater than 100%"
summary(as.factor(ld$Error))

ld[which(ld$TwigSucculence>=100),]
ld$Error[which(ld$TwigSucculence>=100 & is.na(ld$Error)==F)]=paste(ld$Error[which(ld$TwigSucculence>=100 & is.na(ld$Error)==F)], "TwigSucculence greater than 100%", sep=", ")
ld$Error[which(ld$TwigSucculence>=100 & is.na(ld$Error)==T)]="TwigSucculence greater than 100%"
summary(as.factor(ld$Error))

write.csv(ld, "Data/LabData_errorflags.csv", row.names=F)


########### Traits by Species Dataframe ############
########### Excluding Errors #######################

## Note: this takes a very conservative approach to dealing with errors, and excludes anything that is flagged as a potential error

tr=read.csv("Data/20110329_FieldData_namescorrected.csv")
head(tr)
dim(tr)
labdt=read.csv("Data/LabData_errorflags.csv")
head(labdt)
dim(labdt)

##Dealing with errors

# If a weight measurement is below the scale threshold, replace it with NA
labdt$RawLeafFresh_g[grepl("RawLeafFresh below scale threshold", labdt$Error)==T]=NA
labdt$RawLeafDry_g[grepl("RawLeafDry below scale threshold", labdt$Error)==T]=NA
labdt$TwigFresh_g[grepl("TwigFresh below scale threshold", labdt$Error)==T]=NA
labdt$TwigDry_g[grepl("TwigDry below scale threshold", labdt$Error)==T]=NA

# If a weight measurement is below the scale threshold, replace derived measurements with NA
labdt$LeafFresh_g[grepl("RawLeafFresh below scale threshold", labdt$Error)==T]=NA
labdt$LeafDry_g[grepl("RawLeafDry below scale threshold", labdt$Error)==T]=NA
labdt$SLA[grepl("RawLeafDry below scale threshold", labdt$Error)==T]=NA
labdt$LeafSucculence[grepl("RawLeafFresh below scale threshold", labdt$Error)==T]=NA
labdt$LeafSucculence[grepl("RawLeafDry below scale threshold", labdt$Error)==T]=NA
labdt$TwigSucculence[grepl("TwigFresh below scale threshold", labdt$Error)==T]=NA
labdt$TwigSucculence[grepl("TwigDry below scale threshold", labdt$Error)==T]=NA

# If difference between fresh & dry weights is within the scale threshold, replace succulence measurements with NA
labdt$LeafSucculence[grepl("Leaf Difference below scale threshold", labdt$Error)==T]=NA
labdt$TwigSucculence[grepl("Twig Difference below scale threshold", labdt$Error)==T]=NA

# If succulence is greater than 100%, replace weights & succulence with NA
labdt$RawLeafFresh_g[grepl("LeafSucculence greater than 100%", labdt$Error)==T]=NA
labdt$LeafFresh_g[grepl("LeafSucculence greater than 100%", labdt$Error)==T]=NA
labdt$RawLeafDry_g[grepl("LeafSucculence greater than 100%", labdt$Error)==T]=NA
labdt$LeafDry_g[grepl("LeafSucculence greater than 100%", labdt$Error)==T]=NA
labdt$LeafSucculence[grepl("LeafSucculence greater than 100%", labdt$Error)==T]=NA
labdt$TwigFresh_g[grepl("TwigSucculence greater than 100%", labdt$Error)==T]=NA
labdt$TwigDry_g[grepl("TwigSucculence greater than 100%", labdt$Error)==T]=NA
labdt$TwigSucculence[grepl("TwigSucculence greater than 100%", labdt$Error)==T]=NA



##Means
means=aggregate(labdt, by=list(labdt$NewGenus, labdt$NewSpecies), mean, na.rm=T)
#warnings--that's ok

summary(means)

means=subset(means, select=c("Group.1","Group.2","NumLeaves",
  "RawLeafArea_cm2","LeafArea_cm2","LeafLength_cm","AvgLeafWidth_cm","MaxLeafWidth_cm","LeafThickness_mm",
  "RawLeafFresh_g","RawLeafDry_g","LeafFresh_g","LeafDry_g","TwigFresh_g","TwigDry_g","SLA","LeafSucculence","TwigSucculence"))

colnames(means)
colnames(means)=paste(colnames(means),"_Mean", sep="")
colnames(means)

means=rename.vars(means, from="Group.1_Mean", to="Genus")
means=rename.vars(means, from="Group.2_Mean", to="Species")
colnames(means)
summary(means)
dim(means)

##Stand Dev
stdev=aggregate(labdt, by=list(labdt$NewGenus, labdt$NewSpecies), sd, na.rm=T)
#warnings--that's ok

summary(stdev)

stdev=subset(stdev, select=c("Group.1","Group.2","NumLeaves",
  "RawLeafArea_cm2","LeafArea_cm2","LeafLength_cm","AvgLeafWidth_cm","MaxLeafWidth_cm","LeafThickness_mm",
  "RawLeafFresh_g","RawLeafDry_g","LeafFresh_g","LeafDry_g","TwigFresh_g","TwigDry_g","SLA","LeafSucculence","TwigSucculence"))

colnames(stdev)
colnames(stdev)=paste(colnames(stdev),"_SD", sep="")
colnames(stdev)

stdev=rename.vars(stdev, from="Group.1_SD", to="Genus")
stdev=rename.vars(stdev, from="Group.2_SD", to="Species")
colnames(stdev)
summary(stdev)
dim(stdev)

##Number
natest=subset(labdt, select=c("NewGenus","NewSpecies","NumLeaves",
  "RawLeafArea_cm2","LeafArea_cm2","LeafLength_cm","AvgLeafWidth_cm","MaxLeafWidth_cm","LeafThickness_mm",
  "RawLeafFresh_g","RawLeafDry_g","LeafFresh_g","LeafDry_g","TwigFresh_g","TwigDry_g","SLA","LeafSucculence","TwigSucculence"))

natest[,3:18]=!is.na(natest[,3:18])
head(natest)
natest[natest=="TRUE"]=1
head(natest)

n=aggregate(natest[,3:18], by=list(natest$NewGenus, natest$NewSpecies), sum)

summary(n)

colnames(n)
colnames(n)=paste(colnames(n),"_N", sep="")
colnames(n)

n=rename.vars(n, from="Group.1_N", to="Genus")
n=rename.vars(n, from="Group.2_N", to="Species")
colnames(n)
summary(n)
dim(n)

##Field Data Maximums
mx=subset(tr, select=c("NewGenus","NewSpecies","Height_cm","CanopyX_cm","CanopyY_cm","BranchingOrder"))
mx=aggregate(mx[3:6], by=list(mx$NewGenus, mx$NewSpecies), max, na.rm=T)
#warnings--that's ok

head(mx)

mx$BranchingOrder[mx$BranchingOrder==-Inf]=NA

colnames(mx)
colnames(mx)=paste(colnames(mx),"_Max", sep="")
colnames(mx)

mx=rename.vars(mx, from="Group.1_Max", to="Genus")
mx=rename.vars(mx, from="Group.2_Max", to="Species")
colnames(mx)
summary(mx)
dim(mx)

##Veg traits
cpall=read.csv("Data/Releve66_96_NamesCorrected.csv", header=T)
head(cpall)
dim(cpall)

grw=subset(cpall, select=c("NewGenus","NewSpecies","DISPERSAL","REGENERATION","GROWTHFORM"))

grw=rename.vars(grw, from=c("NewGenus","NewSpecies"), to=c("Genus","Species"))
colnames(grw)
summary(grw)
dim(grw)

##Isotope data
iso=read.csv("Data/isotopes_namescorrected.csv")
head(iso)

isomean=aggregate(iso[5:16], by=list(iso$NewGenus, iso$NewSpecies), mean, na.rm=T)

head(isomean)
dim(isomean) 

isomean=rename.vars(isomean, from="Group.1", to="Genus")
isomean=rename.vars(isomean, from="Group.2", to="Species")

##### Combining #####
head(means)
head(stdev)
head(n)
head(mx)
head(grw)
head(isomean)

dim(means)
dim(stdev)
dim(n)
dim(mx)
dim(grw)
dim(isomean)

#There is one extra species in field data
meanscheck=paste(means$Genus, means$Species)
mxcheck=paste(mx$Genus, mx$Species)
mxcheck[mxcheck %in% meanscheck==F]  # Cussonia thyrsiflora; I am just going to exclude this...

#Mismatch between isotope data and lab data
isocheck=paste(isomean$Genus, isomean$Species)
meanscheck[meanscheck %in% isocheck==F]   # Syncarpha canescens, Ficinia filiformis, Ficinia trichodes - in lab data but not isotope data
isocheck[isocheck %in% meanscheck==F]   # Syncarpha gnaphaloides, Ficinia oligantha - in isotope data but not lab data
#Will have to deal with this....

#Duplicates in veg traits
grwcheck=paste(grw$Genus, grw$Species)
grw1=grw[grwcheck %in% meanscheck==T,]
dim(grw1)
grw1check=paste(grw1$Genus, grw1$Species)
length(grw1check)
length(unique(grw1check))
grw1[duplicated(grw1check)==T,]
grw1[grw1$Genus=="Erica" & grw1$Species=="hispidula",]  #ok to remove row with NAs; no conflicts in values
grw1[grw1$Genus=="Ficinia" & grw1$Species=="filiformis",] #no conflicts in values; ok to remove duplicate
grw1[grw1$Genus=="Zygophyllum" & grw1$Species=="spinosum",] # Regeneration value differs between these two, so there is still a conflict
# Remove duplicates
grw2=grw1[duplicated(grw1check)==F,]


allsptr=merge(means, stdev, by=c("Genus","Species"), all=T)
colnames(allsptr)
dim(allsptr)
allsptr=merge(allsptr, n, by=c("Genus","Species"), all=T)
colnames(allsptr)
dim(allsptr)
allsptr=merge(allsptr, mx, by=c("Genus","Species"), all.x=T, all.y=F)
colnames(allsptr)
dim(allsptr)
allsptr=merge(allsptr, isomean, by=c("Genus","Species"), all=T)
colnames(allsptr)
dim(allsptr)
allsptr1=merge(allsptr, grw2, by=c("Genus","Species"), all.x=T)
colnames(allsptr)
dim(allsptr)

write.csv(allsptr, "Data/SpeciesTraits_ErrorsExcluded.csv",row.names=F)