#setwd("C:/Work/Dimensions/Data") # Change this to your local Dimensions/Data directory
setwd("D:\\Jasper\\Side projects\\Taylor plots\\GitData\\Dimensions\\Data")
#########################################################################
#########################################################################
################### STEP 1: SYNONYM CORRECTIONS  ########################
#########################################################################
#########################################################################

library(gregmisc)

# Read in synonym data
syn=read.csv("PreprocessedData/MasterSynonymCorrections.csv")

#########################################################################
################### Correct synonyms in releve data #####################
#########################################################################

#Read in releve data
new=read.csv("PreprocessedData/ReleveQuadrat2010.csv")
old=read.table("PreprocessedData/capepointALL.txt", header=T)

# Correct from synonym spreadsheet
old=rename.vars(old, from=c("GENUS","SPECIES"), to=c("Genus","Species"))
old$SpeciesID=paste(old$Genus, old$Species)
new$SpeciesID=paste(new$Genus, new$Species)
syn$SpeciesID=paste(syn$OldGenus, syn$OldSpecies)

old$SpeciesID=sub("  ", " ", old$SpeciesID)
new$SpeciesID=sub("  ", " ", new$SpeciesID)
syn$SpeciesID=sub("  ", " ", syn$SpeciesID)

new$SpeciesID[new$SpeciesID==" Erica muscosa"]="Erica muscosa"

head(syn)
syn1=subset(syn, select=c("SpeciesID", "NewGenus", "NewSpecies"))
head(syn1)

old1=merge(old, syn1, by="SpeciesID", all.x=T, all.y=F)
head(old1)
summary(old1)

new1=merge(new, syn1, by="SpeciesID", all.x=T, all.y=F)
head(new1)
summary(new1)

###check to make sure all have been corrected
old1[is.na(old1$NewGenus)==T,]
nrow(old1[is.na(old1$NewGenus)==T,])  #0
new1[is.na(new1$NewGenus)==T,]
nrow(new1[is.na(new1$NewGenus)==T,]) #0


##Write corrected files
write.csv(new1, "Data/ReleveQuadrat2010_NamesCorrected.csv", row.names=F)
write.csv(old1, "Data/Releve66_96_NamesCorrected.csv", row.names=F)

#########################################################################
################### Correct synonyms in (field & lab) trait data ########
#########################################################################

fieldtr=read.csv("PreprocessedData/20110329_FieldData.csv", stringsAsFactors=F)

fieldtr$SpeciesID=paste(fieldtr$Genus, fieldtr$Species)
fieldtr$SpeciesID=sub("  ", " ", fieldtr$SpeciesID)

fieldtr1=merge(fieldtr, syn1, by="SpeciesID", all.x=T, all.y=F)

fieldtr1$NewGenus=as.character(fieldtr1$NewGenus)
fieldtr1$NewSpecies=as.character(fieldtr1$NewSpecies)
fieldtr1$NewGenus[fieldtr1$SpeciesID==" p57sp1"]="Syncarpha"
fieldtr1$NewSpecies[fieldtr1$SpeciesID==" p57sp1"]="speciosissima"
fieldtr1$NewGenus[fieldtr1$SpeciesID==" p57sp2"]="Tetraria"
fieldtr1$NewSpecies[fieldtr1$SpeciesID==" p57sp2"]="sylvatica"
fieldtr1$NewGenus[fieldtr1$SpeciesID==" p57sp3"]="Tetraria"
fieldtr1$NewSpecies[fieldtr1$SpeciesID==" p57sp3"]="cuspidata"
fieldtr1$NewGenus[fieldtr1$SpeciesID==" p57sp4"]="Ficinia"
fieldtr1$NewSpecies[fieldtr1$SpeciesID==" p57sp4"]="stolonifera"
fieldtr1$NewGenus[fieldtr1$SpeciesID=="Crassula orbiculata"]="Crassula"
fieldtr1$NewSpecies[fieldtr1$SpeciesID=="Crassula orbiculata"]="orbicularis"
fieldtr1$NewGenus[fieldtr1$SpeciesID=="Muraltia p56sp"]="Tetraria"
fieldtr1$NewSpecies[fieldtr1$SpeciesID=="Muraltia p56sp"]="pleiosticha"
fieldtr1$NewGenus[fieldtr1$SpeciesID=="Pentamaris p98sp1"]="Psuedopentameris"
fieldtr1$NewSpecies[fieldtr1$SpeciesID=="Pentamaris p98sp1"]="macrantha"
fieldtr1$NewGenus[fieldtr1$SpeciesID=="Pentaschistis p59sp3"]="Pentaschistis"
fieldtr1$NewSpecies[fieldtr1$SpeciesID=="Pentaschistis p59sp3"]="colorata"
fieldtr1$NewGenus[fieldtr1$SpeciesID=="Plot12SP7 "]="Senecio"
fieldtr1$NewSpecies[fieldtr1$SpeciesID=="Plot12SP7 "]="umbellatus"
fieldtr1$NewGenus[fieldtr1$SpeciesID=="Tetraria p55sp1"]="Tetraria"
fieldtr1$NewSpecies[fieldtr1$SpeciesID=="Tetraria p55sp1"]="crassa"
fieldtr1$NewGenus[fieldtr1$SpeciesID=="Tetraria p63sp1"]="Tetraria"
fieldtr1$NewSpecies[fieldtr1$SpeciesID=="Tetraria p63sp1"]="crassa"
fieldtr1$NewGenus[fieldtr1$SpeciesID=="Thesium p55sp3"]="Thesium"
fieldtr1$NewSpecies[fieldtr1$SpeciesID=="Thesium p55sp3"]="nigromontanum"
fieldtr1$NewGenus[fieldtr1$SpeciesID=="Thesium plot12Sp5"]="Thesium"
fieldtr1$NewSpecies[fieldtr1$SpeciesID=="Thesium plot12Sp5"]="acuminatum"    
fieldtr1$NewGenus[fieldtr1$SpeciesID=="Watsonia "]="Watsonia"
fieldtr1$NewSpecies[fieldtr1$SpeciesID=="Watsonia "]="tabularis" 

###check to make sure all have been corrected
fieldtr1[is.na(fieldtr1$NewGenus)==T,]
nrow(fieldtr1[is.na(fieldtr1$NewGenus)==T,])  #1

#Exclude one problematic row (IDed only to family)
fieldtr2=fieldtr1[is.na(fieldtr1$NewGenus)==F,]  
dim(fieldtr2)

#Duplicate Watsonia row
fieldtr2[nrow(fieldtr2)+1,]=fieldtr2[fieldtr2$SpeciesID=="Watsonia ",]
dim(fieldtr2)
fieldtr2$NewSpecies[nrow(fieldtr2)]="meriana"

###check to make sure all have been corrected
fieldtr2[is.na(fieldtr2$NewGenus)==T,]
nrow(fieldtr2[is.na(fieldtr2$NewGenus)==T,])  #0

##Write corrected file
write.csv(fieldtr2, "Data/20110329_FieldData_namescorrected.csv", row.names=F)

#########################################################################
################### Correct synonyms in isotope data ####################
#########################################################################

iso=read.csv("PreprocessedData/isotopes.csv")
head(iso)
dim(iso)

iso1=merge(iso, syn1, by.x="Species", by.y="SpeciesID", all.x=T, all.y=F)
dim(iso1)

###check to make sure all have been corrected
iso1[is.na(iso1$NewGenus)==T,]

iso1$NewGenus=as.character(iso1$NewGenus)
iso1$NewSpecies=as.character(iso1$NewSpecies)
iso1$NewGenus[iso1$Species=="X p57sp1"]="Syncarpha"
iso1$NewSpecies[iso1$Species=="X p57sp1"]="speciosissima"
iso1$NewGenus[iso1$Species=="X p57sp2"]="Tetraria"
iso1$NewSpecies[iso1$Species=="X p57sp2"]="sylvatica"
iso1$NewGenus[iso1$Species=="X p57sp3"]="Tetraria"
iso1$NewSpecies[iso1$Species=="X p57sp3"]="cuspidata"
iso1$NewGenus[iso1$Species=="X p57sp4"]="Ficinia"
iso1$NewSpecies[iso1$Species=="X p57sp4"]="stolonifera"
iso1$NewGenus[iso1$Species=="Crassula orbiculata"]="Crassula"
iso1$NewSpecies[iso1$Species=="Crassula orbiculata"]="orbicularis"
iso1$NewGenus[iso1$Species=="Muraltia p56sp"]="Tetraria"
iso1$NewSpecies[iso1$Species=="Muraltia p56sp"]="pleiosticha"
iso1$NewGenus[iso1$Species=="Pentamaris p98sp1"]="Psuedopentameris"
iso1$NewSpecies[iso1$Species=="Pentamaris p98sp1"]="macrantha"
iso1$NewGenus[iso1$Species=="Pentaschistis p59sp3"]="Pentaschistis"
iso1$NewSpecies[iso1$Species=="Pentaschistis p59sp3"]="colorata"
iso1$NewGenus[iso1$Species=="Plot12SP7 "]="Senecio"
iso1$NewSpecies[iso1$Species=="Plot12SP7 "]="umbellatus"
iso1$NewGenus[iso1$Species=="Tetraria p55sp1"]="Tetraria"
iso1$NewSpecies[iso1$Species=="Tetraria p55sp1"]="crassa"
iso1$NewGenus[iso1$Species=="Tetraria p63sp1"]="Tetraria"
iso1$NewSpecies[iso1$Species=="Tetraria p63sp1"]="crassa"
iso1$NewGenus[iso1$Species=="Thesium p55sp3"]="Thesium"
iso1$NewSpecies[iso1$Species=="Thesium p55sp3"]="nigromontanum"
iso1$NewGenus[iso1$Species=="Thesium plot12Sp5"]="Thesium"
iso1$NewSpecies[iso1$Species=="Thesium plot12Sp5"]="acuminatum"    
iso1$NewGenus[iso1$Species=="Stoebe cyathuloides"]="Stoebe"
iso1$NewSpecies[iso1$Species=="Stoebe cyathuloides"]="cyathuloides"
iso1$NewGenus[iso1$Species=="Syncharpha gnaphaloides"]="Syncarpha"
iso1$NewSpecies[iso1$Species=="Syncharpha gnaphaloides"]="gnaphaloides"
iso1$NewGenus[iso1$Species=="Ficinia p12sp4"]="Ficinia"
iso1$NewSpecies[iso1$Species=="Ficinia p12sp4"]="bulbosa"
iso1$NewGenus[iso1$Species=="Watsonia sp"]="Watsonia"
iso1$NewSpecies[iso1$Species=="Watsonia sp"]="meriana"


###check to make sure all have been corrected
iso1[is.na(iso1$NewGenus)==T,]

#Exclude one problematic row (IDed only to family)
iso2=iso1[is.na(iso1$NewGenus)==F,]  
dim(iso2)

write.csv(iso2, "Data/isotopes_namescorrected.csv", row.names=F)


#########################################################################
#########################################################################
################### STEP 2: COMBINE RELEVE DATA #########################
#########################################################################
#########################################################################

library(reshape)
library(gregmisc)

#Read in data
new=read.csv("Data/ReleveQuadrat2010_namescorrected.csv")
old=read.csv("Data/Releve66_96_NamesCorrected.csv")

str(new)

#plot-level metrics for new data
pc=subset(new, select= c("A_PercCov","B_PercCov","C_PercCov","D_PercCov",
        "E_PercCov","F_PercCov","G_PercCov","H_PercCov","I_PercCov","J_PercCov"))
new$MeanPercCov=rowSums(pc, na.rm=T)/10

abun=subset(new, select=c(A_N, B_N, C_N, D_N, E_N, F_N, G_N, H_N, I_N, J_N))
new$TotalAbun=rowSums(abun,na.rm=T)


releve=subset(new, select=c(Plot, NewGenus, NewSpecies, MeanPercCov, TotalAbun))
releve$Year=2010

#reshape old data
mold=melt.data.frame(old, id.vars=c("NewGenus","NewSpecies"), measure.vars=colnames(old)[24:185])

#new columns
mold$Plot=sub("X","",mold$variable)
mold$Plot=sub("[.].*","",mold$Plot)
mold$Plot=as.numeric(mold$Plot)

mold$Year[grepl(".66",mold$variable)==T]=1966
mold$Year[grepl(".66",mold$variable)==F]=1996

summary(mold)

mold=rename.vars(mold, from="value", to="AbunClass")

#merge old & new
releve2=merge(releve,mold,by=c("Year","Plot","NewGenus","NewSpecies"),all=T)

releve2=subset(releve2, select=c("Year","Plot","NewGenus","NewSpecies","MeanPercCov","TotalAbun","AbunClass"))

write.csv(releve2, "Data/ReleveAll.csv")


#########################################################################
#########################################################################
############# STEP 3: COMBINE TRAIT DATA &  #############################
############# CREATE TRAITS x SPECIES DATAFRAME #########################
#########################################################################
#########################################################################

library(gregmisc)

#################### Lab Data ######################
####################################################

tr=read.csv("Data/20110329_FieldData_namescorrected.csv", stringsAsFactors=F)
head(tr)
dim(tr)
str(tr)
labtr=read.csv("PreprocessedData/Cape Point Lab Data.csv", stringsAsFactors=F)
head(labtr)
dim(labtr)
str(labtr)

labtr=rename.vars(labtr, from="ID", to="Replicate")
labtr=rename.vars(labtr, from="LeafArea", to="RawLeafArea_cm2")
labtr=rename.vars(labtr, from="LeafLength", to="LeafLength_cm")
labtr=rename.vars(labtr, from="LeafFresh_g", to="RawLeafFresh_g")
labtr=rename.vars(labtr, from="LeafDry_g", to="RawLeafDry_g")
labtr=rename.vars(labtr, from="AvgLeafWidth", to="AvgLeafWidth_cm")
labtr=rename.vars(labtr, from="MaxLeafWidth", to="MaxLeafWidth_cm")
labtr=rename.vars(labtr, from="LeafThickness", to="LeafThickness_mm")

tr$UID=paste(tr$Date, tr$Sample, tr$Collector)
labtr$UID=paste(labtr$Date, labtr$Sample, labtr$Collector)

alltr=merge(tr, labtr, by="UID", suffixes=c(".x",""), all=T)
head(alltr)
dim(alltr)
colnames(alltr)

#add coordinates
coord=read.csv("PreprocessedData/Taylor_COGHNR.csv")
head(coord)
coord=subset(coord, select=c(PLOT, Latitude..Ross.Turner.2010., Longitude..Ross.Turner.2010.))
coord=rename.vars(coord,from="PLOT", to="plot")
coord=rename.vars(coord, from="Latitude..Ross.Turner.2010.", to="Latitude2010")
coord=rename.vars(coord, from="Longitude..Ross.Turner.2010.", to="Longitude2010")

coord=coord[!is.na(coord$Latitude2010),]


alltr=merge(alltr,coord,by="plot",all.x=T)
dim(alltr)

alltr=subset(alltr, select=c("Collector","GardenField","Date","Sample","Replicate",
  "NewGenus","NewSpecies","plot","VeldAge","Latitude2010","Longitude2010","Height_cm",
  "CanopyX_cm","CanopyY_cm","BranchingOrder","NumLeaves",
  "RawLeafArea_cm2","LeafLength_cm","AvgLeafWidth_cm","MaxLeafWidth_cm","LeafThickness_mm",
  "RawLeafFresh_g","RawLeafDry_g","TwigFresh_g","TwigDry_g","SLA","LeafSucculence","TwigSucculence"))

#Exclude 28 specimens with no lab data
alltr=alltr[!is.na(alltr$Collector),]

dim(alltr)
str(alltr)

#convert from percent
head(alltr$LeafSucculence)
alltr$LeafSucculence=sub("%","",alltr$LeafSucculence)
head(alltr$LeafSucculence)

head(alltr$TwigSucculence)
alltr$TwigSucculence=sub("%","",alltr$TwigSucculence)
head(alltr$TwigSucculence)

#convert to numeric
alltr$RawLeafArea_cm2=as.numeric(alltr$RawLeafArea_cm2)
alltr$AvgLeafWidth_cm=as.numeric(alltr$AvgLeafWidth_cm)
alltr$MaxLeafWidth_cm=as.numeric(alltr$MaxLeafWidth_cm)
alltr$SLA=as.numeric(alltr$SLA)
alltr$LeafSucculence=as.numeric(alltr$LeafSucculence)
alltr$TwigSucculence=as.numeric(alltr$TwigSucculence)

str(alltr)

#divide by number of leaves
alltr$LeafArea_cm2=alltr$RawLeafArea_cm2/alltr$NumLeaves
alltr$LeafFresh_g=alltr$RawLeafFresh_g/alltr$NumLeaves
alltr$LeafDry_g=alltr$RawLeafDry_g/alltr$NumLeaves


summary(alltr)


write.csv(alltr, "Data/LabData.csv",row.names=F)

######### Correcting Errors in Lab Data ############
####################################################
labdt=read.csv("Data/LabData.csv")
head(labdt)
dim(labdt)

labdt[labdt$AvgLeafWidth_cm==22.0,]
labdt$AvgLeafWidth_cm[labdt$AvgLeafWidth_cm==22.0]=2.2
labdt[labdt$AvgLeafWidth_cm==2.2,]

labdt[labdt$LeafThickness_mm==146,]
labdt$LeafThickness_mm[labdt$LeafThickness_mm==146]=.146
labdt[labdt$LeafThickness_mm==.146,]

write.csv(labdt, "Data/LabData.csv",row.names=F)
 
########### Traits by Species Dataframe ############
####################################################


tr=read.csv("Data/20110329_FieldData_namescorrected.csv")
head(tr)
dim(tr)
labdt=read.csv("Data/LabData.csv")
head(labdt)
dim(labdt)

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
mxcheck[mxcheck %in% meanscheck==F]  # Cussonia thyrsifolia; I am just going to exclude this...

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
grw1[grw1$Genus=="Erica" & grw1$Species=="hispidula",]  # remove duplicated
grw1[grw1$Genus=="Ficinia" & grw1$Species=="filiformis",] #remove duplicated
grw1[grw1$Genus=="Zygophyllum" & grw1$Species=="spinosum",] # Regeneration value differs between these two
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

write.csv(allsptr, "Data/SpeciesTraits.csv",row.names=F)