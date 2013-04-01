##################################################################
########################## Sp/Plot (L) ##########################

######### CP
setwd("C:/Work/Dimensions/Data")
library(reshape)
library(lattice)

##Read in data
pl1=read.csv("PostprocessedData/ReleveAll.csv")
pl1$NewSpeciesID=paste(pl1$NewGenus, pl1$NewSpecies)

#Transform AbunClass to median # of individuals per class (from 2010 where both were measured)
pl1$AbunClass2[pl1$AbunClass==1]=2
pl1$AbunClass2[pl1$AbunClass==2]=7
pl1$AbunClass2[pl1$AbunClass==3]=23
pl1$AbunClass2[pl1$AbunClass==4]=69
pl1$AbunClass2[pl1$AbunClass==5]=200.5
pl2=pl1[is.na(pl1$AbunClass2)==F,]

# Normalize #individuals per plot (convert to percent)
pl2$PlotYear=paste(pl2$Plot, pl2$Year)      #unique identifier for plot x year
plottotind=matrix(nrow=length(unique(pl2$PlotYear)), ncol=1)
for (i in 1:length(unique(pl2$PlotYear))){
  plottotind[i]=sum(pl2$AbunClass2[pl2$PlotYear==(unique(pl2$PlotYear)[i])])
  }
plottotind1=cbind(unique(pl2$PlotYear), plottotind)
colnames(plottotind1)=c("PlotYear","TotalIndiv")
plottotind1=as.data.frame(plottotind1)
plottotind1$TotalIndiv=as.numeric(as.character(plottotind1$TotalIndiv))

#species x plot/year matrix
spabun_pn=cbind(pl2[,c("PlotYear","NewSpeciesID","AbunClass2")])
for (i in 1:nrow(spabun_pn)){
  spabun_pn$PlotTotInd[i]=plottotind1$TotalIndiv[plottotind1$PlotYear==spabun_pn$PlotYear[i]]
  }
spabun_pn$WghtedAbun=spabun_pn$AbunClass2/spabun_pn$PlotTotInd
spabun_plot=cast(spabun_pn, PlotYear~NewSpeciesID, value="WghtedAbun", sum)

# check
#table(rowSums(spabun_plot, na.rm=T))   #should all be 1

# 1996 only
l.cp96=spabun_plot[which(grepl("1996",spabun_plot$PlotYear)==T),]
rownames(l.cp96)=sub("[ ]1996","",l.cp96$PlotYear)
l.cp96=l.cp96[,2:ncol(l.cp96)]
l.cp96=l.cp96[,which(colSums(l.cp96)!=0)]

######### 2012
setwd("C:/Work/Brian_work/2012DataExploration/CSVs")

###read data
c.r=read.csv("Cederberg_ReleveAbund.csv")
h.r=read.csv("Hangklip_ReleveAbund.csv")
l1.r=read.csv("Langeberg1_ReleveAbund.csv")
#l2.r    - need to address issues with langeberg2 abundance

#read synonym file (for releve data)
c.syn=read.csv("Cederberg_Synonyms.csv")
h.syn=read.csv("Hangklip_Synonyms.csv")
l1.syn=read.csv("Langeberg1_Synonyms.csv")
#l2.syn=read.csv("Langeberg2_Synonyms.csv")

## Releve data formatting/rescaling

# Releve data: update names
c.r1=merge(c.syn, c.r, by.x="Species.Name.in.Cederberg.Plots", by.y="Releve.number", all=F)
h.r1=merge(h.syn, h.r, by.x="Species.Name.in.Hangklip.Plots", by.y="Releve.number", all=F)
l1.r1=merge(l1.syn, l1.r, by.x="Species.Name.in.Langeberg1.Plots", by.y="Releve.number", all=F)
#l2.r1

# Rescale to relative abundance
c.r2=matrix(nrow=482, ncol=191)
for (i in 1:191){
  c.r2[,i]=c.r1[,i+2]/sum(c.r1[,i+2])
  }

h.r2=matrix(nrow=412, ncol=251)
for (i in 1:251){
  h.r2[,i]=h.r1[,i+2]/sum(h.r1[,i+2])
  }

l1.r2=matrix(nrow=428, ncol=150)
for (i in 1:150){
  l1.r2[,i]=l1.r1[,i+2]/sum(l1.r1[,i+2])
  }

#add row & col names
rownames(c.r2)=c.r1$Species.Name.in.Cederberg.Plots
colnames(c.r2)=colnames(c.r1)[3:193]
rownames(h.r2)=h.r1$Species.Name.in.Hangklip.Plots
colnames(h.r2)=colnames(h.r1)[3:253]
rownames(l1.r2)=l1.r1$Species.Name.in.Langeberg1.Plots
colnames(l1.r2)=colnames(l1.r1)[3:152]

#aggregate LB1 species (duplicate names in file)
l1.r7=aggregate(l1.r2, by=list(rownames(l1.r2)), FUN=sum)

#invert releve matrix
c.r6=t(c.r2)
h.r6=t(h.r2)
l1.r6=t(l1.r7[,2:ncol(l1.r7)])
colnames(l1.r6)=l1.r7$Group.1


######### BK
setwd("C:/Work/Baviaanskloof/Data")

#read data
bsp=read.csv("Baviaans_96_plots.csv", row.names=1)
# change NA to 0s
bsp[is.na(bsp)==T]=0

# Synonym correction
library(xlsx)
synbk=read.xlsx("Synonyms/Bkloof_NameMatching.xlsx",1)
synbk$Genus=as.character(synbk$Genus)
synbk$Species=as.character(synbk$Species)

# Exclude genus-only IDs
synbk$Genus[is.na(synbk$Species)==T]=NA

# match new name
synbk$NewID=paste(synbk$Genus,synbk$Species)
synbk=subset(synbk, select=c("OriginalName","NewID"))

name=as.data.frame(rownames(bsp))
name=merge(name, synbk, by.x=1,by.y="OriginalName",all.x=T,all.y=F)
name[name=="NA NA"]=NA

# exclude NAs
name=name[is.na(name$NewID)==F,]
bsp1=bsp[rownames(bsp) %in% name[,1]==T,]

# Rescale abundances
for(j in 1:ncol(bsp1)){
    bsp1[,j]=bsp1[,j]/colSums(bsp1)[j]
}

bsp1=t(bsp1)

######### Combining sp/plot

#Plot names
rownames(l.cp96)=paste("CP",rownames(l.cp96),sep="_")

#HK
rownames(h.r6)=sub("X","",rownames(h.r6))
rownames(h.r6)=as.numeric(rownames(h.r6))-2294
rownames(h.r6)=paste("HK",rownames(h.r6),sep="_")
#CB
rownames(c.r6)=sub("X","",rownames(c.r6))
rownames(c.r6)=as.numeric(rownames(c.r6))-760
rownames(c.r6)=paste("CB",rownames(c.r6),sep="_")
#LB1
rownames(l1.r6)=sub("X","",rownames(l1.r6))
rownames(l1.r6)=paste("LB1",rownames(l1.r6),sep="_")
#BK
rownames(bsp1)=sub("X","",rownames(bsp1))
rownames(bsp1)=paste("BK",rownames(bsp1),sep="_")


### Combine!
#as data frame, column for plot (otherwise funky things happen)
h.r7=as.data.frame(h.r6)
h.r7$Plot=rownames(h.r7)
c.r7=as.data.frame(c.r6)
c.r7$Plot=rownames(c.r7)
l1.r7=as.data.frame(l1.r6)
l1.r7$Plot=rownames(l1.r7)
bsp2=as.data.frame(bsp1)
bsp2$Plot=rownames(bsp2)
l.cp96.1=as.data.frame(l.cp96)
l.cp96.1$Plot=rownames(l.cp96.1)

#merge
L=merge(h.r7, c.r7, all=T)
L=merge(L, l1.r7, all=T)
L=merge(L, bsp2, all=T)
L=merge(L, l.cp96.1, all=T)

#add rownames, drop Plot column
rownames(L)=L$Plot
L=L[,which(colnames(L)!="Plot")]


setwd("C:/Work/ProtPelTrait/MultivariateAnalysis")
write.csv(L,"Data/L.csv")

##################################################################
########################## Sp/Trait (Q) ##########################

# Match to species in L
setwd("C:/Work/ProtPelTrait/MultivariateAnalysis")
L=read.csv("Data/L.csv", row.names=1)

######### CP
setwd("C:/Work/Dimensions/Data")
##Read in data
tr1=read.csv("PostprocessedData/SpeciesTraits_ErrorsExcluded.csv")

#LMA, canopy area, FWC, LWR, leaf thickness, plant height
q.cp=subset(tr1, select=c("Genus","Species","SLA_Mean","CanopyArea_sqcm_Max","RawLeafDry_g_Mean","RawLeafFresh_g_Mean","LeafLength_cm_Mean","AvgLeafWidth_cm_Mean","LeafThickness_mm_Mean","Height_cm_Max"))
q.cp$SpeciesID=paste(q.cp$Genus, q.cp$Species)
q.cp$LMA=1/q.cp$SLA_Mean
q.cp$CanopyArea=q.cp$CanopyArea_sqcm_Max
q.cp$FWC=(q.cp$RawLeafFresh_g_Mean-q.cp$RawLeafDry_g_Mean)/q.cp$RawLeafDry_g_Mean
q.cp$LWR=q.cp$LeafLength_cm_Mean/q.cp$AvgLeafWidth_cm_Mean
q.cp$LeafThick=q.cp$LeafThickness_mm_Mean
q.cp$Height=q.cp$Height_cm_Max

q.cp1=subset(q.cp, select=c("LMA","CanopyArea","FWC","LWR","LeafThick","Height"))
rownames(q.cp1)=q.cp$SpeciesID
rownames(q.cp1)=sub("[ ]","\\.",rownames(q.cp1))
#
q.cp1=q.cp1[which(rownames(q.cp1) %in% colnames(L)==T),]

######### 2012
setwd("C:/Work/Brian_work/2012DataExploration/CSVs")

#Field data
c.f=read.csv("CederbergTraitData2012_Field.csv")
h.f=read.csv("HangklipTraitData2012_Field.csv")
l.f=read.csv("LangebergTraitData2012_Field.csv")

#Lab data
c.l=read.csv("CederbergTraitData2012_Lab.csv")
h.l=read.csv("HangklipTraitData2012_Lab.csv")
l.l=read.csv("LangebergTraitData2012_Lab.csv")

#convert any factors to numeric
c.l[,14]=as.numeric(as.character(c.l[,14]))
c.l[,15]=as.numeric(as.character(c.l[,15]))
c.l[,16]=as.numeric(as.character(c.l[,16]))
h.l[,7]=as.numeric(as.character(h.l[,7]))
h.l[,14]=as.numeric(as.character(h.l[,14]))
h.l[,15]=as.numeric(as.character(h.l[,15]))
l.l[,12]=as.numeric(as.character(l.l[,12]))
l.l[,14]=as.numeric(as.character(l.l[,14]))
l.l[,15]=as.numeric(as.character(l.l[,15]))
l.l[,16]=as.numeric(as.character(l.l[,16]))

#species means
c.f1=aggregate(c.f[,c(5:7)], by=list(c.f$Species.Name..New.), mean, na.rm=T)
c.l1=aggregate(c.l[,c(6:16)], by=list(c.l$Species.Name), mean, na.rm=T)
h.f1=aggregate(h.f[,c(5:7)], by=list(h.f$Species.Name..New.), mean, na.rm=T)
h.l1=aggregate(h.l[,c(6:16)], by=list(h.l$Species.Name), mean, na.rm=T)
l.f1=aggregate(l.f[,c(5:7)], by=list(l.f$Species.Name..New.), mean, na.rm=T)
l.l1=aggregate(l.l[,c(6:16)], by=list(l.l$Species.Name), mean, na.rm=T)

c.t=merge(c.f1, c.l1, by="Group.1", all=T)
h.t=merge(h.f1, h.l1, by="Group.1", all=T)
l.t=merge(l.f1, l.l1, by="Group.1", all=T)

#LMA, canopy area, FWC, LWR, leaf thickness, plant height
c.t$LMA=1/c.t$SLA
c.t$CanopyArea=pi*c.t$CanopyAxis1*c.t$CanopyAxis2
c.t$FWC=(c.t$Fresh.Mass..g.-c.t$Dry.Mass..g.)/c.t$Fresh.Mass..g.
c.t$LWR=c.t$Leaf.Length/c.t$Avg.Leaf.Width
c.t$LeafThick=c.t$Leaf.Thickness
c.t$Height=c.t$Heigh_cm

h.t$LMA=1/h.t$Specific.Leaf.Area
h.t$CanopyArea=pi*h.t$CanopyAxis1*h.t$CanopyAxis2
h.t$FWC=(h.t$Fresh.Mass..g.-h.t$Dry.Mass..g.)/h.t$Fresh.Mass..g.
h.t$LWR=h.t$Leaf.Length/h.t$Avg.Leaf.Width
h.t$LeafThick=h.t$Leaf.Thickness
h.t$Height=h.t$Heigh_cm

l.t$LMA=1/l.t$SLA
l.t$CanopyArea=pi*l.t$CanopyAxis1*l.t$CanopyAxis2
l.t$FWC=(l.t$Fresh.Mass..g.-l.t$Dry.Mass..g.)/l.t$Fresh.Mass..g.
l.t$LWR=l.t$Leaf.Length/l.t$Avg.Leaf.Width
l.t$LeafThick=l.t$Leaf.Thickness
l.t$Height=l.t$Heigh_cm

q.cb=subset(c.t, select=c("LMA","CanopyArea","FWC","LWR","LeafThick","Height"))
rownames(q.cb)=c.t$Group.1
rownames(q.cb)=sub("[ ]","\\.",rownames(q.cb))
q.hk=subset(h.t, select=c("LMA","CanopyArea","FWC","LWR","LeafThick","Height"))
rownames(q.hk)=h.t$Group.1
rownames(q.hk)=sub("[ ]","\\.",rownames(q.hk))
q.lb=subset(l.t, select=c("LMA","CanopyArea","FWC","LWR","LeafThick","Height"))
rownames(q.lb)=l.t$Group.1
rownames(q.lb)=sub("[ ]","\\.",rownames(q.lb))

q.cb1=q.cb[which(rownames(q.cb) %in% colnames(L)==T),]
q.hk1=q.hk[which(rownames(q.hk) %in% colnames(L)==T),]
q.lb1=q.lb[which(rownames(q.lb) %in% colnames(L)==T),]
 ## why are there so many species that have traits measured but are not in plots?

######### BKloof
setwd("C:/Work/Baviaanskloof/Data")

#read data
library(xlsx)
blb=read.xlsx("BavLabDataDigitized.xlsx", 1)
bf=read.xlsx("FieldData.xlsx",1)

#Combine lab/field data
#NEED TO CORRECT SOME BKLOOF NAMES STILL--several species not in synonym file

# Synonym correction
synbk=read.xlsx("Synonyms/Bkloof_NameMatching.xlsx",1)
synbk$Genus=as.character(synbk$Genus)
synbk$Species=as.character(synbk$Species)
# Exclude genus-only IDs
synbk$Genus[is.na(synbk$Species)==T]=NA
# match new name
synbk$NewID=paste(synbk$Genus,synbk$Species)
synbk=subset(synbk, select=c("OriginalName","NewID"))

#make sp ID column, match to synonym file
bf$SpID=paste(bf$Genus, bf$Species)
bf$SpID[bf$SpID ]

name=merge(bf$SpID, synbk, by.x=1,by.y="OriginalName",all.x=T,all.y=F)
name[name=="NA NA"]=NA
name=name[which(duplicated(name)==F),]

bf1=merge(bf,name,by.x="SpID",by.y="x",all.x=T, all.y=F)
bf1$UID=paste(bf1$Recorder,bf1$Date.MDD.,bf1$ID,sep=".")

bf1=subset(bf1, select=c("UID","NewID","Heigh_cm","CanopyAxis1","CanopyAxis2"))
bf1=bf1[is.na(bf1$NewID)==F,]

#species values for field data
h=aggregate(bf1$Heigh_cm, by=list(bf1$NewID), FUN=max)
c=aggregate(bf1[,4:5], by=list(bf1$NewID), FUN=mean)
bf2=merge(h,c)

#names from field data for lab data
name=bf1[,1:2]
blb$UID=paste(blb$Recorder,blb$Date,blb$ID,sep=".")
blb1=merge(blb,name,by="UID")

#subset lab data
blb1=subset(blb1, select=c("UID","NewID","LeafLength","AvgLeafWidth","LeafThickness","LeafFresh_g","LeafDry_g","LeafArea"))

#factor to numeric
blb1[,3]=as.numeric(as.character(blb1[,3]))
blb1[,4]=as.numeric(as.character(blb1[,4]))
blb1[,5]=as.numeric(as.character(blb1[,5]))
blb1[,6]=as.numeric(as.character(blb1[,6]))
blb1[,7]=as.numeric(as.character(blb1[,7]))
blb1[,8]=as.numeric(as.character(blb1[,8]))

blb2=aggregate(blb1[,3:8],by=list(blb1$NewID),FUN=mean)

#merge lab & field
q.bk=merge(blb2,bf2,all=F)
#LMA, canopy area, FWC, LWR, leaf thickness, plant height
q.bk$LMA=q.bk$LeafDry_g/q.bk$LeafArea
q.bk$CanopyArea=pi*q.bk$CanopyAxis1*q.bk$CanopyAxis2
q.bk$FWC=(q.bk$LeafFresh_g-q.bk$LeafDry_g)/q.bk$LeafFresh_g
q.bk$LWR=q.bk$LeafLength/q.bk$AvgLeafWidth
q.bk$LeafThick=q.bk$LeafThickness
q.bk$Height=q.bk$x

q.bk1=subset(q.bk, select=c("LMA","CanopyArea","FWC","LWR","LeafThick","Height"))
rownames(q.bk1)=sub("[ ]","\\.",q.bk$Group.1)

q.bk1=q.bk1[which(rownames(q.bk1) %in% colnames(L)==T),]


### Combine all sites for Q matrix
Q=rbind(q.bk1,q.cb1,q.hk1,q.lb1,q.cp1)
names=c(rownames(q.bk1),rownames(q.cb1),rownames(q.hk1),rownames(q.lb1),rownames(q.cp1))
Q=aggregate(Q, by=list(names),FUN=mean)
rownames(Q)=Q$Group.1
Q=Q[,2:7]

########### R,L,Q: Order, align, and write files #############
e4=read.csv("C:/Work/ProtPelTrait/Data/CommunityWilson_20yravg.csv")
e4$ratio=(e4$MMP.10+e4$MMP.11+e4$MMP.12+e4$MMP.01+e4$MMP.02+e4$MMP.03)/(e4$MMP.04+e4$MMP.05+e4$MMP.06+e4$MMP.07+e4$MMP.08+e4$MMP.09)

R=subset(e4, select=c("MATmax","MATmin","ratio","CDD","sdii","MAP","CFD"))
rownames(R)=e4$UID

#remove species which occur in zero plots
L=L[,which(colSums(L,na.rm=T)!=0)]
Q=Q[,which(colnames(Q) %in% colnames(L)==T)]
#remove plots which have zero species
L=L[which(rowSums(L,na.rm=T)!=0),]

R=R[which(rownames(R)%in%rownames(L)==T),]
L=L[which(rownames(L)%in%rownames(R)==T),]
L=L[,which(colnames(L)%in%rownames(Q)==T)]

#order
L=L[order(rownames(L)),order(colnames(L))]
R=R[order(rownames(R)),order(colnames(R))]
Q=Q[order(rownames(Q)),order(colnames(Q))]

#NAs to zeroes in L,R (R bc only NAs are for CFD)
L[is.na(L)==T]=0
R[is.na(R)==T]=0

#negative in FWC / replace NAs with means for Q
Q$FWC[Q$FWC<0]=NA
Q$FWC[is.na(Q$FWC)]=mean(Q$FWC,na.rm=T)
Q$LeafThick[is.na(Q$LeafThick)]=mean(Q$LeafThick,na.rm=T)
Q$LMA[is.na(Q$LMA)]=mean(Q$LMA,na.rm=T)
Q$LWR[is.na(Q$LWR)]=mean(Q$LWR,na.rm=T)



#### Write Files
setwd("C:/Work/ProtPelTrait/MultivariateAnalysis")
write.csv(L,"Data/L.csv")
write.csv(R,"Data/R.csv")
write.csv(Q,"Data/Q.csv")