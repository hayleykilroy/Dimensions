####################################################################
##### Matrix Multiplication of Plot Abundance x Species Traits #####
################### Includes 1966, 1996, & 2010  ###################
####################################################################

setwd("C:/Work/Dimensions/Data")
library(reshape)
library(lattice)

##Read in data
tr1=read.csv("PostprocessedData/SpeciesTraits_ErrorsExcluded.csv")
pl1=read.csv("PostprocessedData/ReleveAll.csv")

head(tr1)
head(pl1)

pl1$NewSpeciesID=paste(pl1$NewGenus, pl1$NewSpecies)

#Transform AbunClass to median # of individuals per class (from 2010 where both were measured)
pl1$AbunClass2[pl1$AbunClass==1]=2
pl1$AbunClass2[pl1$AbunClass==2]=7
pl1$AbunClass2[pl1$AbunClass==3]=23
pl1$AbunClass2[pl1$AbunClass==4]=69
pl1$AbunClass2[pl1$AbunClass==5]=200.5

pl2=pl1[is.na(pl1$AbunClass2)==F,]

##Exploration--was that the appropriate transformation for AbunClass??
pl10=pl1[pl1$Year==2010,]
par(mfrow=c(2,3))
hist(pl10$TotalAbun[pl10$AbunClass==1])
hist(pl10$TotalAbun[pl10$AbunClass==2])
hist(pl10$TotalAbun[pl10$AbunClass==3])
hist(pl10$TotalAbun[pl10$AbunClass==4])
hist(pl10$TotalAbun[pl10$AbunClass==5])

tapply(pl10$TotalAbun,pl10$AbunClass,mean)
tapply(pl10$TotalAbun,pl10$AbunClass,median)
# 150 is very conservative for category 5

##

# Normalize #individuals per plot (convert to percent)
pl2$PlotYear=paste(pl2$Plot, pl2$Year)      #unique identifier for plot x year

plottotind=matrix(nrow=length(unique(pl2$PlotYear)), ncol=1)
for (i in 1:length(unique(pl2$PlotYear))){
  plottotind[i]=sum(pl2$AbunClass2[pl2$PlotYear==(unique(pl2$PlotYear)[i])])
  }
summary(plottotind)

plottotind1=cbind(unique(pl2$PlotYear), plottotind)
colnames(plottotind1)=c("PlotYear","TotalIndiv")
head(plottotind1)
str(plottotind1)
plottotind1=as.data.frame(plottotind1)
str(plottotind1)
plottotind1$TotalIndiv=as.numeric(as.character(plottotind1$TotalIndiv))

#species x plot/year matrix
spabun_pn=cbind(pl2[,c("PlotYear","NewSpeciesID","AbunClass2")])
for (i in 1:nrow(spabun_pn)){
  spabun_pn$PlotTotInd[i]=plottotind1$TotalIndiv[plottotind1$PlotYear==spabun_pn$PlotYear[i]]
  }

spabun_pn$WghtedAbun=spabun_pn$AbunClass2/spabun_pn$PlotTotInd

head(spabun_pn)
summary(spabun_pn)

spabun_plot=cast(spabun_pn, PlotYear~NewSpeciesID, value="WghtedAbun", sum)
head(spabun_plot)
summary(spabun_plot)

# check
table(rowSums(spabun_plot, na.rm=T))   #should all be 1


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
allsp$Plots=allsp$Species%in%sp_p  # Is species in releve data- T/F?
allsp$Traits=allsp$Species%in%sp_t # Is species in trait data- T/F?
head(allsp)
summary(allsp)

#noplot=allsp$Species[allsp$Plots==F]
#noplot   #OK--these species were collected early in 2010 but never showed up in any plots ("Exists==FALSE" in 2010 trait data)

notrait=allsp$Species[allsp$Traits==F]
notrait

#Percentage of each plot that is species with no trait data
nodataperc=subset(spabun_plot, select=notrait)
nodataperc1=rowSums(nodataperc, na.rm=T)
nodataperc1
summary(nodataperc1)
hist(nodataperc1)
nodataperc2=cbind(spabun_plot$PlotYear, nodataperc1)
nodataperc2

nodataperc2=nodataperc2[order(nodataperc2[,2]),]


#### Exploration of most abundant species which we're missing traits for
#miss=spabun_plot[,c(1,which(colnames(spabun_plot) %in%  notrait==T))]
#miss66=miss[grepl("1966",miss$PlotYear)==T,]
#miss96=miss[grepl("1996",miss$PlotYear)==T,]
#miss10=miss[grepl("2010",miss$PlotYear)==T,]

#misstot=cbind(colnames(miss66)[2:ncol(miss66)],colSums(miss66[,2:ncol(miss66)]),colSums(miss96[,2:ncol(miss96)]),colSums(miss10[,2:ncol(miss10)]))
#colnames(misstot)=c("Species","SumMiss66","SumMiss96","SumMiss10")

#miss.occur=miss
#miss.occur[miss.occur>0]=1
#miss.occur$PlotYear=miss$PlotYear
#miss66o=miss.occur[grepl("1966",miss.occur$PlotYear)==T,]
#miss96o=miss.occur[grepl("1996",miss.occur$PlotYear)==T,]
#miss10o=miss.occur[grepl("2010",miss.occur$PlotYear)==T,]

#misstot.occ=cbind(colnames(miss66o)[2:ncol(miss66o)],colSums(miss66o[,2:ncol(miss66o)]),colSums(miss96o[,2:ncol(miss96o)]),colSums(miss10o[,2:ncol(miss10o)]))
#colnames(misstot.occ)=c("Species","NumMiss66","NumMiss96","NumMiss10")

#misstot=merge(misstot, misstot.occ, by="Species") 

#write.csv(misstot,"PostprocessedData/SpeciesMissingTraitsByYear.csv")
## Check to make sure all species which make up at least 10% of any plot are included (they are)
#miss2=data.frame()
#x=1
#for (i in 1:nrow(miss)){
#  for (j in 2:ncol(miss)){
#    if (miss[i,j]>=0.10){
#       miss2[x,1]=miss$PlotYear[i]
#       miss2[x,2]=colnames(miss)[j]
#       miss2[x,3]=miss[i,j]
#       x=x+1
#       }}}
#colnames(miss2)=c("PlotYear","Species","PercAbun")

#miss1=read.xlsx("PostProcessedData/SpeciesMissingTraits.xlsx",1)
#miss3=miss2[miss2$Species %in% as.character(miss1$Species)==F,]

##### Plot x Species Matrix - Renormalized to Exclude Species w/o Trait Data
colnames(nodataperc2)=c("PlotYear", "PercNA")
nodataperc2=as.data.frame(nodataperc2)
nodataperc2$PercNA=as.numeric(as.character(nodataperc2$PercNA))
dim(nodataperc2)
dim(spabun_plot)

spabun_plot1=merge(spabun_plot, nodataperc2, by="PlotYear")
head(spabun_plot1)



# Exlude plots with more than 25% of cover from species w/no trait data   <- This is 20 plot.years out of 228
spa=spabun_plot1[spabun_plot1$PercNA<0.25,]


dim(spa)
head(spa)

## Plot data for all years?
yearcheck=cbind(spa$PlotYear, sub("[ ].*$", "", spa$PlotYear), sub("^.*[ ]","", spa$PlotYear))
colnames(yearcheck)=cbind("PlotYear","Plot","Year")
head(yearcheck)
yearcheck=as.data.frame(yearcheck)
yc1=merge(yearcheck, nodataperc2, by="PlotYear")
hist(yc1$PercNA[which(yc1$Year==1966)])

#cast(yearcheck, Plot~Year, value=1)


# Remove species with no trait data & re-normalize
spa=subset(spa, select=c("PlotYear", c(colnames(spa)[colnames(spa)%in%sp_t==T])))
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

#Excluding NAs--this is problematic!!! NAs can't just be zeros - should re-normalize species abundance to exclude NAs for each variable
trcat=tr3[,2:ncol(tr3)]
trcat=as.matrix(trcat)
head(trcat)
dim(trcat)
str(trcat)

spacat=spa2[,2:ncol(spa2)]
spacat=as.matrix(spacat)

trcat[is.na(trcat)==T]=0


catm=spacat%*%trcat
head(catm)
dim(catm)
summary(catm)

catm=as.data.frame(catm)
catm2=cbind(spa$PlotYear, catm)
colnames(catm2)[1]="PlotYear"

py=unlist(strsplit(as.character(catm2$PlotYear), " "))
py=matrix(py, ncol=2, byrow=TRUE)

catm2$Plot=py[,1]
catm2$Year=py[,2]

write.csv(catm2, "PostprocessedData/CAT_byPlotYear.csv", row.names=F)