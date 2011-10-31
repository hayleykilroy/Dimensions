setwd("C:/Work/Cape Point Data/2010survey/FinalData")
library(reshape)
library(lattice)

##Preliminary

tr=read.csv("Traits/SpeciesTraits_ErrorFlagsExcluded.csv") #Once traits.r is edited to better exclude errors, then use new file here
pl=read.csv("Veg/ReleveQuadrat2010.csv")

#pl=read.csv("Veg/ReleveQuadrat2010.csv", stringsAsFactors=F)
#str(pl)
#pl$H_N=as.integer(pl$H_N)
#head(pl)
#str(pl)
#write.csv(pl, "Veg/ReleveQuadrat2010.csv", row.names=F)

head(tr)
head(pl)

##############Dealing with Synonyms###############
syn=read.csv("C:/Work/Cape Point Data/Synonyms/CapePointSynonyms.csv")

pl$SpeciesID=paste(pl$Genus, pl$Species)
tr$SpeciesID=paste(tr$Genus, tr$Species)
syn$SpeciesID=paste(syn$OldGenus, syn$OldSpecies)

head(syn)
syn1=subset(syn, select=c("SpeciesID", "NewGenus", "NewSpecies"))
head(syn1)

tr1=merge(tr, syn1, by="SpeciesID", all.x=T, all.y=F)
head(tr1)
summary(tr1)

###correct NAs
tr1[is.na(tr1$NewGenus)==T,]

##Check if missing species is in releve data
#pl[grepl("Plot70", pl$SpeciesID)==T,]
#pl[pl$SpeciesID=="Pentaschistis macrocalycina",]

tr1$NewGenus[tr1$SpeciesID=="Cynodon dactylon"]="Cynodon"
tr1$NewSpecies[tr1$SpeciesID=="Cynodon dactylon"]="dactylon"
tr1$NewGenus[tr1$SpeciesID=="Pentaschistis "]="Pentaschistis"
tr1$NewSpecies[tr1$SpeciesID=="Pentaschistas "]=""
tr1$NewGenus[tr1$SpeciesID=="Stilbe ericoides"]="Stilbe"
tr1$NewSpecies[tr1$SpeciesID=="Stilbe ericoides"]="ericoides"
tr1$NewGenus[tr1$SpeciesID=="Syncarpha speciosissimum"]="Syncarpha"
tr1$NewSpecies[tr1$SpeciesID=="Syncarpha speciosissimum"]="speciosissima"
tr1$NewGenus[tr1$SpeciesID=="Thesium nigromontanum"]="Thesium"
tr1$NewSpecies=factor(tr1$NewSpecies, levels=c(levels(tr1$NewSpecies), "nigromontanum"))
tr1$NewSpecies[tr1$SpeciesID=="Thesium nigromontanum"]="nigromontanum"


### correct missing values
tr1[tr1$NewGenus=="",]

tr1$NewGenus=as.character(tr1$NewGenus)
tr1$NewSpecies=as.character(tr1$NewSpecies)

tr1$NewGenus[tr1$NewGenus==""]=as.character(tr1$Genus[tr1$NewGenus==""])
tr1$NewSpecies[tr1$NewGenus==""]=as.character(tr1$Species[tr1$NewGenus==""])

tr1$NewGenus=as.factor(tr1$NewGenus)
tr1$NewSpecies=as.factor(tr1$NewSpecies)

head(tr1)
summary(tr1)
str(tr1)

##
tr1$NewSpeciesID=paste(tr1$NewGenus, tr1$NewSpecies)



##Plot Synonymy
head(pl)
pl1=merge(pl, syn1, by="SpeciesID", all.x=T, all.y=F)
head(pl1)
summary(pl1)

pl1$NewSpeciesID=paste(pl1$NewGenus, pl1$NewSpecies)
summary(pl1)
pl1$NewSpeciesID=as.factor(pl1$NewSpeciesID)
summary(pl1$NewSpeciesID)
pl1$NewSpeciesID=as.character(pl1$NewSpeciesID)
pl1$NewSpeciesID[pl1$NewSpeciesID=="NA NA"]=pl1$SpeciesID[pl1$NewSpeciesID=="NA NA"]

##Fixing Details
tr1$NewSpeciesID=sub("  ", " ", tr1$NewSpeciesID)
pl1$NewSpeciesID=sub("  ", " ", pl1$NewSpeciesID)
tr1$NewSpeciesID[tr1$NewSpeciesID=="Pentaschistis NA"]="Pentaschistis "
tr1$NewSpeciesID[tr1$NewSpeciesID=="Chondropetalum nudum"]="Elegia nuda"
tr1$NewSpeciesID[tr1$NewSpeciesID=="NA NA"]=tr1$SpeciesID[tr1$NewSpeciesID=="NA NA"]
pl1[pl1$NewSpeciesID==" ",]
pl1$NewSpeciesID[pl1$NewSpeciesID==" "]="Gnidia juniperina"
pl1$NewSpeciesID[pl1$NewSpeciesID==" Erica muscosa"]="Erica muscosa"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Asparagus (Protasparagus) rubicundus"]="Asparagus rubicundus"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Capelio (Alciope) tabularis"]="Capelio tabularis"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Anaxaeton laeve"]="Anaxeton laeve"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Authospermiun galioides"]="Anthospermum galioides"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Authospermum galioides"]="Anthospermum galioides"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Callumia setosa"]="Cullumia setosa"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Campianthus falsiformis"]="Lampranthus falciformis"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Corumbium glabrum"]="Corymbium glabrum"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Crassula sp. minute imbricate leaves"]="Crassula "
pl1$NewSpeciesID[pl1$NewSpeciesID=="Disparago aricoides"]="Disparago ericoides"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Disparago ericoides (lasiocarpa)"]="Disparago ericoides"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Elegia (Chondropetalum) microcarpum"]="Elegia microcarpa"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Elegia (Chondropetalum) nuda"]="Elegia nuda"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Elegia nudug"]="Elegia nuda"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Chondropetalum nudum"]="Elegia nuda"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Erharta ramosa"]="Ehrharta ramosa"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Erica abietina ssp. atrorosea"]="Erica abietina"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Erica allabella"]="Erica glabella"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Erica galabella"]="Erica glabella"
pl1$NewSpeciesID[pl1$NewSpeciesID=="erica hirtiflora"]="Erica hirtiflora"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Erica paliiflora"]="Erica palliflora"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Finia rigida"]="Ficinia rigida"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Ischyrolepis palludosa"]="Ischyrolepis paludosa"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Lachnaea (Cryptadenia) grandiflora"]="Lachnaea grandiflora"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Lachnaea densifolia"]="Lachnaea densiflora"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Manulea cheilanthes"]="Manulea cheiranthes"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Microdon dubia"]="Microdon dubius"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Microndon dubius"]="Microdon dubius"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Microdon (Agathelpis) dubius"]="Microdon dubius"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Muraltia sp. Prev. Lapa"]="Muraltia "
pl1$NewSpeciesID[pl1$NewSpeciesID=="Passerina corymbos (vulgaris)"]="Passerina corymbosa"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Passerina corymbosa (vulgaris)"]="Passerina corymbosa"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Ploygala bracteolata"]="Polygala bracteolata"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Pseudognaphalium spuria"]="Pseudoselago spuria"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Restio (Calopsis) ramosissimus (gracilis)"]="Calopsis gracilis"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Restio (Ischyrolepis) capensis"]="Ischyrolepis capensis"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Restio (Ischyrolepis) cincinnata"]="Ischyrolepis cincinnata"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Restio (Ischyrolepis) cincinnatus"]="Ischyrolepis cincinnata"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Restio capensis"]="Ischyrolepis capensis"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Rhus laevigata"]="Searsia chirindensis"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Robsonodendron maritinum"]="Robsonodendron maritimum"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Safgria odorum"]="Satyrium odorum"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Shizaea pectinata"]="Schizaea pectinata"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Stylapteris fruticulosus"]="Stylapterus fruticulosus"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Tetraria pleiosticha"]="Tetraria fasciata"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Trichocephalus (Phylica) stipularis"]="Trichocephalus stipularis"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Trichogyne (Ifloga) ambigua"]="Trichogyne ambigua"
pl1$NewSpeciesID[pl1$NewSpeciesID=="Willdnowia glomerata"]="Willdenowia glomerata"

head(tr1)
head(pl1)


##Checking
sp_p=unique(pl1$NewSpeciesID)
sp_p
sp_t=unique(tr1$NewSpeciesID)
sp_t

allsp=matrix(nrow=length(unique(c(sp_p, sp_t))), ncol=3)
colnames(allsp)=c("Species", "Plots", "Traits")
allsp=as.data.frame(allsp)
allsp$Species=unique(c(sp_p, sp_t))
allsp$Plots=allsp$Species%in%sp_p
allsp$Traits=allsp$Species%in%sp_t
head(allsp)
summary(allsp)

allsp_p=allsp[allsp$Plots==T,]

noplot=allsp$Species[allsp$Plots==F]
notrait=allsp$Species[allsp$Traits==F]

write.csv(noplot, "noplot.csv")
write.csv(notrait, "notrait.csv")

tr1$NewSpeciesID[grepl("Erica", tr1$NewSpeciesID)==T]
tr1$NewSpeciesID[grepl("eucomoides", tr1$NewSpeciesID)==T]

tr1$NewSpeciesID[tr1$NewSpeciesID=="Paraspalathus callosa"]

#Checking old surveys
pall=read.csv("Veg/ReveleAll.csv")
head(pall)
pall$SpeciesID=paste(pall$Genus, pall$Species)

pall=merge(pall, syn1, by="SpeciesID", all.x=T, all.y=F)
head(pall)
summary(pall)

pall$NewSpeciesID=paste(pall$NewGenus, pall$NewSpecies)
summary(pall)
pall$NewSpeciesID=as.factor(pall$NewSpeciesID)
summary(pall$NewSpeciesID)
pall$NewSpeciesID=as.character(pall$NewSpeciesID)
pall$NewSpeciesID[pall$NewSpeciesID=="NA NA"]=pall$SpeciesID[pall$NewSpeciesID=="NA NA"]


sp_p=unique(pall$NewSpeciesID)
sp_p
sp_t=unique(tr1$NewSpeciesID)
sp_t

allsp=matrix(nrow=length(unique(c(sp_p, sp_t))), ncol=3)
colnames(allsp)=c("Species", "Plots", "Traits")
allsp=as.data.frame(allsp)
allsp$Species=unique(c(sp_p, sp_t))
allsp$Plots=allsp$Species%in%sp_p
allsp$Traits=allsp$Species%in%sp_t
head(allsp)
summary(allsp)

#allsp_p=allsp[allsp$Plots==T,]

noplot=allsp$Species[allsp$Plots==F]
#notrait=allsp$Species[allsp$Traits==F]

write.csv(noplot, "noplot.csv")
#write.csv(notrait, "notrait.csv")

tr1$NewSpeciesID[grepl("Restio", tr1$NewSpeciesID)==T]
unique(pall$NewSpeciesID[grepl("Gnidia", pall$NewSpeciesID)==T])

##Write corrected files
write.csv(tr1, "Traits/SpeciesTraits_namescorrected_errorsexcluded.csv", row.names=F)
write.csv(pl1, "Veg/ReleveQuadrat2010_namescorrected.csv", row.names=F)


#############################################
tr1=read.csv("Traits/SpeciesTraits_namescorrected_errorsexcluded.csv")
pl1=read.csv("Veg/ReleveQuadrat2010_namescorrected.csv")


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


#for (i in 1:length(unique(pl1$Plot))){
#spabun_pn=cbind(pl1[,c("Plot","Genus","Species")],comagabund=pl1$TotCov/plottotcov1[plottotcov1[,1]==(unique(pl1$Plot)[i]),2]) 
#}
#head(spabun_pn)

#spabun_pn$SpeciesID=paste(spabun_pn$Genus, spabun_pn$Species)

spabun_plot=cast(spabun_pn, Plot~NewSpeciesID, value="WghtedCov")
head(spabun_plot)
summary(spabun_plot)

#max(spabun_plot)  #should be 1
#ind=which(spabun_plot==4, arr.ind=T)
#spabun_plot[ind[,1], ind[,2]]
#spabun_pn[spabun_pn$NewSpeciesID=="Adenandra villosa",]
#pl1[pl1$NewSpeciesID=="Adenandra villosa",]

##by quadrat
#quadcov=matrix(nrow=length(unique(pl1$Plot)), ncol=10)
#colnames(quadcov)=grep("PercCov",colnames(pl1), value=T)
#colnames(quadcov)
#for (i in 1:nrow(quadcov)){
#for (j in 1:ncol(quadcov)){
#  quadcov[i,j]=sum(pl1[pl1$Plot==(unique(pl1$Plot)[i]), colnames(quadcov)[j]])
#  }}

#colnames(quadcov)=paste(colnames(quadcov), "Tot", sep="_")


table(rowSums(spabun_plot, na.rm=T)) #should all be 1


#### Percent of plot made up of species with no trait data
sp_p=colnames(spabun_plot[2:length(colnames(spabun_plot))])
sp_p
sp_t=as.character(tr1$NewSpeciesID)
sp_t

allsp=matrix(nrow=length(unique(c(sp_p, sp_t))), ncol=3)
colnames(allsp)=c("Species", "Plots", "Traits")
allsp=as.data.frame(allsp)
allsp$Species=unique(c(sp_p, sp_t))
allsp$Plots=allsp$Species%in%sp_p
allsp$Traits=allsp$Species%in%sp_t
head(allsp)
summary(allsp)

#noplot=allsp$Species[allsp$Plots==F]
notrait=allsp$Species[allsp$Traits==F]
notrait

nodataperc=subset(spabun_plot, select=notrait)
nodataperc1=rowSums(nodataperc, na.rm=T)
nodataperc1
summary(nodataperc1)
hist(nodataperc1)
nodataperc2=cbind(spabun_plot$Plot, nodataperc1)
nodataperc2
nodataperc2=nodataperc2[order(nodataperc2[,2]),]



nodataperc2[38,]
nodataperc2[36,]

plot49=spabun_plot[spabun_plot$Plot==49,]
plot49=subset(plot49, select=notrait)
plot49=plot49[,is.na(plot49)==F]

plot47=spabun_plot[spabun_plot$Plot==47,]
plot47=subset(plot47, select=notrait)
plot47=plot47[,is.na(plot47)==F]


##### Plot x Species Matrix - Renormalized to Exclude Species w/o Trait Data
colnames(nodataperc2)=c("Plot", "PercNA")
dim(nodataperc2)
dim(spabun_plot)

spabun_plot1=merge(spabun_plot, nodataperc2, by="Plot")
head(spabun_plot1)

# Exlude plots with more than 10% of cover of species w/no trait data
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