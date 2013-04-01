setwd("C:/Work/Dimensions/Data")

releve3=read.csv("PostprocessedData/ReleveAll.csv")
tr=read.csv("PostprocessedData/LabData.csv")

releve3$ID=paste(releve3$NewGenus, releve3$NewSpecies)
tr$ID=paste(tr$NewGenus, tr$NewSpecies)

releve.missing=releve3[which(releve3$ID %in% tr$ID==F),]
releve.missing$ID2=paste(releve.missing$ID,releve.missing$Year)
releve.missing$EstAbun[releve.missing$AbunClass==1]=2.5
releve.missing$EstAbun[releve.missing$AbunClass==2]=7.5
releve.missing$EstAbun[releve.missing$AbunClass==3]=25
releve.missing$EstAbun[releve.missing$AbunClass==4]=75
releve.missing$EstAbun[releve.missing$AbunClass==5]=100


relmis=as.data.frame(matrix(nrow=length(unique(releve.missing$ID2)),ncol=1))
relmis[,1]=unique(releve.missing$ID2)
colnames(relmis)="ID2"

for(i in 1:nrow(relmis)){
  relmis$NumPlots[i]=length(releve.missing$Plot[which(releve.missing$ID2==relmis$ID2[i])])
  relmis$MeanPercCov[i]=mean(releve.missing$MeanPercCov[which(releve.missing$ID2==relmis$ID2[i])])
  relmis$MaxPercCov[i]=max(releve.missing$MeanPercCov[which(releve.missing$ID2==relmis$ID2[i])])
  relmis$MeanRealAbun[i]=mean(releve.missing$TotalAbun[which(releve.missing$ID2==relmis$ID2[i])])
  relmis$MaxRealAbun[i]=max(releve.missing$TotalAbun[which(releve.missing$ID2==relmis$ID2[i])])
  relmis$MeanEstAbun[i]=mean(releve.missing$EstAbun[which(releve.missing$ID2==relmis$ID2[i])])
  relmis$MaxEstAbun[i]=max(releve.missing$EstAbun[which(releve.missing$ID2==relmis$ID2[i])])
}

relmis$Species=sub("[ ][0-9]*$", "", relmis$ID2, perl=T)
relmis$Year=sub("^[A-Z|a-z| |.|-]*", "", relmis$ID2, perl=T)

relmis=subset(relmis, select=c("Species","Year","NumPlots","MeanPercCov","MaxPercCov","MeanRealAbun","MaxRealAbun","MeanEstAbun","MaxEstAbun"))

write.csv(relmis, "PostprocessedData/CapePointMissing.csv", row.names=F)

