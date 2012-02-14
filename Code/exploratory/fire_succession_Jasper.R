###Get fire histories and play with successional models
#Get fire histories from fire layers

###Get libraries
library(rgdal)
library(adehabitat)

###Get plot locations
loc1<-read.csv("D:\\SAEON\\Projects\\Cape Point\\Taylor plots\\Data\\latlong_Turner2010.csv")
#loc2<-read.csv("D:\\SAEON\\Projects\\Cape Point\\Taylor plots\\Data\\latlong_vHLD2012.csv")

###Get fire layers
fire1<-readOGR(dsn="D:\\GIS\\Cape Peninsula\\Fire\\CapePenFires",layer="CapePenFires")    #all vegmap
#fire2<-readOGR(dsn="D:\\GIS\\Cape Peninsula\\Fire\\WGS84_csir2010",layer="CapePenFires")    #all vegmap

###Playing with projections
#fire<-spTransform(fire2, CRS("+proj=utm +south +zone=34"))
#"+proj=utm +south +zone=34"
#"+proj=utm +ellps=WGS84 +datum=WGS84 +south +zone=34"

###Intersect points and layer
S1<-SpatialPoints(loc1[,c(3,2)], proj4string=CRS(proj4string(fire2)))
#y<-overlay(fire1[1],S)
fhist<-over(S1, fire1, returnList = TRUE)

###Draw out veg age at time of each survey
age<-function(x,clnm, yr) #x=dataframe, clnm=column name, yr=year
{
  tag<-which(names(x)==clnm)
  if(dim(x)[1]<1) paste("unburnt") else 
  if(length(which(x[tag]<yr))<1) paste("unburnt") else
    return(min(yr-x[tag][which(x[tag]<yr),]))
}

#tno<-sapply(fhist,FUN="dim")[1,]

firehist<-cbind(loc1[,1],sapply(fhist, FUN=age, clnm="YEAR", yr=1966),sapply(fhist, FUN=age, clnm="YEAR", yr=1996),sapply(fhist, FUN=age, clnm="YEAR", yr=2010))
colnames(firehist)<-c("Plot","1966","1996","2010")
