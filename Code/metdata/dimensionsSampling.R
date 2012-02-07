## extract interpolated weather for all sampling locations
library(sp);library(ncdf4);library(raster);library(reshape)

setwd("/media/Data/Work/Regional/CFR/dimensions")

### read in points
p=read.csv("dimensionsSampling.csv")
coordinates(p)=c('lon','lat')
p$nid=paste(p$fam,p$id,sep="_")

spplot(p,zcol="fam")

vars=c("tmax_mean","tmax_sd","tmin_mean","tmin_sd","ppt_mean","ppt_sd")

### Connect to interpolated data
tmax_mean_nc=brick("/media/Data/Work/Regional/CFR/Weather/ClimateInterpolation/output/v1.3b/weather_fine.nc",varname="tmax_mean")
tmax_sd_nc=brick("/media/Data/Work/Regional/CFR/Weather/ClimateInterpolation/output/v1.3b/weather_fine.nc",varname="tmax_sd")
tmin_mean_nc=brick("/media/Data/Work/Regional/CFR/Weather/ClimateInterpolation/output/v1.3b/weather_fine.nc",varname="tmin_mean")
tmin_sd_nc=brick("/media/Data/Work/Regional/CFR/Weather/ClimateInterpolation/output/v1.3b/weather_fine.nc",varname="tmin_sd")
ppt_mean_nc=brick("/media/Data/Work/Regional/CFR/Weather/ClimateInterpolation/output/v1.3b/weather_fine.nc",varname="ppt_mean")
ppt_sd_nc=brick("/media/Data/Work/Regional/CFR/Weather/ClimateInterpolation/output/v1.3b/weather_fine.nc",varname="ppt_sd")

## crop points to only include ones in CFR proper
CFR=polygonFromExtent(tmax_mean_nc, sp=TRUE)
p$cfr=!is.na(overlay(p,CFR))

## Confirm location of points
plot(subset(ppt_mean_nc,subset=1))
points(p)
click(subset(ppt_mean_nc,subset=1))  #click on map to get values
## drop additional locations outside prediction region
click(p,3)  #click on map to get values
p$cfr[coordinates(p)[,1]>22&coordinates(p)[,2]>(-33)]=F
p$cfr[coordinates(p)[,1]>25.5]=F

p=p[p$cfr,]


### run the extractions
tmax_mean=extract(tmax_mean_nc,p)
tmax_sd=extract(tmax_sd_nc,p)
tmin_mean=extract(tmin_mean_nc,p)
tmin_sd=extract(tmin_sd_nc,p)
ppt_mean=extract(ppt_mean_nc,p)
ppt_sd=extract(ppt_sd_nc,p)

## update colnames as dates
lapply(vars,function(v){
  assign("temp",get(v))
  dimnames(temp)=list(nid=p$nid,date=get(paste(v,"_nc",sep=""))@layernames)
  assign(v,temp,pos=".GlobalEnv")
  return(NULL)
})


### save it
save(tmax_mean,tmax_sd,tmin_mean,tmin_sd,ppt_mean,ppt_sd,file="metdata.Rdata")

### reshape to long format
d=rbind.data.frame(
  data.frame(var="tmax",mom="mean",melt.array(tmax_mean)),
  data.frame(var="tmax",mom="sd",melt.array(tmax_sd)),
  data.frame(var="tmin",mom="mean",melt.array(tmin_mean)),
  data.frame(var="tmin",mom="sd",melt.array(tmin_sd)),
  data.frame(var="ppt",mom="mean",melt.array(ppt_mean)),
  data.frame(var="ppt",mom="sd",melt.array(ppt_sd)))
d$date=as.Date(d$date) #convert date to date

# set negative SDs introduced by the bicubic interpolation after the climate-aided step to 0
d$value[d$mom=="sd"&d$value<0]=0

### Save it
write.csv(d,"MetDataLong.csv")
save(d,file="MetDataLong.Rdata")


## a few plots
library(lattice)
      
       
s=d$var=="tmax"&d$date<as.Date("2001-01-01")&d$nid%in%sample(unique(d$nid),10)
xyplot(value~date|nid,data=d[s,],panel=function(x,y,subscripts){
  dt=d[s,][subscripts,]
  panel.arrows(dt$date[dt$mom=="mean"],dt$value[dt$mom=="mean"]-dt$value[dt$mom=="sd"],dt$date[dt$mom=="mean"],dt$value[dt$mom=="mean"]+dt$value[dt$mom=="sd"],col="grey",length=0)
  panel.xyplot(dt$date[dt$mom=="mean"],dt$value[dt$mom=="mean"],type="l",col="black")
},subscripts=T,layout=c(1,10),scales=list(y=list(log=F)))

s=d$var=="ppt"&d$date<as.Date("2001-01-01")&d$nid%in%sample(unique(d$nid),10)
xyplot(value~date|nid,data=d[s,],panel=function(x,y,subscripts){
  dt=d[s,][subscripts,]
  panel.xyplot(dt$date[dt$mom=="mean"],dt$value[dt$mom=="mean"],type="h",col="black")
},subscripts=T,layout=c(1,10),scales=list(y=list(log=F)))


s=d$var=="ppt"
bwplot(value~1|nid+mom,data=d[s,],horizontal=F)#


## there are a handfull of unrealistically large values in the very dry areas
table(d$value[d$mom=="mean"&d$var=="ppt"]>500)
p$largeppt=p$nid%in%unique(d$nid[d$mom=="mean"&d$var=="ppt"&d$value>500])

table(d$value[d$mom=="mean"&d$var=="ppt"&d$value>500],format(d$date[d$mom=="mean"&d$var=="ppt"&d$value>500],"%m"))
spplot(p,zcol="largeppt")

