

### extract the NDVI timeseries for each plot


library(multicore);library(rgdal);library(sp);library(raster)
library(reshape);library(lattice)

setwd("/media/Data2/Data/LANDSAT/capepoint")

## load plots
p=readOGR("/media/Data/Work/Regional/CFR/CapePoint/PlotLocations","plotpolys")
## transform to UTM
p=spTransform(p,CRS("+proj=utm +zone=34 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
### Import some plot type data
v=read.table("/media/Data/Work/Regional/CFR/CapePoint/data/vegtypes.txt",header=T)
vt=read.csv("/media/Data/Work/Regional/CFR/CapePoint/data/VegKey.csv",header=T)
p$veg=v$Veg.type[match(p$Plot,v$Plot)]
p$veg=as.factor(vt$VegName[match(p$veg,vt$VegType)])


## build the NDVI brick
ndvi=stack(list.files("/media/Data2/Data/LANDSAT/capepoint/ndvi",full=T))

#plot(ndvi)

## extract the timeseries
pndvi=rbind.data.frame(extract(ndvi,coordinates(p)))
pndvi$plot=p$Plot
pndvi$veg=as.factor(p$veg)

pndvil=melt(pndvi,id.var=c("plot","veg"))
pndvil$date=as.Date(substr(pndvil$variable,1,10))
pndvil$value[pndvil$value==1]=NA  #drop NDVI=1, can't be real
pndvil$value[pndvil$value<0]=0  #drop negative NDVI, also unrealistic

pndvil=pndvil[!is.na(pndvil$value)&!is.nan(pndvil$value),]

pdf("output/EDA.pdf",width=11,height=8.5)
densityplot(1~value|veg,group=as.factor(plot),data=pndvil)

xyplot(value~date|veg,groups=as.factor(plot),data=pndvil,type=c("p","l"),pch=16,cex=.3) #[as.numeric(format(pndvil$date,"%m"))%in%c(10,11,12,1,2),]


xyplot(value~date|as.factor(plot),data=pndvil,type=c("p","l"),pch=16,cex=.3,col="black") #[as.numeric(format(pndvil$date,"%m"))%in%c(10,11,12,1,2),]

xyplot(value~date|as.factor(plot),data=pndvil,type=c("p","l"),pch=16,cex=.3,col="black",xlim=as.Date(c("1999-01-01","2011-12-31"))) #[as.numeric(format(pndvil$date,"%m"))%in%c(10,11,12,1,2),]

dev.off()




