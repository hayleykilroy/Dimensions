##################################################################################
### create rectangular shapefiles for each plot

## read in table of coordinates
p=read.csv("/media/Data/Work/Regional/CFR/CapePoint/PlotLocations/latlong_Turner2010.csv")
## convert to spatialpoints and add projection
coordinates(p)=c("Longitude","Latitude")
proj4string(p)=CRS("+proj=longlat +datum=WGS84")

## convert to UTM for accurate accounting in meters
p=spTransform(p,CRS("+init=epsg:32634"))
p@data[,c("x","y")]=coordinates(p)

plotx=5  #m per plot side
ploty=10

## Create the polygons
pp=list()
for(i in 1:nrow(p)) {
pp[[i]]=Polygon(
           cbind(c(p$x[i],p$x[i],p$x[i]+plotx,p$x[i]+plotx,p$x[i]),
           c(p$y[i],p$y[i]+ploty,p$y[i]+ploty,p$y[i],p$y[i])),hole=F)
}
pp2=list()
for(i in 1:nrow(p)) pp2[[i]]=Polygons(list(pp[[i]]),as.numeric(i))
pp2=SpatialPolygons(pp2,proj4string=CRS("+init=epsg:32634"))
pp2=spTransform(pp2,CRS("+proj=longlat +datum=WGS84"))


## Generate Data table from original points
pp2=SpatialPolygonsDataFrame(pp2,data=p@data)

## write out a shapefile
writeOGR(pp2,"/media/Data/Work/Regional/CFR/CapePoint/PlotLocations/plotpolys.shp","plotpolys",driver="ESRI Shapefile")


