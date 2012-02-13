#################################################################################
###  Process the LANDSAT data to develop the per-plot NDVI trajectories
###
library(multicore)

## data were downloaded from http://earthexplorer.usgs.gov/
## region limited to cape point via website search
## all daytime landsat 1-7 scenes available there were included
## downloaded using the "bulk download application"

datadir="/media/Data2/Data/LANDSAT/capepoint"

##################################################################################
###  Look at animation of 'quicklook' jpegs
### Unzip data
qlf=list.files(datadir,pattern=".zip")

mclapply(qlf,function(f) {
  system(paste("unzip -O -d ",tempdir()," ",datadir,"/",f,sep=""))
  ## copy the files to different directory
  jpgs=grep("jpg$",list.files(tempdir(),
    pattern=paste(sub(".zip","",f),".jpg",sep=""),full=T),value=T)
  system(paste("cp ",paste(jpgs,collapse=" ")," ",datadir,"/quicklook",sep=""))
})

###################################################################################
### Process the L1B images
datadir="/media/Data2/Data/LANDSAT/capepoint"


#### below this point is from IJGIS paper, need to revise for this case....


### extract the LANDSAT files from downloaded tarballs
f=list.files("1_tarballs")
f=f[grep("tar.gz",f)]
f=do.call("c",lapply(strsplit(f,split=".",fixed=T),function(x) x[[1]]))
dir.create("2_extracted") #create a folder to hold the extracted tifs
setwd("2_extracted")
mclapply(as.list(f),function(x) system(paste("tar -xvvzf ","../1_tarballs/",x,".tar.gz ",sep="")),mc.cores=7,mc.preschedule=F) #unzip them using multiple cores

### Warp them to latitude-longitude with gdalwarp
setwd("/media/Data/Work/Regional/CFR/LANDSAT/")
dir.create("3_latlon") #create a folder to hold the warped tifs
f2=list.files("2_extracted",pattern="TIF")
mclapply(as.list(f2),function(x) system(paste("gdalwarp -multi 2_extracted/",x," ","3_latlon/",x," -t_srs EPSG:4326",sep="")),mc.cores=7,mc.preschedule=F)
 
### import bands 2-6 to grass for cloud detection
f3=list.files("3_latlon",pattern="TIF")
f3=f3[grep("B20|B30|B40|B50|B61",f3)]
for(i in 1:length(f3)){
  file=strsplit(f3[i],split=".",fixed=T)[[1]][1]
  file2=substr(file,1,21)
  banda=as.numeric(substr(file,24,24))
  bandb=as.numeric(substr(file,25,25))
  band=ifelse(bandb==0,banda,paste(banda,bandb,sep=""))
##  if(!band%in%3:4) next
  system(paste("r.in.gdal 3_latlon/",f3[i]," output=",file2,".",band," --overwrite",sep=""))
  print(paste(i," out of ",length(f3)))
}

###move metadata to new folder 
dir.create("3_metadata")
metad=list.files("2_extracted",full.names=T);metad=metad[grep("MTL",metad)]
file.copy(metad,"3_metadata")

### run atmospheric correction
metad=list.files("3_metadata")
scenes=unique(substr(f3,1,21))  ## unique scenes
for(i in 1:length(scenes)){
  s=scenes[i]
  meta=metad[grep(s,metad)]
  system(paste("g.region rast=",s,".3",sep=""))
  system(paste("i.landsat.toar -7 -v method=corrected band_prefix=",s," metfile=3_metadata/",meta," --o",sep=""))
  print(paste(i," out of ",length(scenes)))
}

### get cloud masks
for(i in 1:length(scenes)){
  s=scenes[i]
  system(paste("g.region rast=",s,".3",sep=""))
  system(paste("i.landsat.acca -2s band_prefix=",s,".toar output=",s,".acca --o",sep=""))
  print(paste(i," out of ",length(scenes)))
}


### run topographic correction for bands 3 & 4
for(i in 1:length(scenes)){
  s=scenes[i]
  meta=metad[grep(s,metad)]
  tmeta=scan(paste("3_metadata/",meta,sep=""),what="char");tmeta=tmeta[tmeta!=""] #get metadata
  azi=as.numeric(tmeta[grep("AZI",tmeta)+2]) #extract azimuth
  zen=90-as.numeric(tmeta[grep("SUN_ELEVATION",tmeta)+2]) #extract zenith
  system(paste("g.region rast=",s,".3",sep=""))
  system(paste("i.topo.corr -i base=dem@dem zenith=",zen," azimuth=",azi," out=cosi method=c-factor --o --v",sep=""))
  system(paste("i.topo.corr base=cosi input=",s,".toar.3,",s,".toar.4 output=tcor zenith=",zen," method=c-factor --v --o",sep=""))
  ##  Set strange values to null
     system(paste("r.mapcalc \"tcor.",s,".toar.3=if(tcor.",s,".toar.3<0,null(),tcor.",s,".toar.3)\"",sep=""))
     system(paste("r.mapcalc \"tcor.",s,".toar.3=if(tcor.",s,".toar.3>1,null(),tcor.",s,".toar.3)\"",sep=""))
     system(paste("r.mapcalc \"tcor.",s,".toar.4=if(tcor.",s,".toar.4<0,null(),tcor.",s,".toar.4)\"",sep=""))
     system(paste("r.mapcalc \"tcor.",s,".toar.4=if(tcor.",s,".toar.4>1,null(),tcor.",s,".toar.4)\"",sep=""))
  system(paste("r.colors map=tcor.",s,".toar.3 color=grey1.0",sep=""))
  system(paste("r.colors map=tcor.",s,".toar.4 color=grey1.0",sep=""))
  print(paste(i," out of ",length(scenes)))
}

#calculate NDVI
for(i in 1:length(scenes)){
  s=scenes[i]
  system(paste("g.region rast=",s,".3",sep=""))
  system(paste("r.mapcalc \"ndvi.tcor.toar_",s,"=1.0*(tcor.",s,".toar.4-tcor.",s,".toar.3)/(tcor.",s,".toar.4+tcor.",s,".toar.3)\"",sep=""))
  system(paste("r.mapcalc \"ndvi.toar_",s,"=1.0*(",s,".toar.4-",s,".toar.3)/(",s,".toar.4+",s,".toar.3)\"",sep=""))
  system(paste("r.colors map=ndvi.tcor.toar_",s," color=ndvi",sep=""))
  system(paste("r.colors map=ndvi.toar_",s," color=ndvi",sep=""))
  print(paste(i," out of ",length(scenes)))
}


system(paste("r.mapcalc \"ndvi.diff_",s,"=ndvi.tcor.toar_",s,"-ndvi.toar_",s,"\"",sep=""))
system(paste("r.colors -e map=ndvi.diff_",s," color=grey1.0",sep=""))


tcor=readRAST6(paste("ndvi.tcor.toar_",s,sep=""),plugin=T)
toar=readRAST6(paste("ndvi.toar_",s,sep=""),plugin=T)
cloud=readRAST6(paste(s,".acca",sep=""),plugin=T)

subs=is.na(cloud@data[,])
png("~/Desktop/topo.png",width=1000,height=800)
plot(tcor@data[subs,]~toar@data[subs,],ylab="NDVI (Topologically Corrected)",xlab="NDVI (Not Topologically Corrected)",pch=16,cex=1,col=rgb(0,0,0,alpha=.1),las=1,cex.axis=1.5,cex.lab=1.5,main="Comparison of NDVI calculated from topologically corrected and uncorrected reflectance data",sub=paste("LANDSAT image:",s),ylim=c(-.5,1),xlim=c(-.5,1))
l=lm(tcor@data[s,]~toar@data[s,])
text(-.3,.8,bquote(paste(r^2,"=",.(round(summary(l)$r.squared,2)),",   RMSE=",.(round(sqrt(mean(l$residuals^2)),2)))),cex=1.5)
abline(0,1,col="red")
dev.off()
