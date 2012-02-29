#################################################################################
###  Process the LANDSAT data to develop the per-plot NDVI trajectories
###

## start GRASS
system("grass /media/Data/Work/grassdata/CapePoint/LANDSAT")
g.mapset LANDSAT

## set region to just the peninsula (used QGIS to find bounds)
g.region -a n=-3753134 s=-3804939 e=270544 w=249726 save=peninsula

## import 10m DEM
gdalwarp -multi /media/Data/Work/Regional/CFR/CapePoint/BaseGISData/CapePointDEM/cct_10m /media/Data/Work/Regional/CFR/CapePoint/BaseGISData/CapePointDEM/10mdem.tif -t_srs EPSG:32634
r.in.gdal input=/media/Data/Work/Regional/CFR/CapePoint/BaseGISData/CapePointDEM/10mdem.tif output=dem title="Cape Point 10m DEM"
r.colors dem@LANDSAT color=srtm

## peninsula boundary
v.proj input=CFR location=CFR mapset=PERMANENT
v.to.rast input=CFR@LANDSAT type=area output=cfr use=cat

##  Get reserve boundaries
system("v.proj input=Reserves location=CFR mapset=CFR")
system("v.extract input=Reserves output=GoodHope where=\"MASTERNAME=\'CAPE OF GOOD HOPE\'\"")
system("g.region vect=GoodHope save=cgh")


## start R in GRASS
R
library(multicore);library(rgdal);library(sp)

  system(paste("g.mapset LANDSAT"))

## data were downloaded from http://earthexplorer.usgs.gov/
## region limited to cape point via website search
## all daytime landsat 1-7 scenes available there were included
## downloaded using the "bulk download application"

setwd("/media/Data2/Data/LANDSAT/capepoint")


### create ramdisk for faster processing
if(!file.exists("/tmp/ramdisk"))  system("sudo mkdir /tmp/ramdisk; sudo chmod 777 /tmp/ramdisk")
  system("sudo mount -t tmpfs -o size=20G tmpfs /tmp/ramdisk/")
  scratch="/tmp/ramdisk/"

### or just use a normal disk
#scratch=tempdir()


##################################################################################
###  Look at animation of 'quicklook' jpegs
### Unzip data
qlf=list.files("quicklook_zip",pattern=".zip",full=T)

mclapply(qlf,function(f) {
  system(paste("unzip -d ",scratch," ",f,sep=""))
  ## copy the files to different directory
  jpgs=grep("jpg$",list.files(scratch,
    pattern=paste(sub("quicklook_zip/","",sub("[.]zip","",f)),".jpg",sep=""),full=T),value=T)
  system(paste("cp ",paste(jpgs,collapse=" ")," quicklook_jpg",sep=""))
})


###################################################################################
###################################################################################
###################################################################################
### Process the L1B images

### get list of LANDSAT files from downloaded tarballs to process
files=list.files("1_tarballs",pattern="tar[.]gz")
files=files[grepl("LT5",files)]


## update those that have been completed
files2=do.call(rbind.data.frame,mclapply(files,function(f) {
  print(paste("Processing file:",f," which is ",which(f==files)," out of ",length(files)))
  g=grep("tif|TIF",system(paste("tar -tf ","1_tarballs/",f,sep=""),intern=T),value=T)
  g=g[!grepl("gap",g)][1]
  g=sub("_B.*[.]TIF","",sub("_B.*[.]tif","",g))
  return(
         data.frame(file=f,scene=g))
}
  ))

save(files2,file="scenenumbers.Rdata")

## add column indicating whether scene has been processed (and exported to tif)
load("scenenumbers.Rdata")
files2$complete=do.call(c,lapply(files2$scene,function(g) any(grepl(g,sub("[.]tif","",list.files("ndvi"))))))

which(f==files)


### loop through files
f=files[109]
f=files2$file[files2$complete][1];f


#mclapply(files,function(f){
for(f in files2$file[!files2$complete]){
print(paste("##################################################      Starting:",f,"  ###########################################"))
  ## get Scene ID
  scene=substr(sub(scratch,"",f),1,21)

  ## create new mapset for scene
  system(paste("g.mapset -c ",scene))
  system("g.region cgh@LANDSAT")
  
  ## uncompress them to scratch drive
  tdir=paste(scratch,scene,sep="")
  dir.create(tdir)
  system(paste("tar -C ",tdir," -xvzf ","1_tarballs/",f,sep=""))
  
  ## reproject to identical UTM (if any were not already there...)
  lapply(list.files(tdir,pattern=".*TIF",recur=T),function(x)
           system(paste("gdalwarp -multi ",tdir,"/",x," ",tdir,"/",x," -t_srs EPSG:32634",sep="")))

                                        #system(paste("gdalinfo ",list.files(scratch,full=T)[1]))
  
print(paste("##################################################      import bands for :",f,"  ###########################################"))
### import bands 2-6 to grass for cloud detection
f2=list.files(tdir,full=T)
f2=f2[grep("B10|B20|B30|B40|B50|B60|B61|B7",f2)]
fs=substr(sub(tdir,"",f2[1]),2,22)  #scene ID

for(i in 1:length(f2)){
  file=strsplit(f2[i],split=".",fixed=T)[[1]][1]
  banda=as.numeric(substr(sub(tdir,"",file),25,25))
  if(substr(scene,2,2)=="M")   banda=as.numeric(substr(sub(tdir,"",file),24,24))
  if(substr(scene,1,3)=="LT4")   banda=as.numeric(substr(sub(tdir,"",file),24,24))
  if(substr(scene,1,3)=="LT5")   banda=as.numeric(substr(sub(tdir,"",file),24,24))
                                        #  bandb=as.numeric(substr(sub(tdir,"",file),26,26))
  band=banda #ifelse(bandb==0,banda,paste(banda,bandb,sep=""))  #use only band 61 (not 62)
##  if(!band%in%3:4) next
  system(paste("r.in.gdal ",f2[i]," output=landsat.",band," --overwrite",sep=""))
  print(paste(i," out of ",length(f2)))
}

  ### if gap mask exists, nullify the gap data from the bands
if(file.exists(paste(tdir,"/gap_mask",sep=""))){
  f2g=list.files(paste(tdir,"/gap_mask",sep=""),full=T)
  f2g=f2g[grep("B10|B20|B30|B40|B50|B61|B7",f2g)]
  lapply(f2g,function(f2gi) system(paste("gunzip ",f2gi,sep="")))
  f2g=list.files(paste(tdir,"/gap_mask",sep=""),full=T)
  f2g=f2g[grep("B10|B20|B30|B40|B50|B61|B7",f2g)]
  for(i in 1:length(f2g)){
  file=strsplit(f2g[i],split=".",fixed=T)[[1]][1]
  banda=as.numeric(substr(sub(paste(tdir,"/gap_mask/",sep=""),"",file),24,24))
  #  bandb=as.numeric(substr(sub(tdir,"",file),26,26))
  band=banda #ifelse(bandb==0,banda,paste(banda,bandb,sep=""))  #use only band 61 (not 62)
##  if(!band%in%3:4) next
  system(paste("r.in.gdal ",f2g[i]," output=gap.landsat.",band," --overwrite",sep=""))
  system(paste("r.mapcalc \"landsat.",banda,"=if(gap.landsat.",banda,"==0,null(),landsat.",banda,")\"",sep=""))
}}
  
  
### Import metadata
  tmeta=scan(list.files(tdir,pattern="MTL",full=T),what="char");tmeta=tmeta[tmeta!=""] #get metadata
  ## get some metadata for the filenames
  adate=tmeta[grep("ACQUISITION_DATE",tmeta)+2] #extract date
  sensor=sub("[+]","",tmeta[grep("SENSOR_ID",tmeta)+2]) #extract sensor
sensorcode=ifelse(grepl("MSS",sensor),"mss1",ifelse(grepl("TM",sensor),"tm5",""))
  ### run atmospheric correction
  system(paste("i.landsat.toar --quiet -t method=dos4 input_prefix=landsat. metfile=",list.files(tdir,full=T,pattern="MTL")," output_prefix=toar_landsat. --o",sep=""))
  
  ## get cloud mask from corrected data
  system("i.landsat.acca --verbose -5xf histogram=100 b45ratio=0.5 b56composite=225 input_prefix=toar_landsat. output=acca --o")

  ## topographic correction
  azi=as.numeric(tmeta[grep("AZI",tmeta)+2]) #extract azimuth
  zen=90-as.numeric(tmeta[grep("SUN_ELEVATION",tmeta)+2]) #extract zenith 
  system(paste("i.topo.corr -i base=dem@LANDSAT zenith=",zen," azimuth=",azi," out=cosi method=c-factor --o --v",sep=""))
  system(paste("i.topo.corr base=cosi input=toar_landsat.2,toar_landsat.3,toar_landsat.4,toar_landsat.5,toar_landsat.6 output=tcor zenith=",zen," method=c-factor --v --o",sep=""))
  ##  Set strange values to null in 2:5,7
  for(b in c(2:5))   {
    system(paste("r.mapcalc \"tcor.toar_landsat.",b,"=if(tcor.toar_landsat.",b,"<0,null(),tcor.toar_landsat.",b,")\"",sep=""))
    system(paste("r.mapcalc \"tcor.toar_landsat.",b,"=if(tcor.toar_landsat.",b,">1,null(),tcor.toar_landsat.",b,")\"",sep=""))
    system(paste("r.colors -e map=tcor.toar_landsat.",b," color=grey1.0",sep=""))
  }


#calculate NDVI
  system(paste("r.mapcalc \"ndvi.tcor.toar=1.0*(tcor.toar_landsat.4-tcor.toar_landsat.3)/(tcor.toar_landsat.4+tcor.toar_landsat.3)\"",sep=""))
## mask out clouds AND non-land AND gaps
  system("r.mapcalc \"ndvi.tcor.toar=if(cfr@LANDSAT==1&isnull(acca),ndvi.tcor.toar,null())\"")
  if(file.exists(paste(tdir,"/gap_mask",sep="")))  system("r.mapcalc \"ndvi.tcor.toar=if(gap.landsat.4=0,null(),ndvi.tcor.toar)\"")

  system(paste("r.colors map=ndvi.tcor.toar color=ndvi",sep=""))
#  system(paste("r.mapcalc \"ndvi.toar_",fs,"=1.0*(",fs,"_toar.4-",fs,"_toar.3)/(",fs,"_toar.4+",fs,"_toar.3)\"",sep=""))
#  system(paste("r.colors map=ndvi.toar_",fs," color=ndvi",sep=""))

## write out the tif
  opts=paste("COMPRESS=LZW,ZLEVEL=9,TIFFTAG_DATETIME=",adate,sep="")
system(paste("r.out.gdal format=GTiff createopt=\"",opts,"\" input=ndvi.tcor.toar output=ndvi/",adate,"_",sensor,"_",fs,".tif",sep=""))


  system(paste("i.landsat.rgb  red=toar_landsat.3 green=toar_landsat.2 blue=toar_landsat.1",sep=""))
                                        # create RGB group
                                        #system(paste("i.group group=landsat.rgb in=toar_landsat.3,toar_landsat.2,toar_landsat.1",sep=""))
  system(paste("r.composite red=toar_landsat.3 green=toar_landsat.2 blue=toar_landsat.1 output=landsat_rgb --o",sep=""))
  ## write out images  for just cape of good hope
  system(paste("r.out.png -w in=landsat_rgb out=rgb/cgh_",adate,"_",fs,"_rgb",sep=""))
  system(paste("r.out.png -w in=ndvi.tcor.toar out=rgb/cgh_",adate,"_",fs,"_ndvi",sep=""))
  
### remove mapset
#system(paste("rm -rf /media/Data/Work/grassdata/CapePoint/",scene,"/",sep=""))

  ## delete the unzipped files to free up space on scratch
  file.remove(list.files(tdir,full=T,recursive=T))


 
print(paste("#################################################    Finished ",scene, "######################################################"))

}
) #close mclapply



## Write out RGB tiff
system("g.region peninsula@LANDSAT")


# export RGB images
  system(paste("r.out.png -w in=landsat_rgb out=rgb/pen_",adate,"_",fs,"_rgb",sep=""))
  system(paste("r.out.png -w in=ndvi.tcor.toar out=rgb/pen_",adate,"_",fs,"_ndvi",sep=""))


##############################################################
### stuff for display  - mostly broken probably...

## create RGB image
system(paste("i.tasscap -7 band1=",fs,".1 band2=",fs,".2 band3=",
             fs,".1 band4=",fs,".4 band5=",fs,".5 band7=",fs,".7 outprefix=dehaze --o",sep=""))

system(paste("i.landsat.dehaze band1=",fs,"_toar.1 band2=",fs,"_toar.2 band3=",
             fs,"_toar.1 band4=",fs,"_toar.4 band5=",fs,"_toar.5 band7=",fs,"_toar.7 outprefix=dehaze tasscap4=dehaze.4",sep=""))

## not working
##system(paste("i.fusion.brovey -l ms1=",fs,"_toar.2 ms2=",fs,"_toar.4 ms3=",fs,"_toar.5 pan=",fs,"_toar.7 outputprefix=brovfus_",fs,sep=""))
##system(paste("r.composite green=brovfus_",fs,".green red=brovfus_",fs,".red blue=brovfus_",fs,".blue output=rgb_",fs,sep=""))


## netcdf
##opts="format=nc4c zlevel=9 write_bottomup=yes write_lonlat=yes"
##system(paste("r.out.gdal format=netCDF type=Float32 createopt=\"",opts,"\" input=ndvi.tcor.toar_",fs," output=",fs,".nc",sep=""))



###########################  Old stuff below



system(paste("r.mapcalc \"ndvi.diff_",fs,"=ndvi.tcor.toar_",fs,"-ndvi.toar_",fs,"\"",sep=""))
system(paste("r.colors -e map=ndvi.diff_",fs," color=grey1.0",sep=""))

library(spgrass6)
tcor=readRAST6(paste("ndvi.tcor.toar_",fs,sep=""),plugin=T)
toar=readRAST6(paste("ndvi.toar_",fs,sep=""),plugin=T)
cloud=readRAST6(paste(fs,"_acca",sep=""),plugin=T)


#system(paste("r.out.gdal input=ndvi.tcor.toar_",fs," output=kml/",fs,"_NDVI.tif",sep=""))
#system(paste("gdalwarp kml/",fs,"_NDVI.tif kml/",fs,"_NDVI_geo.tif -t_srs EPSG:4326",sep=""))
#system(paste("gdal2tiles.py -k kml/",fs,"_NDVI_geo.tif kml",sep=""))


subs=is.na(cloud@data[,])
png("~/Desktop/topo.png",width=1000,height=800)
plot(tcor@data[subs,]~toar@data[subs,],ylab="NDVI (Topologically Corrected)",xlab="NDVI (Not Topologically Corrected)",pch=16,cex=1,col=rgb(0,0,0,alpha=.1),las=1,cex.axis=1.5,cex.lab=1.5,main="Comparison of NDVI calculated from topologically corrected and uncorrected reflectance data",sub=paste("LANDSAT image:",fs),ylim=c(-.5,1),xlim=c(-.5,1))
l=lm(tcor@data[s,]~toar@data[s,])
text(-.3,.8,bquote(paste(r^2,"=",.(round(summary(l)$r.squared,2)),",   RMSE=",.(round(sqrt(mean(l$residuals^2)),2)))),cex=1.5)
abline(0,1,col="red")
dev.off()
