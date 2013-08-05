#################################################################################################
############## This script is for downloading available Cordex files ############################
############## Note that the permissions for the file servers are prone to change, ##############
############## so this method may no longer work until the Cordex project is further along ######
#################################################################################################


### Download and process projected climate
library(reshape)

## Build list of available models

# main url (defunct)
#url="http://cordexesg.dmi.dk/thredds/fileServer/cordex/AFR-44/" 
# new url (as of 2013/01/05, the permissions at this url do not allow downloading): 
url="http://cordexesg.dmi.dk/thredds/esgcet/catalog.html"

##
name=read.table("RCMModelName.full.txt", fill=T)
name=name[name$V3=="confirmed",]

## Domain names (for each model)
#dom=c("CCCma","CLMcom","CLMcom","CLMcom","CLMcom","KNMI")
dom=name$V2
## GCM Model names
ms=c("CCCma-CanESM2","CNRM-CM5","EC-EARTH","HadGEM2-ES","MPI-ESM-LR","MetEir-ECEARTH")
## RCM Model Names
#rs=c("CCCma-CanRCM4","CCCLM","CCCLM","CCCLM","CCCLM","KNMI-RACMO22TT")
rs=name$V1
#domain/model table
dm=data.frame(cbind(dom,ms,rs))
colnames(dm)=c("domain","GCM","RCM")

## scenarios available
ss=c("historical","rcp45","rcp85")
## Variables available
vars=c("pr","tasmax","tasmin")

## Possible CMIP5 Ensemble values
es=c("r1i1p1","r2i1p1")
## Possible version IDs
vers=c("r2","4-8-17")
## Possible date ranges
dates=c("195101-195512.nc","195101-196012.nc","196101-197012.nc","197101-198012.nc","198101-199012.nc",
        "199101-200012.nc","200101-200512.nc",
        # monthly dates (above) and daily dates (below)
        "20410101-20451231.nc","20460101-20501231.nc","20510101-20551231.nc","20560101-20601231.nc",
        "20810101-20851231.nc","20860101-20901231.nc","20910101-20951231.nc","20960101-21001231.nc")

## build table of models/scenarios to get
g=expand.grid(scenario=ss,GCM=dm$GCM,vars=vars,ensemble=es,vers=vers,dates=dates)
g=merge(dm,g,all=T)

# add timeframe to get: monthly climatologies for historical, daily for future
g$time="day"
g$time[g$scenario=="historical"]="mon"

#get rid of impossible date/timeframe combinations
g$dates=as.character(g$dates)
g=g[c(which(nchar(g$dates)==16 & g$time=="mon"), which(nchar(g$dates)==20 & g$time=="day")),]

# create path & file names
g$path=paste(g$domain,g$GCM,g$scenario,g$ensemble,g$RCM,g$vers,g$time,g$vars,sep="/")
g$file=paste(g$vars,"AFR-44",g$GCM,g$scenario,g$ensemble,g$RCM,g$vers,g$time,g$dates,sep="_")
g$url=paste(g$path, g$file, sep="/")
g$url=paste(url,g$url,sep="")



#### Download files
for (i in 1:nrow(g)){
  try(download.file(g$url[i], destfile=paste('/media/Data/Work/Regional/CFR/AR5Climate/nc_files/',
                                             i,"_",g$RCM[i],"_",g$scenario[i],"_",g$vars[i],"_",
                                             g$dates[i], sep="")), 
      silent=T)
}







########################## Junk

##### fix error in naming files
#ncf=list.files("nc_files", full.names=T)

#file.rename(from=ncf[which(grepl("historical_pr",ncf)==T)], to=paste("nc_files1/CCma-CanRCM4_historical_pr_",dates[1:6],sep="") )
#file.rename(from=ncf[which(grepl("historical_tasmax",ncf)==T)], to=paste("nc_files1/CCma-CanRCM4_historical_tasmax_",dates[1:6],sep="") )
#file.rename(from=ncf[which(grepl("historical_tasmin",ncf)==T)], to=paste("nc_files1/CCma-CanRCM4_historical_tasmin_",dates[1:6],sep="") )

#######  ^^^^^^^^^^^^ check dates above (dates[7] is not included)

#file.rename(from=ncf[which(grepl("rcp45_pr",ncf)==T)], to=paste("nc_files1/CCma-CanRCM4_rcp45_pr_",dates[8:15],sep="") )
#file.rename(from=ncf[which(grepl("rcp45_tasmax",ncf)==T)], to=paste("nc_files1/CCma-CanRCM4_rcp45_tasmax_",dates[8:15],sep="") )
#file.rename(from=ncf[which(grepl("rcp45_tasmin",ncf)==T)], to=paste("nc_files1/CCma-CanRCM4_rcp45_tasmin_",dates[8:15],sep="") )

#file.rename(from=ncf[which(grepl("rcp85_pr",ncf)==T)], to=paste("nc_files1/CCma-CanRCM4_rcp85_pr_",dates[8:15],sep="") )
#file.rename(from=ncf[which(grepl("rcp85_tasmax",ncf)==T)], to=paste("nc_files1/CCma-CanRCM4_rcp85_tasmax_",dates[8:15],sep="") )
#file.rename(from=ncf[which(grepl("rcp85_tasmin",ncf)==T)], to=paste("nc_files1/CCma-CanRCM4_rcp85_tasmin_",dates[8:15],sep="") )

#### try again to fix dates in naming files
# get min and max data range
ncf=list.files("nc_files1", full.names=T)
ncf=data.frame(file=ncf, mindate=NA, maxdate=NA)
ncf$varname=sub("^.+(pr|tasmin|tasmax).+$","\\1",ncf$file, perl=T)

for(i in 1:nrow(ncf)){
  ncf1=brick(as.character(ncf$file[i]), varname=ncf$varname[i])
  ncf$mindate[i]=ncf1@zvalue[1]
  ncf$maxdate[i]=ncf1@zvalue[length(ncf1@zvalue)]
}

# add 49 to each year
ncf$minyear=as.numeric(sub("[-].*$", "", ncf$mindate)) + 1949
ncf$maxyear=as.numeric(sub("[-].*$", "", ncf$maxdate)) + 1949

# new file name
ncf$newfile=sub("(19|20).*$", "", ncf$file)
ncf$newfile=paste(ncf$newfile,ncf$minyear,"-",ncf$maxyear,".nc", sep="")

# rename files
file.rename(from=as.character(ncf$file), to=ncf$newfile)
