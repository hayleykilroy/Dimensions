setwd("/media/Data/Work/Regional/CFR/AR5Climate")
files=list.files("nc_files1", full.names=T)

library(raster)

# test reading nc file
test=brick(files[1])
plot(test[[1]])

# stacks
# precipitation
pr.stack=stack(files[which(grepl("_pr_",files)==T)], quick=T)  # <- does not work

test=brick(files[which(grepl("_pr_",files)==T)][[11]], varname="pr")  # <- works


f=data.frame(file=files)

f[,c("model","scenario","variable","time")]=
  do.call(rbind.data.frame,strsplit(sub("^.*[/]","",f$file),"_"))
f[,c("starttime","stoptime")]=do.call(rbind.data.frame,strsplit(sub("[.]nc","",f$time),"-"))
#f$starttime=as.Date(as.character(f$starttime),"%Y %m")

### model to process
scenario="rcp85"
model="CCCma-CanRCM4"

#### check files and remove "[1]"!!
#historical files
rcm_mon_hist_tasmax=paste(f$file[which(f$scenario=="historical" & f$variable=="tasmax")][1],collapse=" ")
rcm_mon_hist_tasmin=paste(f$file[which(f$scenario=="historical" & f$variable=="tasmin")][1],collapse=" ")
rcm_mon_hist_pr=paste(f$file[which(f$scenario=="historical" & f$variable=="pr")][1],collapse=" ")
### Future daily
rcm_day_future_tasmax=paste(f$file[which(f$scenario==scenario & f$variable=="tasmax")][1],collapse=" ")
rcm_day_future_tasmin=paste(f$file[which(f$scenario==scenario & f$variable=="tasmin")][1],collapse=" ")
rcm_day_future_pr=paste(f$file[which(f$scenario==scenario & f$variable=="pr")][1],collapse=" ")


## output files
rcm_mon_hist=paste("output/",model,"_",scenario,"_monthlypast.nc",sep="")
rcm_clim_hist=paste("output/",model,"_",scenario,"_monthlypastclimate.nc",sep="")

### Create monthly historical climatologies                     ###### <- ERROR HERE: when "[1]"s are removed above, variables with same names are treated as different variables
system(paste("cdo -O -merge -mergetime ",rcm_mon_hist_tasmin,
             " -mergetime ",rcm_mon_hist_tasmax,
             " -mergetime ",rcm_mon_hist_pr," ",
             rcm_mon_hist,sep=""))  
system(paste("cdo -O ymonmean ",rcm_mon_hist," ",rcm_clim_hist,sep=""))  
file.remove(rcm_mon_hist)

### Create daily future timeseries of anomalies
rcm_day_future=paste("output/",model,"_",scenario,"_futuredaily.nc",sep="")
rcm_day_futureanom=paste("output/",model,"_",scenario,"_futuredailyanomalies1.nc",sep="")
rcm_day_futureanom2=paste("output/",model,"_",scenario,"_futuredailyanomalies.nc",sep="")

system(paste("cdo -O -merge -mergetime ",rcm_day_future_tasmin, ###### <- ERROR HERE: when "[1]"s are removed above, variables with same names are treated as different variables
             " -mergetime ",rcm_day_future_tasmax,
             " -mergetime ",rcm_day_future_pr," ",
             rcm_day_future,sep=""))  
system(paste("cdo -O ymonsub ",rcm_day_future," ",rcm_clim_hist," ",rcm_day_futureanom,sep=""))  

## convert units
system(paste("cdo -O merge",
            " -chunit,XXXXXXXXXXX -mulc,86400 -selname,pr ",rcm_day_futureanom,
            " -selname,tasmax,tasmin ",rcm_day_futureanom,
#            " -chunit,K,C -addc,273.15 -selname,tasmax ",rcm_day_futureanom,
#            " -chunit,K,C -addc,273.15 -selname,tasmin ",rcm_day_futureanom,
             " ",rcm_day_futureanom2,sep=""))  

file.remove(rcm_day_future,rcm_day_futureanom)

## add future daily anomalies to past monthly high resolution climates
## need: netcdf file with mean monthly tasmax (K), tasmin (K), pr (kg/m2/s) 
##path to schulze|wilson monthly climatologies as netcdf"
obs_day_hist="/media/Data/Work/Regional/CFR/Weather/ClimateInterpolation/output/v1.3b/weather_fine.nc"
obs_clim_hist="output/obs_clim_hist.nc"
## if using Wilson's daily data
## make 'observed' monthly historical climatologies 
system(paste("cdo -O ymonmean -selname,pr,tasmax,tasmin -chname,ppt_mean,pr -chname,tmax_mean,tasmax -chname,tmin_mean,tasmin ",
             obs_day_hist," ",obs_clim_hist))

## remapbic does an interpolation to a new grid
## ymonadd adds the historical 'observed' climatology to the future daily anomalies
system(paste("cdo -O ymonadd -remapbic,",obs_clim_hist," ",rcm_day_futureanom," ",obs_clim_hist," ",rcm_day_futureanom,sep=""))
  
