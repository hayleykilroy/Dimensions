###Script to analyse Cape Point weather data
library(bfast)

dat<-read.table("D:\\Jasper\\Side projects\\Taylor plots\\Climate\\weatherdataforcapepoint\\TM_all_Cpoint_Slangkop.txt", header=T, stringsAsFactors =F)

###Some data summarizing
dat[,1]<-as.Date(dat$Date)

month<-cut(dat[,1],"month")

mmp<-tapply(dat$Rain.1, month, "sum")
mxt<-tapply(dat$MaxTemp.1, month, "mean")
mnt<-tapply(dat$MinTemp.1, month, "mean")

mmp<-cbind(mmp,levels(month))
mxt<-cbind(mxt,levels(month))
mnt<-cbind(mnt,levels(month))

###bfast time-series analysis
#Rainfall
x<-na.omit(mmp)
datTS<-ts(as.numeric(x[,1]), frequency =12) #
fit <- bfast(datTS,h=12/length(x[,1]), season="dummy", max.iter=10)

pdf("D:\\SAEON\\Projects\\Cape Point\\Taylor plots\\Results_temp\\monthlyrain.pdf",width=6, height=6)
plot.bfast(fit, main="Mean monthly rainfall")
dev.off()

#Max Temp
x<-mxt[757:1320,]
#x<-na.omit(x)
datTS<-ts(as.numeric(x[,1]), frequency =12) #
datTS<-na.spline(datTS)#, xout=levels(month))
datTS<-ts(datTS, frequency =12)
fit <- bfast(datTS,h=12/length(x[,1]), season="dummy", max.iter=10) 

pdf("D:\\SAEON\\Projects\\Cape Point\\Taylor plots\\Results_temp\\monthlymaxtemp_splined.pdf",width=6, height=6)
plot(fit, main="Mean monthly maximum temperature")
dev.off()

#Min Temp
x<-mnt[757:1320,]
#x<-na.omit(mnt)
datTS<-ts(as.numeric(x[,1]), frequency =12) #
datTS<-na.approx(datTS)#, xout=levels(month))
datTS<-ts(datTS, frequency =12)
fit <- bfast(datTS,h=12/length(x[,1]), season="dummy", max.iter=10)

pdf("D:\\SAEON\\Projects\\Cape Point\\Taylor plots\\Results_temp\\monthlymintemp_approxed.pdf",width=6, height=6)
plot(fit, main="Mean monthly minimum temperature")
dev.off()


###Climate indices
#install.packages("RClimDex")
#library(RClimDex)
#