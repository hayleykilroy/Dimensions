setwd("C:/Work/Dimensions/Data")
library(lattice)
library(reshape)

cat=read.csv("PostprocessedData/CAT_byPlotYear.csv")

table(cat$Plot)

cat2=melt(cat[,2:16], id.vars=c("Year","Plot"))

bwplot(value~Year|variable,  data=cat2, horizontal=F, scales=list(y=list(relation='free')))




####
catavg=aggregate( as.matrix(cat[,2:14]) ~ cat$Year, FUN=mean)
catsd=aggregate( as.matrix(cat[,2:14]) ~ cat$Year, FUN=sd)
colnames(catavg)=c("Year", colnames(cat[2:14]))
colnames(catsd)=c("Year", colnames(cat[2:14]))

catavg=melt(catavg, id.vars="Year")

 xyplot(value~Year|variable, data=catavg,
        scales=list(y=list(relation='free')))