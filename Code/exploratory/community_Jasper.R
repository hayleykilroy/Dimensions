###Script to explore veg change between surveys:
#Get veg data
#Convert abundance classes to 2010 median values
#Make species-accumulation curves
#Make NMS plot
#Test for compositional differences between years based on Bray-Curtis distance using AnoSim, MRPP and PERMANOVA
#Indicator species analysis

###Libraries
library(picante)
library(ecodist)
library(BiodiversityR)
#library(ellipse)
library(labdsv)

###Get veg data
veg<-read.csv("D:\\SAEON\\Projects\\Cape Point\\Taylor plots\\Data\\PostprocessedData\\ReleveAll.csv")
veg<-veg[which(veg[,8]!=0),]
veg<-cbind(veg,vector("numeric",dim(veg)[1]))
colnames(veg)[9]<-"Abundance Estimate"

###Calculate abundance classes from 2010 data
aclass<-tapply(veg[which(veg$Year==2010),7], INDEX=list(as.factor(veg[which(veg$Year==2010),8])), FUN="median")

###Convert classes to abundance for all surveys
for(i in 1:5) {veg[which(veg[,8]==i),9]<-aclass[i]}


#########################################################
###Some exploratory analyses

###Making community data matrices - one for each survey time and a complete one
veg66<-veg[veg$Year==1966,]
veg96<-veg[veg$Year==1996,]
veg10<-veg[veg$Year==2010,]

x<-cbind(veg66[,3],veg66[,9],apply(veg66[,4:5], MARGIN=1, FUN="paste", sep="", collapse="_"))
x<-as.data.frame(x[which(x[,2]!=0),])
x[,2]<-as.numeric(as.character(x[,2]))
veg66<-sample2matrix(x)

x<-cbind(veg96[,3],veg96[,9],apply(veg96[,4:5], MARGIN=1, FUN="paste", sep="", collapse="_"))
x<-as.data.frame(x[which(x[,2]!=0),])
x[,2]<-as.numeric(as.character(x[,2]))
veg96<-sample2matrix(x)

x<-cbind(veg10[,3],veg10[,9],apply(veg10[,4:5], MARGIN=1, FUN="paste", sep="", collapse="_"))
x<-as.data.frame(x[which(x[,2]!=0),])
x[,2]<-as.numeric(as.character(x[,2]))
veg10<-sample2matrix(x)

x<-cbind(veg[,3],veg[,9],apply(veg[,4:5], MARGIN=1, FUN="paste", sep="", collapse="_"))
x<-as.data.frame(x[which(x[,2]!=0),])
x[,2]<-as.numeric(as.character(x[,2]))
vegComb<-sample2matrix(x) #veg with each plot composition combined across all years

x<-merge(veg66,veg96, all=T, sort=FALSE)
vegA<-merge(x, veg10, all=T, sort=FALSE) #Combined matrix of all vegplots
vegA[is.na(vegA)] <- 0 

year<-as.data.frame(c(rep("1966",dim(veg66)[1]),rep("1996",dim(veg96)[1]),rep("2010",dim(veg10)[1]))) #factor indicating plot date for vegA
colnames(year)<-"Year"

###Species accumulation curves
spp.curve66 <- specaccum(comm = veg66, method = "random", permutations = 1000)
spp.curve96 <- specaccum(comm = veg96, method = "random", permutations = 1000)
spp.curve10 <- specaccum(comm = veg10, method = "random", permutations = 1000)

pdf("D:\\SAEON\\Projects\\Cape Point\\Taylor plots\\Results_temp\\speciesaccumulation.pdf", height=6, width=6)
plot(spp.curve66, col=palette()[1], ylab="Species number")
plot(spp.curve96, col=palette()[2], add=TRUE)
plot(spp.curve10, col=palette()[3], add=TRUE)
legend("bottomright", legend=c("1966","1996","2010"), text.col=palette())
dev.off()

###Ordination
dis <- distance(vegA, method = "bray-curtis")
scree <- nmds(dis, mindim = 1, maxdim = 5, nits = 10) #NMS
stress <- scree$stress
plot(stress)

axis.seq <- c(seq(1, 1, length = 10), seq(2, 2, length = 10),
+ seq(3, 3, length = 10), seq(4, 4, length = 10), seq(5, 5, length = 10))
plot(stress ~ factor(axis.seq))

nms.final <- nmds(dis, mindim = 2, maxdim = 2, nits = 50)
nms.min <- nmds.min(nms.final, dims = 2)
min(nms.final$stress)

pdf("D:\\SAEON\\Projects\\Cape Point\\Taylor plots\\Results_temp\\NMS.pdf", height=6, width=6)
plot(nms.min[, 1], nms.min[, 2], ylab="Axis2", xlab="Axis1",col=c(rep("black",dim(veg66)[1]),rep("red",dim(veg96)[1]),rep("green",dim(veg10)[1])))
legend("bottomright", legend=c("1966","1996","2010"), text.col=palette())
dev.off()

#pdf("D:\\SAEON\\Projects\\Cape Point\\Taylor plots\\Results_temp\\NMS2.pdf", height=6, width=6)
#nms.plot <- ordiplot(nms.min, type = "n")
#ordisymbol(nms.plot, y = year, factor = "Year", col=palette() , legend = F)#, col=c(rep("black",dim(veg66)[1]),rep("blue",dim(veg96)[1]),rep("red",dim(veg10)[1])), legend = F)
#legend("bottomright", legend=c("1966","1996","2010"), text.col=palette())
#dev.off()

###Tests for differences in community composition among 3 time periods

year<-as.factor(c(rep("1966",dim(veg66)[1]),rep("1996",dim(veg96)[1]),rep("2010",dim(veg10)[1]))) #factor indicating plot date for vegA

anosim(vegA, year, permutations = 999, distance = "bray") #analysis of similarity
mrpp(vegA, year, permutations = 999, distance = "bray") #multi-response permutation procedure
adonis(formula = vegA ~ year, method = "bray", permutations = 999) #permanova

###Indicator species analysis
IS <- indval(vegA, year)
summary(IS)

IS2 <- isamic(vegA, year)
summary(IS2)

#detach(package:labdsv)
#install.packages("MCMCpack")

