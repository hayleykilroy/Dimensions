setwd("C:/Work/Cape Point Data/2010survey/FinalData/Traits")

ld=read.csv("LabData.csv")
head(ld)
summary(ld)
dim(ld)
str(ld)


#measurement below 0.006 (2 SD of scale's repeatability)
ld$Error[ld$RawLeafFresh_g<=0.006]="RawLeafFresh below scale threshold"
summary(as.factor(ld$Error))

ld$Error[ld$RawLeafDry_g<=0.006 & is.na(ld$Error)==F]=paste(ld$Error[ld$RawLeafDry_g<=0.006 & is.na(ld$Error)==F], "RawLeafDry below scale threshold", sep=", ")
ld$Error[ld$RawLeafDry_g<=0.006 & is.na(ld$Error)==T]="RawLeafDry below scale threshold"
summary(as.factor(ld$Error))

ld$Error[ld$TwigFresh_g<=0.006 & is.na(ld$Error)==F]=paste(ld$Error[ld$TwigFresh_g<=0.006 & is.na(ld$Error)==F], "TwigFresh below scale threshold", sep=", ")
ld$Error[ld$TwigFresh_g<=0.006 & is.na(ld$Error)==T]="TwigFresh below scale threshold"
summary(as.factor(ld$Error))

ld$Error[ld$TwigDry_g<=0.006 & is.na(ld$Error)==F]=paste(ld$Error[ld$TwigDry_g<=0.006 & is.na(ld$Error)==F], "TwigDry below scale threshold", sep=", ")
ld$Error[ld$TwigDry_g<=0.006 & is.na(ld$Error)==T]="TwigDry below scale threshold"
summary(as.factor(ld$Error))

#difference btwn dry & fresh measurements are within 0.006
ld$Error[abs(ld$RawLeafFresh_g-ld$RawLeafDry_g)<=0.006 & is.na(ld$Error)==F]=paste(ld$Error[abs(ld$RawLeafFresh_g-ld$RawLeafDry_g)<=0.006 & is.na(ld$Error)==F], "Leaf Difference below scale threshold", sep=", ")
ld$Error[abs(ld$RawLeafFresh_g-ld$RawLeafDry_g)<=0.006 & is.na(ld$Error)==T]="Leaf Difference below scale threshold"
summary(as.factor(ld$Error))

ld$Error[abs(ld$TwigFresh_g-ld$TwigDry_g)<=0.006 & is.na(ld$Error)==F]=paste(ld$Error[abs(ld$TwigFresh_g-ld$TwigDry_g)<=0.006 & is.na(ld$Error)==F], "Twig Difference below scale threshold", sep=", ")
ld$Error[abs(ld$TwigFresh_g-ld$TwigDry_g)<=0.006 & is.na(ld$Error)==T]="Twig Difference below scale threshold"
summary(as.factor(ld$Error))

#Succulence greater than 100%
ld$Error[ld$LeafSucculence>=100 & is.na(ld$Error)==F]=paste(ld$Error[ld$LeafSucculence>=100 & is.na(ld$Error)==F], "LeafSucculence greater than 100%", sep=", ")
ld$Error[ld$LeafSucculence>=100 & is.na(ld$Error)==T]="LeafSucculence greater than 100%"
summary(as.factor(ld$Error))

ld$Error[ld$TwigSucculence>=100 & is.na(ld$Error)==F]=paste(ld$Error[ld$TwigSucculence>=100 & is.na(ld$Error)==F], "TwigSucculence greater than 100%", sep=", ")
ld$Error[ld$TwigSucculence>=100 & is.na(ld$Error)==T]="TwigSucculence greater than 100%"
summary(as.factor(ld$Error))

write.csv(ld, "LabData.csv", row.names=F)