setwd("C:/Work/Dimensions")
library(rjags)

labdt=read.csv("Data/Data/LabData_errorflags.csv")
head(labdt)
dim(labdt)


##Dealing with errors

# If a weight measurement is below the scale threshold, replace it with NA
labdt$RawLeafFresh_g[grepl("RawLeafFresh below scale threshold", labdt$Error)==T]=NA
labdt$RawLeafDry_g[grepl("RawLeafDry below scale threshold", labdt$Error)==T]=NA
labdt$TwigFresh_g[grepl("TwigFresh below scale threshold", labdt$Error)==T]=NA
labdt$TwigDry_g[grepl("TwigDry below scale threshold", labdt$Error)==T]=NA

# If a weight measurement is below the scale threshold, replace derived measurements with NA
labdt$LeafFresh_g[grepl("RawLeafFresh below scale threshold", labdt$Error)==T]=NA
labdt$LeafDry_g[grepl("RawLeafDry below scale threshold", labdt$Error)==T]=NA
labdt$SLA[grepl("RawLeafDry below scale threshold", labdt$Error)==T]=NA
labdt$LeafSucculence[grepl("RawLeafFresh below scale threshold", labdt$Error)==T]=NA
labdt$LeafSucculence[grepl("RawLeafDry below scale threshold", labdt$Error)==T]=NA
labdt$TwigSucculence[grepl("TwigFresh below scale threshold", labdt$Error)==T]=NA
labdt$TwigSucculence[grepl("TwigDry below scale threshold", labdt$Error)==T]=NA

# If difference between fresh & dry weights is within the scale threshold, replace succulence measurements with NA
labdt$LeafSucculence[grepl("Leaf Difference below scale threshold", labdt$Error)==T]=NA
labdt$TwigSucculence[grepl("Twig Difference below scale threshold", labdt$Error)==T]=NA

# If succulence is greater than 100%, replace weights & succulence with NA
labdt$RawLeafFresh_g[grepl("LeafSucculence greater than 100%", labdt$Error)==T]=NA
labdt$LeafFresh_g[grepl("LeafSucculence greater than 100%", labdt$Error)==T]=NA
labdt$RawLeafDry_g[grepl("LeafSucculence greater than 100%", labdt$Error)==T]=NA
labdt$LeafDry_g[grepl("LeafSucculence greater than 100%", labdt$Error)==T]=NA
labdt$LeafSucculence[grepl("LeafSucculence greater than 100%", labdt$Error)==T]=NA
labdt$TwigFresh_g[grepl("TwigSucculence greater than 100%", labdt$Error)==T]=NA
labdt$TwigDry_g[grepl("TwigSucculence greater than 100%", labdt$Error)==T]=NA
labdt$TwigSucculence[grepl("TwigSucculence greater than 100%", labdt$Error)==T]=NA

### Create lists for looping

labdt$SpeciesID=paste(labdt$NewGenus, labdt$NewSpecies)

species=unique(labdt$SpeciesID)
traits=c("LeafLength_cm","AvgLeafWidth_cm","MaxLeafWidth_cm",
          "LeafThickness_mm","SLA","LeafSucculence","TwigSucculence")
# Height, CanopyX, & CanopyY need to be done differently from the others - were measured fewer times

#### JAG models
for (s in 1:length(species){
  for (t in 1:length(traits){
  
## make a list of your data elements
data=list(
  sp=species[s],
  tr=traits[t],
  value=labdt[labdt$SpeciesID==species[s], traits[t]])

## write Function to generate initial values
gen.inits=function() { list(
  ## spatial terms
  alpha=runif(nGrid,0.1,0.5),
  gamma=runif(nGrid,0.1,.9),
  lambda=runif(nGrid,0.2,1),
  k=runif(nGrid,0.4,.9),
  A=runif(nGrid,0,1),
  k.tau=runif(1,0,2),
  tau=runif(1,0,2)
  )
}

## make list of parameters to monitor (if you don't monitor it, it will be lost)
params=c("lambda.sigma","A.beta","A.sigma","sigma","phi")

## compile it
m= jags.model(data=data, n.chains=3, n.adapt=100)

### run it
mc = coda.samples(m, params, n.iter=100)