#Paddlefish Metapopulation Viability Analysis (PAMVAN)
#setwd("/Users/HenryHershey/Desktop/pamvan/")
directory = getwd()
outdir    = paste(directory,"/output/", sep="")
source(paste(directory, "/source/FunctionSourcer.R", sep =''))
library(reshape2)
library(plyr)

starts = list(
  popinit = 500, #initial pop size in each res
  nt = 100,      #number of time steps
  MA = .29,       #adult mortality (Rider 2011)
  MJ = .5,       #juvenile mortality
  R = 2,      #average recruitment (don't go above 2.2)
  Rvar = 0.09,
  dens = T,
  K = 500,
  #recruitment variability
  nres = 4,      #number of reservoir segments
  pdown = .5,    #probability of returning downstream after upstream passage (or dispersing downstream as age 0) immature fish don't move
  pup = c(1,1,1,0.00000001)       #probability of upstream passage by mature adults
)
#hist(ages)

pop <- initpop(starts) #create a population

abund <- runpop(pop,starts) #run the population through the metapopulation model

#visualize age distribution over time
#lapply(pop,function(x) hist(x[,2]))

#visualize abund over time
#pdf(paste(outdir,"pamvan_out.pdf",sep=""))
plot(NA,xlim=c(0,starts$nt+1),ylim=c(0,max(abund$N+100,na.rm=T)))
lapply(1:4,function(x) lines(abund$N[abund$Res==x]~abund$T[abund$Res==x],col=x))
legend("topleft",title="reservoir",fill=1:4,legend=c(1:4))
#dev.off()
       
 
       
       
       

#now, run 100 simulations with some variability in hydrology (recruitment) and see how many of the
#simulations result in a crashed population
#count the total number of fish that are alive in the last year. if it's zero, return a 1.
crash = function(abund,starts) ifelse(sum(abund$N[abund$T==starts$nt & abund$Res >0],na.rm=T)<10,1,0) 

propcrashed <- function(pop,starts,Nsim){
  
  colMeans(plyr::ldply(1:Nsim,function(x)crash(runpop(pop,starts),starts)),na.rm=T)
  
}
pop <- initpop(starts) #create a population
abund <- runpop(pop,starts) #run the population through the metapopulation model

#what proportion of ten 100-year timeseries results in a crashed population?
propcrashed(pop,starts,10)

#sensitivity analysis
#see how changing starting values affects the proportion of runs that crash

#let's make 3 plots showing how the proportion of simulations that crash 
#the population changes with different starting values of combinations of 2 parameters
#CAUTION THIS TAKES A LONG TIME TO RUN ON MY MACHINE

par(mfrow=c(1,3))
#Experiment 1: natural mortality x prob of upstream passage
MA = seq(0.10,.38,0.04)
pup = seq(0.1,1,0.1)
prop=matrix(NA,nrow=length(MA),ncol=length(pup))
for(a in 1:length(pup)){
  starts$pup = pup[a]
  for(l in 1:length(MA)){
    starts$MA = MA[l]
    prop[l,a] = propcrashed(pop,starts,10)
  }
}
prop
plot(NULL, main="Prob Upstream Passage = 0.1-1.0",xlim=c(min(MA),max(MA)),ylim=c(0,1),xlab="Natural Mortality",ylab="Proportion of Simulations Resulting in Population Crash")
apply(prop,2,function(x) lines(x~MA))

#Experiment 2 Recruitment x Prob of Upstream Passage
pop <- initpop(starts)
R = seq(1.5,3.5,0.1)
pup = seq(0.1,1,0.1)
prop=matrix(NA,nrow=length(R),ncol=length(pup))
for(a in 1:length(pup)){
  starts$pup = pup[a]
  for(l in 1:length(R)){
    starts$R = R[l]
    prop[l,a] = propcrashed(pop,starts,10)
  }
}
prop
plot(NULL,main="Prob of Upstream Passage = 0.1 - 1.0",xlim=c(min(R),max(R)),ylim=c(0,1),xlab="Recruitment",ylab="Proportion of Simulations Resulting in Population Crash")
apply(prop,2,function(x) lines(x~R))

#Experiment 3 Juvenile mortality x prob of downstream dispersal
pop <- initpop(starts)
MJ = seq(.4,.9,.1)
pdown = seq(0.1,1,0.1)
prop=matrix(NA,nrow=length(MJ),ncol=length(pdown))
for(a in 1:length(pdown)){
  starts$pdown = pdown[a]
  for(l in 1:length(MJ)){
    starts$MJ = MJ[l]
    prop[l,a] = propcrashed(pop,starts,10)
  }
}
prop
plot(NULL,main="Prob of Juvenile Downstream Dispersal = 0.1 - 1.0",xlim=c(min(MJ),max(MJ)),ylim=c(0,1),xlab="Juvenile Mortality",ylab="Proportion of Simulations Resulting in Population Crash")
apply(prop,2,function(x) lines(x~MJ))
