#Paddlefish Metapopulation Viability Analysis (PAMVAN)
setwd("/Users/HenryHershey/Desktop/pamvan/")
directory = getwd()
outdir    = paste(directory,"/output/", sep="")
source(paste(directory, "/source/FunctionSourcer.R", sep =''))


#Variables:

starts = list(
  popinit = 100, #initial pop size in each res
  nt = 50, #number of time steps
  MA = .29, #adult mortality (Rider 2011)
  MJ = .8, #juvenile mortality
  R = 1.5,#recruitment
  nres = 4, #number of reservoir segments
  pdown = 0.25, #probability of downstream dispersal by age 0
  pup = 0.5 #probability of upstream passage by mature adults
)
#hist(ages)

pop <- initpop(starts)

abund = list(NA)

for(t in 2:starts$nt){    #loop through years
  pop[[t]] = matrix(NA,ncol=2,nrow=dim(pop[[t-1]])[1])
    for(i in 1:dim(pop[[t-1]])[1]){ #loop through individuals in each year
    
    pop[[t]][i,2] = pop[[t-1]][i,2]+1       #all fish get a year older
    pop[[t]][i,1] = ifelse(pop[[t-1]][i,2]<1,
                        max(pop[[t-1]][i,1]-rbinom(1,1,starts$pdown),1)*rbinom(1,1,1-starts$MJ), #age 0s either move down (or not) and die (or not)
                        ifelse(pop[[t-1]][i,2]>0 & pop[[t-1]][i,2]<8,
                               pop[[t-1]][i,1]*rbinom(1,1,1-starts$MA), #if the fish is older than 0 but younger than 8, die or survive, no move
                        ifelse(pop[[t-1]][i,1]==0,0,min(pop[[t-1]][i,1]+rbinom(1,1,starts$pup),4)*rbinom(1,1,1-starts$MA)))) #if the fish is older than 7, move up (or not) and die (or not) 
   
    }
  abund[[t]] <- table(pop[[t]][,1]) #after each year, how many fish are in each reservoir (or dead)
  pop[[t]] = rbind(recruit(abund[[t]],starts),pop[[t]][pop[[t]][,1]>0,]) #add new recruits to the population
  }
abund = melt(abund)
colnames(abund)=c("N","Res","T")

#visualize age distribution over time
#lapply(pop,function(x) hist(x[,2]))

#visualize abund over time
pdf(paste(outdir,"pamvan_out.pdf",sep=""))
plot(NA,xlim=c(0,starts$nt+1),ylim=c(0,max(abund$N+100,na.rm=T)))
lapply(1:4,function(x) lines(abund$N[abund$Res==x]~abund$T[abund$Res==x],col=x))
legend("topleft",title="reservoir",fill=1:4,legend=c(1:4))
dev.off()
