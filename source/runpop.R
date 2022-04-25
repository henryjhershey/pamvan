runpop <- function(pop,starts){
  
 # pop=initpop(starts)
abund = list(NA)
  for(t in 2:starts$nt){    #loop through years
    if(sum(pop[[t-1]])>0){
    pop[[t]] = matrix(NA,ncol=2,nrow=dim(pop[[t-1]])[1])
    for(i in 1:dim(pop[[t-1]])[1]){ #loop through individuals in each year
      
      pop[[t]][i,2] = pop[[t-1]][i,2]+1       #all fish get a year older
      pop[[t]][i,1] = ifelse(pop[[t-1]][i,2]<1,
                             max(pop[[t-1]][i,1]-rbinom(1,1,starts$pdown),1)*rbinom(1,1,1-starts$MJ), #age 0s either move down (or not) and die (or not)
                             ifelse(pop[[t-1]][i,2]>0 & pop[[t-1]][i,2]<8,
                                    pop[[t-1]][i,1]*rbinom(1,1,1-starts$MA), #if the fish is older than 0 but younger than 8, die or survive, no move
                                    ifelse(pop[[t-1]][i,1]==0,0,min(pop[[t-1]][i,1]+rbinom(1,1,starts$pup[pop[[t-1]][i,1]]),4)*rbinom(1,1,1-starts$MA)))) #if the fish is older than 7, move up (or not) and die (or not) 
      
    }
    abund[[t]] <- table(factor(pop[[t]][,1],levels=0:starts$nres)) #after each year, how many fish are in each reservoir (or dead)
    pop[[t]] = rbind(recruit(abund[[t]],starts),pop[[t]][pop[[t]][,1]>0,]) #add new recruits to the population
  } else {
    abund[[t]] = c(0,0,0,0,0)
    pop[[t]] = 0
  }
  }
  abund = melt(abund)
  colnames(abund)=c("N","Res","T")
  return(abund)
  #allow the migrators to move back down to the reservoir they came from after they get counted for the annual abundance estimate. this way, they contribute to the recruitment of the 
  #reservoir they migrated to, but they can move back downstream after.
  for(t in 2:starts$nt){
    for(i in 1:dim(pop)[1]){
    pop[[t]][i,1] = ifelse(pop[[t-1]][i,1]<pop[[t]][i,1] & pop[[t]][i,1] > 0,pop[[t]][i,1]-rbinom(1,1,starts$pdown),pop[[t]][i,1])
  }
  }
  
  
  }


