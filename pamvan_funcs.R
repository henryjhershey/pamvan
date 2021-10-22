#PAMVAN FUNCTIONS

#initialize the same distribution of aged individuals to each reservoir segment
initpop = function(starts){
  pop = matrix(c(rep(1:starts$nres,each=starts$popinit), rep(rnbinom(starts$popinit,2,.3), #rnbinom is a place holder for now
                                                             starts$nres)),nrow=starts$popinit*starts$nres,ncol=2,byrow=F)
  arr <- list(NA)
  arr[[1]] = pop
  return(arr)
}

#make a function for adding new recruits to each reservoir each year
recruit = function(abund,starts){
  newfish = round(starts$R*abund)
  nnew = sum(newfish[c("1","2","3","4")],na.rm=T)
  resnew = as.numeric(names(newfish)[names(newfish)>0])
  recruits = matrix(c(rep(resnew,newfish[c(as.character(resnew))]),rep(0,nnew)),nrow=nnew,ncol=2,byrow=F)
  return(recruits)
}
