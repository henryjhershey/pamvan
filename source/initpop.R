#initialize the same distribution of aged individuals to each reservoir segment
initpop = function(starts){
  pop = matrix(c(rep(1:starts$nres,each=starts$popinit), rep(rnbinom(starts$popinit,2,.3), #rnbinom is a place holder for now
                                                             starts$nres)),nrow=starts$popinit*starts$nres,ncol=2,byrow=F)
  arr <- list(NA)
  arr[[1]] = pop
  return(arr)
}
