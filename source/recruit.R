
#make a function for adding new recruits to each reservoir each year
recruit = function(abund,starts){
  R = max(c(0,rnorm(starts$R,starts$Rvar)))
  #newfish = ifelse(starts$dens==T, starts$K/(1+((starts$K-abund)/abund)*exp(-R)), round(abund*R))
  newfish= round(R*abund)
  #newfish= round(starts$K/(1+((starts$K-abund)/abund)*exp(-R)))
  nnew = sum(newfish[c("1","2","3","4")],na.rm=T)
  resnew = as.numeric(names(newfish)[names(newfish)>0])
  recruits = matrix(c(rep(resnew,newfish[c(as.character(resnew))]),rep(0,nnew)),nrow=nnew,ncol=2,byrow=F)
  return(recruits)
}
