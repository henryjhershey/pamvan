
#make a function for adding new recruits to each reservoir each year
recruit = function(abund,starts){
  newfish = round(starts$R*abund)
  nnew = sum(newfish[c("1","2","3","4")],na.rm=T)
  resnew = as.numeric(names(newfish)[names(newfish)>0])
  recruits = matrix(c(rep(resnew,newfish[c(as.character(resnew))]),rep(0,nnew)),nrow=nnew,ncol=2,byrow=F)
  return(recruits)
}
