# Calculate t statistic
# for modified t-test for small samples (ne<=30)
# Zwiers and von Storch, 1995, J. Clim, Eq. 16

calc_Tstat_small <- function(x,y,xAve,yAve){
  
  source(paste("calc_poolSampVar.R",sep="/"))
  
  xnyrs<-length(x)
  ynyrs<-length(y)
  
  #Pooled sample variance, Eq. 13
  sp2<-calc_poolSampVar(x,y,xAve,yAve)
  
  #t-statistic, Eq. 16
  tval<-(yAve-xAve)/(sqrt(sp2)*(sqrt(1/xnyrs+1/ynyrs)))
  
  return(tval)
}
