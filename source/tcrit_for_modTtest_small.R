# evaluate tcrit for modified t-test for small samples based on
# Zwiers and von Storch, 1995, J. Clim 

lookupTcrit <- function(r1,siglev,nobs,y=NULL){

source(paste("create_LookupTabs_modTtest.R",sep="/"))

a<-seq(-0.35,0.95,length=27)
b<-a-0.025
c<-a+0.025

alpha<-array(NA,dim=c(27,3))
alpha[,1]<-a
alpha[,2]<-b
alpha[,3]<-c

stat_val<-lookupTabs(nobs)
if (is.null(y)) {
    if (isTRUE(all.equal(siglev,0.10))){
      tab<-stat_val[,1]
    } else if (isTRUE(all.equal(siglev,0.05))){
      tab<-stat_val[,2]
    } else if (isTRUE(all.equal(siglev, 0.025))){
      tab<-stat_val[,3]
    } else if (isTRUE(all.equal(siglev, 0.01))){
      tab<-stat_val[,4]
    } else if (isTRUE(all.equal(siglev, 0.005))){
      tab<-stat_val[,5]
    } else stop("significance level not found in lookup table, exiting")

} else {

    if (isTRUE(all.equal(siglev, 0.20))){
      tab<-stat_val[,1]
    } else if (isTRUE(all.equal(siglev, 0.10))){
      tab<-stat_val[,2]
    } else if (isTRUE(all.equal(siglev, 0.05))){
      tab<-stat_val[,3]
    } else if (isTRUE(all.equal(siglev, 0.02))){
      tab<-stat_val[,4]
    } else if (isTRUE(all.equal(siglev, 0.01))){
      tab<-stat_val[,5]
    }
}

rn<-length(r1)

tcrit = NA

for (i in 1:rn){
  r = r1[i]
  if (r<min(alpha[,2]) || r>max(alpha[,3])){
    tcrit[i]<-NA
    print('Error in tcrit_for_modTtest_small.R, no tcrit found')
  } else if (min(which(alpha[,3]>=r),na.rm=T) && max(which(alpha[,2]<r),na.rm=T)){
	  tindx<-min(which(alpha[,3]>=r),na.rm=T)
    tcrit[i]<-tab[tindx]
	} else {
		tcrit[i]<-NA
		print('Error in tcrit_for_modTtest_small.R, no tcrit found')
  }
}


return(tcrit)

} 


