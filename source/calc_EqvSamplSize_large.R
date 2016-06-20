# Calculate equivalent sample size 
# for Modified t-test for large samples (ne>=30)
# Zwiers and von Storch, 1995, J.Clim, Eq. 10

calc_EqvSamplSize_large <- function(x,y=NULL,r1){
  
  xnyrs<-length(x)
  
  if (!is.null(y)){
    ynyrs<-length(y)
  }
  
  xne_p = NA
  ne = NA
  
  if ((xnyrs>50) & (any(abs(r1)!=1.0))){
  	xne<-xnyrs*((1-r1)/(1+r1))
  } else {
    den = 0
  	for (n in 1:xnyrs-1){
  		if (!(all(is.na(x[n])))){
  		  den<-den+(1-n/xnyrs)*r1^n
  		} 
  	}
  	xne<-xnyrs/(1+2*den)
  }
  
  if (!is.null(y)){
      yne_p = NA
      if ((ynyrs>50) && (abs(r1)!=1.0)){
  	    yne<-ynyrs*((1-r1)/(1+r1))
      } else {
        den = 0
  	    for (n in 1:ynyrs-1){
  		    if (!(all(is.na(y[n])))){
  		      den<-den+(1-n/ynyrs)*r1^n
  		    }
  	    }
  	    yne<-ynyrs/(1+2*den)
      }
  }
  
  for (i in 1:xnyrs){
    if (all(is.na(x[i]))) {
      xne_p[i]<-NA
      if (!is.null(y)){
         yne_p[i]<-NA
      }
      ne[i]<-NA
    } else {
      if (r1[i]==0){
      	xne_p[i]<-xnyrs
      	if (!is.null(y)){
      	   yne_p[i]<-ynyrs
      	}
      } else {
        if (xne[i] <= 2){
          xne_p[i]<-2
        } else if ((xne[i]>2) && (xne[i]<=xnyrs)){
          xne_p[i]<-xne[i]
        } else {
          xne_p[i]<-xnyrs
        }
      	if (!is.null(y)){
      	  if (yne[i] <= 2){
      		  yne_p[i]<-2
      	  } else if ((yne[i]>2) && (yne[i]<=ynyrs)){
      		  yne_p[i]<-yne[i]
      	  } else {
      		  yne_p[i]<-ynyrs
      	  }
      	}
      }
    }
  
    if (!is.null(y)){
       ne<-(sqrt(1/xne_p+1/yne_p))
       #ne<-(1/sqrt(xne_p)+1/sqrt(yne_p))
       return(list(Xne_p=xne_p,Yne_p=yne_p,NE=ne))	
    } else {
      ne<-xne_p
      return(ne)
    }
  }
}


