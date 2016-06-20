# Calculate lag 1 correlation coefficient
# for modified t-test (large and small samples)
# Zwiers and von Storch, 1995, J.Clim, Eq. 14 (two sample case)

calc_lag1Corr <- function(x,y = NULL,xAve,yAve = NULL){
  
  ## Sum of Squares
  xsumsq = sum((x-xAve)^2, na.rm=TRUE)
  
  ## Sum of lag product
  xsumlag = sum((tail(x,-1) - xAve) * (head(x,-1) - xAve))
  
  if (!is.null(y)) {
    ysumsq = sum((y-yAve)^2, na.rm=TRUE)
    ysumlag = sum((tail(y,-1) - yAve) * (head(y,-1) - yAve))
    
    xysumsq = xsumsq + ysumsq
    xysumlag = xsumlag + ysumlag
    
    r1 = xysumlag / xysumsq
    
  } else {
    r1 = xsumlag / xsumsq
  }

  return(r1)
}


