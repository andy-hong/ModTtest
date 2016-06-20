# Calculate pooled sample Variance
# for modified t-test for large samples (ne>=30)
# Zwiers and von Storch, 1995, J.Clim, Eq. 13 (two sample case)

calc_poolSampVar <- function(x,y,xAve,yAve){
  
  sp2 = (sum((x-xAve)^2) + sum((y-yAve)^2))  / (length(x) + length(y) - 2)
  
  return(sp2)
  
}


