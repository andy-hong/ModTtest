#  modified t-test after Zwiers and von Storch 1995, J.Clim 8, p.336-351
#  
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  This version is an adapted version of the codes:  
#  https://github.com/ruthlorenz/stat_tests_correlated_climdata


modTtest = function(x, ...) UseMethod("modTtest")

modTtest.default  = 
function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
         mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95,
         time.vals = NULL, ...)
{
        library(abind)
        source("calc_lag1Corr.R")
        source("calc_EqvSamplSize_large.R")
        source("tcrit_for_modTtest_small.R")
        source("calc_poolSampVar.R")

#   alternative = 'two.sided'
#   time.vals = NULL
#   paired = TRUE
#   var.equal = TRUE; conf.level = 0.95
  
  ## -------
  
  alternative = match.arg(alternative)

	if(!missing(mu) && (length(mu) != 1 || is.na(mu)))
	    stop("'mu' must be a single number")
	if(!missing(conf.level) &&
	   (length(conf.level) != 1 || !is.finite(conf.level) ||
	    conf.level < 0 || conf.level > 1))
	    stop("'conf.level' must be a single number between 0 and 1")

	if( !is.null(y) ) {
	  dname = paste(deparse(substitute(x)),"and", deparse(substitute(y)))
    if(paired) { 
	    xok = yok = complete.cases(x, y)
    } else {
      yok = !is.na(y)
    	xok = !is.na(x)
    }
	  y = y[yok,drop=FALSE]
	} else {
    dname = deparse(substitute(x))
	  if (paired) stop("'y' is missing for paired test")
	  xok = !is.na(x)
	  yok = NULL
	}
  
	x = x[xok,drop=FALSE]
	
	if (paired) {
	  x = x-y
	  y = NULL
	}

	nx = length(x)
	
	if(!is.null(time.vals)){
		if(nx!=time.vals){
			stop("Error: wrong dimension, the time in X does not correspond to time.vals")
		}
	}

	if(!is.null(y)) {
	    ny = length(y)
	}

	mx = mean(x, na.rm=TRUE)
	vx = var(x, na.rm=TRUE)

	if(is.null(y)) {
	  
	#------ One-sample test ------#
    if(nx < 2) stop("not enough 'x' observations")
	  
    #------ Lag-1 Autocorrelation ------#
    r1 = calc_lag1Corr(x=x,xAve=mx)
    
    if (nx >= 30){
       #------ Equivalent Sample Size ------#
      ne = calc_EqvSamplSize_large(x=x,r1=r1)
      df = ne-1
      stderr = sqrt(vx/ne)
      if(all(stderr < 10 *.Machine$double.eps * abs(mx)))
  	    stop("data are essentially constant")
      tstat = (mx-mu)/stderr
      method = if(paired) "Paired modified t-test for time series data"
        else "One Sample modified t-test for time series data"
      estimate = setNames(mx, if(paired) "Mean of the differences" else "mean of x")

    } else {
      #------ Lookup table test, df undefined ------#
      df = NA
      stderr = sqrt(vx/nx)
      tstat = (mx-mu)/stderr
      tcrit = lookupTcrit(r1,1-conf.level,nx)
      method = if(paired) "Paired modified lookup table t-test for time series data"
  	    else "One Sample modified lookup table t-test for time series data"
      estimate = setNames(mx, if(paired) "Mean of the differences" else "mean of x")
    }
	} else {
	  
	#------ Two-sample test ------#
	  
	  ny = length(y)

	  my = mean(y, na.rm=TRUE)
	  vy = var(y, na.rm=TRUE)

    if(nx < 2) stop("not enough 'x' observations")
    if(ny < 2) stop("not enough 'y' observations")
    if(!var.equal) stop("the two sample modified t-test assumes equal variance")
    if(nx+ny < 3) stop("not enough observations")
    
    #------ Pooled sample variance, Eq. 13 ------#
    sp2 = calc_poolSampVar(x=x,y=y,xAve=mx,yAve=my)
    estimate = mx-my
    names(estimate) = "Difference between x and y"
    
    #------ Lag-1 Autocorrelation ------#
    r1 = calc_lag1Corr(x=x,y=y,xAve=mx,yAve=my)
    
    if (nx+ny >= 30){
      #------ Equivalent Sample Size ------#
      EqvSize = calc_EqvSamplSize_large(x=x,y=y,r1=r1)
      ne = EqvSize$NE
      xne_p = EqvSize$Xne_p
      yne_p = EqvSize$Yne_p
      df = xne_p+yne_p-2
      stderr = (sqrt(sp2)*ne)
      tstat = (mx-my)/stderr	#Eq. 12
      method = paste("Two Sample modified t-test for time series data")
    } else {
      #------ Lookup table test, df undefined ------#
      df = NA
      stderr = (sqrt(sp2)*(sqrt(1/nx+1/ny)))
      tstat = calc_Tstat_small(x=x,y=y,xAve=mx,yAve=my)		#Eq.16
      tcrit = lookupTcrit(r1,1-conf.level,nx+ny)
      method = paste("Two Sample modified lookup table t-test for time series data") 
    }
    
    if(all(stderr < 10 *.Machine$double.eps * max(abs(mx), abs(my))))
        stop("data are essentially constant")
	}
	
	cint  = array(NA,dim=c(2,1))

	if (all(is.na(df))) {
    #------ Lookup table test, pval undefined ------#
	  pval = NA
	  if (alternative == "less") {
	    cint[1] = -Inf
	    cint[2] = tcrit
	  } else if (alternative == "greater") {
	    cint[1] = tcrit
	    cint[2] = Inf
	  } else {
		  ci = tcrit
		  cint[1] = tstat - ci
		  cint[2] = tstat + ci
	  }
	} else {
	  if (alternative == "less") {
		  pval = pt(tstat, df)
		  cint[1] = -Inf
		  cint[2] = tstat + qt(conf.level, df)
	  } else if (alternative == "greater") {
		  pval = pt(tstat, df, lower.tail = FALSE)
		  cint[1] = tstat - qt(conf.level, df)
		  cint[2] = Inf
	  } else {
		  pval = 2 * pt(-abs(tstat), df)
		  alpha = 1 - conf.level
		  ci = qt(1 - alpha/2, df)
		  cint[1] = tstat - ci
		  cint[2] = tstat + ci
    }
	}
	
	cint[1] = mu + cint[1,drop=FALSE] * stderr
	cint[2] = mu + cint[2,drop=FALSE] * stderr
	
	names(tstat) = "t"
	names(df) = "df"
	names(mu) = if(paired || !is.null(y)) "difference in means" else "mean"
	attr(cint,"conf.level") = conf.level
	rval = list(statistic = tstat, parameter = df, p.value = pval,
		   conf.int = drop(cint), estimate = drop(estimate), null.value = mu,
		   alternative = alternative,
		   method = method, data.name = dname)
	class(rval) = "htest"
	
	return(rval)
	
} #end function


