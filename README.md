# modTtest
Implementation of the modified t-test proposed by Zwiers and von Storch 1995, J.Clim 8

![Logic Model](logic model.png)

Usage:
Include the following code in the main R code, and call modTtest function

```
## Load modified t-test functions -----------------------------
source(paste(MODTTEST,'modTtest.R',sep='/'))
source(paste(MODTTEST,"calc_lag1Corr.R",sep='/'))
source(paste(MODTTEST,"calc_EqvSamplSize_large.R",sep='/'))
source(paste(MODTTEST,"tcrit_for_modTtest_small.R",sep='/'))
source(paste(MODTTEST,"calc_poolSampVar.R",sep='/'))
## ------------------------------------------------------------
```

Example of calling modTtest function:

```
modTtest(vector1, vector2, alternative="two.sided", paired=F)
```

