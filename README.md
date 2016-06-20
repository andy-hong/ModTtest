# modTtest
Implementation of the modified t-test proposed by Zwiers and von Storch 1995, J.Clim 8

Below is the logic model of the modified t-test.

![Logic Model](logic model.png)

Usage:
Include the following code in the main R code, and call modTtest function

```
source('modTtest.R')
source("calc_lag1Corr.R")
source("calc_EqvSamplSize_large.R")
source("tcrit_for_modTtest_small.R")
source("calc_poolSampVar.R")

```

Example of calling modTtest function:

```
modTtest(vector1, vector2, alternative="two.sided", paired=F)
```

