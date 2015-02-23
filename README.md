<!-- README.md is generated from README.Rmd. Please edit that file -->



quickpsy fits psychometric functions for several conditions. The package is still under construction.

Install
-------

Install the package devtools

``` {.r}
install.packages("devtools")
```

and then

``` {.r}
devtools::install_github("danilinares/quickpsy")
```

Example
-------

``` {.r}
data(quickpsydat)
fit <- quickpsy(quickpsydat, FASE, RESP,
                between = .(ECC, INTERVAL, obs), B = 100)
plotcurves(fit)
plotpara(fit)
plotthresholds(fit)
```

Tutorials
---------

``` {.r}
browseVignettes('quickpsy')
```
