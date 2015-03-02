<!-- README.md is generated from README.Rmd. Please edit that file -->



quickpsy fits psychometric functions for several conditions.

Install
-------

First, you need to download and install [R](http://cran.rstudio.com) (we also recommend [Rstudio](http://www.rstudio.com/)).

Then, you need to install the package boot, DEoptim, devtools, dplyr, ggplot2 and tidyr. For example, to install devtools

``` {.r}
install.packages('devtools')
```

and then to install quickpsy from github (the vignettes, which have extra tutorials, take a while to be built; if you don't want to build them \`build\_vignettes = F')

``` {.r}
devtools::install_github('danilinares/quickpsy', build_vignettes = T)
```

Example
-------

``` {.r}
library(MPDiR) # contains the Vernier data
data(Vernier) # ?Venier for the reference
fit <- quickpsy(Vernier, Phaseshift, NumUpward, N,
                grouping = .(Direction, WaveForm, TempFreq))
plotcurves(fit)
plotpara(fit)
plotthresholds(fit)
```

Help and tutorials
------------------

For specific functions

``` {.r}
?plotcurves
```

You can find further guidande in the vignettes

``` {.r}
browseVignettes('quickpsy')
```
