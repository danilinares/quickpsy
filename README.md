<!-- README.md is generated from README.Rmd. Please edit that file -->



quickpsy is an R library developed by [Daniel Linares](http://www.dlinares.org/) and [Joan López-Moliner](http://www.ub.edu/viscagroup/joan/) to fit psychometric functions for several conditions and produce associated plots. In comparison with other R libraries that fit psychometric functions, such as [psyphy](http://cran.r-project.org/web/packages/psyphy/index.html) and [modelfree](http://personalpages.manchester.ac.uk/staff/d.h.foster/software-modelfree/latest/home), quickpsy was built with the idea of fitting and plotting many conditions as automatically as possible. To that end, it depends heavily on Hadley Wickham libraries [ggplot2](http://ggplot2.org/) and [dplyr](http://cran.r-project.org/web/packages/dplyr/index.html).

To understand the fundamentals of fitting psychometric functions in R, we recommend the book [Modeling Psychophysical Data in R](http://www.springer.com/gp/book/9781461444749).

Install
-------

First, you need to download and install [R](http://cran.rstudio.com) (we also recommend [Rstudio](http://www.rstudio.com/)).

Then, you need to install the package boot, DEoptim, devtools, dplyr, ggplot2 and tidyr. For example, to install devtools

``` {.r}
install.packages('devtools')
```

and then install quickpsy from github

``` {.r}
library(devtools)
install_github('danilinares/quickpsy', build_vignettes = T)
```

The vignettes, which have extra tutorials, take a while to be built. If you don't want to build them use `build_vignettes = F` in the command above.

Example
-------

``` {.r}
library(boot)
library(DEoptim)
library(dplyr)
library(ggplot2)
library(tidyr)

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

You can find further guidande in the vignettes (if you built them)

``` {.r}
browseVignettes('quickpsy')
```
