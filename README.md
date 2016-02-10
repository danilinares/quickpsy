<!-- README.md is generated from README.Rmd. Please edit that file -->
quickpsy is an R package developed by [Daniel Linares](http://www.dlinares.org) and [Joan López-Moliner](http://www.ub.edu/viscagroup/joan/) to quickly fit and plot psychometric functions for multiple conditions. It makes an extensive use of Hadley Wickham's packages [ggplot2](http://ggplot2.org/) and [dplyr](http://cran.r-project.org/web/packages/dplyr/index.html).

To understand the fundamentals of fitting psychometric functions in R, we recommend the book [Modeling Psychophysical Data in R](http://www.springer.com/gp/book/9781461444749).

Features
--------

-   Fits and plots multiple conditions with minimal coding.

-   Exploits the computational speed of [dplyr](http://cran.r-project.org/web/packages/dplyr/index.html).

-   The user does not need to introduce initial parameters.

-   Calculates parametric and non-parametric bootstrap confidence intervals.

-   Compares parameters and thresholds for different conditions using bootstrap.

-   Guess and lapses can be fixed or free as parameters.

-   Fits cumulative normal, logistic, weibull functions or any function defined by the user.

-   Facilitates the reading of several data files.

Install
-------

Download and install [R](http://cran.rstudio.com) (we also recommend [Rstudio](http://www.rstudio.com/)).

In R, install the following packages: boot, DEoptim, dplyr, ggplot2, tidyr and devtools.

``` r
install.packages('boot')
install.packages('DEoptim')
install.packages('tidyr')
install.packages('devtools')
```

Quickpsy can be installed from CRAN

``` r
install.packages('quickpsy')
```

To install the latest developed version, you can install quickpsy from github (which will also install dplyr and ggplot2)

``` r
library(devtools)
install_github('danilinares/quickpsy')
```

Example
-------

``` r
library(quickpsy)
library(MPDiR) # contains the Vernier data; use ?Venier for the reference

fit <- quickpsy(Vernier, Phaseshift, NumUpward, N, 
                grouping = .(Direction, WaveForm, TempFreq))
plotcurves(fit)
```

![plots of vernier fits.](vernierFitPlot.png)

``` r
plotpar(fit) #plot the parameters
```

![params of vernier fits](vernierParamsPlot.png)

``` r
plotthresholds(fit)
```

![thresholds of vernier fits](vernierThreshesPlot.png)

Help
----

To obtain information and examples for specific functions use *?*

``` r
?plotcurves
```

For more examples and information visit [www.dlinares.org/quickpsy.html](http://www.dlinares.org/quickpsy.html)

Other R packages
----------------

-   [psyphy](http://cran.r-project.org/web/packages/psyphy/index.html): among other things, it provides links functions to [fit psychometric functions using an approach based on generalized linear models](http://www.dlinares.org/psychopract.html#fitting-using-generalized-linear-models).

-   [modelfree](http://personalpages.manchester.ac.uk/staff/d.h.foster/software-modelfree/latest/home): fits psychometric functions using a non-parametric approach.
