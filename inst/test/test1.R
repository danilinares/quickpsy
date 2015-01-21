###############################################
library(dplyr)
library(ggplot2)
library(quickpsy)

data <- qreadfiles('inst/extdata/linareslopezmolinerjohnston2007/',
                   obs = c('dl','ss','at'), exp = c('exp1'))

### no groups
fit <- quickpsy(data, FASE, RESP)
plotcurves(fit)
plotthresholds(fit)

### 1 group
fit <- quickpsy(data, FASE, RESP, random = .(EXC_F))
plotcurves(fit)
plotthresholds(fit)
plotthresholds(fit, x = EXC_F)
plotthresholds(fit, color = EXC_F)

### 2 groups
fit <- quickpsy(data, FASE, RESP, random = .(EXC_F, obs))
plotcurves(fit)
plotcurves(fit, ypanel = EXC_F)
plotcurves(fit, xpanel = EXC_F)
plotthresholds(fit)
plotthresholds(fit, x = obs)
plotthresholds(fit, x = EXC_F, geom = 'point')

### 3 groups
fit <- quickpsy(data, FASE, RESP, random = .(EXC_F, obs, INTERVAL))
plotcurves(fit)
plotcurves(fit, ypanel = EXC_F)
plotcurves(fit, xpanel = EXC_F)
plotcurves(fit, color = EXC_F)
plotcurves(fit, xpanel = obs, color = EXC_F)
plotthresholds(fit)
plotthresholds(fit, x = obs)
plotthresholds(fit, x = EXC_F, geom = 'point')
plotthresholds(fit, x = EXC_F, xpanel = INTERVAL, geom = 'point')
plotthresholds(fit, panel = EXC_F)

### NSE
fit <- quickpsy_(data, 'FASE', 'RESP', random = c('EXC_F', 'obs', 'INTERVAL'))
plotcurves(fit)
plotcurves(fit, ypanel = EXC_F)
plotcurves_(fit, ypanel = 'EXC_F')
plotthresholds(fit, x = EXC_F, geom = 'point')
plotthresholds_(fit, x = 'EXC_F', geom = 'point')

