###############################################
library(dplyr)
library(ggplot2)

data <- qreadfiles('inst/extdata/linareslopezmolinerjohnston2007/',
                   obs = c('dl','ss','at'), exp = c('exp1'))

### no groups
fit <- quickpsy(data, FASE, RESP, B = 3)


plotcurves(fit)
plotpara(fit)
plotthresholds(fit)


### 1 group
fit <- quickpsy(data, FASE, RESP, random = .(EXC_F), B = 3)

plotcurves(fit)
plotthresholds(fit)
plotthresholds(fit, x = EXC_F)
plotthresholds(fit, color = EXC_F)
plotpara(fit, color = EXC_F)
b<-bootstrap(fit, B=30)

b %>% group_by(EXC_F, paran) %>% summarise( m = mean(para))

### 2 groups
fit <- quickpsy(data, FASE, RESP, random = .(EXC_F, obs), B = 10)
plotcurves(fit)
plotcurves(fit, ypanel = EXC_F)
plotcurves(fit, xpanel = EXC_F)
plotthresholds(fit)
plotthresholds(fit, x = obs)
plotthresholds(fit, x = EXC_F, geom = 'point')
plotpara(fit)
plotpara(fit, x=EXC_F, geom = 'point')

### 3 groups
fit <- quickpsy(data, FASE, RESP, random = .(EXC_F, obs, INTERVAL), B = 100)
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
plotpara(fit)
plotpara(fit, x = obs)
plotpara(fit, x = EXC_F, geom = 'point')
plotpara(fit, x = EXC_F, xpanel = INTERVAL, geom = 'point')

### NSE
fit <- quickpsy_(data, 'FASE', 'RESP', random = c('EXC_F', 'obs', 'INTERVAL'))
plotcurves(fit)
plotcurves(fit, ypanel = EXC_F)
plotcurves_(fit, ypanel = 'EXC_F')
plotthresholds(fit, x = EXC_F, geom = 'point')
plotthresholds_(fit, x = 'EXC_F', geom = 'point')


### different shapes
fit_cum_normal_fun <- quickpsy(data, FASE, RESP, fun = cum_normal_fun)
fit_logistic_fun <- quickpsy(data, FASE, RESP, fun = logistic_fun)
fit_weibull_fun <- quickpsy(data, FASE, RESP, fun = weibull_fun)

cum <- fit_cum_normal_fun$curves %>% mutate(fun = 'cumulative normal')
logi <- fit_logistic_fun$curves %>% mutate(fun = 'logistic function')
wei <- fit_weibull_fun$curves %>% mutate(fun = 'weibull')

curv <- rbind(cum, logi, wei)

ggplot()+
  geom_line(data = curv, aes(x = x, y = y, color = fun)) +
  geom_point(data = fit_cum_normal_fun$averages, aes(x = FASE, y = y ))

### lapses and guesses
fit <- quickpsy(data, FASE, RESP, guess = .25)
plotcurves(fit)
fit <- quickpsy(data, FASE, RESP, lapses = .25)
plotcurves(fit)
fit <- quickpsy(data, FASE, RESP, guess = .25, lapses = .25)
plotcurves(fit)
fit <- quickpsy(data, FASE, RESP, guess = T)
plotcurves(fit)
fit <- quickpsy(data, FASE, RESP, lapses = T)
plotcurves(fit)
fit <- quickpsy(data, FASE, RESP, guess = T, lapses = T)
plotcurves(fit)
fit <- quickpsy(data, FASE, RESP, random = .(EXC_F, obs, INTERVAL), guess = T)
plotcurves(fit)

