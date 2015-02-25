###############################################
library(dplyr)
library(ggplot2)
library(boot)
library(tidyr)

dat <- quickreadfiles('inst/extdata/linareslopezmolinerjohnston2007/',
                   obs = c('dl','ss','at'), exp = c('exp1'))

### exceptions
fit <- quickpsy(dat, FASE, RESP, bootstrap = 'asdfas')
fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL, obs), B = 10, lapses = T)

### curves
fit <- quickpsy(dat, FASE, RESP)
plotcurves(fit)
plotcurves(fit, averages = T, curves = F, thresholds = F, ci = F)

fit <- quickpsy(dat, FASE, RESP, bootstrap = 'none')
plotcurves(fit)

fit <- quickpsy(dat, FASE, RESP, thresholds = F)
plotcurves(fit)

fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL), B = 10)
plotcurves(fit)
plotcurves(fit, color = INTERVAL)
plotcurves(fit, panel = INTERVAL)
plotcurves(fit, xpanel = INTERVAL)
plotcurves(fit, ypanel = INTERVAL)


fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL, obs), B = 10)
plotcurves(fit)
plotcurves(fit, color = INTERVAL)
plotcurves(fit, panel = INTERVAL)
plotcurves(fit, xpanel = INTERVAL)
plotcurves(fit, ypanel = INTERVAL)
plotcurves(fit, xpanel = INTERVAL, ypanel = obs)

fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL, obs, EXC_F), B = 10)
plotcurves(fit)
plotcurves(fit, color = INTERVAL)
plotcurves(fit, xpanel = INTERVAL)
plotcurves(fit, ypanel = INTERVAL)
plotcurves(fit, xpanel = INTERVAL, ypanel = obs)
plotcurves(fit, xpanel = INTERVAL, color = obs)


### parameters
fit <- quickpsy(dat, FASE, RESP)
plotpara(fit)
plotpara(fit, geom = 'point')

fit <- quickpsy(dat, FASE, RESP, lapses = T)
plotpara(fit)
plotpara(fit, geom = 'point')

fit <- quickpsy(dat, FASE, RESP, bootstrap = 'none')
plotpara(fit)

fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL), B=10)
plotpara(fit)
plotpara(fit, color = INTERVAL)
plotpara(fit, x = INTERVAL)
plotpara(fit, x = INTERVAL, geom = 'point')

fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL, obs), B = 10)
plotpara(fit)
plotpara(fit, color = obs)
plotpara(fit, color = INTERVAL)
plotpara(fit, x = obs)
plotpara(fit, x = INTERVAL)
plotpara(fit, color = obs, geom = 'point')
plotpara(fit, color = INTERVAL, , geom = 'point')
plotpara(fit, x = obs, , geom = 'point')
plotpara(fit, x = INTERVAL, , geom = 'point')

fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL, obs), B = 10, lapses = T,
                bootstrap = 'nonparametric')
plotpara(fit)
plotpara(fit, color = obs)
plotpara(fit, color = INTERVAL)
plotpara(fit, x = obs)
plotpara(fit, x = INTERVAL)
plotpara(fit, color = obs, geom = 'point')
plotpara(fit, color = INTERVAL, , geom = 'point')
plotpara(fit, x = obs, , geom = 'point')
plotpara(fit, x = INTERVAL, , geom = 'point')


fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL, obs,EXC_F), B = 40, lapses = T,
                bootstrap = 'nonparametric')
plotpara(fit)
plotpara(fit, xpanel = EXC_F)
plotpara(fit, color = obs)
plotpara(fit, color = INTERVAL)
plotpara(fit, x = obs)
plotpara(fit, x = INTERVAL)
plotpara(fit, color = obs, geom = 'point')
plotpara(fit, color = INTERVAL, , geom = 'point')
plotpara(fit, x = obs, , geom = 'point')
plotpara(fit, x = INTERVAL, , geom = 'point')

### thresholds
fit <- quickpsy(dat, FASE, RESP)
plotthresholds(fit)
plotthresholds(fit, geom = 'point')

fit <- quickpsy(dat, FASE, RESP, lapses = T)
plotthresholds(fit)
plotthresholds(fit, geom = 'point')

fit <- quickpsy(dat, FASE, RESP, bootstrap = 'none')
plotthresholds(fit)

fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL), B=10)
plotthresholds(fit)
plotthresholds(fit, color = INTERVAL)
plotpara(fit, x = INTERVAL)
plotpara(fit, x = INTERVAL, geom = 'point')

fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL, obs), B = 10)
plotthresholds(fit)
plotthresholds(fit, color = obs)
plotthresholds(fit, color = INTERVAL)
plotthresholds(fit, x = obs)
plotthresholds(fit, x = INTERVAL)
plotthresholds(fit, color = obs, geom = 'point')
plotthresholds(fit, color = INTERVAL, , geom = 'point')
plotthresholds(fit, x = obs, , geom = 'point')
plotthresholds(fit, x = INTERVAL, , geom = 'point')

fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL, obs, EXC_F), B = 10, lapses = T,
                bootstrap = 'nonparametric')
plotthresholds(fit)
plotthresholds(fit, xpanel = EXC_F)
plotthresholds(fit, ypanel = EXC_F)
plotthresholds(fit, panel = INTERVAL)
plotthresholds(fit, color = obs)
plotthresholds(fit, color = INTERVAL)
plotthresholds(fit, x = obs)
plotthresholds(fit, x = INTERVAL)
plotthresholds(fit, color = obs, geom = 'point')
plotthresholds(fit, color = INTERVAL, , geom = 'point')
plotthresholds(fit, x = obs, , geom = 'point')
plotthresholds(fit, x = INTERVAL, , geom = 'point')


### NSE
fit <- quickpsy_(dat, 'FASE', 'RESP', random = c('EXC_F', 'obs', 'INTERVAL'), B = 10)
plotcurves(fit)
plotcurves(fit, ypanel = EXC_F)
plotcurves_(fit, ypanel = 'EXC_F')
plotpara(fit, x = EXC_F, geom = 'point')
plotpara_(fit, x = 'EXC_F', geom = 'point')
plotthresholds(fit, x = EXC_F, geom = 'point')
plotthresholds_(fit, x = 'EXC_F', geom = 'point')


### different shapes
fit_cum_normal_fun <- quickpsy(dat, FASE, RESP, fun = cum_normal_fun)
fit_logistic_fun <- quickpsy(dat, FASE, RESP, fun = logistic_fun)
fit_weibull_fun <- quickpsy(dat, FASE, RESP, fun = weibull_fun)

cum <- fit_cum_normal_fun$curves %>% mutate(fun = 'cumulative normal')
logi <- fit_logistic_fun$curves %>% mutate(fun = 'logistic function')
wei <- fit_weibull_fun$curves %>% mutate(fun = 'weibull')

curv <- rbind(cum, logi, wei)

ggplot()+
  geom_line(data = curv, aes(x = x, y = y, color = fun)) +
  geom_point(data = fit_cum_normal_fun$averages, aes(x = FASE, y = y ))

### lapses and guesses
plotcurves(quickpsy(dat, FASE, RESP, guess = .25))
plotcurves(quickpsy(dat, FASE, RESP, lapses = .25))
plotcurves(quickpsy(dat, FASE, RESP, guess = .25, lapses = .25))
plotcurves(quickpsy(dat, FASE, RESP, guess = T, B=1))
plotcurves(quickpsy(dat, FASE, RESP, lapses = T))
plotcurves(quickpsy(dat, FASE, RESP, guess = T, lapses = T, B = 10))

fit <- quickpsy(dat, FASE, RESP, random = .(EXC_F, obs, INTERVAL),
                bootstrap = 'nonparametric', guess = T, B = 2)
plotcurves(fit)

