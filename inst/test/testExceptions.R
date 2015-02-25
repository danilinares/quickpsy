###############################################
library(dplyr)
library(ggplot2)
library(boot)
library(tidyr)

dat <- quickreadfiles('inst/extdata/linareslopezmolinerjohnston2007/',
                      obs = c('dl','ss','at'), exp = c('exp1'))

fit <- quickpsy(dat, FASE, RESP, bootstrap = 'asdfas')
fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL, obs), B = 10, lapses = T)
fit <- quickpsy(dat, FASE, RESP, random = .(INTERVAL, obs,EXC_F), lapses = T,
                bootstrap = 'none', DE = T)
