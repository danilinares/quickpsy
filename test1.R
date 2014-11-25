library(dplyr)
library(ggplot2)
library(quickpsy)

dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)

av <- dat %>% group_by(phase) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)
fit <- quickpsy(av, phase, nyes, ntrials,
                psy_fun_name = cum_normal_fun, pini=c(.5,.3))
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=fit$curve)
p

av <- dat %>% group_by(phase, subject) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)
fit <- quickpsy(av, phase, nyes, ntrials, random = subject,
                psy_fun_name = cum_normal_fun, pini=c(.5,.3))
p <- ggplot(data=av,aes(x=phase,y=y))+
  facet_wrap(~subject)+
  geom_point()+
  geom_line(data=fit$curve)
p

av <- dat %>% group_by(phase, subject, speed) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)
fit <- quickpsy(av, phase, nyes, ntrials, random = subject, within = speed,
                psy_fun_name = cum_normal_fun, pini=c(.5,.3))
p <- ggplot(data=av,aes(x=phase,y=y))+
  facet_grid(speed~subject)+
  geom_point()+
  geom_line(data=fit$curve)
p

av <- dat %>% group_by(phase, subject, speed, cond) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)
fit <- quickpsy(av, phase, nyes, ntrials,
                random = subject, within = speed, between = cond,
                psy_fun_name = cum_normal_fun, pini=c(.5,.3))
p <- ggplot(data=av,aes(x=phase,y=y, color = cond))+
  facet_grid(speed~subject)+
  geom_point()+
  geom_line(data=fit$curve)
p


