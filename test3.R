### Errors and inf in the nll function

library(dplyr)
library(ggplot2)
library(quickpsy)

dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)

av <- dat %>% group_by(phase, subject, cond, speed) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)

av1 <- filter(av, subject == 'jlm', cond == 'auditory', speed == 15)
fit <- quickpsy(av1, phase, nyes, ntrials, psy_fun = logistic_fun, guess=T, lapses=T)

p <- ggplot(data=av1,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=fit$curve) +
  theme()
p

