library(dplyr)
library(ggplot2)
library(quickpsy)

dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)
av <- dat %>% group_by(phase, subject, speed, cond) %>% summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)
fit <- quickpsy(av, phase, nyes, ntrials,within = .(speed,subject, cond))

av <- dat %>% group_by(phase, speed, cond) %>% summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)
fit <- quickpsy(av, phase, nyes, ntrials,  within = .(speed, cond))

av <- dat %>% group_by(phase, cond) %>% summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)
fit <- quickpsy(av, phase, nyes, ntrials,  within = .( cond))

