library(dplyr)
library(ggplot2)
library(quickpsy)

dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)

dat$response[dat$response == 0] <- -1

fit <- quickpsy(dat, phase, response, within = .(speed,subject,cond))

fit <- quickpsy(dat, phase, response,  within = .(speed, cond))

fit <- quickpsy(dat, phase, response,  within = .(cond))

fit <- quickpsy(dat, phase, response)

