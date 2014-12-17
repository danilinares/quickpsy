library(dplyr)
library(ggplot2)
library(quickpsy)
library(gridExtra)

dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)

fit <- quickpsy(dat, phase, response, within=.(cond, speed, subject))

