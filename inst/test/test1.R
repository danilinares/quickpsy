library(dplyr)
library(ggplot2)
library(quickpsy)
library(gridExtra)

data <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)

fit <- quickpsy(x = phase)

