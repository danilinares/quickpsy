### From Modeling Psychophysical data in R
library(MPDiR)
library(dplyr)
library(quickpsy)

threprob <- .6
data(HSP)
names(HSP) <- c('Quanta', 'PerCent', 'N', 'Obs', 'Run')
HSP <- HSP %>% mutate(NumYes = round(N * PerCent / 100), NumNo = N- NumYes,
                      logQ = log(Quanta))
SHR1 <- HSP %>% filter(Obs == 'SH', Run == 'R1')

### glm
SHR1.glm <- glm(cbind(NumYes, NumNo) ~ log(Quanta), data = SHR1,
                family = binomial(probit))
xseq <- seq(20, 450, len = 100)
yseq <- predict(SHR1.glm, newdata = data.frame(Quanta = xseq), type = 'response')
curve.glm <- data.frame(xseq, yseq)

m <- -coef(SHR1.glm)[[1]] / coef(SHR1.glm)[[2]]
sta <- 1 / coef(SHR1.glm)[[2]]
thre <- exp(qnorm(threprob, m, sta))

### quickpsy
fit <- quickpsy(SHR1, Quanta, NumYes, N, log = T, xmin = 20, xmax = 450,
                threprob = threprob)

ggplot() +
  geom_point(data = SHR1, aes(x = Quanta, y = PerCent / 100))+
  geom_line(data = curve.glm, aes(x = xseq, y = yseq)) +
  geom_line(data = fit$curve, aes(x = Quanta, y = y), color = 'red', lty = 2) +
  geom_linerange(data = data.frame(thre), aes(x= thre, ymin = 0, ymax = threprob)) +
  geom_linerange(data = data.frame(thre = fit$thre$thre),
                 aes(x= thre, ymin = 0, ymax = threprob), color = 'red', lty = 2)


