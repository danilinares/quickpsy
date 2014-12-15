### From Modeling Psychophysical data in R
library(MPDiR)
library(dplyr)
library(quickpsy)

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

### quickpsy
fit <- quickpsy(SHR1, Quanta, NumYes, N, log = T, xmin = 20, xmax = 450)

ggplot(SHR1, aes(x = Quanta, y = PerCent / 100)) +
  geom_point()+
  geom_line(data = curve.glm, aes(x = xseq, y = yseq))+
  geom_line(data = fit$curve, aes(x = Quanta, y = y), color = 'red', lty = 2)

