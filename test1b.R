library(dplyr)
library(ggplot2)

ntrials <- 100
intensity <- c(.2, .4, .6, .8, 1) #luminance
nyes <- c(10, 26, 73, 94, 97) # number of times that the observer reports that can see the stimulus
y <- nyes/n
dat <- data.frame(x, intensity, nyes, ntrials)

cum_normal_fun <- function(x, p) pnorm(x, p[1], p[2])

create_psy_fun <- function(guess, lapses, fun) {
  function(x,p) guess + (1 - guess) * fun(x,p)

}

create_nll <- function(d, x, k, n, psy_fun){
  function(p) {
    phi <- psy_fun(d[[x]], p)
    -sum(d[[k]] * log(phi) + (d[[n]] - d[[k]]) * log(1 - phi))
  }
}

fit_main <- function(d, x, k, n, guess, lapses, fun, pini) {
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  n <- deparse(substitute(n))
  psy_fun <- create_psy_fun(guess,lapses,fun)
  nll <- create_nll(d, x, k, n, psy_fun)
  optim(pini, nll)$p
}

para<-fit_main(dat, intensity, nyes, ntrials, .25, .1, cum_normal_fun,c(.5,.5))
para


para<-fit_main(dat, intensity, nyes, ntrials, .25, lapses=F, cum_normal_fun)
para




psy_fun <- create_psy_fun(.25,F,cum_normal_fun)
nll <- create_nll(dat,intensity,nyes,ntrials,psy_fun)

para <- optim(c(.7, .7), nll)$p
para

xseq <- seq(0, 1, len = 100)
yseq <- psy_fun(xseq, para)
curve <- data.frame(x=xseq,y=yseq)


p <- ggplot(data=dat,aes(x=x,y=y))+
  geom_point()+
  geom_line(data=curve)
p




psy_fun <- function(x,p) {
  print(tail(p))
  (1 - tail(p)) * cum_normal_fun(x,head(p,-1))
}
psy_fun(.5, c(.25,.5,.1))



xseq <- seq(0, 1, len = 100)
yseq <- psy_fun(xseq, c(.25,.5,.1))
curve <- data.frame(x=xseq,y=yseq)


p <- ggplot(data=dat,aes(x=x,y=y))+
  geom_point()+
  geom_line(data=curve)
p


tmpfun <- function(a,b,...) {
 # print(as.list(match.call()))
  print(as.list(match.call(expand.dots=FALSE)))
}




fun_sq <- function(x) x^2

create_psychometric <- function(f, guess, lapses){
  function(x) {
    guess + (1 - lapses) * f
  }
}

zz<-create_psychometric(fun_sq,.5,.1)

dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)

av <- dat %>%
  group_by(phase) %>%
  summarise(n = n(), k = sum(response), r = n - k, y = k / n)

cum_normal <- function(x, para) pnorm(x, para[1], para[2])
psy_shapes <- c(cum_normal) #put all the functions in a list

fit_main <- function(d, x, k, r, fun) { # fit_main is a functional as it takes a function as an argument
  neglikelihood <- function(p, d, x, k, r, fun){
    pr <- fun(d[[x]], p)

    -sum(d[[k]] * log(pr) + d[[r]] * log(1-pr))


  }

  create_nll <- function(d, x, k, r) {
    function(p) {
      pr <- pnorm(d[[x]], p)
      -sum(d[[k]] * log(pr) + d[[r]] * log(1-pr))
    }
  }

  nll <- create_nll(d)
  print(nll)

  para <- optim(c(1, 1), nll)$par # it is a mathematical functional
  xseq <- seq(-1, 3, len = 100)
  yseq <- fun(xseq, para)

  data.frame(xseq, yseq)
}

fit <- fit_main(av, 'phase', 'k', 'r', cum_normal)

plotav <- ggplot(av, aes(x = phase, y= y)) +
  geom_point()
 # geom_line(data=fit,aes(x=xseq,y=yseq))
plotav

fit_main <- function(d, x, k, r, fun) { # fit_main is a functional as it takes a function as an argument
  neglikelihood <- function(p, d, x, k, r, fun){
    pr <- fun(d[[x]], p)

    -sum(d[[k]] * log(pr) + d[[r]] * log(1-pr))
  }

  para <- optim(c(1, 1), neglikelihood, d = d, x = x, k = k, r = r, fun = fun)$par # it is a mathematical functional
  xseq <- seq(-1, 3, len = 100)
  yseq <- fun(xseq, para)

  data.frame(xseq, yseq)
}

