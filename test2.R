library(dplyr)
library(ggplot2)

dat <- read.table('inst/extdata/lopezmolinerlinares2006.txt', header = TRUE)

av <- dat %>%
  group_by(phase) %>%
  summarise(ntrials = n(), nyes = sum(response), y = nyes / ntrials)


fit_psy <- function(d, x, k, n, psy_fun, pini, guess, lapses) {
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  n <- deparse(substitute(n))
  para <- fit_main(d, x, k, n, psy_fun, pini)
  list(para = para,
       psy_fun = psy_fun)
}

fit<-fit_psy(av, phase, nyes, ntrials, cum_normal_fun, c(.5,.5))
xseq <- seq(0, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=av,aes(x=phase,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p


ntrials <- 100
intensity <- c(.2, .4, .6, .8, 1)
nyes <- c(10, 26, 73, 94, 97)
y <- nyes/ntrials
dat <- data.frame(intensity, y, nyes, ntrials)


cum_normal_fun2 <- function(x, p) .25 + (1-.25)*pnorm(x, p[1], p[2])
gauss_fun <- function(x,p) exp(-0.5*((x-p[2])/p[3])^2)/(1+exp(-p[1]))





fit<-fit_psy(dat, intensity, nyes, ntrials, cum_normal_fun, c(.5,.5))
xseq <- seq(0, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=dat,aes(x=intensity,y=y))+
  geom_point()+
  geom_line(data=curve,aes(x=x,y=y))
p


fit_psy_function <- function(d, x, k, n, psy_fun, pini, guess, lapses) {
  x <- deparse(substitute(x))
  k <- deparse(substitute(k))
  n <- deparse(substitute(n))
  if (class(psy_fun) == 'function') {
    para <- fit_main(d, x, k, n, psy_fun, pini)
  }
  para
}


#   if (class(psy_fun) == 'character') {
#     print(8)
#     psy_fun <- create_psy_fun(psy_fun,guess,lapses)
#     para <- fit_main(d, x, k, n, psy_fun, pini)
#
#   }

create_psy_fun <- function(psy_fun, guess, lapses) {
  if (psy_fun == 'cum_normal_fun') shape <- function(x,p) pnorm(x, p[1], p[2])
  function (x,p) {
    if (is.numeric(guess) && is.numeric(lapses)) {
      guess <- guess
      lapses <- lapses
      pshape <- p
    }
    if (is.logical(guess) && is.logical(lapses)){
      if (guess && lapses) {
        guess <- tail(p,2)[1]
        lapses <- tail(p,2)[2]
        pshape <- head(p,-2)
      }
    }
    return(guess + (1 - guess- lapses) * shape(x, pshape))
  }
}



xseq <- seq(0, 2, len = 100)
yseq <- fit$psy_fun(xseq, fit$para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=dat,aes(x=x,y=y))+
  geom_point()+
  geom_line(data=curve)
p


para<-fit_main(dat, intensity, nyes, ntrials, cum_normal_fun2, c(.5,.5))
para
xseq <- seq(0, 2, len = 100)
yseq <- cum_normal_fun2(xseq,para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=dat,aes(x=x,y=y))+
  geom_point()+
  geom_line(data=curve)
p

para<-fit_main(dat, intensity, nyes, ntrials, gauss_fun, c(1,0,1))
para
xseq <- seq(0, 2, len = 100)
yseq <- gauss_fun(xseq,para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=dat,aes(x=x,y=y))+
  geom_point()+
  geom_line(data=curve)
p



xseq <- seq(0, 10, len = 100)
yseq <- psy_fun(xseq, c(2,.5))
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=dat,aes(x=x,y=y))+
  geom_point()+
  geom_line(data=curve)
p



para<-fit_main(dat, intensity, nyes, ntrials, cum_normal_fun, c(.5,.5))
para

xseq <- seq(0, 2, len = 100)
yseq <- cum_normal_fun(xseq, para)
curve <- data.frame(x=xseq,y=yseq)
p <- ggplot(data=dat,aes(x=x,y=y))+
  geom_point()+
  geom_line(data=curve)
p

para<-fit_main(dat, intensity, nyes, ntrials, .25, lapses=F, cum_normal_fun)
para


psy_fun <- create_psy_fun(guess,lapses,fun)

create_psy_fun <- function(guess, lapses, fun) {
  if (!lapses) function(x,p) guess + (1 - guess) * fun(x,p)
  if (lapses) function(x,p) guess + (1 - guess - tail(p,1)) * fun(x,head(p,-1))
}

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

