getAll <- function(df){
  results<-list()
  fit <- lm(mpg ~ disp, data = df)
  x <- df$disp
  y <- predict(fit, data.frame(disp= x), type = "response")

  results$pars <- data.frame(intercept=coef(fit)[1],slope=coef(fit)[2])
  results$prediction <- data.frame(x,y)

  results
}

models <- mtcars %>% group_by(cyl) %>% do(m = getAll(.))

foo2<-function(d){
  d$m[[1]]$prediction
}
ddply(models,.(cyl),foo2)

foo <- function(l) {
  l
}
kk <-models %>% do(k=foo(.))




t(sapply(kk$k,function(l) l$m$prediction$x))




by_cyl <- group_by(mtcars, cyl)

by_cyl %>% summarize(cyl2=cyl)

models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))
summarise(models, coe = mod$coef[[1]])




coefficients <-models %>% do(data.frame(coef = coef(.$mod)[[1]], group = .[[1]],var = names(coef(.$mod))))

zz<- kk %>% summarize(gear,par=m$prediction)


summarise(kk,gear,prediction)

ff <- function(l) {
  print(l$m$prediction)
}
kk %>% do(ff(.))


by_cyl <- group_by(mtcars, cyl)
do(by_cyl, head(., 2))

models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))
models

models %>% do(data.frame(coef = coef(.$mod)))

models %>% do(data.frame(
  var = names(coef(.$mod)),
  coef(summary(.$mod)))
)






