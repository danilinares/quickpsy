library(dplyr)
library(lazyeval)

g <- function(d,x) data.frame(m = mean(d[[x]]), sta = sd(d[[x]]))

mtcars %>% group_by(NULL) %>% do(g(.,'mpg'))

mtcars %>% group_by(cyl) %>% do(z = g(.,'mpg'), z2 = g(.,'disp'))
.

f <- function(d, groupvar, x , y) {
  groupvar <- lazy(groupvar)
  x <- lazy(x)
  y <- lazy(y)
  d %>% group_by_(groupvar) %>%
    do(m = interp(~ mean(var), var = quote(x))
    #do(mod = lm(interp(quote(y ~ x), y = y, x = x), data = .))
}

f(mtcars, cyl, disp, mpg)

f$m[[1]]
