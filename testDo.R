library('dplyr')

mtcars %>% group_by(cyl) %>% summarise(m = mean(disp))

myfun <- function(d, groupvar, x) {
  groupvar <- deparse(substitute(groupvar))
 # x <- deparse(substitute(x))
  d %>% group_by_(groupvar) %>% summarise_(m = 'mean(x)')
}

myfun(mtcars, cyl, disp)



mtcars %>% group_by(cyl) %>% do(mod = lm(mpg ~ disp, data = .))

myfun <- function(d, groupvar, x, y) {
  d %>% group_by_(groupvar) %>% do(mod = lm(y ~ x, data = .))
}

myfun(mtcars, cyl, disp)


mtcars %>% group_by(cyl) %>% mutate(cyl2 = 2 * cyl)

myfun <- function(d, groupvar, x) {
  groupvar <- deparse(substitute(groupvar))
  exp <- lazy(x)
  d %>% mutate_(cyl2 = exp)
}
myfun(mtcars, cyl)


keep_min_n_by_species <- function(expr, n) {
  expr <- lazy(expr)

  formula <- interp(~rank(x, ties.method = 'random') <= y,
                    x = expr, y = n)

  iris %>%
    group_by(Species) %>%
    filter_(formula) %>%
    ungroup()
}

keep_min_n_by_species(Petal.Width, 3)
