#' Creates bootstrap samples
#'
#' \code{avbootstrap} creates bootstrap samples
#' @param qp output from quickpsy
#' @param bootstrap \code{'parametric'} performs parametric bootstrap;
#' \code{"nonparametric"} performs non-parametric bootstrap;
#' \code{"none"} does not perform bootstrap (default is \code{'parametric'}).
#' @param B number of bootstrap samples (default is 100 ONLY).
avbootstrap <- function(averages, ypred, bootstrap, B) {

  if (bootstrap == "parametric") averages$y <- ypred$y
  if (bootstrap != "parametric") averages$y <- averages$prob

  averages$prob <- NULL

  data.frame(sample = 1:B) %>%
    crossing(averages) %>%
    group_by(sample) %>%
    mutate(k = rbinom(n(), size = n, prob = y), prob = k / n) %>%
    dplyr::select(-y) %>%
    group_by(sample)


  # one_avbootstrap <- function(averages, ypred, bootstrap, B) {
  #
  #   if (bootstrap == "parametric") averages$ypred <- ypred$y
  #
  #   data.frame(sample = 1:B) %>%
  #     crossing(averages) %>%
  #     group_by(sample) %>%
  #     mutate(k = rbinom(n(), size = n, prob = ypred))
  # }
  #
  #
  # averages_n <- averages %>% nest_by(.key = "averages")
  # ypred_n <- ypred %>% nest_by(.key = "ypred")
  #
  # averages_n %>%
  #   left_join(ypred_n, by = group_vars(averages)) %>%
  #   summarise(one_avbootstrap(averages, ypred, bootstrap, B), .groups= "keep") %>%
  #   group_by(sample)

}


