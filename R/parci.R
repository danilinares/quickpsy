#' Calculates the confidence intervals for the parameters
#' \code{parci} calculates the confidence intervals for the parameters
#' @keywords internal
#' @importFrom rlang .data
#' @importFrom stats quantile
parci <- function(par, parbootstrap, ci) {
  ci <- parbootstrap %>%
    group_by(!!!(groups(par))) %>%
    group_by(.data$parn, .add = TRUE) %>%
    summarise(parinf =  quantile(par, .5*(1 - ci)),
              parsup = quantile(par, 1 - .5*(1 - ci)), .groups = "keep")

  par %>% left_join(ci, by = c(group_vars(par), "parn"))

}

