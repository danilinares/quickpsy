#' Pair comparisons of the parameters using bootstrap
#' \code{parcomparisons} Calculates the bootstrap confidence intervals for the
#' difference in the parameters for two groups for all possible pairs
#' of groups
#' @keywords internal
parcomparisons <- function(par_dif, par_difbootstrap, ci) {

  ci <- par_difbootstrap %>%
    group_by(!!!syms(setdiff(names(par_dif), c("par", "par2", "dif")))) %>%
    summarise(difinf = quantile(dif, .5*(1 - ci))[[1]],
              difsup = quantile(dif, 1 - .5*(1 - ci))[[1]],
              signif = ifelse(difinf * difsup < 0, "", "*"), .groups = "keep")

  par_dif %>% left_join(ci, by = setdiff(names(par_dif), c("par", "par2", "dif")))
}
