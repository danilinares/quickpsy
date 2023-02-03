#' Calculates the confidence intervals for the parameters
#' \code{parci} calculates the confidence intervals for the parameters
#' @keywords internal
#' @importFrom rlang .data
#' @importFrom stats quantile
parci <- function(par, hessian, ci) {

  estimate_se_from_hessian <- function(hessian) {
    fisher_info <- solve(hessian[[1]])
    se <- sqrt(diag(fisher_info))

    tibble(parn = paste0("p", seq(1, length(se))), se = se)
  }

  se_from_hessian <- hessian |>
    summarise(estimate_se_from_hessian(hessian),
              .groups = "keep")

  par |>
    left_join(se_from_hessian, by = c("parn", group_vars(par))) |>
    mutate(parinf = par - qnorm(0.5 + 0.5 * ci) * se,
           parsup = par + qnorm(0.5 + 0.5 * ci) * se) |>
    relocate(se, .after = last_col())

}

