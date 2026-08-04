#' Calculates the confidence intervals for the parameters
#' \code{parci} calculates the confidence intervals for the parameters
#' @keywords internal
#' @importFrom rlang .data
#' @importFrom stats quantile qnorm
parci <- function(par, hessian, ci) {

  ## Asymptotic standard errors from the inverse Hessian. When the Hessian is
  ## not positive-definite (a parameter resting on a bound, or a flat /
  ## ill-conditioned likelihood) the corresponding variance is non-positive or
  ## the matrix is singular; the SE is then genuinely undefined, so we return NA
  ## for that parameter rather than sqrt()-ing a negative number (which would
  ## emit a cryptic "NaNs produced" warning and leave NaN in the output).
  estimate_se_from_hessian <- function(hessian) {
    h <- hessian[[1]]
    n_par <- nrow(h)
    vcov <- tryCatch(solve(h), error = function(e) matrix(NA_real_, n_par, n_par))
    v  <- diag(vcov)
    se <- rep(NA_real_, n_par)
    ok <- is.finite(v) & v > 0
    se[ok] <- sqrt(v[ok])
    tibble(parn = paste0("p", seq_len(n_par)), se = se)
  }

  se_from_hessian <- hessian |>
    reframe(estimate_se_from_hessian(hessian))

  grp <- group_vars(par)

  out <- par |>
    left_join(se_from_hessian, by = c("parn", grp)) |>
    mutate(parinf = par - qnorm(0.5 + 0.5 * ci) * se,
           parsup = par + qnorm(0.5 + 0.5 * ci) * se) |>
    relocate(se, .after = last_col())

  ## One informative warning summarising every affected group, instead of a
  ## cryptic base-R warning per group.
  bad <- out |> ungroup() |> filter(is.na(.data$se))
  n_bad <- if (nrow(bad) == 0) 0L
           else if (length(grp) > 0) nrow(distinct(bad, across(all_of(grp))))
           else 1L
  if (n_bad > 0)
    warning(n_bad, if (n_bad == 1) " group has" else " groups have",
            " a Hessian that is not positive-definite (a parameter at a bound ",
            "or a flat/ill-conditioned likelihood); asymptotic confidence ",
            "intervals for ", if (n_bad == 1) "it" else "them", " are NA. ",
            "Use bootstrap = \"parametric\" to obtain confidence intervals for these.",
            call. = FALSE)

  out
}
