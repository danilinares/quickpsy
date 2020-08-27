#' Pair comparisons of the thresholds using bootstrap
#' \code{thresholdscomparisons} Calculates the bootstrap confidence intervals for the
#' difference in the thresholds for two groups for all possible pairs
#' of groups
#' @keywords internal
#' @param thresholds_dif The \code{thresholds_dif} data frame from quickpsy.
#' @param thresholds_difbootstrap The \code{thresholds_difbootstrap} data frame from quickpsy.
#' @param ci Bootstrap confidence intervals level based on percentiles (default is .95).
#' @importFrom rlang .data
thresholdcomparisons <- function(thresholds_dif, thresholds_difbootstrap, ci) {

  ci <- thresholds_difbootstrap %>%
    group_by(!!!syms(setdiff(names(thresholds_dif),
                             c("thre", "thre2", "dif")))) %>%
    summarise(difinf = quantile(.data$dif, .5 * (1 - ci))[[1]],
              difsup = quantile(.data$dif, 1 - .5 * (1 - ci))[[1]],
              signif = ifelse(.data$difinf * .data$difsup < 0, "", "*"), .groups = "keep")


  thresholds_dif %>% left_join(ci, by = setdiff(names(thresholds_dif), c("thre", "thre2", "dif")))
}
