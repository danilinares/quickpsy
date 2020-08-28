#' Calculates the confidence intervals for the thresholds
#' \code{thresholdsci} calculates the confidence intervals for the thresholds
#' @keywords internal
#' @importFrom rlang .data
thresholdsci <- function(thresholds, thresholdsbootstrap, ci) {

  ci <- thresholdsbootstrap %>%
      group_by(!!!(groups(thresholds))) %>%
      summarise(threinf =  quantile(.data$thre, .5*(1 - ci)),
                thresup = quantile(.data$thre, 1 - .5*(1 - ci)), .groups = "keep")

  thresholds %>% left_join(ci, by = group_vars(thresholds))

}
