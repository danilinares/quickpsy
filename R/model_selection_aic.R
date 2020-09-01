#' Select models using the Akaike Information Criterion
#'
#' The \code{p} indicates the relative probability of the models.
#' @param aic1 The \code{aic1} data frame from quickpsy for the first model.
#' @param aic2 The \code{aic2} data frame from quickpsy for the second model.
#' @export
#' @importFrom rlang .data
model_selection_aic <- function(aic1, aic2){

  aic1 <- aic1 %>% rename(n_par1 = .data$n_par, aic1 = .data$aic)
  aic2 <- aic2 %>% rename(n_par2 = .data$n_par, aic2 = .data$aic)

  if (length(group_vars(aic1)) > 0)
    aics <- aic1 %>%
    left_join(aic2, by = group_vars(aic1))
  else
    aics <- aic1 %>%
    bind_cols(aic2)

  aics %>%
    mutate(p = exp(-(aic1 - aic2)),
           best = if_else(aic1 < aic2, "first", "second"))

}
