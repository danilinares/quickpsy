#' Select models using the likelihood ratio test
#' @param loglik1 The \code{loglik} data frame from quickpsy for the first model.
#' @param loglik2 The \code{loglik} data frame from quickpsy for the second model.
#' @param alpha The significance level.
#' @export
#' @importFrom rlang .data
model_selection_lrt <- function(loglik1, loglik2, alpha = .05){

  loglik1 <- loglik1 %>% rename(loglik1 = .data$loglik, n_par1 = .data$n_par)
  loglik2 <- loglik2 %>% rename(loglik2 = .data$loglik, n_par2 = .data$n_par)


  if (is.null(groups(loglik1))) {
    loglik <- loglik1 %>%
      bind_cols(.data$loglik2)
  }
  else {
    loglik <- loglik1 %>%
      left_join(loglik2, by = group_vars(loglik1))
  }

  loglik %>%
    mutate(target = if_else(.data$n_par1 > .data$n_par2, "first", "second"),
           null = if_else(.data$n_par1 > .data$n_par2, "second", "first"),
           loglik_target = if_else(.data$target == "first", .data$loglik1, .data$loglik2),
           loglik_null = if_else(.data$n_par1 > .data$n_par2, .data$loglik2, .data$loglik1),
           deviance = -2 * (.data$loglik_null - .data$loglik_target),
           p.value =  pchisq(.data$deviance, abs(.data$n_par1 - .data$n_par2),
                             lower.tail = FALSE),
           best = if_else( (.data$p.value < alpha), .data$target, .data$null)) %>%
    select(-.data$target, -.data$null, -.data$loglik_target, -.data$loglik_null)

}

