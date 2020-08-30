#' Select models using the likelihood ratio test
#' @param loglik1 The \code{loglik} data frame from quickpsy for the first model.
#' @param loglik2 The \code{loglik} data frame from quickpsy for the second model.
#' @param alpha The significance level.
#' @export
model_selection_lrt <- function(loglik1, loglik2, alpha = .05){

  loglik1 <- loglik1 %>% rename(loglik1 = loglik, n_par1 = n_par)
  loglik2 <- loglik2 %>% rename(loglik2 = loglik, n_par2 = n_par)


  if (is.null(groups(loglik1))) {
    loglik <- loglik1 %>%
      bind_cols(loglik2)
  }
  else {
    loglik <- loglik1 %>%
      left_join(loglik2, by = group_vars(loglik1))
  }

  loglik %>%
    mutate(target = if_else(n_par1 > n_par2, "first", "second"),
           null = if_else(n_par1 > n_par2, "second", "first"),
           loglik_target = if_else(target == "first", loglik1, loglik2),
           loglik_null = if_else(n_par1 > n_par2, loglik2, loglik1),
           deviance = -2 * (loglik_null - loglik_target),
           p.value =  pchisq(deviance, abs(n_par1 - n_par2),
                             lower.tail = FALSE),
           best = if_else( (p.value < alpha), target, null)) %>%
    select(-target, -null, -loglik_target, -loglik_null)

}

