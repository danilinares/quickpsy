#' Creates the negative log-likelihood function
#' \code{create_nll} Creates the negative log-likelihood function
#' @keywords internal
create_nll <- function(averages, psych_fun, x_str,  binomial_coef, grouping_fun) {

  psych_fun <- psych_fun # without this there is a data mask error

  averages_l <- averages %>% group_split(!!!syms(grouping_fun))

  function(p) {

    phi <- mapply(function(av, funct) funct(av[[x_str]], p),
                  averages_l, psych_fun$fun, SIMPLIFY = FALSE) %>%
      unlist()

      phi[phi < .Machine$double.eps] <- .Machine$double.eps
      phi[phi > (1 - .Machine$double.eps)] <- 1 - .Machine$double.eps

      if (binomial_coef) {
        -sum(lchoose(averages$n, averages$k) + averages$k * log(phi) + (averages$n - averages$k) * log(1 - phi))
      }
      else {
        -sum(averages$k * log(phi) + (averages$n - averages$k) * log(1 - phi))
      }

    }
}


