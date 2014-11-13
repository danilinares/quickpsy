create_nll <- function(d, x, k, n, psy_fun){
  function(p) {
    phi <- psy_fun(d[[x]], p)
    -sum(d[[k]] * log(phi) + (d[[n]] - d[[k]]) * log(1 - phi))
  }
}
