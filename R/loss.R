quantile.ngradient <- function(y, f, tau){
  tau * ((y - f) > 0) + (tau - 1) * ((y - f) <= 0)
}

quantile.risk <- function(y, f, tau){
  sum(tau * (y - f) * ((y - f) > 0) +
        (tau - 1) * (y - f) * ((y - f) <= 0))
}
