quantile.ngradient <- function(y, f, tau, exact.fit){

  if(exact.fit) {
    return(tau * ((y - f) > 0) + (tau - 1) * ((y - f) < 0))
  } else {
    return(tau * ((y - f) > 0) + (tau - 1) * ((y - f) <= 0))
  }

}

quantile.risk <- function(y, f, tau, weights){
  loss <- (tau * (y - f) * ((y - f) > 0) +
             (tau - 1) * (y - f) * ((y - f) <= 0))
  sum(loss * weights)
}
