quantile.ngradient <- function(y, f, tau){

  ## HUHU hat das Auswirkungen auf Performance, wenn der check jede Iteration mitläuft?
  # assert_numeric(tau, len = 1, upper = 0.99999, lower = 0.00001)
  # assert_numeric(y, any.missing = FALSE)
  # assert_numeric(f, any.missing = FALSE)
  # assert_true(length(y) == length(f))

  tau * ((y - f) > 0) + (tau - 1) * ((y - f) <= 0)
}

quantile.risk <- function(y, f, tau){

  ## HUHU hat das Auswirkungen auf Performance, wenn der check jede Iteration mitläuft?
  # assert_numeric(tau, len = 1, upper = 0.99999, lower = 0.00001)
  # assert_numeric(y, any.missing = FALSE)
  # assert_numeric(f, any.missing = FALSE)
  # assert_true(length(y) == length(f))

  sum(tau * (y - f) * ((y - f) > 0) +
        (tau - 1) * (y - f) * ((y - f) <= 0))
}
