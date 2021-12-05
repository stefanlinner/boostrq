#' Stability Selection for boosting regression quantiles
#'
#' test
#'
#' @param x test
#' @param cutoff test
#' @param q test
#' @param PFER test
#' @param grid stest
#' @param B test
#' @param assumption test
#' @param sampling.type test
#' @param papply test
#' @param FWER test
#' @param verbose test
#' @param ... test
#'
#' @return test
#' @export
#'
#' @import stabs parallel
#'
#' @examples
#' test <- 1:3
#'
stabsel.boostrq <- function(
  x,
  cutoff,
  q,
  PFER,
  grid = 0:mstop(x),
  B = ifelse(sampling.type == "MB", 100, 50),
  assumption = "none",
  sampling.type = "MB",
  papply = mclapply,
  FWER,
  verbose = TRUE,
  ...
) {

  checkmate::assert_class(x, "boostrq")

  p <- length(x$baselearner.names)
  ibase <- seq_len(p)
  n <- length(x$resid())

  checkmate::assert_number(cutoff, lower = 0.5, upper = 1)
  checkmate::assert_int(q, lower = 0, upper = p)
  # assertion on PFER
  checkmate::assert_integerish(grid, lower = 0, any.missing = FALSE)
  checkmate::assert_int(B) # what else?
  checkmate::assert_choice(assumption, choices = c("unimodal", "r-concave", "none"))
  checkmate::assert_choice(sampling.type, choices = c("SS", "MB"))
  checkmate::assert_function(papply)

  if(sampling.type == "MB" & assumption != "none") {
    warning("Assumption was changed to none, as sampling.type = 'MB'")
    assumption <- "none"
  }

  k <- floor(n * 0.5)
  indx <- rep(c(0, 1), c(n - k, k))
  folds <- replicate(B, sample(indx))[sample(1:n),, drop = FALSE]

  pars <- stabs::stabsel_parameters(
    p = p,
    cutoff = cutoff,
    q = q,
    PFER = PFER,
    B = B,
    verbose = verbose,
    sampling.type = sampling.type,
    assumption = assumption,
    FWER = FWER
  )

  cutoff <- pars$cutoff
  q <- pars$q
  PFER <- pars$PFER

  fun <- function(model) {
    xs <- model$selected()
    qq <- sapply(seq_along(xs),
                 function(x) {
                   length(unique(xs[1:x]))
                 }
    )
    xs[qq > q] <- xs[1]
    xs
  }



}
