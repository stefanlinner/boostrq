
#' k-fold Crossvalidation for boostrq
#'
#' @param object a boostrq object
#' @param k number of folds, per default 5
#' @param grid a vetor of stopping parameters the empirical quantile risk is to be evaluated for.
#'
#' @return
#' @export
#'
#' @examples
#'
cvkrisk.boostrq <- function(object, k = 5, grid = 0:mstop(object)) {

  n <- length(object$resid())

  checkmate::assert_class(object, "boostrq")
  checkmate::assert_int(k, lower = 1, upper = n)
  checkmate::assert_integerish(grid, lower = 0, any.missing = FALSE)

  fl <- floor(n/k)

  folds <- c(rep(c(rep(0, fl), rep(1, n)), k - 1),
             rep(0, n * k - (k - 1) * (fl + n)))

  kfolds <- matrix(folds, nrow = n)[sample(1:n),, drop = FALSE]

  fitfct <- object$update
  oobrisk <- matrix(0, nrow = ncol(folds), ncol = length(grid))


  dummyfct <- function(index, risk) {
    mod <- fitfct(index = index, risk)
    mstop(mod) <- max(grid)

    mod$risk()[grid + 1]
  }


}
