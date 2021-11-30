#' k-fold Crossvalidation for boostrq
#'
#' @param object a boostrq object
#' @param k number of folds, per default 5
#' @param grid a vetor of stopping parameters the empirical quantile risk is to be evaluated for.
#' @param mc.preschedule preschedule tasks if are parallelized using mclapply (default: FALSE)?
#' For details see mclapply.
#' @param papply (parallel) apply function, defaults to mclapply. To run sequentially
#' (i.e. not in parallel), one can use lapply.
#'
#' @return crossvalidation object
#' @export
#'
#' @import parallel
#'
#' @examples
#' boosted.rq <-
#' boostrq(
#'  formula = mpg ~ brq(cyl * hp) + brq(am + wt),
#'  data = mtcars,
#'  mstop = 200,
#'  nu = 0.1,
#'  tau = 0.5
#' )
#'
#' cvk.out <- cvkrisk(boosted.rq, k = 5, grid = 0:mstop(boosted.rq))
#'
cvkrisk <- function(object, k = 5, grid = 0:mstop(object), papply = mclapply, mc.preschedule = FALSE) {

  checkmate::assert_class(object, "boostrq")
  n <- length(object$resid())
  call <- deparse(object$call)

  checkmate::assert_int(k, lower = 1, upper = n)
  checkmate::assert_integerish(grid, lower = 0, any.missing = FALSE)
  checkmate::assert_function(papply)
  checkmate::assert_logical(mc.preschedule, any.missing = FALSE, len = 1)


  fl <- floor(n/k)

  folds <- c(rep(c(rep(0, fl), rep(1, n)), k - 1),
             rep(0, n * k - (k - 1) * (fl + n)))

  kfolds <- matrix(folds, nrow = n)[sample(1:n),, drop = FALSE]
  index.kfolds <- lapply(seq_len(k),
                         function(x){
                           which(kfolds[, x] == 1)
                         }
  )


  dummyfct <- function(index, risk) {
    mod <- object$update(index = index, risk = risk)
    mstop(mod) <- max(grid)

    mod$risk()[grid + 1]
  }

  if (identical(papply, mclapply)) {
    oobrisk <- papply(index.kfolds,
                      FUN = function(x) {
                        dummyfct(index = x, risk = "oobag")
                      },
                      mc.preschedule = mc.preschedule
    )
  } else {
    oobrisk <- papply(index.kfolds,
                      FUN = function(x) {
                        dummyfct(index = x, risk = "oobag")
                      }
    )
  }

  oobrisk <- matrix(unlist(oobrisk), nrow = length(grid), ncol = ncol(kfolds))
  oobrisk <- as.data.frame(oobrisk)
  rownames(oobrisk) <- grid
  colnames(oobrisk) <- 1:ncol(oobrisk)

  oobrisk.out <-
    list(
      mstop = grid,
      type = paste(k, "-fold Crossvalidation", sep = ""),
      call = call,
      cvdata = oobrisk
    )

  class(oobrisk.out) <- "cvkrisk"

  oobrisk.out


}


#' printing k-fold cross validation results of boosting regression quantiles
#'
#' @param x object of class boostrq
#' @param ... additional arguments passed to callies
#'
#' @return print shows a results of k-fold cross validation of boosting regression quantiles
#' @export
#'
#' @examples
#' boosted.rq <-
#' boostrq(
#'  formula = mpg ~ brq(cyl * hp) + brq(am + wt),
#'  data = mtcars,
#'  mstop = 200,
#'  nu = 0.1,
#'  tau = 0.5
#' )
#'
#' cvk.out <- cvkrisk(boosted.rq, k = 5, grid = 0:mstop(boosted.rq))
#'
#' cvk.out
#'
print.cvkrisk <- function(x, ...) {

  checkmate::assert_class(x, "cvkrisk")

  cat("\n\t Cross-validated boosting regression quantiles", "\n\t",
      x$call, "\n\n")
  print(rowMeans(x$cvdata, na.rm = TRUE))
  cat("\n\t Optimal number of boosting iterations:", mstop(x), "\n")
  invisible(x)

}


#' Optimal number of iterations determined by k-fold cross validation
#'
#' @param object object of class boostrq
#' @param ... additional arguments passed to callies
#'
#' @return Returns the optimal number of iterations for a boosting regression quantiles model
#' determined by k-fold cross validation.
#' @export
#'
#' @import mboost
#'
#' @examples
#' boosted.rq <-
#' boostrq(
#'  formula = mpg ~ brq(cyl * hp) + brq(am + wt),
#'  data = mtcars,
#'  mstop = 200,
#'  nu = 0.1,
#'  tau = 0.5
#' )
#'
#' cvk.out <- cvkrisk(boosted.rq, k = 5, grid = 0:mstop(boosted.rq))
#'
#' mstop(cvk.out)
#'
#' boosted.rq[mstop(cvk.out)]
#'
mstop.cvkrisk <- function(object, ...){

  checkmate::assert_class(object, "cvkrisk")

  object$mstop[which.min(rowSums(object$cvdata, na.rm = TRUE))]

}

