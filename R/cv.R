#' Crossvalidation for boostrq
#'
#' @param object a boostrq object
#' @param grid a vetor of stopping parameters the empirical quantile risk is to be evaluated for.
#' @param mc.preschedule preschedule tasks if are parallelized using mclapply (default: FALSE)?
#' For details see mclapply.
#' @param papply (parallel) apply function, defaults to mclapply. To run sequentially
#' (i.e. not in parallel), one can use lapply.
#' @param folds a matrix indicating the weights for the k resampling iterations
#' @param fun if fun is NULL, the out-of-sample risk is returned. fun, as a function of object, may
#' extract any other characteristic of the cross-validated models. These are returned as is.
#' @param ... additional arguments passed to callies
#'
#' @return Cross-validated Boosting regression quantiles
#' @export
#'
#' @import mboost parallel
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
#' set.seed(101)
#'
#' cvk.out <-
#' cvrisk(
#'  boosted.rq,
#'  grid = 0:mstop(boosted.rq),
#'  folds = mboost::cv(boosted.rq$weights, type = "kfold", B = 5)
#' )
#'
#' cvk.out
#'
#' plot(cvk.out)
#'
#' mstop(cvk.out)
#'
#' boosted.rq[mstop(cvk.out)]
#'
cvrisk.boostrq <-
  function(
    object,
    folds = mboost::cv(object$weights, type = "kfold"),
    grid = 0:mstop(object),
    papply = parallel::mclapply,
    mc.preschedule = FALSE,
    fun = NULL,
    ...
  ) {

    checkmate::assert_class(object, "boostrq")
    call <- deparse(object$call)
    weights <- object$weights
    n <- length(weights)

    if (any(weights == 0)){
      warning("zero weights")
    }

    checkmate::assert_matrix(folds, any.missing = FALSE, nrows = n)
    checkmate::assert_integerish(grid, lower = 0, any.missing = FALSE)
    checkmate::assert_function(papply)
    checkmate::assert_logical(mc.preschedule, any.missing = FALSE, len = 1)
    checkmate::assert_function(fun, null.ok = TRUE)

    if (is.null(fun)) {
      dummyfct <- function(weights, oobweights, risk) {
        mod <- object$update(weights = weights, oobweights = oobweights, risk = risk)
        mstop(mod) <- max(grid)

        mod$risk()[grid + 1]
      }
    } else {
      dummyfct <- function(weights, oobweights, risk) {
        mod <- object$update(weights = weights, oobweights = oobweights, risk = risk)
        mstop(mod) <- max(grid)

        fun(mod)
      }
    }

    oobweights <- matrix(rep(weights, ncol(folds)), ncol = ncol(folds))
    oobweights[folds > 0] <- 0

    if (identical(papply, parallel::mclapply)) {
      oobrisk <- papply(seq_len(ncol(folds)),
                        FUN = function(x) {
                          dummyfct(weights = folds[, x], oobweights = oobweights[, x], risk = "oobag")
                        },
                        mc.preschedule = mc.preschedule
      )
    } else {
      oobrisk <- papply(seq_len(ncol(folds)),
                        FUN = function(x) {
                          dummyfct(weights = folds[, x], oobweights = oobweights[, x], risk = "oobag")
                        }
      )
    }

    if (!is.null(fun)) {
      return(oobrisk)
    }

    oobrisk <- matrix(unlist(oobrisk), nrow = length(grid), ncol = ncol(folds))
    oobrisk <- t(as.data.frame(oobrisk))
    oobrisk <- oobrisk / colSums(oobweights)
    rownames(oobrisk) <- 1:nrow(oobrisk)
    colnames(oobrisk) <- grid

    attr(oobrisk, "risk") <- "Boosting Regression Quantiles"
    attr(oobrisk, "call") <- call
    attr(oobrisk, "mstop") <- grid
    attr(oobrisk, "type") <- ifelse(!is.null(attr(folds, "type")),
                                    attr(folds, "type"), "user-defined")
    class(oobrisk) <- "cvrisk"
    oobrisk

  }
