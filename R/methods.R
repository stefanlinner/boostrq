#' estimated coefficients of boosting regression quantiles
#'
#' @param object object of class boostrq
#' @param which a subset of base-learners
#' @param aggregate a character specifying how to aggregate coefficients of single base learners. The default returns the coefficient for the final number of boosting iterations. "cumsum" returns a list with matrices (one per base-learner) with the cumulative coefficients for all iterations. "none" returns a list of matrices where the jth columns of the respective matrix contains coefficients of the base-learner of the jth boosting iteration.
#' @param ... additional arguments passed to callies
#'
#' @return coef extracts the regression coefficients of the fitted boostrq model.
#' @export
#'
#' @import checkmate
#'
#' @examples model <- boostrq(mpg ~ brq(hp:cyl, cyl*hp) + brq(am), data = mtcars,
#' mstop = 200, nu = 0.1, tau = 0.5, offset = 0.5, method = "fn")
#' coef(model, aggregate = "cumsum")
coef.boostrq <- function(object, which = NULL, aggregate = "sum", ...) {

  assert_class(object, "boostrq")
  assert_character(aggregate, len = 1, any.missing = FALSE)
  assert_subset(aggregate, choices = c("sum", "cumsum", "none"), empty.ok = FALSE)

  args <- list(...)
  if (length(args) > 0) {
    warning("Arguments ", paste(names(args), sep = ", "), " unknown")
  }

  object$coef(which = which, aggregate = aggregate)

}

#' fitted values of boosting regression quantiles
#'
#' @param object object of class boostrq
#' @param ... additional arguments passed to callies
#'
#' @return fitted returns the fitted values of the fitted boostrq model.
#' @export
#'
#' @import checkmate
#'
#' @examples model <- boostrq(mpg ~ brq(hp:cyl, cyl*hp) + brq(am), data = mtcars,
#' mstop = 200, nu = 0.1, tau = 0.5, offset = 0.5, method = "fn")
#' fitted(model)
fitted.boostrq <- function(object, ...) {

  assert_class(object, "boostrq")

  args <- list(...)
  if (length(args) > 0) {
    warning("Arguments ", paste(names(args), sep = ", "), " unknown")
  }

  object$fitted()

}


#' printing boosting regression quantiles
#'
#' @param x object of class boostrq
#' @param ... additional arguments passed to callies
#'
#' @return print shows a dense representation of the boostrq model fit.
#' @export
#'
#' @import checkmate
#'
#' @examples model <- boostrq(mpg ~ brq(hp:cyl, cyl*hp) + brq(am), data = mtcars,
#' mstop = 200, nu = 0.1, tau = 0.5, offset = 0.5, method = "fn")
#' model
print.boostrq <- function(x, ...){

  assert_class(x, "boostrq")

  cat("\n")
  cat("\t Boosting Regression Qauntiles\n")
  cat("\n")

  cat("Call: ", deparse(x$call))
  cat("\n")

  cat("formula: ", deparse(x$formula), "\n\n", sep = "")
  cat("\n")

  cat("\t Quantile Regression\n")
  cat("Loss Function: tau * (y - f) * ((y - f) > 0) +
        (tau - 1) * (y - f) * ((y - f) <= 0)\n")
  cat("Negative Gradient: tau * ((y - f) > 0) + (tau - 1) * ((y - f) <= 0)\n\n")

  cat("Number of boosting iterations: mstop =", x$mstop(), "\n")
  cat("Step size: =", x$nu, "\n")
  cat("Offset: ", x$offset, "\n")
  cat("Number of baselearners: ", length(x$baselearner.names), "\n")
  cat("\n")
  invisible(x)

}

#' residuals of boosting regression quantiles
#'
#' @param object object of class boostrq
#' @param ... additional arguments passed to callies
#'
#' @return residuals returns the residuals of the fitted boostrq model.
#' @export
#'
#' @import checkmate
#'
#' @examples model <- boostrq(mpg ~ brq(hp:cyl, cyl*hp) + brq(am), data = mtcars,
#' mstop = 200, nu = 0.1, tau = 0.5, offset = 0.5, method = "fn")
#' residuals(model)
residuals.boostrq <- function(object, ...) {

  assert_class(object, "boostrq")

  args <- list(...)
  if (length(args) > 0) {
    warning("Arguments ", paste(names(args), sep = ", "), " unknown")
  }

  object$resid()

}




# "[.boostrq" <- function(x, i, return = TRUE, ...)
# predict.boostrq <- function(object, newdata = NULL, ...)
# summary.boostrq <- function(object, ...)
# print.summary.boostrq <- function(x, ...)



#' #' residuals of boosting regression quantiles
#' #'
#' #' @param object object of class boostrq
#' #' @param ... additional arguments passed to callies
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @import checkmate
#' #'
#' #' @examples model <- boostrq(mpg ~ brq(hp:cyl, cyl*hp) + brq(am), data = mtcars, mstop = 200, nu = 0.1, tau = 0.5, offset = 0.5, method = "fn")
#' #' resid(model)
#' resid.boostrq <- function(object, ...) {
#'
#'   assert_class(object, "boostrq")
#'
#'   args <- list(...)
#'   if (length(args) > 0) {
#'     warning("Arguments ", paste(names(args), sep = ", "), " unknown")
#'   }
#'
#'   object$resid()
#'
#' }



