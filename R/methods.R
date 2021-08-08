#' estimated coefficients of boosting regression quantiles
#'
#' @param object
#' @param which
#' @param aggregate
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
#' @param object
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
fitted.boostrq <- function(object, ...) {

  assert_class(object, "boostrq")

  args <- list(...)
  if (length(args) > 0) {
    warning("Arguments ", paste(names(args), sep = ", "), " unknown")
  }

  object$fitted()

}

#' residuals of boosting regression quantiles
#'
#' @param object
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
resid.boostrq <- function(object, ...) {

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

#' printing boosting regression quantiles
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
#' @param object
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
residuals.boostrq <- function(object, ...) {

  assert_class(object, "boostrq")

  args <- list(...)
  if (length(args) > 0) {
    warning("Arguments ", paste(names(args), sep = ", "), " unknown")
  }

  object$resid()

}
