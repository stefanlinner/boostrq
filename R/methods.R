#' estimated coefficients of boosting regression quantiles
#'
#' @param object object of class boostrq
#' @param which a subset of base-learners
#' @param aggregate a character specifying how to aggregate coefficients of
#' single base learners. The default returns the coefficient for the final number
#' of boosting iterations. "cumsum" returns a list with matrices (one per base-learner)
#' with the cumulative coefficients for all iterations. "none" returns a list of
#' matrices where the jth columns of the respective matrix contains coefficients
#' of the base-learner of the jth boosting iteration.v "sum_aggr" ...
#' @param ... additional arguments passed to callies
#'
#' @return coef extracts the regression coefficients of the fitted boostrq model.
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
#' coef(boosted.rq, aggregate = "cumsum")
#'
coef.boostrq <- function(object, which = NULL, aggregate = "sum", ...) {

  checkmate::assert_class(object, "boostrq")

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
#' fitted(boosted.rq)
#'
fitted.boostrq <- function(object, ...) {

  checkmate::assert_class(object, "boostrq")

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
#' boosted.rq
#'
print.boostrq <- function(x, ...){

  checkmate::assert_class(x, "boostrq")

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
#' residuals(boosted.rq)
#'
residuals.boostrq <- function(object, ...) {

  checkmate::assert_class(object, "boostrq")

  args <- list(...)

  if (length(args) > 0) {
    warning("Arguments ", paste(names(args), sep = ", "), " unknown")
  }

  object$resid()

}


#' Updating number of iterations
#'
#' @param x a boostrq object
#' @param i desired number of boosting iterations
#' @param return TRUE, if the result should be returned
#' @param ... additional arguments passed to callies
#'
#' @return a boostrq object with the updated number of iterations
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
#' boosted.rq[500]
#'
"[.boostrq" <- function(x, i, return = TRUE, ...) {

  checkmate::assert_class(x, "boostrq")
  checkmate::assert_logical(return, any.missing = FALSE, len = 1)

  x$subset(i)

  if(return) return(x)
  invisible(NULL)

}



#' Current number of iterations of boostrq
#'
#' @param object a boostrq object
#' @param ... additional arguments passed to callies
#'
#' @return current number of boosting iterations
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
#' mstop(boosted.rq)
#'
mstop.boostrq <- function(object, ...) {

  checkmate::assert_class(object, "boostrq")

  object$mstop()

}



#' Model predictions for boosting regression quantiles
#'
#' @param object a boostrq object
#' @param newdata a data.frame (or data.table) including all covariates contained in the
#' baselearners
#' @param which a subset of base-learners
#' @param aggregate a character specifying how to aggregate coefficients of
#' single base learners. The default returns the coefficient for the final number
#' of boosting iterations. "cumsum" returns a list with matrices (one per base-learner)
#' with the cumulative coefficients for all iterations. "none" returns a list of
#' matrices where the jth columns of the respective matrix contains coefficients
#' of the base-learner of the jth boosting iteration.
#' @param ... additional arguments passed to callies
#'
#' @return predictions for the new data
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
#' predict.data <- data.frame(hp = 165, cyl = 6, am = 1, wt = 3.125)
#'
#' predict(boosted.rq, newdata = predict.data)
#'
predict.boostrq <- function(object, newdata = NULL, which = NULL, aggregate = "sum", ...) {

  checkmate::assert_class(object, "boostrq")

  object$predict(newdata, which, aggregate)

}



#' Result summaries for a boostrq object
#'
#' @param object a boostrq object
#' @param ... additional arguments passed to callies
#'
#' @return result summaries for a boostrq object including the print-information,
#' estimated coefficients, and selection frequencies
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
#' summary(boosted.rq)
#'
summary.boostrq <- function(object, ...) {

  checkmate::assert_class(object, "boostrq")

  ret <- list(
    boostrq = object,
    coefs = object$coef(aggregate = "sum"),
    selection.freqs = object$selection.freqs()
  )

  class(ret) <- "summary.boostrq"
  ret

}


#' Print result summaries for a boostrq object
#'
#' @param x a summary.boostrq object
#' @param ... additional arguments passed to callies
#'
#' @return printing the result summaries for a boostrq object including the print-information,
#' estimated coefficients, and selection frequencies
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
#' summary(boosted.rq)
#'
print.summary.boostrq <- function(x, ...) {

  checkmate::assert_class(x, "summary.boostrq")

  print(x$boostrq)

  cat("Estimated coefficients:\n")
  print(x$coefs)
  cat("\n")

  cat("Selection frequencies:\n")
  print(x$selection.freqs)
  cat("\n")

}




#' Extract indices of selected base learners
#'
#' @param object a boostrq object
#' @param ... additional arguments passed to callies
#'
#' @return an index vector indicating the selected base learner in each iteration
#' @export
#'
#' @import stabs
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
#' selected(boosted.rq)
#'
selected.boostrq <- function(object, ...) {

  checkmate::assert_class(object, "boostrq")

  object$xselect()

}



#' Update and Re-fit a boostrq model
#'
#' @param object a boostrq object
#' @param risk string indicating how the empirical risk should be computed for each boosting iteration.
#' inbag leads to risks computed for the learning sample (i.e. observations with non-zero weights),
#' oobag to risks based on the out-of-bag (i.e. observations with non-zero oobagweights).
#' @param ... additional arguments passed to callies
#' @param weights (optional) a numeric vector indicating which weights to used in the fitting process
#' (default: all observations are equally weighted, with 1).
#' @param oobweights an additional vector of out-of-bag weights, which is used for the out-of-bag risk.
#'
#' @return a re-fitted boostrq model
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
#' update(
#' boosted.rq,
#' weights = c(rep(1, 30), 0, 0),
#' oobweights = c(rep(0, 30), 1,1),
#' risk = "oobag"
#' )
#'
update.boostrq <- function(object, weights, oobweights, risk, ...) {

  checkmate::assert_class(object, "boostrq")

  object$update(risk = risk, weights = weights, oobweights = oobweights)

}


#' Empirical Quantile Risk of boostrq Object
#'
#' @param object a boostrq object
#' @param ... additional arguments passed to callies
#'
#' @return numeric vector containing the respective empirical quantile risk of the
#' different boosting iterations.
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
#' risk(boosted.rq)
#'
risk.boostrq <- function(object, ...) {

  checkmate::assert_class(object, "boostrq")

  object$risk()

}
