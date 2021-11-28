#' base learner for boosting linear regression quantiles
#'
#' Base-learner for linear quantile regression.
#'
#' @param method the algortihm used to fit the quantile regression, the default
#' is set to "fn", referring to the Frisch-Newton inferior point method.
#' For more details see the documentation of quantreg::rq.
#'
#' @param formula  a symbolic description of the base learner.
#'
#' @return brq returns a string, which is used to specifiy the formula in the fitting process.
#' @export
#'
#' @examples brq(cyl * hp)
brq <- function(formula, method = "fn") {

  checkmate::assert_string(method)
  checkmate::assert_choice(method, choices = c("br", "fn", "pfn", "sfn", "fnc", "conquer", "ppro", "lasso"))

  bl <- as.list(match.call(expand.dots = FALSE))[2][[1]]
  bl <- deparse(bl)

  checkmate::assert_string(bl)

  list(
    baselearner = "brq",
    formula = bl,
    method = method
  )

}

#'
#' #' base learner for boosting nonlinear regression quantiles
#' #'
#' #' @param ...
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' brqss <- function(formula){
#'
#' }
