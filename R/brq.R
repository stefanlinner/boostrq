#' base learner for boosting linear regression quantiles
#'
#' Base-learner for linear quantile regression.
#'
#' @param ... one or more predictor variables.
#' @param method the algortihm used to fit the quantile regression, the default is set to "fn", referring to the Frisch-Newton inferior point method. For more details see the documentation of quantreg::rq.
#'
#'
#' @return brq returns a string, which is used to specifiy the formula in the fitting process.
#' @export
#'
#' @import checkmate
#'
#' @examples brq(hp:cyl, cyl*hp, method = "fn")
brq <- function(..., method = "fn") {
  checkmate::assert_character(method, len = 1, any.missing = FALSE)
  checkmate::assert_subset(method,
                choices = c("br", "fn", "pfn", "sfn", "fnc", "conquer", "ppro", "lasso"))


  bl <- as.list(match.call(expand.dots = FALSE))[2][[1]]

  bl <- sapply(bl,
               function(x) {
                 as.character(x)
               })


  ## HUHU ist das stabil?
  if (is.list(bl)) {
    bl.length <- sapply(bl, function(x) {
      length(x)
    })
    bl.multiple <- which(bl.length > 1)
    bl.single <- sapply(bl.multiple, function(x) {
      paste(bl[[x]][2], bl[[x]][1], bl[[x]][3], sep = "")
    })
    bl <- c(unlist(bl[-bl.multiple]), bl.single)
  }

  if (is.matrix(bl)) {
    bl <- apply(bl, MARGIN = 2, function(x) {
      paste(x[2], x[1], x[3], sep = "")
    })
  }

  checkmate::assert_vector(bl,
                strict = TRUE,
                any.missing = FALSE,
                all.missing = FALSE)
  checkmate::assert_character(bl, null.ok = FALSE)

  list(
    baselearner = "brq",
    formula = paste(bl, collapse = " + "),
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
#' brqss <- function(...){
#'
#' }
