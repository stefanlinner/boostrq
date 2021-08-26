#' boosting regression quantiles
#'
#' Component-wise functional gradient boosting algorithm to fit a quantile regression model.
#'
#' @param mstop number of iterations, as integer
#' @param nu learning rate, as numeric
#' @param tau quantile parameter, as numeric
#' @param offset quantile paramter used to initialize the algortihm
#' @param formula a symbolic description of the model to be fit.
#' @param data a data frame containing the variables stated in the formula.
#'
#' @import quantreg checkmate
#' @importFrom stats terms as.formula model.matrix na.omit quantile model.frame
#'
#' @return A (generalized) additive quantile regression model is fitted using the boosting regression quantiles algorithm, which is a functional component-wise boosting algorithm.
#' The base-learner can be specified via the formula object. brq (linear quantile regression) and brqss(nonlinear quantile regression) are available base-learner.
#' @export
#'
#' @examples boostrq(mpg ~ brq(hp:cyl, cyl*hp) + brq(am), data = mtcars,
#' mstop = 200, nu = 0.1, tau = 0.5, offset = 0.5)
#'
boostrq <- function(formula, data = NULL, mstop = 100, nu = 0.1, tau = 0.5, offset = 0.5) {

  mstop <- as.integer(mstop)


  ### Asserting input parameters
  checkmate::assert_integer(mstop, lower = 1, len = 1)
  checkmate::assert_numeric(nu, len = 1, upper = 1, lower = 0.00001)
  checkmate::assert_numeric(offset, len = 1, upper = 0.99999, lower = 0.00001)
  checkmate::assert_numeric(tau, len = 1, upper = 0.99999, lower = 0.00001)
  checkmate::assert_data_frame(data, all.missing = FALSE)

  ### Getting response variable name
  response <- all.vars(formula[[2]])
  checkmate::assert_string(response)

  if(any(is.na(data))){
    warning("Data contains missing values. Missing values are removed for each baselearner seperately. As a result, the number of observations may differ between the baselearner.\nConsider removing the missing values.")
  }

  ### Removing observations, where response value is NA
  data <- data[!is.na(data[, response]), ]
  y <- data[[response]]

  ### Getting baselearner names
  baselearner <-
    attr(stats::terms(formula), "term.labels")


  ### Evaluating baselearner functions (brq() and brqss())
  baselearer.out <-
    lapply(baselearner,
           function(x){
             eval(parse(text = x))
           }
    )
  names(baselearer.out) <- baselearner


  ### Getting baselearner model matrices
  baselearer.model.matrix <-
    lapply(baselearer.out,
           function(x){
             stats::na.omit(
               stats::model.matrix(
                 stats::as.formula(
                   paste(response, "~", x[["formula"]])
                 ),
                 data = data)
             )
           }
    )
  names(baselearer.model.matrix) <- baselearner

  ### Setting up empty coefficient path matrix
  coefpath <-
    lapply(baselearner,
           function(x){
             coefpath.mat <- matrix(data = 0, nrow = mstop, ncol = ncol(baselearer.model.matrix[[x]]))
             colnames(coefpath.mat) <-  colnames(baselearer.model.matrix[[x]])
             coefpath.mat
           }
    )
  names(coefpath) <- baselearner


  ### Setting up empty appearances vector
  appearances <- vector("integer", length = mstop)

  ### Setting up empty bl.risk vector
  bl.risk <- vector("numeric", length = length(baselearner))
  names(bl.risk) <- baselearner

  ### Setting up empty empirical risk vector
  risk <- vector("numeric", length = mstop + 1)

  ### Defining intial fitted values
  fit <- rep(stats::quantile(y, offset), length(y))


  risk[1] <- quantile.risk(y = y, f = fit, tau = tau)


  ### Setting up counter variable
  count.m <- 0


  ### Setting up fitting function for boosting
  boostrq.fit <- function(niter){
    for(m in (count.m + 1):(count.m + niter)) {

      ### Determining current working residuals (negative gradients)
      q.ngradient <- quantile.ngradient(y = y, f = fit, tau = tau)

      ### Estimating quantile regression models and determining empirical quantile risk for each baselearner
      qr.res <-
        lapply(baselearner,
               function(x) {
                 qreg <- quantreg::rq.fit(y = q.ngradient, x = baselearer.model.matrix[[x]], tau = tau, method = baselearer.out[[x]][["method"]])
                 bl.risk[x] <<- quantile.risk(y = q.ngradient, f = qreg$fitted.values, tau = tau)

                 qreg
               }
        )
      names(qr.res) <- baselearner

      if(sum(bl.risk == min(bl.risk)) > 1){
        warning(paste("Warning: There is no unique best base learner in iteration", m))
      }

      ### Determining best fitting baselearner of current iteration
      best.baselearner <- names(which.min(bl.risk))

      ### Updating coefficient path
      coefpath[[best.baselearner]][m, ] <<- qr.res[[best.baselearner]]$coef * nu

      ### Determining the best fitting component
      ### HUHU: Denk nochmal über die Genze 10te Nachkommastelle nach...
      if(all(abs(round(qr.res[[best.baselearner]]$coef[-1], 10)) > 0)){
        appearances[m] <<- which.min(bl.risk)
      } else {
        appearances[m] <<- 0
      }

      ### Updating  fitted values
      fit <<- fit + qr.res[[best.baselearner]]$fitted.values * nu

      ### Updating empirical quantile risk
      risk[m + 1] <<- quantile.risk(y = y, f = fit, tau = tau)
    }

    ### Increasing iteration counter to current number of iterations (mstop)
    count.m <<- count.m + niter
  }

  ### Executing boostrq.fit
  if(mstop > 0) {
    boostrq.fit(mstop)
  }


  ### Defining rich output
  RETURN <- list(formula = formula,
                 nu = nu,
                 offset = stats::quantile(y, offset),
                 baselearner.names = baselearner,
                 call = match.call())

  ### Number of iterations run
  RETURN$mstop <- function() count.m

  ### Selected component in each iteration
  RETURN$xselect <- function() {
    appearances[1:count.m]
  }

  ### Current fitted values
  RETURN$fitted <- function() fit

  ### Current residuals
  RETURN$resid <- function() y - fit

  ### Current empirical quantile risk
  RETURN$risk <- function() {
    risk[1:(count.m + 1)]
  }

  ### Current working residuals (negative gradients)
  RETURN$neg.gradients <- function() {
    quantile.ngradient(y, fit, tau)
  }


  ### Underlying baselearner model matrices
  RETURN$baselearner.matrix <- function(which = NULL) {

    checkmate::assert_character(which, max.len = length(baselearner), null.ok = TRUE)
    checkmate::assert_subset(which, choices = baselearner)

    if(is.null(which)){
      which <- baselearner
    }
    baselearer.model.matrix[which]
  }

  ### Current coefficient estimates
  RETURN$coef <- function(which = NULL, aggregate = "sum") {

    checkmate::assert_character(which, max.len = length(baselearner), null.ok = TRUE)
    checkmate::assert_subset(which, choices = baselearner)
    checkmate::assert_character(aggregate, len = 1)
    checkmate::assert_choice(aggregate, choices = c("sum", "none", "cumsum"))


    if(is.null(which)){
      which <- baselearner
    }

    if(aggregate == "none"){
      coefpath.none <- lapply(which,
                              function(x){
                                coefpath[[x]][1:count.m, ]
                              }
      )
      names(coefpath.none) <- which
      coefpath.none$offset <- stats::quantile(y, offset)
      return(coefpath.none)
    }

    if(aggregate == "sum"){
      coefpath.sum <- lapply(which,
                             function(x){
                               colSums(coefpath[[x]][1:count.m, ])
                             })
      names(coefpath.sum) <- which
      coefpath.sum$offset <- stats::quantile(y, offset)
      return(coefpath.sum)
    }

    if(aggregate == "cumsum"){
      coefpath.cumsum <- lapply(which,
                                function(x){
                                  apply(coefpath[[x]][1:count.m, ], MARGIN = 2, FUN = cumsum)
                                })
      names(coefpath.cumsum) <- which
      coefpath.cumsum$offset <- stats::quantile(y, offset)
      return(coefpath.cumsum)
    }

  }

  ### Predicition function
  RETURN$predict <- function(newdata = NULL, which = NULL, aggregate = "sum"){

    checkmate::assert_character(which, max.len = length(baselearner), null.ok = TRUE)
    checkmate::assert_subset(which, choices = baselearner)
    checkmate::assert_character(aggregate, len = 1)
    checkmate::assert_choice(aggregate, choices = c("sum", "none", "cumsum"))
    ## HUHU: Überprüfe diese checks auf den data.frame, ist das wirklich was ich will?
    checkmate::assert_data_frame(newdata, min.rows = 1
                                 # , ncols = length(data), col.names = names(data)
    )

    if(is.null(which)){
      which <- baselearner
    }

    newdata.model.matrix <-
      lapply(baselearer.out[which],
             function(x){
               stats::na.omit(
                 stats::model.matrix(
                   stats::as.formula(
                     paste(response, "~", x[["formula"]])
                   ),
                   stats::model.frame(
                     ~ .,
                     data = newdata,
                     na.action = "na.pass")
                 )
               )
             }
      )
    names(newdata.model.matrix) <- which

    if(aggregate == "sum"){
      bl.predictions <-
        lapply(which,
               function(x){
                 newdata.model.matrix[[x]] %*% RETURN$coef(which = which, aggregate = aggregate)[[x]]
               }
        )

      predictions <- Reduce('+', bl.predictions) + stats::quantile(y, offset)
      return(predictions)
    }

    ### HUHU add "cumsum" and "none"
    # if(aggregate == "cumsum")
    # if(aggregate == "none")

  }


  ### Update to a new number of boosting iterations mstop (without refitting the whole model)
  RETURN$subset <- function(i) {
    i <- as.integer(i)
    checkmate::assert_integer(i, lower = 1, any.missing = FALSE, len = 1)

    if(i <= count.m || i <= length(appearances)) {
      if(i != count.m){
        count.m <<- i
        fit <<- RETURN$predict(newdata = data, which = NULL, aggregate = "sum")
      }
    } else {
      ### if prior reduction of count.m, first increase count.m to old value
      if(count.m != length(appearances)) {
        count.m <<- length(appearances)
        fit <<- RETURN$predict(newdata = data, which = NULL, aggregate = "sum")
      }
      coefpath <<-
        lapply(baselearner,
               function(x){
                 rbind(coefpath[[x]], matrix(0, nrow = i - count.m, ncol = ncol(coefpath[[x]])))
               }
        )
      names(coefpath) <<- baselearner
      boostrq.fit(i - count.m)
    }

  }


  RETURN$selection.freqs <- function() {
    freq.table <- table(RETURN$xselect()) / length(RETURN$xselect())
    ind <- as.numeric(names(freq.table)) + 1
    names(freq.table) <- c("Intercept", baselearner)[ind]
    freq.table
  }

  class(RETURN) <- "boostrq"

  RETURN

}

