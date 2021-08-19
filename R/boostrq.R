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

  assert_integer(mstop, lower = 1, len = 1)
  assert_numeric(nu, len = 1, upper = 1, lower = 0.00001)
  assert_numeric(offset, len = 1, upper = 0.99999, lower = 0.00001)
  assert_numeric(tau, len = 1, upper = 0.99999, lower = 0.00001)
  assert_data_frame(data, all.missing = FALSE)

  response <- all.vars(formula[[2]])
  assert_character(response, len = 1)

  if(any(is.na(data))){
    warning("Data contains missing values. Missing values are removed for each baselearner seperately. As a result, the number of observations may differ between the baselearner.\nConsider removing the missing values.")
  }

  data <- data[!is.na(data[, response]), ]
  y <- data[[response]]

  baselearner <-
    attr(terms(formula), "term.labels")


  baselearer.out <-
    lapply(baselearner,
           function(x){
             eval(parse(text = x))
           }
    )
  names(baselearer.out) <- baselearner


  baselearer.model.matrix <-
    lapply(baselearer.out,
           function(x){
             na.omit(
               model.matrix(
                 as.formula(
                   paste(response, "~", x[["formula"]])
                 ),
                 data = data)
             )
           }
    )
  names(baselearer.model.matrix) <- baselearner

  appearances <- vector("integer", length = mstop)

  bl.risk <- vector("numeric", length = length(baselearner))
  names(bl.risk) <- baselearner

  risk <- vector("numeric", length = mstop + 1)

  fit <- rep(quantile(y, offset), length(y))

  risk[1] <- quantile.risk(y = y, f = fit, tau = tau)

  ## HUHU eigene Funktion für boostrq.fit schreiben
  ## HUHU das hier vielleicht anpassen, dass wenn man mehr iterationen möchte nicht wieder von vorne beginnen muss
  ## Siehe auch subset Funktion
  for(m in seq_len(mstop)) {

    q.ngradient <- quantile.ngradient(y = y, f = fit, tau = tau)

    qr.res <-
      lapply(baselearner,
             function(x) {
               qreg <- rq.fit(y = q.ngradient, x = baselearer.model.matrix[[x]], tau = tau, method = baselearer.out[[x]][["method"]])
               bl.risk[x] <<- quantile.risk(y = q.ngradient, f = qreg$fitted.values, tau = tau)

               qreg
             }
      )
    names(qr.res) <- baselearner

    if(m == 1){
      coefpath <-
        lapply(baselearner,
               function(x){
                 coefpath.mat <- matrix(data = 0, ncol = mstop, nrow = length(qr.res[[x]]$coef))
                 rownames(coefpath.mat) <-  names(qr.res[[x]]$coef)
                 coefpath.mat
               }
        )
      names(coefpath) <- baselearner
    }

    if(sum(bl.risk == min(bl.risk)) > 1){
      warning(paste("Warning: There is no unique best base learner in iteration", m))
    }

    best.baselearner <- names(which.min(bl.risk))

    coefpath[[best.baselearner]][, m] <- qr.res[[best.baselearner]]$coef * nu

    if(all(abs(round(qr.res[[best.baselearner]]$coef[-1], 10)) > 0)){
      appearances[m] <- which.min(bl.risk)
    } else {
      appearances[m] <- 0
    }

    fit <- fit + qr.res[[best.baselearner]]$fitted.values * nu

    risk[m + 1] <- quantile.risk(y = y, f = fit, tau = tau)
  }

  RETURN <- list(formula = formula,
                 nu = nu,
                 offset = quantile(y, offset),
                 baselearner.names = baselearner,
                 call = match.call())

  RETURN$mstop <- function() mstop

  RETURN$xselect <- function() appearances

  RETURN$fitted <- function() fit

  RETURN$resid <- function() y - fit

  RETURN$risk <- function() {
    risk[mstop + 1]
  }

  RETURN$neg.gradients <- function() {
    quantile.ngradient(y, fit, tau)
  }

  RETURN$baselearner.matrix <- function(which = NULL) {

    assert_character(which, max.len = length(baselearner), null.ok = TRUE)
    assert_subset(which, choices = baselearner)

    if(is.null(which)){
      which <- baselearner
    }
    baselearer.model.matrix[which]
  }

  RETURN$coef <- function(which = NULL, aggregate = "sum") {

    assert_character(which, max.len = length(baselearner), null.ok = TRUE)
    assert_subset(which, choices = baselearner)
    assert_character(aggregate, len = 1)
    assert_subset(aggregate, choices = c("sum", "none", "cumsum"))


    if(is.null(which)){
      which <- baselearner
    }

    if(aggregate == "none"){
      coefpath.none <- coefpath[which]
      names(coefpath.none) <- which
      coefpath.none$offset <- quantile(y, offset)
      return(coefpath.none)
    }

    if(aggregate == "sum"){
      coefpath.sum <- lapply(coefpath[which],
                             function(x){
                               rowSums(x)
                             })
      names(coefpath.sum) <- which
      coefpath.sum$offset <- quantile(y, offset)
      return(coefpath.sum)
    }

    if(aggregate == "cumsum"){
      coefpath.cumsum <- lapply(coefpath[which],
                                function(x){
                                  apply(x, MARGIN = 1, FUN = cumsum)
                                })
      names(coefpath.cumsum) <- which
      coefpath.cumsum$offset <- quantile(y, offset)
      return(coefpath.cumsum)
    }

  }

  RETURN$predict <- function(newdata = NULL, which = NULL, aggregate = "sum"){

    assert_character(which, max.len = length(baselearner), null.ok = TRUE)
    assert_subset(which, choices = baselearner)
    assert_character(aggregate, len = 1)
    assert_subset(aggregate, choices = c("sum", "none", "cumsum"))
    assert_data_frame(newdata, min.rows = 1, ncols = length(data)
                      #, col.names = names(data)
    )

    if(is.null(which)){
      which <- baselearner
    }

    newdata.model.matrix <-
      lapply(baselearer.out[which],
             function(x){
               na.omit(
                 model.matrix(
                   as.formula(
                     paste(response, "~", x[["formula"]])
                   ),
                   model.frame(
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

      predictions <- Reduce('+', bl.predictions) + quantile(y, offset)
      return(predictions)
    }

    ### HUHU add "cumsum" and "none"
    # if(aggregate == "cumsum")
    # if(aggregate == "none")

  }




  # RETURN$subset <- function()

  class(RETURN) <- "boostrq"

  RETURN

}

