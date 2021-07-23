#' boosting regression quantiles
#'
#' @param y independent variable, as vector
#' @param X covariate matrix including an intercept in the first column, as matrix
#' @param mstop number of iterations, as integer
#' @param nu learning rate, as numeric
#' @param tau quantile parameter, as numeric
#' @param offset quantile paramter used to initialize the algortihm
#' @param method the algortihm used to fit the quantile regression, the default is set to "fn", referring to the Frisch-Newton inferior point method. For more details see the documentation of quantreg::rq.
#'
#' @return coefficient estimastes, coefficient path, and appearances of the different covariates, as list
#' @export
#'
#' @examples boostrq(mpg ~ brq(hp:cyl, cyl*hp) + brq(am), data = mtcars, mstop = 200, nu = 0.1, tau = 0.5, offset = 0.5, method = "fn")
#'
#' @imports quantreg, checkmate
boostrq <- function(formula, data = NULL, mstop = 100, nu = 0.1, tau = 0.5, offset = 0.5, method = "fn") {

  mstop <- as.integer(mstop)

  assert_integer(mstop, lower = 1, len = 1)
  assert_numeric(nu, len = 1, upper = 1, lower = 0.00001)
  assert_numeric(offset, len = 1, upper = 0.99999, lower = 0.00001)
  assert_numeric(tau, len = 1, upper = 0.99999, lower = 0.00001)
  assert_character(method, len = 1)
  assert_subset(method, choices = c("br", "fn", "pfn", "sfn", "fnc", "conquer", "ppro", "lasso"))
  assert_data_frame(data)

  response <- all.vars(formula[[2]])
  assert_character(response, len = 1)

  if(any(is.na(data))){
    warning("Data contains missing values. Missing values are removed for each baselearner seperately. As a result, the number of observations may differ between the baselearner.\nConsider removing the missing values.")
  }

  data <- data[!is.na(data[, response]), ]
  y <- data[[response]]

  baselearner <-
    attr(terms(formula), "term.labels")

  baselearer.model.matrix <-
    lapply(baselearner,
           ## HUHU .X ist gefährlich, wenn in Datensatzvariable so benannt ist
           function(.X){
             eval(parse(text = .X))
           }
    )
  names(baselearer.model.matrix) <- baselearner

  appearances <- vector("integer", length = mstop)

  risk <- vector("numeric", length = length(baselearner))
  names(risk) <- baselearner


  fit <- rep(quantile(y, offset), length(y))


  ## HUHU das hier vielleicht anpassen, dass wenn man mehr iterationen möchte nicht wieder von vorne beginnen muss
  ## Siehe auch subset Funktion
  for(m in seq_len(mstop)) {

    q.ngradient <- ngradient(y = y, f = fit, tau = tau)

    qr.res <-
      lapply(baselearner,
             function(x) {
               qreg <- rq.fit(y = q.ngradient, x = baselearer.model.matrix[[x]], tau = tau, method = method)
               risk[x] <<- loss(y = q.ngradient, f = qreg$fitted.values, tau = tau)

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

    if(sum(risk == min(risk)) > 1){
      warning(paste("Warning: There is no unique best base learner in iteration", m))
    }

    best.baselearner <- names(which.min(risk))

    coefpath[[best.baselearner]][, m] <- qr.res[[best.baselearner]]$coef * nu

    if(all(abs(round(qr.res[[best.baselearner]]$coef[-1], 10)) > 0)){
      appearances[m] <- which.min(risk)
    } else {
      appearances[m] <- 0
    }

    fit <- fit + qr.res[[best.baselearner]]$fitted.values * nu

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
    loss(y, fit, tau)
  }

  RETURN$neg.gradients <- function() {
    ngradient(y, fit, tau)
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

  # RETURN$predict <- function()
  # RETURN$subset <- function()

  class(RETURN) <- "boostrq"

  RETURN

}

