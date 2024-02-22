#' Fitting a boosting regression quantiles model
#'
#' Component-wise functional gradient boosting algorithm to fit a quantile
#' regression model.
#'
#' @param mstop number of iterations, as integer
#' @param nu learning rate, as numeric
#' @param tau quantile parameter, as numeric
#' @param offset a numeric vector used as offset.
#' @param formula a symbolic description of the model to be fit.
#' @param data a data frame (or data.table) containing the variables stated in the formula.
#' @param digits number of digits the slope parameter different from zero to be
#' considered the best-fitting component, as integer.
#' @param exact.fit logical, if set to TRUE the negative gradients of exact fits are set to 0.
#' @param weights (optional) a numeric vector indicating which weights to used in the fitting process
#' (default: all observations are equally weighted, with 1).
#' @param risk string indicating how the empirical risk should be computed for each boosting iteration.
#' inbag leads to risks computed for the learning sample (i.e. observations with non-zero weights),
#' oobag to risks based on the out-of-bag (i.e. observations with non-zero oobagweights).
#' @param oobweights an additional vector of out-of-bag weights, which is used for the out-of-bag risk.
#'
#' @return A (generalized) additive quantile regression model is fitted using
#' the boosting regression quantiles algorithm, which is a functional component-wise
#' boosting algorithm.
#' The base-learner can be specified via the formula object. brq (linear quantile regression)
#' and brqss(nonlinear quantile regression) are available base-learner.
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
#')
#'
#' boosted.rq$mstop()
#'
#' boosted.rq$selection.freqs()
#'
#' boosted.rq$coef()
#'
#' boosted.rq$risk()
#'
boostrq <-
  function(
    formula,
    data,
    mstop = 100,
    nu = NULL,
    tau = 0.5,
    offset = NULL,
    weights = NULL,
    oobweights = NULL,
    risk = "inbag",
    digits = 10,
    exact.fit = FALSE
  ) {

    ### Asserting input parameters
    checkmate::assert_formula(formula)
    checkmate::check_data_frame(data, any.missing = FALSE, min.rows = 1, min.cols = 2, col.names = "named")
    checkmate::assert_int(mstop, lower = 0)
    checkmate::assert_number(nu, upper = 1, lower = 0, null.ok = TRUE)
    checkmate::assert_number(tau, upper = 1, lower = 0)
    checkmate::assert_numeric(offset, len = nrow(data), null.ok = TRUE)
    checkmate::assert_numeric(weights, lower = 0, any.missing = FALSE, null.ok = TRUE, len = nrow(data))
    checkmate::assert_numeric(oobweights, lower = 0, any.missing = FALSE, null.ok = TRUE, len = nrow(data))
    checkmate::assert_string(risk)
    checkmate::assert_choice(risk, c("inbag", "oobag"))
    checkmate::assert_int(digits, lower = 1)
    checkmate::assert_logical(exact.fit, any.missing = FALSE, len = 1)

    if(is.null(nu)){
      nu <- -0.05 * abs(tau - 0.5) + 0.11
    }

    ### Getting response variable name
    response <- all.vars(formula[[2]])
    checkmate::assert_string(response)
    checkmate::assert_choice(response, choices = names(data))

    ### Checking response
    checkmate::assert_numeric(data[[response]], any.missing = FALSE, len = nrow(data))
    y <- eval(formula[[2]], data)

    ### Setting up weights
    if(is.null(weights)){
      weights <- rep(1, nrow(data))
    }

    if(is.null(oobweights)){
      oobweights <- as.numeric(weights == 0)
    }

    ### Checking weights and oobweights
    if(any(oobweights[weights > 0] > 0)){
      stop("weights and oobweights must not both have non-zero weights for the same observations.")
    }

    if(any(weights[oobweights > 0] > 0)){
      stop("weights and oobweights must not both have non-zero weights for the same observations.")
    }

    if(length(c(weights[weights > 0], oobweights[oobweights > 0])) != nrow(data)){
      warning("at least one observation is neiter used for fitting nor for validation.")
    }

    if(risk == "oobag" & all(oobweights == 0)){
      stop("In order to compute out-of-bag risk you cannot use all observations for the fitting process.\nPlease check your specification of arguments weights and oobweights.")
    }

    ## Getting covariate names
    covariates <- all.vars(formula[[3]])
    checkmate::assert_character(covariates, all.missing = FALSE)
    checkmate::assert_subset(covariates, choices = names(data), empty.ok = FALSE)

    ### Getting baselearner names
    baselearner <- attr(stats::terms(formula), "term.labels")
    checkmate::assert_character(baselearner, any.missing = FALSE, pattern = "^(brq\\(|brqss\\().+\\)$")


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
                     paste("~", x[["formula"]])
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
    emp.risk <- vector("numeric", length = mstop + 1)

    ### Defining intial fitted values
    if(is.null(offset)){
      offset <- stats::quantile(y, tau)
      fit <- rep(offset, nrow(data))
    } else {
      fit <- offset
      if(length(unique(offset)) == 1){
        offset <- offset[1]
      }
    }

    if(risk == "oobag"){
      riskfct <- function(y, f) {
        quantile.risk(y, f, tau, oobweights)
      }
    } else {
      riskfct <- function(y, f) {
        quantile.risk(y, f, tau, weights)
      }
    }

    emp.risk[1] <- riskfct(y = y, f = fit)


    ### Setting up counter variable
    count.m <- 0

    ### Setting up fitting function for boosting
    boostrq.fit <- function(niter){
      for(m in (count.m + 1):(count.m + niter)) {

        ### Determining current working residuals (negative gradients)
        q.ngradient <- quantile.ngradient(y = y, f = fit, tau = tau, exact.fit = exact.fit)

        ### Estimating quantile regression models and determining empirical quantile risk for each baselearner
        # HUHU: use mclapply (?)
        if(all(weights == 1)){
          qr.res <-
            lapply(baselearner,
                   function(x) {
                     qreg <- quantreg::rq.fit(y = q.ngradient, x = baselearer.model.matrix[[x]], tau = tau, method = baselearer.out[[x]][["method"]])
                     bl.risk[x] <<- quantile.risk(y = q.ngradient, f = qreg$fitted.values, tau = tau, weights = weights)

                     qreg
                   }
            )
          names(qr.res) <- baselearner
        } else {
          qr.res <-
            lapply(baselearner,
                   function(x) {
                     qreg <- quantreg::rq.wfit(y = q.ngradient, x = baselearer.model.matrix[[x]], tau = tau, method = baselearer.out[[x]][["method"]], weights = weights)
                     bl.risk[x] <<- quantile.risk(y = q.ngradient, f = qreg$fitted.values, tau = tau, weights = weights)

                     qreg
                   }
            )
          names(qr.res) <- baselearner
        }

        if(sum(bl.risk == min(bl.risk)) > 1){
          warning(paste("Warning: There is no unique best base learner in iteration", m))
        }

        ### Determining best fitting baselearner of current iteration
        best.baselearner <- names(which.min(bl.risk))

        ### Updating coefficient path
        coefpath[[best.baselearner]][m, ] <<- qr.res[[best.baselearner]]$coef * nu

        ### Determining the best fitting component
        ### HUHU: Denk nochmal über die Genze 10te Nachkommastelle nach...
        if(any(abs(round(qr.res[[best.baselearner]]$coef[-1], digits)) > 0)){
          appearances[m] <<- which.min(bl.risk)
        } else {
          appearances[m] <<- 0
        }

        ### Updating  fitted values
        fit <<- fit + qr.res[[best.baselearner]]$fitted.values * nu

        ### Updating empirical quantile risk
        emp.risk[m + 1] <<- riskfct(y = y, f = fit)

      }

      ### Increasing iteration counter to current number of iterations (mstop)
      count.m <<- count.m + niter

    }

    ### Executing boostrq.fit
    if(mstop > 0) {
      boostrq.fit(mstop)
    }

    ### Defining rich output
    RETURN <- list(
      formula = formula,
      nu = nu,
      offset = offset,
      baselearner.names = baselearner,
      call = match.call(),
      weights = weights,
      oobweights = oobweights
    )

    ### Number of iterations run
    RETURN$mstop <- function() count.m

    ### Selected component in each iteration
    RETURN$xselect <- function() {
      if(count.m > 0) {
        return(appearances[1:count.m])
      } else {
        return(NULL)
      }
    }

    ### Current fitted values
    RETURN$fitted <- function() fit

    ### Current residuals
    RETURN$resid <- function() y - fit

    ### Current empirical quantile risk
    RETURN$risk <- function() {
      emp.risk[1:(count.m + 1)]
    }

    ### Current working residuals (negative gradients)
    RETURN$neg.gradients <- function() {
      quantile.ngradient(y, fit, tau, exact.fit = exact.fit)
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

    RETURN$update <- function(weights, oobweights, risk){

      if(length(offset) == 1){
        offset.vec <- rep(offset, length(y))
      }

      boostrq(
        formula = formula,
        data = data,
        mstop = count.m,
        nu = nu,
        tau = tau,
        offset = offset.vec,
        digits = digits,
        exact.fit = exact.fit,
        weights = weights,
        oobweights = oobweights,
        risk = risk
      )

    }

    ### Current coefficient estimates
    RETURN$coef <- function(which = NULL, aggregate = "sum") {

      checkmate::assert_character(which, max.len = length(baselearner), null.ok = TRUE)
      checkmate::assert_subset(which, choices = baselearner)
      checkmate::assert_character(aggregate, len = 1)
      checkmate::assert_choice(aggregate, choices = c("sum_aggr", "sum", "none", "cumsum"))

      if(is.null(which)){
        which <- baselearner
      }

      if(count.m == 0) {
        return(list(offset = offset))
      }

      if(aggregate == "sum_aggr" & count.m > 0){
        coefpath.sum <- lapply(which,
                               function(x){
                                 colSums(coefpath[[x]][1:count.m, , drop = FALSE])
                               }
        )

        coef.names <-
          unique(
            unlist(
              lapply(
                coefpath.sum,
                function(x){
                  names(x)
                }
              )
            )
          )

        coefpath.sum_aggr <-
          unlist(
            lapply(
              coef.names,
              function(name){
                sum(
                  unlist(
                    lapply(
                      coefpath.sum,
                      function(x){
                        x[name]
                      }
                    )
                  ),
                  na.rm = TRUE)
              }
            )
          )

        names(coefpath.sum_aggr) <- coef.names

        coefpath.sum_aggr["(Intercept)"] <-
          coefpath.sum_aggr["(Intercept)"] + offset

        return(coefpath.sum_aggr)
      }

      if(aggregate == "none" & count.m > 0){
        coefpath.none <- lapply(which,
                                function(x){
                                  coefpath[[x]][1:count.m, , drop = FALSE]
                                }
        )
        names(coefpath.none) <- which
        coefpath.none$offset <- offset
        return(coefpath.none)
      }

      if(aggregate == "sum" & count.m > 0){
        coefpath.sum <- lapply(which,
                               function(x){
                                 colSums(coefpath[[x]][1:count.m, , drop = FALSE])
                               }
        )
        names(coefpath.sum) <- which
        coefpath.sum$offset <- offset
        return(coefpath.sum)
      }

      if(aggregate == "cumsum" & count.m > 0){
        coefpath.cumsum <- lapply(which,
                                  function(x){
                                    if(count.m == 1) {
                                      return(t(as.matrix(apply(coefpath[[x]][1:count.m, , drop = FALSE], MARGIN = 2, FUN = cumsum))))
                                    } else {
                                      return(apply(coefpath[[x]][1:count.m, , drop = FALSE], MARGIN = 2, FUN = cumsum))
                                    }
                                  }
        )
        names(coefpath.cumsum) <- which
        coefpath.cumsum$offset <- offset
        return(coefpath.cumsum)
      }

    }

    ### Predicition function
    RETURN$predict <- function(newdata = NULL, which = NULL, aggregate = "sum"){

      checkmate::check_data_frame(newdata, min.rows = 1, min.cols = 2, col.names = "named", any.missing = FALSE)
      checkmate::assert_subset(covariates, choices = names(newdata), empty.ok = FALSE)
      checkmate::assert_character(which, max.len = length(baselearner), null.ok = TRUE)
      checkmate::assert_subset(which, choices = baselearner)
      checkmate::assert_character(aggregate, len = 1)
      checkmate::assert_choice(aggregate, choices = c("sum", "none", "cumsum"))

      if(is.null(which)){
        which <- baselearner
      }

      newdata.model.matrix <-
        lapply(baselearer.out[which],
               function(x){
                 stats::na.omit(
                   stats::model.matrix(
                     stats::as.formula(
                       paste("~", x[["formula"]])
                     ),
                     newdata
                   )
                 )
               }
        )
      names(newdata.model.matrix) <- which

      if(count.m == 0) {
        if(length(offset) == 1){
          predictions <- rep(offset, length(y))
        }
        if(length(offset) > 1){
          predictions <- offset
        }
        names(predictions) <- NULL
        return(predictions)
      }

      if(aggregate == "sum" & count.m > 0) {
        bl.predictions <-
          lapply(which,
                 function(x){
                   newdata.model.matrix[[x]] %*% RETURN$coef(which = which, aggregate = aggregate)[[x]]
                 }
          )
        predictions <- Reduce('+', bl.predictions) + offset
        return(predictions)
      }

      if(aggregate == "cumsum" & count.m > 0) {
        bl.predictions <-
          lapply(which,
                 function(x){
                   apply(X = RETURN$coef(which = which, aggregate = aggregate)[[x]], MARGIN = 1,
                         FUN = function(X){
                           newdata.model.matrix[[x]] %*% X
                         }
                   )
                 }
          )
        predictions.cum <- Reduce('+', bl.predictions) + offset
        return(predictions.cum)
      }

      if(aggregate == "none" & count.m > 0) {
        bl.predictions <-
          lapply(which,
                 function(x){
                   apply(X = RETURN$coef(which = which, aggregate = aggregate)[[x]], MARGIN = 1,
                         FUN = function(X){
                           newdata.model.matrix[[x]] %*% X
                         }
                   )
                 }
          )
        predictions.none <- Reduce('+', bl.predictions)
        predictions.none[, 1] <- predictions.none[, 1] + offset
        return(predictions.none)
      }

    }

    ### Update to a new number of boosting iterations mstop (without refitting the whole model)
    RETURN$subset <- function(i) {


      checkmate::assert_int(i, lower = 0)

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

    ### Table with selection frequency for each baselearner
    RETURN$selection.freqs <- function() {
      freq.table <- table(RETURN$xselect()) / length(RETURN$xselect())
      ind <- as.numeric(names(freq.table)) + 1
      names(freq.table) <- c("Intercept", baselearner)[ind]
      freq.table
    }

    class(RETURN) <- "boostrq"

    RETURN

  }

