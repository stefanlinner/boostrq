#' Stability Selection for boosting regression quantiles
#'
#' @param x a fitted model of class "boostrq"
#' @param cutoff cutoff between 0.5 and 1. Preferably a value between 0.6 and 0.9 should be used
#' @param q number of (unique) selected componenents (base-learners) that are selected in each subsample.
#' @param PFER upper bound for the per-family error rate. This specifies the amount of falsely selected
#' base-learners, which is tolerated.
#' @param grid a numeric vector of the form 0:m.
#' @param B umber of subsampling replicates. Per default, we use 50 complementary pairs for the
#' error bounds of Shah & Samworth (2013) and 100 for the error bound derived in
#' Meinshausen & Buehlmann (2010). As we use B complementray pairs in the former
#' case this leads to 2B subsamples.
#' @param assumption Defines the type of assumptions on the distributions of the selection probabilities
#' and simultaneous selection probabilities. Only applicable for sampling.type = "SS". For
#' sampling.type = "MB" we always use code"none".
#' @param sampling.type use sampling scheme of of Shah & Samworth (2013), i.e., with complementarty pairs
#' (sampling.type = "SS"), or the original sampling scheme of Meinshausen & Buehlmann (2010).
#' @param papply (parallel) apply function, defaults to mclapply. To run sequentially
#' (i.e. not in parallel), one can use lapply.
#' @param verbose logical (default: TRUE) that determines wether warnings should be issued.
#' @param ... additional arguments passed to callies
#' @param folds a weight matrix with number of rows equal to the number of observations. Usually one should
#' not change the default here as subsampling with a fraction of 1/2 is needed for the error bounds to hold.
#'
#' @return An object of class stabsel.
#' @export
#'
#' @import stabs parallel
#'
#' @examples
#' boosted.rq <-
#' boostrq(
#'  formula = mpg ~ brq(cyl) + brq(hp) + brq(am) + brq(wt) + brq(drat),
#'  data = mtcars,
#'  mstop = 600,
#'  nu = 0.1,
#'  tau = 0.5
#' )
#'
#' stabsel_parameters(
#'  q = 3,
#'  PFER = 1,
#'  p = 5,
#'  sampling.type = "SS",
#'  assumption = "unimodal"
#' )
#'
#' \donttest{
#' set.seed(100)
#' brq.stabs <-
#' stabsel(
#'  x = boosted.rq,
#'  q = 3,
#'  PFER = 1,
#'  sampling.type = "SS",
#'  assumption = "unimodal"
#' )
#'
#' brq.stabs
#' }
#'
stabsel.boostrq <-
  function(
    x,
    cutoff,
    q,
    PFER ,
    grid = 0:mstop(x),
    folds = stabs::subsample(x$weights, B = B),
    B = ifelse(sampling.type == "MB", 100, 50),
    assumption = "unimodal",
    sampling.type = "SS",
    papply = parallel::mclapply,
    verbose = TRUE,
    ...
  ) {

    checkmate::assert_class(x, "boostrq")

    p <- length(x$baselearner.names)
    ibase <- seq_len(p)
    weights <- x$weights
    n <- length(weights)
    cll <- match.call()

    checkmate::assert_integerish(grid, lower = 0, any.missing = FALSE)
    if (!isTRUE(all.equal(grid, 0:max(grid), check.attributes = FALSE))) {
      stop("grid must be of the form 0:m, i.e., starting at 0 with increments of 1")
    }
    checkmate::assert_matrix(folds, any.missing = FALSE, nrows = n)
    checkmate::assert_int(B, lower = 1)
    checkmate::assert_choice(assumption, choices = c("unimodal", "r-concave", "none"))
    checkmate::assert_choice(sampling.type, choices = c("SS", "MB"))
    checkmate::assert_function(papply)
    checkmate::assert_logical(verbose, any.missing = FALSE, len = 1)

    if(sampling.type == "MB" & assumption != "none") {
      warning("Assumption was changed to none, as sampling.type = 'MB'")
      assumption <- "none"
    }

    if (ncol(folds) != B) {
      B <- ncol(folds)
      warning("B should be equal to number of folds, i.e., ncol(folds). B was set to ncol(folds)")
    }

    pars <- stabs::stabsel_parameters(
      p = p,
      cutoff = cutoff,
      q = q,
      PFER = PFER,
      B = B,
      verbose = verbose,
      sampling.type = sampling.type,
      assumption = assumption
    )

    cutoff <- pars$cutoff
    q <- pars$q
    PFER <- pars$PFER

    fun <- function(model) {
      xs <- model$xselect()
      qq <- sapply(seq_along(xs),
                   function(x) {
                     length(
                       setdiff(
                         unique(xs[1:x]),
                         0
                       )
                     )
                   }
      )
      xs[qq > q] <- xs[xs != 0][1]
      xs
    }

    if (sampling.type == "SS") {
      folds <- cbind(folds, weights - folds)
    }

    ss <- cvrisk(
      object = x,
      fun = fun,
      folds = folds,
      papply = papply,
      grid = grid
    )

    qq <- sapply(ss, function(x){
      length(unique(x[x != 0]))
    })
    sum_of_violations <- sum(qq < q)
    if (sum_of_violations > 0)
      warning(sQuote("mstop"), " too small in ",
              sum_of_violations, " of the ", ncol(folds),
              " subsampling replicates to select ", sQuote("q"),
              " base-learners; Increase ", sQuote("mstop"),
              " before applying ", sQuote("stabsel"))

    ## HUHU: Nochmal drüber nachdenken, ob das so passt
    ## Der Unterschied mit brq ist natürlich, dass man auch 0 haben kann, welches einfach ignoriert wird
    ## Sollte aber eigentlich hier kein Problem sein, und oben beim zählen wurde das berücksichtigt..
    m <- max(grid)
    ret <- matrix(0, nrow = length(ibase), ncol = m)
    for (i in 1:length(ss)) {
      tmp <- sapply(ibase, function(x)
        ifelse(x %in% ss[[i]], which(ss[[i]] == x)[1], m + 1))
      ret <- ret + t(sapply(tmp, function(x) c(rep(0, x - 1), rep(1, m - x + 1))))
    }


    phat <- ret / length(ss)
    rownames(phat) <- x$baselearner.names

    ret <- list(phat = phat, selected = which((mm <- apply(phat, 1, max)) >= cutoff),
                max = mm, cutoff = cutoff, q = q, PFER = PFER, p = p, B = B,
                sampling.type = sampling.type, assumption = assumption,
                call <- cll)
    ret$call[[1]] <- as.name("stabsel")
    class(ret) <- c("stabsel_boostrq", "stabsel")
    ret

  }
