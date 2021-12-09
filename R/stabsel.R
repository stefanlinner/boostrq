#' Stability Selection for boosting regression quantiles
#'
#' test
#'
#' @param x test
#' @param cutoff test
#' @param q test
#' @param PFER test
#' @param grid stest
#' @param B test
#' @param assumption test
#' @param sampling.type test
#' @param papply test
#' @param FWER test
#' @param verbose test
#' @param ... test
#'
#' @return test
#' @export
#'
#' @import stabs parallel
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
stabsel.boostrq <- function(
  x,
  cutoff,
  q,
  PFER,
  grid = 0:mstop(x),
  B = ifelse(sampling.type == "MB", 100, 50),
  assumption = "none",
  sampling.type = "MB",
  papply = mclapply,
  FWER,
  verbose = TRUE,
  ...
) {

  checkmate::assert_class(x, "boostrq")

  p <- length(x$baselearner.names)
  ibase <- seq_len(p)
  weights <- x$weights
  n <- length(weights)

  checkmate::assert_number(cutoff, lower = 0.5, upper = 1)
  checkmate::assert_int(q, lower = 0, upper = p)
  # assertion on PFER See checks in stabsel_parameters
  checkmate::assert_integerish(grid, lower = 0, any.missing = FALSE)
  if (!isTRUE(all.equal(grid, 0:max(grid), check.attributes = FALSE))) {
    stop("grid must be of the form 0:m, i.e., starting at 0 with increments of 1")
  }
  checkmate::assert_int(B) # what else?
  checkmate::assert_choice(assumption, choices = c("unimodal", "r-concave", "none"))
  checkmate::assert_choice(sampling.type, choices = c("SS", "MB"))
  checkmate::assert_function(papply)

  if(sampling.type == "MB" & assumption != "none") {
    warning("Assumption was changed to none, as sampling.type = 'MB'")
    assumption <- "none"
  }

  k <- floor(n * 0.5)
  indx <- rep(c(0, 1), c(n - k, k))
  folds <- replicate(B, sample(indx))[sample(1:n),, drop = FALSE] * weights

  pars <- stabs::stabsel_parameters(
    p = p,
    cutoff = cutoff,
    q = q,
    PFER = PFER,
    B = B,
    verbose = verbose,
    sampling.type = sampling.type,
    assumption = assumption,
    FWER = FWER
  )

  cutoff <- pars$cutoff
  q <- pars$q
  PFER <- pars$PFER

  q <- 1

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
    ## use complementary pairs
    folds <- cbind(folds, weights - folds)
  }

  ss <- cvkrisk(
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
            " bevor applying ", sQuote("stabsel"))


  ### HUHU: Hier weiter: wie muss das aufgebaut werden, damit es stimmig ist
  ### Unterscheidung zwischen mstop und mslope berücksichtigen? Vermutlich schon.

  # browser()
  # mslope <- max(grid) - sum(x$xselect() == 0)
  # ret <- matrix(0, nrow = length(ibase), ncol = mslope)
  # for (i in 1:length(ss)) {
  #   tmp <- sapply(ibase, function(x)
  #     ifelse(x %in% ss[[i]], which(ss[[i]][ss[[i]] != 0] == x)[1], mslope + 1))
  #   ret <- ret + t(sapply(tmp, function(x) c(rep(0, x - 1), rep(1, mslope - x + 1))))
  # }
  #
  # ret


  # phat <- ret / length(ss)
  # rownames(phat) <- names(variable.names(x))
  # if (extends(class(x), "glmboost"))
  #   rownames(phat) <- variable.names(x)
  # ret <- list(phat = phat, selected = which((mm <- apply(phat, 1, max)) >= cutoff),
  #             max = mm, cutoff = cutoff, q = q, PFER = PFER, p = p, B = B,
  #             sampling.type = sampling.type, assumption = assumption,
  #             call = cll)
  # ret$call[[1]] <- as.name("stabsel")
  # class(ret) <- c("stabsel_mboost", "stabsel")
  # ret

}
