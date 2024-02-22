#' #' Test
#' #'
#' #' @param object test
#' #' @param parm test
#' #' @param level test
#' #' @param B tet
#' #' @param B.mstop test
#' #' @param newdata test
#' #' @param papply test
#' #' @param ... test
#' #'
#' #' @return test
#' #' @export
#' #'
#' #' @import parallel
#' #'
#' #' @examples
#' #'
#' #' test <- 1:3
#' #'
#' confint.boostrq <-
#'   function(
#'     object,
#'     parm = NULL,
#'     level = 0.95,
#'     B = 1000,
#'     B.mstop = 25,
#'     newdata = NULL,
#'     papply = ifelse(B.mstop == 0, parallel::mclapply, lapply),
#'     ...
#'   ) {
#'
#'     checkmate::assert_class(object, "boostrq")
#'     checkmate::assert_character(parm, max.len = length(object$baselearner.names), null.ok = TRUE)
#'     checkmate::assert_subset(parm, choices = object$baselearner.names)
#'     checkmate::assert_number(level, lower = 0, upper = 1)
#'     checkmate::assert_int(B, lower = 1)
#'     checkmate::assert_int(B.mstop, lower = 0)
#'     checkmate::assert(
#'       checkmate::check_data_frame(newdata, min.rows = 1, min.cols = 2, col.names = "named"),
#'       checkmate::check_data_table(newdata, min.rows = 1, min.cols = 2, col.names = "named"),
#'       combine = "or"
#'     )
#'     checkmate::assert_function(papply)
#'
#'     # newdata <- .create_newdata(object, newdata, which) HUHU: Was passiert hier?
#'
#'     outer.folds <- mboost::cv(object$weights, B = B)
#'
#'     cat("Start computing bootstrap confidence intervals... \n")
#'
#'     do_update <- function(i) {
#'
#'       cat("\rB =", i)
#'
#'       mod <- stats::update(object, weights = outer.folds[, i],
#'                     risk = "inbag", trace = FALSE)
#'
#'       if (B.mstop > 0) {
#'         cvr <-
#'           cvrisk(
#'             object = mod,
#'             folds = cv(mod$weights, B = B.mstop)
#'           )
#'
#'         mod[mstop(cvr)]
#'       }
#'
#'       # .predict_confint(mod, newdata = newdata, which = which) HUHU
#'
#'     }
#'
#'     predictions <- papply(1:B, do_update, ...)
#'
#'     cat("\n")
#'
#'     res <- list(level = level, boot_pred = predictions, data = newdata,
#'                 model = object)
#'     attr(res, "which") <- which
#'     class(res) <- "boostrq.ci"
#'     return(res)
#'
#'   }
