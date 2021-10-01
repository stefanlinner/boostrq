boosted.rq.0 <- boostrq(
  mpg ~ brq(cyl*hp) + brq(am),
  data = mtcars,
  mstop = 0
)

boosted.rq.1 <- boostrq(
  mpg ~ brq(cyl*hp) + brq(am),
  data = mtcars,
  mstop = 1
)

boosted.rq.100 <- boostrq(
  mpg ~ brq(cyl*hp) + brq(am),
  data = mtcars,
  mstop = 100
)


test_that("output format of boostrq is correct", {

  testthat::expect_s3_class(boosted.rq.0, "boostrq")
  testthat::expect_s3_class(boosted.rq.1, "boostrq")
  testthat::expect_s3_class(boosted.rq.100, "boostrq")

  testthat::expect_output(str(boosted.rq.0), "List of 16")
  testthat::expect_output(str(boosted.rq.1), "List of 16")
  testthat::expect_output(str(boosted.rq.100), "List of 16")

})

test_that("coefficient output is correct", {

  testthat::expect_output(str(boosted.rq.0$coef(aggregate = "sum")), "List of 1")
  testthat::expect_output(str(boosted.rq.0$coef(aggregate = "cumsum")), "List of 1")
  testthat::expect_output(str(boosted.rq.0$coef(aggregate = "none")), "List of 1")

  testthat::expect_output(str(boosted.rq.1$coef(aggregate = "sum")), "List of 3")
  testthat::expect_output(str(boosted.rq.1$coef(aggregate = "cumsum")), "List of 3")
  testthat::expect_output(str(boosted.rq.1$coef(aggregate = "none")), "List of 3")

  testthat::expect_output(str(boosted.rq.100$coef(aggregate = "sum")), "List of 3")
  testthat::expect_output(str(boosted.rq.100$coef(aggregate = "cumsum")), "List of 3")
  testthat::expect_output(str(boosted.rq.100$coef(aggregate = "none")), "List of 3")

  testthat::expect_output(str(boosted.rq.1$coef(aggregate = "sum", which = "brq(am)")), "List of 2")
  testthat::expect_output(str(boosted.rq.100$coef(aggregate = "sum", which = "brq(am)")), "List of 2")


  coefficients.sum.0 <- boosted.rq.0$coef(aggregate = "sum")
  coefficients.cumsum.0 <- boosted.rq.0$coef(aggregate = "cumsum")
  coefficients.none.0 <- boosted.rq.0$coef(aggregate = "none")

  coefficients.sum.1 <- boosted.rq.1$coef(aggregate = "sum")
  coefficients.cumsum.1 <- boosted.rq.1$coef(aggregate = "cumsum")
  coefficients.none.1 <- boosted.rq.1$coef(aggregate = "none")

  coefficients.sum.100 <- boosted.rq.100$coef(aggregate = "sum")
  coefficients.cumsum.100 <- boosted.rq.100$coef(aggregate = "cumsum")
  coefficients.none.100 <- boosted.rq.100$coef(aggregate = "none")


  testthat::expect_output(str(coefficients.sum.0$offset), "Named num")
  testthat::expect_output(str(coefficients.cumsum.0$offset), "Named num")
  testthat::expect_output(str(coefficients.none.0$offset), "Named num")


  testthat::expect_output(str(coefficients.sum.1$offset), "Named num")
  testthat::expect_output(str(coefficients.sum.1$`brq(am)`), "Named num [1:2]", fixed = TRUE)
  testthat::expect_output(str(coefficients.sum.1$`brq(cyl * hp)`), "Named num [1:4]", fixed = TRUE)

  testthat::expect_output(str(coefficients.cumsum.1$offset), "Named num")
  testthat::expect_output(str(coefficients.cumsum.1$`brq(am)`), "num [1, 1:2]", fixed = TRUE)
  testthat::expect_output(str(coefficients.cumsum.1$`brq(cyl * hp)`), "num [1, 1:4]", fixed = TRUE)

  testthat::expect_output(str(coefficients.none.1$offset), "Named num")
  testthat::expect_output(str(coefficients.none.1$`brq(am)`), "num [1, 1:2]", fixed = TRUE)
  testthat::expect_output(str(coefficients.none.1$`brq(cyl * hp)`), "num [1, 1:4]", fixed = TRUE)


  testthat::expect_output(str(coefficients.sum.100$offset), "Named num")
  testthat::expect_output(str(coefficients.sum.100$`brq(am)`), "Named num [1:2]", fixed = TRUE)
  testthat::expect_output(str(coefficients.sum.100$`brq(cyl * hp)`), "Named num [1:4]", fixed = TRUE)

  testthat::expect_output(str(coefficients.cumsum.100$offset), "Named num")
  testthat::expect_output(str(coefficients.cumsum.100$`brq(am)`), "num [1:100, 1:2]", fixed = TRUE)
  testthat::expect_output(str(coefficients.cumsum.100$`brq(cyl * hp)`), "num [1:100, 1:4]", fixed = TRUE)

  testthat::expect_output(str(coefficients.none.100$offset), "Named num")
  testthat::expect_output(str(coefficients.none.100$`brq(am)`), "num [1:100, 1:2]", fixed = TRUE)
  testthat::expect_output(str(coefficients.none.100$`brq(cyl * hp)`), "num [1:100, 1:4]", fixed = TRUE)

})


test_that("prediction output is correct", {

  newdat.1 <- mtcars
  newdat.2 <- mtcars[, c("mpg", "cyl", "hp", "am")]
  newdat.3 <- mtcars[, c("mpg", "cyl", "hp", "am")]
  newdat.3$mpg <- NA

  testthat::expect_output(str(boosted.rq.0$predict(newdata = newdat.1, aggregate = "sum")), "num [1:32]", fixed = TRUE)
  testthat::expect_output(str(boosted.rq.0$predict(newdata = newdat.1, aggregate = "cumsum")), "num [1:32]", fixed = TRUE)
  testthat::expect_output(str(boosted.rq.0$predict(newdata = newdat.1, aggregate = "none")), "num [1:32]", fixed = TRUE)
  testthat::expect_output(str(boosted.rq.0$predict(newdata = newdat.2, aggregate = "sum")), "num [1:32]", fixed = TRUE)
  testthat::expect_output(str(boosted.rq.0$predict(newdata = newdat.3, aggregate = "sum")), "num [1:32]", fixed = TRUE)

  testthat::expect_equal(boosted.rq.0$predict(newdata = newdat.1, aggregate = "sum"), rep(median(newdat.1$mpg), 32))


  testthat::expect_output(str(boosted.rq.1$predict(newdata = newdat.1, aggregate = "sum")), "num [1:32, 1]", fixed = TRUE)
  testthat::expect_output(str(boosted.rq.1$predict(newdata = newdat.1, aggregate = "cumsum")), "num [1:32, 1]", fixed = TRUE)
  testthat::expect_output(str(boosted.rq.1$predict(newdata = newdat.1, aggregate = "none")), "num [1:32, 1]", fixed = TRUE)
  testthat::expect_output(str(boosted.rq.1$predict(newdata = newdat.2, aggregate = "sum")), "num [1:32, 1]", fixed = TRUE)
  testthat::expect_output(str(boosted.rq.1$predict(newdata = newdat.3, aggregate = "sum")), "num [1:32, 1]", fixed = TRUE)


  testthat::expect_output(str(boosted.rq.100$predict(newdata = newdat.1, aggregate = "sum")), "num [1:32, 1]", fixed = TRUE)
  testthat::expect_output(str(boosted.rq.100$predict(newdata = newdat.1, aggregate = "cumsum")), "num [1:32, 1:100]", fixed = TRUE)
  testthat::expect_output(str(boosted.rq.100$predict(newdata = newdat.1, aggregate = "none")), "num [1:32, 1:100]", fixed = TRUE)
  testthat::expect_output(str(boosted.rq.100$predict(newdata = newdat.2, aggregate = "sum")), "num [1:32, 1]", fixed = TRUE)
  testthat::expect_output(str(boosted.rq.100$predict(newdata = newdat.3, aggregate = "sum")), "num [1:32, 1]", fixed = TRUE)


})


# test_that("refitting is done correctly", {
#
#
#
# })

## teste auch auf expect_error unter extra file: test-conditions.R

