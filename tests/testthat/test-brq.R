atomar.bl <- brq(hp)
combined.bl <- brq(hp + disp + wt)
formula.bl1 <- brq(hp + I(hp^2))
formula.bl2 <- brq((hp + disp + wt) * am)
formula.bl3 <- brq(hp:disp + am*cyl)
formula.bl4 <- brq(hp + log(disp) + wt)


test_that("brq output is correct", {

  testthat::expect_output(str(atomar.bl), "List of 3")
  testthat::expect_output(str(combined.bl), "List of 3")
  testthat::expect_output(str(formula.bl1), "List of 3")
  testthat::expect_output(str(formula.bl2), "List of 3")
  testthat::expect_output(str(formula.bl3), "List of 3")
  testthat::expect_output(str(formula.bl4), "List of 3")

  testthat::expect_equal(atomar.bl$formula, "hp")
  testthat::expect_equal(combined.bl$formula, "hp + disp + wt")
  testthat::expect_equal(formula.bl1$formula, "hp + I(hp^2)")
  testthat::expect_equal(formula.bl2$formula, "(hp + disp + wt) * am")
  testthat::expect_equal(formula.bl3$formula, "hp:disp + am * cyl")
  testthat::expect_equal(formula.bl4$formula, "hp + log(disp) + wt")

})


test_that("boostrq handles formula correctly", {

  boostrq.atomar.bl <- boostrq(
    mpg ~ brq(hp),
    data = mtcars,
    mstop = 100
  )

  boostrq.combined.bl <- boostrq(
    mpg ~ brq(hp + disp + wt),
    data = mtcars,
    mstop = 100
  )

  boostrq.formula.bl1 <- boostrq(
    mpg ~ brq(hp + I(hp^2)),
    data = mtcars,
    mstop = 100
  )

  boostrq.formula.bl2 <- boostrq(
    mpg ~ brq((hp + disp + wt) * am),
    data = mtcars,
    mstop = 100
  )

  boostrq.formula.bl3 <- boostrq(
    mpg ~ brq(hp:disp + am * cyl),
    data = mtcars,
    mstop = 100
  )

  boostrq.formula.bl4 <- boostrq(
    mpg ~ brq(hp + log(disp) + wt),
    data = mtcars,
    mstop = 100
  )


  testthat::expect_equal(length(boostrq.atomar.bl$baselearner.matrix()), 1)
  testthat::expect_equal(length(boostrq.combined.bl$baselearner.matrix()), 1)
  testthat::expect_equal(length(boostrq.formula.bl1$baselearner.matrix()), 1)
  testthat::expect_equal(length(boostrq.formula.bl2$baselearner.matrix()), 1)
  testthat::expect_equal(length(boostrq.formula.bl3$baselearner.matrix()), 1)
  testthat::expect_equal(length(boostrq.formula.bl4$baselearner.matrix()), 1)


  testthat::expect_equal(
    colnames(boostrq.atomar.bl$baselearner.matrix()$`brq(hp)`),
    c("(Intercept)", "hp")
  )

  testthat::expect_equal(
    colnames(boostrq.combined.bl$baselearner.matrix()$`brq(hp + disp + wt)`),
    c("(Intercept)", "hp", "disp", "wt")
  )

  testthat::expect_equal(
    colnames(boostrq.formula.bl1$baselearner.matrix()$`brq(hp + I(hp^2))`),
    c("(Intercept)", "hp", "I(hp^2)")
  )

  testthat::expect_equal(
    colnames(boostrq.formula.bl2$baselearner.matrix()$`brq((hp + disp + wt) * am)`),
    c("(Intercept)", "hp", "disp", "wt", "am", "hp:am", "disp:am", "wt:am")
  )

  testthat::expect_equal(
    colnames(boostrq.formula.bl3$baselearner.matrix()$`brq(hp:disp + am * cyl)`),
    c("(Intercept)", "am", "cyl", "hp:disp", "am:cyl")
  )

  testthat::expect_equal(
    colnames(boostrq.formula.bl4$baselearner.matrix()$`brq(hp + log(disp) + wt)`),
    c("(Intercept)", "hp", "log(disp)", "wt")
  )


  testthat::expect_equal(
    as.vector(boostrq.formula.bl1$baselearner.matrix()$`brq(hp + I(hp^2))`[, "I(hp^2)"]),
    mtcars$hp^2
  )

  testthat::expect_equal(
    as.vector(boostrq.formula.bl2$baselearner.matrix()$`brq((hp + disp + wt) * am)`[, "hp:am"]),
    mtcars$hp * mtcars$am
  )

  testthat::expect_equal(
    as.vector(boostrq.formula.bl4$baselearner.matrix()$`brq(hp + log(disp) + wt)`[, "log(disp)"]),
    log(mtcars$disp)
  )


})
