
<!-- README.md is generated from README.Rmd. Please edit that file -->

# boostrq

Package for boosting regression quantiles (Bauer, Haupt, & Linner,
2023). The functionality as well as the implementation was heavily
inspired by the great package for model-based boosting
[mboost](https://github.com/boost-R/mboost). Until now, only the linear
baselearner (brq) is implemented, the nonlinear baselearner (brqss) will
follow soon.

## Installation

You can install the development version of boostrq from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stefanlinner/boostrq")
```

## Example

The following demonstrates the basic functionality of the package with a
simple example.

``` r
### Attaching the package to the search path
library(boostrq)
#> Lade nötiges Paket: mboost
#> Lade nötiges Paket: parallel
#> Lade nötiges Paket: stabs

### Fitting your first boosting regression quantiles model.
boosted.rq <- 
  boostrq(
    formula = mpg ~ brq(cyl * hp) + brq(am + wt), # all common formula operators (*,:,^, etc.) can be used in the function brq()
    data = mtcars, 
    mstop = 200, 
    nu = 0.1, 
    tau = 0.5
  )

### Get some basic information about your model 
boosted.rq$formula # the model formula
#> mpg ~ brq(cyl * hp) + brq(am + wt)
#> <environment: 0x000002d865949d38>

boosted.rq$nu # the learning rate
#> [1] 0.1

boosted.rq$offset # the initialization of the algorithm (default: median of response)
#>  50% 
#> 19.2

boosted.rq$baselearner.names # names of the baselearners
#> [1] "brq(cyl * hp)" "brq(am + wt)"

boosted.rq$call # the model call
#> boostrq(formula = mpg ~ brq(cyl * hp) + brq(am + wt), data = mtcars, 
#>     mstop = 200, nu = 0.1, tau = 0.5)

boosted.rq$mstop() # current number of iterations
#> [1] 200

# or use
mstop(boosted.rq)
#> [1] 200

### Print your fitted model to get a collection of that basic information
boosted.rq
#> 
#>   Boosting Regression Qauntiles
#> 
#> Call:  boostrq(formula = mpg ~ brq(cyl * hp) + brq(am + wt), data = mtcars,      mstop = 200, nu = 0.1, tau = 0.5)
#> formula: mpg ~ brq(cyl * hp) + brq(am + wt)
#> 
#> 
#>   Quantile Regression
#> Loss Function: tau * (y - f) * ((y - f) > 0) +
#>         (tau - 1) * (y - f) * ((y - f) <= 0)
#> Negative Gradient: tau * ((y - f) > 0) + (tau - 1) * ((y - f) <= 0)
#> 
#> Number of boosting iterations: mstop = 200 
#> Step size: = 0.1 
#> Offset:  19.2 
#> Number of baselearners:  2

### Selection frequency of each component
boosted.rq$selection.freqs()
#>     Intercept brq(cyl * hp)  brq(am + wt) 
#>         0.025         0.645         0.330

### Estimated coefficients for current number of iterations
boosted.rq$coef(aggregate = "sum") # also try aggregate = "cumsum" or "none"
#> $`brq(cyl * hp)`
#> (Intercept)         cyl          hp      cyl:hp 
#> 19.33792010 -2.47917356 -0.09859763  0.01061430 
#> 
#> $`brq(am + wt)`
#> (Intercept)          am          wt 
#>    3.133808    1.861487   -1.167737 
#> 
#> $offset
#>  50% 
#> 19.2

# or use
coef(boosted.rq, aggregate = "sum")
#> $`brq(cyl * hp)`
#> (Intercept)         cyl          hp      cyl:hp 
#> 19.33792010 -2.47917356 -0.09859763  0.01061430 
#> 
#> $`brq(am + wt)`
#> (Intercept)          am          wt 
#>    3.133808    1.861487   -1.167737 
#> 
#> $offset
#>  50% 
#> 19.2

### Printing result summaries 
summary(boosted.rq)
#> 
#>   Boosting Regression Qauntiles
#> 
#> Call:  boostrq(formula = mpg ~ brq(cyl * hp) + brq(am + wt), data = mtcars,      mstop = 200, nu = 0.1, tau = 0.5)
#> formula: mpg ~ brq(cyl * hp) + brq(am + wt)
#> 
#> 
#>   Quantile Regression
#> Loss Function: tau * (y - f) * ((y - f) > 0) +
#>         (tau - 1) * (y - f) * ((y - f) <= 0)
#> Negative Gradient: tau * ((y - f) > 0) + (tau - 1) * ((y - f) <= 0)
#> 
#> Number of boosting iterations: mstop = 200 
#> Step size: = 0.1 
#> Offset:  19.2 
#> Number of baselearners:  2 
#> 
#> Estimated coefficients:
#> $`brq(cyl * hp)`
#> (Intercept)         cyl          hp      cyl:hp 
#> 19.33792010 -2.47917356 -0.09859763  0.01061430 
#> 
#> $`brq(am + wt)`
#> (Intercept)          am          wt 
#>    3.133808    1.861487   -1.167737 
#> 
#> $offset
#>  50% 
#> 19.2 
#> 
#> 
#> Selection frequencies:
#>     Intercept brq(cyl * hp)  brq(am + wt) 
#>         0.025         0.645         0.330
```

``` r
### Have a look at the underlying baselearner model matrices
boosted.rq$baselearner.matrix()

### Selected component for each iteration
boosted.rq$xselect()
c("Intercept", boosted.rq$baselearner.names)[boosted.rq$xselect() + 1]

### Current working residuals (negative gradients)
boosted.rq$neg.gradients()

### Course of empirical risk during the boosting process
boosted.rq$risk()

### Current fitted values
boosted.rq$fitted()
# or use
fitted(boosted.rq)

### Current residuals
boosted.rq$resid()
# or use
residuals(boosted.rq)

### Model predictions
dat.pred <- data.frame(
  cyl = c(4, 6),
  hp = c(90, 134),
  wt = c(3.125, 2.485), 
  am = c(1, 0)
)
boosted.rq$predict(newdata = dat.pred, aggregate = "sum") # also try aggregate = "cumsum" or "none"
# or use
predict(boosted.rq, newdata = dat.pred, aggregate = "sum")
```

``` r

### Update the number of iterations without to fully refit the model
### If mstop_new > mstop_current: The fitting process will start at the current number of iterations
### If mstop_new < mstop_current: The model result are subsetted, thus, the model is not refitted

### current number of iterations
boosted.rq$mstop()
#> [1] 200

### Update number of iterations
boosted.rq <- boosted.rq[300]
boosted.rq$mstop()
#> [1] 300

# or use
boosted.rq$subset(400)
boosted.rq$mstop()
#> [1] 400

# or use
mstop(boosted.rq) <- 100
boosted.rq$mstop()
#> [1] 100
```
