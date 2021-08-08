
<!-- README.md is generated from README.Rmd. Please edit that file -->

# boostrq

<!-- badges: start -->
<!-- badges: end -->

Package for boosting regression quantiles (Bauer, Haupt, & Linner, 2021)

## Installation

You can install the development version of boostrq from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stefanlinner/boostrq")
```

## Example

This is a basic example which shows you how to use the boostrq function:

``` r
library(boostrq)
## basic example code
boosted.rq <- 
  boostrq(formula = mpg ~ brq(hp:cyl, cyl * hp) + brq(am), data = mtcars, mstop = 200, nu = 0.1, tau = 0.5, offset = 0.5, method = "fn")

boosted.rq
#> 
#>   Boosting Regression Qauntiles
#> 
#> Call:  boostrq(formula = mpg ~ brq(hp:cyl, cyl * hp) + brq(am), data = mtcars,      mstop = 200, nu = 0.1, tau = 0.5, offset = 0.5, method = "fn")
#> formula: mpg ~ brq(hp:cyl, cyl * hp) + brq(am)
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
coef(boosted.rq)
#> $`brq(hp:cyl, cyl * hp)`
#>  (Intercept)          cyl           hp       hp:cyl 
#> 19.689758198 -2.337799370 -0.093223812  0.009128088 
#> 
#> $`brq(am)`
#> (Intercept)          am 
#>   -1.399999    2.999999 
#> 
#> $offset
#>  50% 
#> 19.2
```
