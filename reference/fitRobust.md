# Robust regression wrapper

Provides a simple interface to
[`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html) for robust
estimation.

## Usage

``` r
fitRobust(model, data = NULL, ...)
```

## Arguments

- model:

  A fitted model of class `lm` or a formula.

- data:

  Optional data frame if `model` is a formula.

- ...:

  Additional arguments passed to
  [`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html).

## Value

An object of class `rlm`.

## Examples

``` r
data(mtcars)
m <- fitRobust(mpg ~ wt + qsec, mtcars)
summary(m)
#> 
#> Call: rlm(formula = form, data = data)
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -4.13823 -1.87652 -0.01779  1.63282  6.03088 
#> 
#> Coefficients:
#>             Value    Std. Error t value 
#> (Intercept)  20.6990   4.9889     4.1490
#> wt           -5.1046   0.4597   -11.1031
#> qsec          0.8757   0.2517     3.4785
#> 
#> Residual standard error: 2.603 on 29 degrees of freedom
```
