# Perform Fligner-Killeen test for homogeneity of variances

Non-parametric test based on ranks.

## Usage

``` r
performFlignerKilleenTest(model, data = NULL, group)
```

## Details

Absolute residuals are ranked after adjusting for group medians. The
statistic approximates a chi-square distribution with \\k-1\\ degrees of
freedom.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`. When `NULL`, the default, the model
  frame of `model` is used, so `group` must then be a variable of the
  model. The model frame holds the terms of the model rather than its
  variables (`log(x)` rather than `x`), so a model that transforms a
  variable needs `data`, as does a model with an
  [`offset()`](https://rdrr.io/r/stats/offset.html) term.

- group:

  name of the grouping variable.

## Value

An object of class `htest` containing the chi-squared statistic, p-value
and degrees of freedom.

## References

Fligner, M. A., & Killeen, T. J. (1976). Distribution-free two-sample
tests for scale. *Journal of the American Statistical Association*,
71(353), 210–213.

## Examples

``` r
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performFlignerKilleenTest(m, mtcars, "cyl")
#> 
#>  Fligner-Killeen test for homogeneity of variances
#> 
#> data:  mpg ~ wt
#> X-squared = 4.5637, df = 2, p-value = 0.1021
#> 
```
