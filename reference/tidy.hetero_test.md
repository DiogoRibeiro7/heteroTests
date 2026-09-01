# broom methods for heteroscedasticity diagnostics

Tidy, glance and augment methods integrating heteroscedasticity
diagnostics with the broom generics from the generics package.

## Usage

``` r
# S3 method for class 'hetero_test'
tidy(x, ...)
# S3 method for class 'hetero_test'
glance(x, ...)
# S3 method for class 'hetero_test'
augment(x, data = NULL, ...)
# S3 method for class 'hetero_test_suite'
tidy(x, ...)
# S3 method for class 'hetero_test_suite'
glance(x, ...)
# S3 method for class 'hetero_test_suite'
augment(x, data = NULL, ...)
# S3 method for class 'hetero_grouped_suite'
tidy(x, ...)
```

## Arguments

- x:

  Heteroscedasticity diagnostic result(s) returned by `runHeteroTests`.

- data:

  Optional data frame used to augment diagnostics; defaults to the data
  embedded within the fitted model when available.

- ...:

  Passed through for compatibility.

## Value

A data frame summarising the diagnostic(s), or an augmented data set
containing fitted values and residuals alongside the original
predictors.

## See also

[`generics::tidy`](https://generics.r-lib.org/reference/tidy.html),
[`generics::glance`](https://generics.r-lib.org/reference/glance.html),
[`generics::augment`](https://generics.r-lib.org/reference/augment.html)

## Examples

``` r
if (requireNamespace("generics", quietly = TRUE)) {
  data(mtcars)
  fit <- lm(mpg ~ wt + qsec, data = mtcars)
  suite <- runHeteroTests(fit, mtcars, tests = c("white", "breusch_pagan"))
  generics::tidy(suite)
}
#>      diagnostic statistic parameter    p.value estimate
#> 1         white  11.82248         5 0.03730286       NA
#> 2 breusch_pagan   3.13479         2 0.20858780       NA
#>                  alternative                                    method nobs
#> 1 heteroscedasticity present       White's test for heteroscedasticity   32
#> 2                       <NA> Breusch-Pagan test for heteroscedasticity   32
#>   status message suggestions
#> 1     ok    <NA>        <NA>
#> 2     ok    <NA>        <NA>
```
