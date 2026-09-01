# Summarise heteroscedasticity test results

Runs selected diagnostics for a single model and returns a tidy data
frame of test statistics and p-values.

## Usage

``` r
compareTestResults(model, data = NULL, tests = c("white", "breusch_pagan"))
```

## Arguments

- model:

  A fitted `lm` model or a formula.

- data:

  Data frame used if `model` is a formula.

- tests:

  Character vector of heteroscedasticity tests.

## Value

Data frame with columns `test`, `statistic`, and `p.value`.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, mtcars)
compareTestResults(m)
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.8225 df = 5 p = 0.0373
#> [INFO] Running Breusch-Pagan test
#>            test statistic    p.value
#> 1         white  11.82248 0.03730286
#> 2 breusch_pagan   3.13479 0.20858780
```
