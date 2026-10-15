# Summarise heteroscedasticity test results

Runs selected diagnostics for a single model and returns a tidy data
frame of test statistics and p-values.

## Usage

``` r
compareTestResults(model, data = NULL, tests = c("white", "koenker"))
```

## Arguments

- model:

  A fitted `lm` model or a formula.

- data:

  Data frame used if `model` is a formula.

- tests:

  Character vector of heteroscedasticity tests. Defaults to the White
  and Koenker tests, the default of
  [`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md);
  it was the White and the classical Breusch-Pagan tests up to 0.14.0.

## Value

Data frame with columns `test`, `statistic`, and `p.value`.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, mtcars)
compareTestResults(m)
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.8225 df = 5 p = 0.0373
#> [INFO] Running Koenker test
#>      test statistic    p.value
#> 1   white 11.822480 0.03730286
#> 2 koenker  3.085836 0.21375642
```
