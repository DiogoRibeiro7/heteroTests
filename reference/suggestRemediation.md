# Suggest remediation actions for heteroscedasticity

Given diagnostic test results from
[`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md),
this helper provides a basic summary of recommended follow-up steps. It
evaluates the number of significant tests and proposes variance
stabilising transformations or modelling approaches.

## Usage

``` r
suggestRemediation(diagnostic_results)

# S3 method for class 'remediation_suggestions'
print(x, ...)
```

## Arguments

- diagnostic_results:

  Named list of `htest` objects as returned by
  [`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md).

- x:

  Object of class `remediation_suggestions`.

- ...:

  Not used.

## Value

An object of class `remediation_suggestions` containing a summary of
potential actions.

## Details

The function counts how many diagnostic tests yield a p-value below
0.05. If none are significant it returns a brief conclusion that no
action is needed. Otherwise a severity level is assigned and appropriate
transformations or variance modelling approaches are suggested.

## Examples

``` r
data(mtcars)
mod <- lm(mpg ~ wt + qsec, data = mtcars)
res <- runHeteroTests(mod, mtcars)
suggestRemediation(res)
#> $severity
#> [1] "Low"
#> 
#> $transformations
#> [1] "log"  "sqrt"
#> 
#> attr(,"class")
#> [1] "remediation_suggestions"
```
