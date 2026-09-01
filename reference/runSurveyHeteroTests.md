# Run heteroscedasticity diagnostics on survey designs

Fits a survey-weighted linear model with
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) and
forwards the result to
[`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
so standard diagnostics can be reused with complex survey data.

## Usage

``` r
runSurveyHeteroTests(formula, design, tests = c("white", "breusch_pagan"), ...)
```

## Arguments

- formula:

  Model formula specifying the survey-weighted mean structure.

- design:

  A `survey::survey.design` object describing weights (and optionally
  strata or clusters).

- tests:

  Diagnostic names passed on to `runHeteroTests`.

- ...:

  Additional arguments forwarded to `runHeteroTests`.

## Value

An object of class `hetero_test_suite` (or `hetero_grouped_suite` when
grouped survey data are analysed).

## See also

[`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md),
[`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html)

## Examples

``` r
# \donttest{
if (requireNamespace("survey", quietly = TRUE)) {
  data(api, package = "survey")
  design <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat)
  res <- runSurveyHeteroTests(api00 ~ api99 + ell, design)
  generics::tidy(res)
}
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.2317 df = 5 p = 0.2843
#> [INFO] Running Breusch-Pagan test
#>      diagnostic statistic parameter    p.value estimate
#> 1         white  6.231719         5 0.28432020       NA
#> 2 breusch_pagan  4.981994         2 0.08282735       NA
#>                  alternative                                    method nobs
#> 1 heteroscedasticity present       White's test for heteroscedasticity  200
#> 2                       <NA> Breusch-Pagan test for heteroscedasticity  200
#>   status message suggestions
#> 1     ok    <NA>        <NA>
#> 2     ok    <NA>        <NA>
# }
```
