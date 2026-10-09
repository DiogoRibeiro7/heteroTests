# Run heteroscedasticity diagnostics on the data of a survey design

A convenience wrapper that takes the data out of a
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
object, fits the model by ordinary least squares and runs
[`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md).
**It does not use the sampling weights, strata or clusters of the
design**, and warns when the design has unequal weights or clusters.

## Usage

``` r
runSurveyHeteroTests(formula, design, tests = c("white", "breusch_pagan"), ...)
```

## Arguments

- formula:

  Model formula specifying the mean structure.

- design:

  A
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
  object. Only its data are used.

- tests:

  Diagnostic names passed on to `runHeteroTests`.

- ...:

  Additional arguments forwarded to `runHeteroTests`.

## Details

The tests are the ones
[`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
runs on any `lm`: they treat the observations as an independent sample
with equal weights. They hold their level on survey data when the design
can be ignored given the regressors, that is when the chance of being
sampled depends only on variables in the model, and the sample has no
clusters. When selection depends on the response, or observations are
clustered, they reject a model with constant error variance far more
often than their nominal level.

The package has no design-based test of constant variance. Sampling
weights are not precision weights: they say how many population units an
observation stands for and nothing about its error variance. Passing
them to `lm(weights = )`, or testing a
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) fit,
makes the tests examine residuals multiplied by the square roots of the
weights, and a homoscedastic model is then rejected whenever the weights
vary with the regressors. `svyglm` fits are refused for that reason.

`inst/validation/survey-designs-size.R` measures all of this, together
with two design-based tests that were tried and not adopted because they
reject too often at the sample sizes examined.

## Earlier versions

Up to 0.12.0 this page described a survey-weighted fit. The weights did
not reach the fit, which was by ordinary least squares, as it is now, so
the reported values are unchanged.

## Value

An object of class `hetero_test_suite`.

## See also

[`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)

## Examples

``` r
# \donttest{
if (requireNamespace("survey", quietly = TRUE)) {
  data(api, package = "survey")
  design <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat)
  # Warns: the weights of this design are unequal and are not used.
  res <- runSurveyHeteroTests(api00 ~ api99 + ell, design)
  generics::tidy(res)
}
#> Warning: runSurveyHeteroTests() does not use the sampling weights of the survey design. The model is fitted by ordinary least squares and the tests treat the observations as an independent sample, so they hold their level only if the design can be ignored given the regressors. See ?runSurveyHeteroTests.
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
