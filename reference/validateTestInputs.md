# Validate model and data inputs before running diagnostics

Provides the compatibility wrapper used by the public testing interface
to ensure that fitted-model objects and their associated data satisfy
minimal quality requirements. The helper guards against the most common
issues that invalidate heteroscedasticity tests and produces actionable
error messages that reference the calling diagnostic.

## Usage

``` r
validateTestInputs(model, data, test_name, min_obs = 10)
```

## Arguments

- model:

  A fitted model created by
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

- data:

  A `data.frame` containing the variables used to fit `model`.

- test_name:

  Character scalar naming the diagnostic that is about to run; included
  in error messages for clarity.

- min_obs:

  Non-negative integer giving the minimum sample size accepted by the
  diagnostic. Defaults to `10`.

## Value

Invisibly returns `TRUE` when validation succeeds. Execution stops with
an informative error when any check fails.

## Details

The routine performs four layers of validation:

1.  confirm that `model` inherits from `lm` or `glm` and that its
    coefficients and residuals are finite;

2.  verify that `data` is a `data.frame` with at least `min_obs` rows;

3.  ensure the residual vector is available, finite, and aligned with
    the supplied data;

4.  emit warnings when studentised residuals exceed five standard
    deviations in absolute value or when the dataset is very large (more
    than 10,000 observations).

Results are cached (when the **digest** package is installed) so
repeated calls with unchanged inputs return immediately.

## References

Fox, J. (2015). *Applied Regression Analysis and Generalized Linear
Models* (3rd ed.). SAGE.

Belsley, D. A., Kuh, E., & Welsch, R. E. (1980). *Regression
Diagnostics: Identifying Influential Data and Sources of Collinearity*.
Wiley.

## See also

[`checkModel()`](https://diogoribeiro7.github.io/heteroTests/reference/checkModel.md),
[`checkModelEnhanced()`](https://diogoribeiro7.github.io/heteroTests/reference/checkModelEnhanced.md),
[`rvalidateModelInputs()`](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateModelInputs.md),
[`rvalidateTestRequirements()`](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateTestRequirements.md)

## Examples

``` r
data(mtcars)
lm_fit <- stats::lm(mpg ~ wt + hp, data = mtcars)
validateTestInputs(lm_fit, mtcars, "white")

# \donttest{
noisy <- mtcars
noisy$mpg[1] <- 100
outlier_fit <- stats::lm(mpg ~ wt + hp, data = noisy)
validateTestInputs(outlier_fit, noisy, "white")
#> Warning: Residual outliers detected at rows 1 (|z| > 5). Inspect leverage before running white.
# }
```
