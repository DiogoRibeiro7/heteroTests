# Validate distributional assumptions

Assesses frequently used distributional assumptions before running
heteroscedasticity diagnostics. The checks cover approximate normality,
positivity for log-transformed quantities, sufficient variation, and the
presence of extreme outliers.

## Usage

``` r
rvalidateDistributionalAssumptions(data, assumptions = list())
```

## Arguments

- data:

  A data.frame containing the variables required for assessment.

- assumptions:

  A named list describing the desired checks. Recognised entries are:

  - `normality` – character vector or list with element `variables`
    specifying which columns should satisfy approximate normality.
    Optional list elements `alpha` (significance level) and
    `sample_limit` (maximum sample size for the Shapiro–Wilk test) can
    be supplied.

  - `positive` – character vector or list identifying variables that
    must contain positive values. When provided as a list, `test_name`
    overrides the label used in error messages.

  - `variation` – character vector or list (with `variables` and
    optional `tolerance`) describing variables that must exhibit
    non-negligible variance.

  - `outliers` – character vector or list of variables to screen for
    extreme observations. Lists may include a numeric `threshold`
    specifying the acceptable number of robust standard deviations.

## Value

A list containing `passed` (logical flag), `messages` (character vector
of violations), `warnings` (character vector of recoverable issues), and
`details` (named list with diagnostic information for each assumption).

## Examples

``` r
res <- heteroTests:::rvalidateDistributionalAssumptions(
  mtcars,
  assumptions = list(
    normality = list(variables = "mpg"),
    positive = list(variables = "disp", test_name = "Demo Test")
  )
)
res$passed
#> [1] TRUE
```
