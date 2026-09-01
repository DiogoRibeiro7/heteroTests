# Validate data inputs for heteroscedasticity tests

Performs standard checks on input data.frames prior to running
heteroscedasticity diagnostics. The routine verifies that required
variables are available and that the sample size meets minimum criteria.

## Usage

``` r
rvalidateDataInputs(data, required_vars = NULL, min_obs = 10)
```

## Arguments

- data:

  A data.frame containing the variables required by the test.

- required_vars:

  Optional character vector of column names that must be present in
  `data`.

- min_obs:

  Minimum number of observations required. Defaults to `10`.

## Value

Invisibly returns the validated `data` object.

## Examples

``` r
heteroTests:::rvalidateDataInputs(mtcars, required_vars = c("mpg", "wt"))
```
