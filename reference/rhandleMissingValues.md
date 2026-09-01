# Handle missing values according to a specified strategy

Provides a centralized mechanism for dealing with missing values in
heteroscedasticity tests. The function can either drop incomplete cases,
emit warnings, or fail fast when missingness is not permitted.

## Usage

``` r
rhandleMissingValues(data, variables, strategy = "complete_cases")
```

## Arguments

- data:

  A data.frame containing the data to be processed.

- variables:

  Character vector of variable names to inspect for missing values.

- strategy:

  Strategy describing how missing values should be handled. The options
  are:

  - `"complete_cases"` – remove incomplete rows and emit a warning that
    summarizes the data loss.

  - `"warn"` – behaves identically to `"complete_cases"` but is provided
    as a semantic alias when a calling test wants to emphasize the
    warning behaviour explicitly.

  - `"fail"` – abort when any missing values are detected.

## Value

A list with components `data` (the processed data frame),
`removed_cases` (row indices removed), `removed_count` (number of
removed observations), `removed_fraction` (proportion removed relative
to the original data), `removed_variables` (variables with observed
missingness), and `loss_message` (the formatted warning text).

## Examples

``` r
cleaned <- heteroTests:::rhandleMissingValues(mtcars, c("mpg", "wt"))
cleaned$removed_count
#> [1] 0
```
