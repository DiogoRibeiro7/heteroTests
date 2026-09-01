# Plot diagnostics

Produce basic residual diagnostic plots.

## Usage

``` r
# S3 method for class 'HeteroDiagnostic'
plot(x, plots = c("residuals_fitted", "spread_level",
  "density", "qq", "bubble_variance"), ...)
```

## Arguments

- x:

  A `HeteroDiagnostic` object.

- plots:

  Character vector selecting which diagnostic panels to draw.

- ...:

  Further arguments passed to the underlying plotting routines.

## Value

List of ggplot objects.
