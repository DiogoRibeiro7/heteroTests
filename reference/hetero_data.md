# Simulated dataset with heteroscedastic errors

This dataset is generated from the model \\y_i = 1 + 2 x_i +
\varepsilon_i\\, where \\\varepsilon_i \sim N(0, (0.5 + 2 x_i)^2)\\. A
fixed seed ensures repeatable values.

## Usage

``` r
data(hetero_data)
```

## Format

A data frame with 100 rows and 2 variables:

- x:

  predictor

- y:

  response

## Source

Simulated with `set.seed(42); runif()` and
[`rnorm()`](https://rdrr.io/r/stats/Normal.html).

## Examples

``` r
data(hetero_data)
plot(hetero_data$x, hetero_data$y)
```
