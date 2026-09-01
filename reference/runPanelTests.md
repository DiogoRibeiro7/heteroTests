# Run panel-data heteroscedasticity tests

Breusch-Pagan LM and Pesaran CD tests for panel models.

## Usage

``` r
runPanelTests(model, data, id, time = NULL,
          tests = c("bp_random", "pesaran"))
```

## Arguments

- model:

  A fitted `lm` object.

- data:

  Data frame used to fit `model`.

- id:

  Column identifying individuals.

- time:

  Column identifying time periods for the Pesaran test.

- tests:

  Character vector of test names.

## Value

A named list of `htest` objects.

## See also

[`runTimeSeriesTests`](https://diogoribeiro7.github.io/heteroTests/reference/runTimeSeriesTests.md)

## Examples

``` r
 df <- data.frame(id = rep(1:3, each = 4), time = rep(1:4, 3),
                  x = runif(12), y = rnorm(12))
 m <- lm(y ~ x, data = df)
 runPanelTests(m, df, id = "id", time = "time")
#> $bp_random
#> 
#>  Breusch-Pagan LM test for random effects
#> 
#> data:  y ~ x
#> LM = 3.0889, = 1, p-value = 0.07883
#> 
#> 
#> $pesaran
#> 
#>  Pesaran CD test for cross-sectional dependence
#> 
#> data:  y ~ x
#> z = -0.049429, p-value = 0.9606
#> 
#> 
```
