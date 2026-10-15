# Perform Pesaran CD test

Checks for cross-sectional dependence in panel residuals.

## Usage

``` r
performPesaranTest(model, data, id, time)
```

## Details

The statistic averages pairwise correlation coefficients of the
residuals and is standardized to follow a normal distribution under the
null of independence.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`.

- id:

  individual identifier column.

- time:

  time column.

## Value

An object of class `htest`.

## References

Pesaran, M. H. (2004). General diagnostic tests for cross section
dependence in panels. *Cambridge Working Papers in Economics*.

## Examples

``` r
 df <- data.frame(id = rep(1:3, each = 5), time = rep(1:5, 3), x = runif(15), y = rnorm(15))
 m <- lm(y ~ x, data = df)
 performPesaranTest(m, df, "id", "time")
#> 
#>  Pesaran CD test for cross-sectional dependence
#> 
#> data:  y ~ x
#> z = 0.40875, p-value = 0.6827
#> 
```
