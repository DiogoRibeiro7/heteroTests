# Spatial heteroscedasticity test

Evaluates spatial clustering in squared residuals using Moran's I with a
permutation reference distribution. Significant positive autocorrelation
in the squared residuals is evidence of spatially varying variance.

## Usage

``` r
performSpatialHeteroTest(
  model,
  data,
  listw,
  permutations = 499,
  zero.policy = NULL
)
```

## Arguments

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object
  describing the mean structure whose residual variance is to be
  assessed.

- data:

  A [base::data.frame](https://rdrr.io/r/base/data.frame.html) (or
  compatible object) containing the variables referenced in `model`. The
  data must include all observations used to fit `model` and should not
  contain unresolved missing values.

- listw:

  Spatial weights in `spdep` `listw` format or a numeric matrix
  coercible via
  [`spdep::mat2listw()`](https://r-spatial.github.io/spdep/reference/mat2listw.html).

- permutations:

  Number of Monte Carlo permutations used to compute the reference
  distribution.

- zero.policy:

  Logical flag forwarded to the spatial diagnostic to permit islands
  with no neighbours.

## Value

An `htest` result with Moran's I statistic applied to squared residuals.

## References

Anselin, L. (1988). *Spatial Econometrics: Methods and Models*. Kluwer.

Bivand, R. S., Pebesma, E., & Gómez-Rubio, V. (2013). *Applied Spatial
Data Analysis with R* (2nd ed.). Springer.

## Examples

``` r
if (requireNamespace("spdep", quietly = TRUE)) {
  data(mtcars)
  coords <- cbind(runif(nrow(mtcars)), runif(nrow(mtcars)))
  nb <- spdep::knn2nb(spdep::knearneigh(coords, k = 4))
  lw <- spdep::nb2listw(nb)
  model <- lm(mpg ~ wt + hp, data = mtcars)
  performSpatialHeteroTest(model, mtcars, listw = lw, permutations = 199)
}
#> 
#>  Spatial heteroscedasticity test (Moran's I on squared residuals)
#> 
#> data:  mpg ~ wt + hp
#> statistic = -0.069454, permutations = 199, p-value = 0.555
#> alternative hypothesis: greater
#> 
```
