# Rank-based permutation heteroscedasticity test

Performs a non-parametric permutation test based on the rank correlation
between absolute residuals and a chosen ordering variable. Significant
correlations imply systematic changes in residual spread.

## Usage

``` r
performRankPermutationTest(
  model,
  data,
  order_by = NULL,
  B = 999,
  progress = interactive()
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

- order_by:

  Optional name of a predictor variable used to rank the observations.
  Defaults to the first non-intercept term in the model matrix.

- B:

  Number of permutation replications used to approximate the null
  distribution.

- progress:

  Logical toggle for progress reporting during permutations.

## Value

An `htest` object containing the observed Spearman correlation and a
permutation-based p-value.

## References

Hollander, M., Wolfe, D. A., & Chicken, E. (2013). *Nonparametric
Statistical Methods* (3rd ed.). Wiley.

## Examples

``` r
data(mtcars)
model <- lm(mpg ~ wt + hp, data = mtcars)
set.seed(42)
performRankPermutationTest(model, mtcars, B = 199, progress = FALSE)
#> 
#>  Rank permutation heteroscedasticity test
#> 
#> data:  mpg ~ wt + hp
#> rho = -0.16173, B = 199, p-value = 0.39
#> 
```
