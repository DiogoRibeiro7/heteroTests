# High-dimensional projection heteroscedasticity test

Applies principal component projections to the design matrix and
evaluates a Breusch-Pagan style statistic on the reduced representation.
The procedure is designed for settings where the number of predictors
rivals or exceeds the sample size.

## Usage

``` r
performHighDimensionalTest(
  model,
  data,
  variance_threshold = 0.9,
  max_components = NULL
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

- variance_threshold:

  Proportion of predictor variance that the selected principal
  components should explain. Defaults to 0.9.

- max_components:

  Optional upper bound on the number of principal components to retain.
  Defaults to `min(10, n - 5)`.

## Value

An `htest` object summarising the chi-squared statistic of the auxiliary
regression on principal component scores.

## References

Fan, J., & Lv, J. (2008). Sure independence screening for ultrahigh
dimensional feature space. *Journal of the Royal Statistical Society:
Series B, 70*(5), 849–911.

## Examples

``` r
set.seed(123)
X <- matrix(rnorm(80 * 20), nrow = 80)
beta <- c(rep(1, 5), rep(0, 15))
y <- X %*% beta + rnorm(80, sd = 0.5 + 0.2 * scale(X[, 1]))
#> Warning: NAs produced
df <- as.data.frame(cbind(y = as.numeric(y), X))
model <- lm(y ~ ., data = df)
performHighDimensionalTest(model, df)
#> Warning: Removed 1 observations due to missing values in y
#> 
#>  High-dimensional projection test for heteroscedasticity
#> 
#> data:  y ~ V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 +     V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21
#> X-squared = 17.115, df = 16, p-value = 0.3782
#> 
```
