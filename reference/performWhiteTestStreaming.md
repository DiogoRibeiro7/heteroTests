# Memory-efficient White test for large datasets

Computes White's statistic via chunked cross-products rather than
fitting the full auxiliary regression in memory. This streaming approach
allows the test to scale to datasets that would otherwise exhaust
available RAM.

## Usage

``` r
performWhiteTestStreaming(
  model,
  data,
  chunk_size = 10000,
  cross_products = TRUE,
  max_interactions = 10,
  progress = interactive()
)
```

## Arguments

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object
  representing the mean specification to be diagnosed.

- data:

  A [base::data.frame](https://rdrr.io/r/base/data.frame.html) (or
  object coercible to one) containing the variables referenced by
  `model`. It must include the observations used to fit `model` and will
  be checked for missing values.

- chunk_size:

  Positive integer specifying the number of observations per chunk.
  Smaller values reduce memory usage at the expense of additional
  iteration overhead.

- cross_products:

  Logical scalar indicating whether to include all pairwise
  cross-products of the regressors in the auxiliary regression. Defaults
  to `TRUE` and should remain enabled unless dimensionality makes the
  regression unstable.

- max_interactions:

  Single positive integer giving the maximum number of original
  predictors for which cross-products are generated. When the number of
  regressors exceeds this threshold, cross-products are dropped to avoid
  explosive growth in columns. Defaults to `10`.

- progress:

  Logical flag indicating whether a progress bar should be displayed
  while streaming the data. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

A `htest` object containing the chi-squared statistic, p-value, and
metadata about the chunked computation.

## Details

The streaming implementation iteratively builds the cross-product
matrices required for the auxiliary regression without materialising the
full design matrix. Each chunk contributes to \\X'X\\, \\X'y\\, and
summary statistics for the response. The final \\n R^2\\ statistic is
then computed exactly as in the standard White test, ensuring numerical
equivalence while dramatically reducing peak memory usage. When `Matrix`
is installed, sparse cross-products are leveraged automatically for
large chunks.

## References

White, H. (1980). A heteroskedasticity-consistent covariance matrix
estimator and a direct test for heteroscedasticity. *Econometrica,
48*(4), 817–838.

## See also

[`performWhiteTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTest.md)
for the exact computation and
[performWhiteTestRobust()](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestRobust.md)
for enhanced reporting.

## Examples

``` r
data(mtcars)
performWhiteTestStreaming(lm(mpg ~ wt + qsec, data = mtcars), mtcars, chunk_size = 16)
#> 
#>  White's test for heteroscedasticity (streaming)
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 11.822, df = 5, p-value = 0.0373
#> 
```
