# Streaming Breusch\\Pagan test for large datasets

Computes the classical Breusch\\Pagan statistic using chunked
cross-products so that large datasets can be evaluated without
materialising the full auxiliary regression in memory. The streamed
result is algebraically identical to
[`performBPTest`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md).

## Usage

``` r
performBPTestStreaming(model, data, chunk_size = 10000, progress = interactive())
```

## Arguments

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object
  describing the mean structure whose residual variance is to be
  assessed.

- data:

  A [base::data.frame](https://rdrr.io/r/base/data.frame.html)
  containing the variables referenced in `model`. The data must include
  all observations used to fit the model.

- chunk_size:

  Positive integer giving the number of observations processed per
  streaming chunk. Smaller values reduce peak memory usage at the
  expense of additional iteration overhead.

- progress:

  Logical flag controlling whether a textual progress bar is displayed
  while chunks are processed. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

A `htest` object mirroring
[`performBPTest`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)
and reporting the chi-squared statistic, degrees of freedom, p-value,
and metadata describing the chunked computation.

## Details

Each chunk contributes to the cross-product matrices \\X'X\\ and \\X'y\\
for the auxiliary regression of squared residuals on the original
regressors. The chunks are aggregated to recover the exact
Breusch\\Pagan statistic while keeping memory usage bounded. When Matrix
is installed, sparse cross-products are used automatically for large
chunks.

## See also

[`performBPTest`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)
for the standard implementation.

## Examples

``` r
data(mtcars)
mod <- lm(mpg ~ wt + qsec, data = mtcars)
performBPTestStreaming(mod, mtcars, chunk_size = 16, progress = FALSE)
#> 
#>  Breusch-Pagan test for heteroscedasticity (streaming)
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 3.1348, df = 2, p-value = 0.2086
#> 
```
