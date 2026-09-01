# Streaming Koenker studentized Breusch\\Pagan test

Evaluates Koenker's studentized variant of the Breusch\\Pagan test (the
\\n R^2\\ statistic from regressing the squared residuals on the
regressors) using streamed cross-products so that large datasets can be
processed without allocating the full auxiliary regression matrix.

## Usage

``` r
performKoenkerTestStreaming(model, data, chunk_size = 10000, progress = interactive())
```

## Arguments

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object supplying
  residuals and fitted values for the diagnostic.

- data:

  A [base::data.frame](https://rdrr.io/r/base/data.frame.html)
  containing the variables referenced in `model`. The data must align
  with the observations used to fit the model.

- chunk_size:

  Positive integer giving the number of observations processed per
  streaming chunk.

- progress:

  Logical flag indicating whether a textual progress bar should be shown
  while processing the chunks. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

## Value

A `htest` object equivalent to
[`performKoenkerTest`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md)
with metadata recording the streaming configuration.

## Details

Chunks of the data contribute to the cross-product matrices for the
regression of squared residuals on the model regressors. Aggregating
these matrices yields the exact Koenker statistic while bounding memory
usage. Sparse cross-products from Matrix are used automatically when
available for large chunks.

## See also

[`performKoenkerTest`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md)
for the standard implementation.

## Examples

``` r
data(mtcars)
mod <- lm(mpg ~ wt + qsec, data = mtcars)
performKoenkerTestStreaming(mod, mtcars, chunk_size = 16, progress = FALSE)
#> 
#>  Koenker studentized Breusch-Pagan test (streaming)
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 3.0858, df = 2, p-value = 0.2138
#> 
```
