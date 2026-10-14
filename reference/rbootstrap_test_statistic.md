# Bootstrap a test statistic

Provides a consistent wrapper around bootstrap resampling for test
statistics produced by heteroscedasticity diagnostics. The routine
refits the supplied model on each bootstrap replicate and evaluates
`test_function` on it, returning the bootstrap distribution, a
percentile interval, and—for null-imposed resampling—a p-value.

## Usage

``` r
bootstrapTestStatistic(
  test_function,
  model,
  data,
  B = 1000,
  parallel = FALSE,
  ci_level = 0.95,
  resample = c("null", "pairs"),
  n_cores = NULL,
  progress = interactive(),
  ...
)

rbootstrap_test_statistic(
  test_function,
  model,
  data,
  B = 1000,
  parallel = FALSE,
  ci_level = 0.95,
  resample = c("null", "pairs"),
  n_cores = NULL,
  progress = interactive(),
  ...
)
```

## Details

The `resample` argument decides what the replicates mean.

With `resample = "null"` (the default) the response is regenerated as
\\\hat y_i + e^\*\_i\\, where the \\e^\*\_i\\ are drawn with replacement
from the model's residuals after dividing by \\\sqrt{1 - h_i}\\ and
centring. Those data satisfy homoscedasticity by construction, so the
replicates estimate the null distribution and `p_value` is a test. The
leverage correction matters: OLS residuals have variance \\\sigma^2 (1 -
h_i)\\, and resampling them unscaled under-disperses the regenerated
errors, which inflates the rejection rate.

With `resample = "pairs"` rows are sampled with replacement. The
replicates then follow the statistic's distribution under whatever
variance structure the data actually have, which is useful for
describing its variability but is not a null distribution: the
replicates are centred on the observed statistic, so comparing the two
returns roughly 0.5 whatever the data. Measured rejection under
\\\sigma_i = x_i^2\\ was 0%. `p_value` is therefore `NA` for pairs
resampling.

Neither strategy accepts a `glm`: each replicate is refitted with least
squares, so the family and link would be discarded and the replicates
would describe a different model from the one supplied.

A weighted `lm` is resampled on the scale where its null hypothesis is
constant variance, the regression of \\\sqrt{w_i}\\ y_i\\ on
\\\sqrt{w_i}\\ x_i\\. Null-imposed resampling draws the \\e^\*\_i\\ from
the Pearson residuals \\\sqrt{w_i}\\ e_i\\, divided by \\\sqrt{1 -
h_i}\\ with the leverages of that regression and centred, and divides
each draw by \\\sqrt{w_i}\\ before adding it to \\\hat y_i\\. Pairs
resampling draws each row with its weight. Either way each replicate is
refitted with the weights, so `test_function` sees a weighted fit, as it
did on the original data. The weights are held fixed; when they were
estimated from the same data, see the section on weighted fits in
[`performWildBootstrapTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWildBootstrapTest.md).

Weighted or not, each replicate is refitted with an offset the model was
given as an argument, `lm(..., offset = o)`; an offset written in the
formula, `offset(o)`, is evaluated again from the data of the replicate.
The rows of `data` are matched to those of the fit by row name, so that
each row is regenerated, or drawn, with its own fitted value, weight and
offset. Rows the model was not fitted to, such as those it dropped for
missing values, are left out, for the observed statistic as for the
replicates; the rows that stay keep the order of `data`. `data` that
lacks a row the model was fitted to is refused. Data without row names
of its own, such as a tibble or a data.table, that has as many rows as
the fit is taken in the order of the fit. The model is then refitted to
the rows of `data`, and `data` is refused when that refit does not give
the residuals of the fit to a relative `1e-8`: rows in another order
taken by position, values that differ from those the model was fitted
to, or a variable the formula finds outside `data`, which keeps the
order of the fit. Pairs resampling of a model with such a variable, for
example `offset(o)` with `o` in the environment of the formula, is
refused too; give such an offset as an argument instead.

Null-imposed resampling additionally requires the response to be a plain
variable. For `log(y) ~ x` the fitted values are on the log scale while
the data column holds `y`, so regenerating one from the other and
refitting would take the logarithm twice. That case raises an error;
`resample = "pairs"` resamples rows and is unaffected by it.

## Arguments

- test_function:

  A function that accepts `(model, data, ...)` and returns an object
  with a numeric `statistic` element (typically an `htest` result).

- model:

  A fitted model of class `lm`, with or without weights. A `glm` is
  rejected: each replicate is refitted with least squares, which would
  discard its family and link.

- data:

  Data frame used when fitting `model`, holding the rows it was fitted
  to under their row names. Other rows are left out. See Details.

- B:

  Integer, number of bootstrap replications. Defaults to 1000.

- parallel:

  Logical, compute replicates in parallel when possible?

- ci_level:

  Confidence level for the percentile interval.

- resample:

  Bootstrap strategy. `"null"` (the default) regenerates the response
  under homoscedasticity and yields a testable `p_value`; `"pairs"`
  samples rows of `data` with replacement and yields replicates and an
  interval but no p-value. See Details.

- n_cores:

  Optional integer specifying the number of worker processes to use when
  `parallel = TRUE`. Defaults to `parallel::detectCores() - 1`.

- progress:

  Logical flag controlling whether a textual progress bar is displayed
  during bootstrap replication.

- ...:

  Additional arguments forwarded to `test_function`.

## Value

A list with components:

- `original`:

  Result returned by `test_function` on the original data.

- `original_statistic`:

  Numeric value of the test statistic from the original sample.

- `replicates`:

  Numeric vector of bootstrap statistics.

- `ci`:

  Percentile interval of the bootstrap distribution of the statistic, at
  `ci_level`. This summarises where the resampled statistic falls; it is
  not a confidence interval for a parameter and carries no coverage
  guarantee.

- `p_value`:

  Empirical p-value from the null-imposed replicates, computed as \\(1 +
  \\\\T^\*\_b \ge T\\) / (B\_{\mathrm{eff}} + 1)\\; `NA` when
  `resample = "pairs"`.

- `B`:

  Requested number of replications.

- `effective_samples`:

  Number of non-missing bootstrap replications.

- `call`:

  Matched call for reproducibility.

## Deprecated name

`rbootstrap_test_statistic()` is deprecated since 0.14.0: use
`bootstrapTestStatistic()`, the same function under the package's naming
scheme. The old name still works, returns what it returned and warns.

## Examples

``` r
data(mtcars)
model <- lm(mpg ~ wt + cyl, data = mtcars)
if (interactive()) {
  set.seed(123)
  boot <- bootstrapTestStatistic(performWhiteTest, model, mtcars, B = 100)
  boot$ci
}
```
