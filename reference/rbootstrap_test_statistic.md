# Bootstrap a test statistic

Provides a consistent wrapper around bootstrap resampling for test
statistics produced by heteroscedasticity diagnostics. The routine
refits the supplied model on bootstrap resamples of the working data and
evaluates `test_function` on each replicate. Percentile confidence
intervals and an empirical p-value are returned alongside the bootstrap
distribution.

## Usage

``` r
rbootstrap_test_statistic(
  test_function,
  model,
  data,
  B = 1000,
  parallel = FALSE,
  ci_level = 0.95,
  resample = c("pairs"),
  n_cores = NULL,
  progress = interactive(),
  ...
)
```

## Arguments

- test_function:

  A function that accepts `(model, data, ...)` and returns an object
  with a numeric `statistic` element (typically an `htest` result).

- model:

  A fitted model of class `lm` or `glm`.

- data:

  Data frame used when fitting `model`.

- B:

  Integer, number of bootstrap replications. Defaults to 1000.

- parallel:

  Logical, compute replicates in parallel when possible?

- ci_level:

  Confidence level for the percentile interval.

- resample:

  Bootstrap strategy. Currently only "pairs" sampling is supported where
  rows of `data` are sampled with replacement.

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

  Percentile confidence interval for the statistic.

- `p_value`:

  Empirical bootstrap p-value.

- `B`:

  Requested number of replications.

- `effective_samples`:

  Number of non-missing bootstrap replications.

- `call`:

  Matched call for reproducibility.

## Examples

``` r
data(mtcars)
model <- lm(mpg ~ wt + cyl, data = mtcars)
if (interactive()) {
  set.seed(123)
  boot <- rbootstrap_test_statistic(performWhiteTest, model, mtcars, B = 100)
  boot$ci
}
```
