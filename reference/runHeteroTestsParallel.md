# Parallel test execution helper

Executes a set of registered diagnostics in parallel when multiple CPU
cores are available, falling back to sequential execution otherwise.

## Usage

``` r
runHeteroTestsParallel(model, data, tests, n_cores = NULL)
```

## Arguments

- model:

  Fitted model object passed to each diagnostic. Must be compatible with
  the internal `.test_factory` registry.

- data:

  Data frame used to fit `model` and supplied to each diagnostic.

- tests:

  Character vector naming the diagnostics to execute.

- n_cores:

  Optional positive integer giving the number of worker processes to
  spawn. Defaults to `parallel::detectCores() - 1`, with a lower bound
  of 1.

## Value

Named list of test results in the order provided by `tests`.

## Details

When `n_cores` exceeds one and the `parallel` package is available, the
helper evaluates the diagnostics via
[`parallel::parLapply()`](https://rdrr.io/r/parallel/clusterApply.html)
after exporting the model and data to the worker environment. On systems
without fork support the function gracefully reverts to sequential
execution.

## See also

[`cachedTest()`](https://diogoribeiro7.github.io/heteroTests/reference/cachedTest.md)
for memoised execution of individual diagnostics.
