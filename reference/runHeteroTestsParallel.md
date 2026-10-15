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

When `n_cores` exceeds one the diagnostics run on a socket cluster of
that many workers
([`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html)),
started for the call, each of which loads the installed heteroTests. The
model and the data travel to the workers with each task. With
`n_cores = 1`, or when the default comes to one worker, the diagnostics
run in the session.

Test `i` draws its random numbers from stream `i` of a L'Ecuyer-CMRG
generator seeded by one draw from the session's generator, on the
workers and in the session alike. A seeded call that runs a resampling
test, such as `"wild_bootstrap"`, therefore returns the same value each
time and whatever `n_cores`, `n_cores = 1` included. It is not the value
that test returns when called directly from the same seed, which draws
from the session's generator; both are bootstraps of the same data.

## See also

[`cachedTest()`](https://diogoribeiro7.github.io/heteroTests/reference/cachedTest.md)
for memoised execution of individual diagnostics.
