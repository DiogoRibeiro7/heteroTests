# Benchmark heteroTests against reference implementations

`run_benchmark_suite()` evaluates a selection of heteroscedasticity
diagnostics across varying sample sizes while measuring runtime, memory
allocation, and statistical agreement with the lmtest and car packages.
The companion `generate_benchmark_report()` helper aggregates the raw
output into tidy summary tables and recommendations.

## Usage

``` r
run_benchmark_suite(
  sample_sizes = c(100L, 500L, 1000L, 10000L, 100000L, 1000000L),
  tests = NULL,
  replicates = 3L,
  hetero_patterns = c("none", "linear", "group"),
  hetero_strength = 1,
  baseline_packages = c("lmtest", "car"),
  seed = 123L,
  n_predictors = 4L,
  profile_memory = TRUE,
  progress = interactive()
)

generate_benchmark_report(benchmark_results, accuracy_tolerance = 1e-4)
```

## Arguments

- sample_sizes:

  Integer vector of observation counts to benchmark. Values may range
  from a few hundred observations to one million.

- tests:

  Optional character vector restricting the benchmark to specific
  diagnostics (currently `"breusch_pagan"`, `"koenker"`, and `"ncv"`).

- replicates:

  Scalar integer applied to all sample sizes or an integer vector
  matching `sample_sizes` to control the number of repetitions per
  configuration.

- hetero_patterns:

  Character vector describing the heteroscedasticity patterns used when
  generating synthetic data. Supported values are `"none"`, `"linear"`,
  `"group"`, and `"exponential"`; the vector is recycled across
  replications.

- hetero_strength:

  Positive numeric multiplier controlling the severity of variance
  heterogeneity.

- baseline_packages:

  Character vector of external packages used for comparisons. Missing
  dependencies are skipped and reported in the metadata.

- seed:

  Integer seed passed to the data generator for reproducibility.

- n_predictors:

  Number of regressors (excluding the intercept) used when simulating
  benchmark datasets.

- profile_memory:

  Logical; when `TRUE` and the bench package is installed, the suite
  records peak memory allocations in megabytes.

- progress:

  Logical indicating whether to display a textual progress bar during
  execution.

- benchmark_results:

  List returned by `run_benchmark_suite()`.

- accuracy_tolerance:

  Non-negative numeric tolerance for declaring statistical agreement
  with reference implementations (based on absolute p-value
  differences).

## Value

`run_benchmark_suite()` returns a list with four elements:

- performance:

  Data frame containing runtime, memory, and inference metrics for every
  test, sample size, and replicate.

- accuracy:

  Data frame recording absolute differences in p-values and test
  statistics relative to each baseline implementation.

- summary:

  List of aggregated medians for quick inspection of speed, memory, and
  accuracy.

- metadata:

  Metadata describing the benchmark configuration (sample sizes,
  baselines, seed, and availability of optional dependencies).

`generate_benchmark_report()` converts the raw results into aggregated
speed, memory, accuracy, recommendation, and scalability tables
alongside the original metadata.

## Details

Datasets are generated from a linear regression model with a
configurable number of predictors and variance patterns (none, linear,
group, or exponential). Each diagnostic from heteroTests is executed
alongside equivalent tests from lmtest and car when available. Memory
profiling is enabled via bench when installed; otherwise only runtimes
are collected. The output is suitable for regression testing (to detect
performance regressions) and for building artefacts such as CSV
benchmark reports.

## Examples

``` r
# \donttest{
if (requireNamespace("lmtest", quietly = TRUE) &&
    requireNamespace("car", quietly = TRUE)) {
  results <- run_benchmark_suite(
    sample_sizes = c(100L, 250L),
    replicates = 1L,
    profile_memory = FALSE,
    progress = FALSE
  )
  report <- generate_benchmark_report(results)
  head(report$recommendations)
}
#> [INFO] Running Breusch-Pagan test
#> [INFO] Running Koenker test
#> [INFO] Running NCV score test
#> [INFO] Running Breusch-Pagan test
#> [INFO] Running Koenker test
#> [INFO] Running NCV score test
#>                            test                       label sample_size
#> breusch_pagan.100 breusch_pagan            Breusch-Pagan LM         100
#> koenker.100             koenker      Koenker studentized BP         100
#> ncv.100                     ncv Non-constant variance score         100
#> breusch_pagan.250 breusch_pagan            Breusch-Pagan LM         250
#> koenker.250             koenker      Koenker studentized BP         250
#> ncv.250                     ncv Non-constant variance score         250
#>                   fastest_package fastest_median_time time_per_observation
#> breusch_pagan.100          lmtest               0.001                1e-05
#> koenker.100                lmtest               0.001                1e-05
#> ncv.100                       car               0.002                2e-05
#> breusch_pagan.250          lmtest               0.001                4e-06
#> koenker.250                lmtest               0.000                0e+00
#> ncv.250                       car               0.002                8e-06
#>                   memory_leader memory_median_mb
#> breusch_pagan.100          <NA>               NA
#> koenker.100                <NA>               NA
#> ncv.100                    <NA>               NA
#> breusch_pagan.250          <NA>               NA
#> koenker.250                <NA>               NA
#> ncv.250                    <NA>               NA
#>                                               accuracy_note
#> breusch_pagan.100 Matches baseline within 1.0e-04 tolerance
#> koenker.100       Matches baseline within 1.0e-04 tolerance
#> ncv.100           Matches baseline within 1.0e-04 tolerance
#> breusch_pagan.250 Matches baseline within 1.0e-04 tolerance
#> koenker.250       Matches baseline within 1.0e-04 tolerance
#> ncv.250           Matches baseline within 1.0e-04 tolerance
# }
```
