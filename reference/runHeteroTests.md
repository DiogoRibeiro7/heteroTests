# Run multiple heteroscedasticity diagnostics

Convenience wrapper that executes several heteroscedasticity tests on a
fitted linear model.

## Usage

``` r
runHeteroTests(
  model,
  data = NULL,
  tests = c("white", "breusch_pagan"),
  use_cache = TRUE,
  chunk_threshold_mb = 100,
  chunk_size = 10000,
  progress = interactive()
)
```

## Details

By default runs
[`performWhiteTest`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTest.md)
and
[`performBPTest`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md),
but additional tests can be requested. Custom diagnostics registered via
`registerDiagnostic` are also available.

## Arguments

- model:

  an object of class `lm`.

- data:

  optional data frame used to fit `model`. If omitted,
  `model.frame(model)` is used.

- tests:

  character vector of test names to run. Supported values are any names
  registered via `registerDiagnostic`.

- use_cache:

  logical indicating whether cached results should be reused when
  available. Requires the digest package and defaults to `TRUE`.

- chunk_threshold_mb:

  numeric threshold (in megabytes) above which streaming implementations
  are preferred for supported diagnostics.

- chunk_size:

  integer number of observations processed per chunk when streaming
  diagnostics.

- progress:

  logical flag controlling whether textual progress bars are displayed
  when running multiple diagnostics.

## Value

A named list of `htest` objects.

## References

White, H. (1980). A heteroskedasticity-consistent covariance matrix
estimator and a direct test for heteroskedasticity. *Econometrica*,
48(4), 817–838. Breusch, T. S., & Pagan, A. R. (1979). A Simple Test for
Heteroscedasticity and Random Coefficient Variation. *Econometrica*,
47(5), 1287–1294.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
# Default: White and Breusch-Pagan
runHeteroTests(m, mtcars)
#> $white
#> 
#>  White's test for heteroscedasticity
#> 
#> data:  model
#> X-squared = 11.822, df = 5, p-value = 0.0373
#> alternative hypothesis: heteroscedasticity present
#> 
#> 
#> $breusch_pagan
#> 
#>  Breusch-Pagan test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 3.1348, df = 2, p-value = 0.2086
#> 
#> 
#> attr(,"class")
#> [1] "hetero_test_suite" "list"             
#> attr(,"tests")
#> [1] "white"         "breusch_pagan"
#> attr(,"model")
#> 
#> Call:
#> lm(formula = mpg ~ wt + qsec, data = mtcars)
#> 
#> Coefficients:
#> (Intercept)           wt         qsec  
#>     19.7462      -5.0480       0.9292  
#> 
#> attr(,"data_source")
#> [1] "data.frame"
# Specify additional diagnostics
runHeteroTests(m, mtcars, tests = c("white", "koenker", "ncv"))
#> [INFO] Running Koenker test
#> [INFO] Running NCV score test
#> $white
#> 
#>  White's test for heteroscedasticity
#> 
#> data:  model
#> X-squared = 11.822, df = 5, p-value = 0.0373
#> alternative hypothesis: heteroscedasticity present
#> 
#> 
#> $koenker
#> 
#>  Koenker studentized Breusch-Pagan test
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 3.0858, df = 2, p-value = 0.2138
#> 
#> 
#> $ncv
#> 
#>  Cook-Weisberg score test for non-constant variance
#> 
#> data:  mpg ~ wt + qsec; variance model: fitted values
#> X-squared = 0.59099, df = 1, p-value = 0.442
#> alternative hypothesis: error variance depends on the variance model
#> 
#> 
#> attr(,"class")
#> [1] "hetero_test_suite" "list"             
#> attr(,"tests")
#> [1] "white"   "koenker" "ncv"    
#> attr(,"model")
#> 
#> Call:
#> lm(formula = mpg ~ wt + qsec, data = mtcars)
#> 
#> Coefficients:
#> (Intercept)           wt         qsec  
#>     19.7462      -5.0480       0.9292  
#> 
#> attr(,"data_source")
#> [1] "data.frame"
custom <- function(model, data) list(stat = 1)
registerDiagnostic("custom", custom)
runHeteroTests(m, mtcars, tests = c("white", "custom"))
#> $white
#> 
#>  White's test for heteroscedasticity
#> 
#> data:  model
#> X-squared = 11.822, df = 5, p-value = 0.0373
#> alternative hypothesis: heteroscedasticity present
#> 
#> 
#> $custom
#> $custom$stat
#> [1] 1
#> 
#> 
#> attr(,"class")
#> [1] "hetero_test_suite" "list"             
#> attr(,"tests")
#> [1] "white"  "custom"
#> attr(,"model")
#> 
#> Call:
#> lm(formula = mpg ~ wt + qsec, data = mtcars)
#> 
#> Coefficients:
#> (Intercept)           wt         qsec  
#>     19.7462      -5.0480       0.9292  
#> 
#> attr(,"data_source")
#> [1] "data.frame"
```
