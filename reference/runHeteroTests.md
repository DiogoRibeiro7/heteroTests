# Run multiple heteroscedasticity diagnostics

Convenience wrapper that executes several heteroscedasticity tests on a
fitted linear model.

## Usage

``` r
runHeteroTests(
  model,
  data = NULL,
  tests = c("white", "koenker"),
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
[`performKoenkerTest`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md),
the studentized Breusch-Pagan test, but additional tests can be
requested. Custom diagnostics registered via `registerDiagnostic` are
also available.

Up to 0.14.0 the default was `c("white", "breusch_pagan")`, whose second
test is the classical Breusch-Pagan statistic of
[`performBPTest`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md).
That statistic divides by \\2 \hat\sigma^4\\, the variance of the
squared errors only when they are normal. In the full sweep of
`inst/validation/`, with 150 observations and 400 replications, it
rejected a true null hypothesis 28.75% of the time at the 5% level when
the errors had a \\t_5\\ distribution, against 4.75% for Koenker's
statistic. The classical test is still run by name,
`tests = "breusch_pagan"`.

When a test stops with an error, the substitute registered for it is
run, if one is and it succeeds, and its result is returned under the
name of the test asked for: `"white"` falls back on White's test without
cross products, the classical Breusch-Pagan, Koenker and Cook-Weisberg
tests, `"koenker"` on the Cook-Weisberg test. The `method` of the result
names the test that ran, `attr(result, "diagnostic")` its registry name
and `attr(result, "extras")$status` is `"fallback"`. A weighted fit
whose weights are a column of the data, called without `data`, is such a
case: White's and Koenker's tests stop, and both names hold the
Cook-Weisberg test. Pass `data` for such a fit.

## Arguments

- model:

  a fitted `lm`, or a `glm` with the Gaussian family and the identity
  link, a model formula, a tidymodels workflow or a parsnip model fit.
  The tests refuse a `glm` of any other family or link since 0.15.0, the
  deprecated registry names `"ncv"` and `"student_bp"` and the
  substitutes of the fallback (Details) included: the call stops with
  the error of the first test asked for, and with grouped data each test
  is returned as failed.

- data:

  optional data frame used to fit `model`. If omitted,
  `model.frame(model)` is used, which serves only the tests that find
  what they need there (see their `data` argument). With data grouped by
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  the model is refitted within each group by evaluating its own call on
  that group, so every variable it uses, including weights, offset and
  subset, must be a column of `data`.

- tests:

  character vector of test names to run: any of the names
  [`listDiagnostics()`](https://diogoribeiro7.github.io/heteroTests/reference/listDiagnostics.md)
  returns, which include those added with `registerDiagnostic`. On a fit
  returned by
  [`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md)
  the resampling tests `"wild_bootstrap"` and `"white_bootstrap"`
  estimate the weights again in each replicate, as
  [`performWildBootstrapTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWildBootstrapTest.md)
  and
  [`performWhiteTestBootstrap()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestBootstrap.md)
  do by default since 0.15.0; to hold them fixed, call those functions
  with `reestimate_weights = FALSE`.

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
47(5), 1287–1294. Koenker, R. (1981). A note on studentizing a test for
heteroscedasticity. *Journal of Econometrics*, 17(1), 107–112.
[doi:10.1016/0304-4076(81)90062-2](https://doi.org/10.1016/0304-4076%2881%2990062-2)

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
# Default: White and Koenker
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
#> $koenker
#> 
#>  Koenker studentized Breusch-Pagan test
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 3.0858, df = 2, p-value = 0.2138
#> 
#> 
#> attr(,"class")
#> [1] "hetero_test_suite" "list"             
#> attr(,"tests")
#> [1] "white"   "koenker"
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
# Specify additional diagnostics, or the classical Breusch-Pagan test
runHeteroTests(m, mtcars, tests = c("white", "koenker", "cook_weisberg"))
#> [INFO] Running Cook-Weisberg test
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
#> $cook_weisberg
#> 
#>  Cook-Weisberg test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 0.59099, df = 1, p-value = 0.442
#> alternative hypothesis: error variance depends on the fitted values
#> 
#> 
#> attr(,"class")
#> [1] "hetero_test_suite" "list"             
#> attr(,"tests")
#> [1] "white"         "koenker"       "cook_weisberg"
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
runHeteroTests(m, mtcars, tests = "breusch_pagan")
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
#> [1] "breusch_pagan"
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
