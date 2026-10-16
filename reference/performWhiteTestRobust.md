# Robust White test with bootstrap and effect sizes

Extends
[`performWhiteTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTest.md)
with optional bootstrap resampling, confidence intervals, effect size
reporting, and power analysis.

## Usage

``` r
performWhiteTestRobust(
  model,
  data = NULL,
  method = c("standard", "reduced"),
  bootstrap = FALSE,
  B = 999,
  ci_level = 0.95,
  parallel = FALSE
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
  be checked for missing values. When `NULL`, the default, the model
  frame of `model` is used. It holds the terms of the model rather than
  its variables (`log(x)` rather than `x`), so a model that transforms a
  variable needs `data`, as does a model with an
  [`offset()`](https://rdrr.io/r/stats/offset.html) term, and so may a
  fit whose `weights` or `offset` argument is a column of the data,
  which the model frame holds as `(weights)` or `(offset)`.

- method:

  Character string selecting the auxiliary specification. "standard"
  retains squares and cross-products, while "reduced" excludes
  cross-products for high-dimensional designs.

- bootstrap:

  Logical, compute bootstrap diagnostics for the statistic and p-value?
  The bootstrap is
  [`bootstrapTestStatistic()`](https://diogoribeiro7.github.io/heteroTests/reference/rbootstrap_test_statistic.md),
  so on a fit returned by
  [`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md)
  each replicate estimates the weights again, its default since 0.15.0.
  There is no argument to hold them fixed here;
  [`bootstrapTestStatistic()`](https://diogoribeiro7.github.io/heteroTests/reference/rbootstrap_test_statistic.md)
  with `reestimate_weights = FALSE` does that.

- B:

  Number of bootstrap replications when `bootstrap = TRUE`. Defaults to
  999, at which a bootstrap p-value of the form `(1 + #) / (B + 1)`
  gives an exact test at the 5% and 1% levels; it was 1000 before
  0.15.0. The value is recorded in `robust_details$bootstrap$B` whether
  or not the bootstrap runs.

- ci_level:

  Confidence level for reported intervals.

- parallel:

  Logical, allow parallel bootstrap evaluation when the `parallel`
  package is available. A seeded call gives the same result each time;
  see *Parallel replicates* in
  [`bootstrapTestStatistic()`](https://diogoribeiro7.github.io/heteroTests/reference/rbootstrap_test_statistic.md).

## Value

An object of class `htest` augmented with a `robust_details` list
containing bootstrap, effect size, and power information.

## Details

Provides enriched inference around the White test by combining bootstrap
resampling (Efron & Tibshirani, 1993) with asymptotic approximations.
The `robust_details` element summarises interval estimates, effect
sizes, and power calculations to aid decision-making.

## References

White, H. (1980). A heteroskedasticity-consistent covariance matrix
estimator and a direct test for heteroscedasticity. *Econometrica,
48*(4), 817–838.

Efron, B., & Tibshirani, R. J. (1993). *An Introduction to the
Bootstrap*. Chapman & Hall.

## See also

[`performWhiteTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTest.md)
for the base statistic and
[`performWhiteTestBootstrap()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestBootstrap.md)
for a lighter-weight resampling option.

## Examples

``` r
data(mtcars)
mod <- lm(mpg ~ wt + qsec, data = mtcars)
performWhiteTestRobust(mod, mtcars, bootstrap = TRUE, B = 200)
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.8225 df = 5 p = 0.0373
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.8225 df = 5 p = 0.0373
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.326 df = 5 p = 0.8024
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1625 df = 5 p = 0.675
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4072 df = 5 p = 0.6375
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6901 df = 5 p = 0.7476
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.096 df = 5 p = 0.5357
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.9305 df = 5 p = 0.4244
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.7381 df = 5 p = 0.0033
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0544 df = 5 p = 0.5416
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1683 df = 5 p = 0.8254
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1319 df = 5 p = 0.8306
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5989 df = 5 p = 0.6085
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0637 df = 5 p = 0.8403
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2889 df = 5 p = 0.6555
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1542 df = 5 p = 0.6762
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.5158 df = 5 p = 0.185
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.0912 df = 5 p = 0.4048
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0699 df = 5 p = 0.5394
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2707 df = 5 p = 0.8106
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1923 df = 5 p = 0.8219
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7265 df = 5 p = 0.8855
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7272 df = 5 p = 0.8855
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3695 df = 5 p = 0.6432
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.5011 df = 5 p = 0.0285
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8122 df = 5 p = 0.5768
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.2547 df = 5 p = 0.3856
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.8918 df = 5 p = 0.2288
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6737 df = 5 p = 0.7501
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9636 df = 5 p = 0.5547
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1298 df = 5 p = 0.8309
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0112 df = 5 p = 0.5478
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.6029 df = 5 p = 0.3468
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4902 df = 5 p = 0.4812
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0694 df = 5 p = 0.8395
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8707 df = 5 p = 0.7199
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4923 df = 5 p = 0.7777
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.6278 df = 5 p = 0.463
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.6514 df = 5 p = 0.4599
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.8927 df = 5 p = 0.1134
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4429 df = 5 p = 0.632
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6392 df = 5 p = 0.8965
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9263 df = 5 p = 0.9683
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.4243 df = 5 p = 0.1344
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8774 df = 5 p = 0.5672
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.4212 df = 5 p = 0.0295
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8582 df = 5 p = 0.7218
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2991 df = 5 p = 0.654
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2595 df = 5 p = 0.5127
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5249 df = 5 p = 0.2584
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5204 df = 5 p = 0.2588
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.8946 df = 5 p = 0.0363
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3804 df = 5 p = 0.6416
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.8083 df = 5 p = 0.3253
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.4714 df = 5 p = 0.1321
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.7556 df = 5 p = 0.4464
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.1692 df = 5 p = 0.1472
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4759 df = 5 p = 0.4831
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6547 df = 5 p = 0.753
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.783 df = 5 p = 0.2373
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.3266 df = 5 p = 0.1391
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.3504 df = 5 p = 0.2736
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 25.8919 df = 5 p = 1e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1528 df = 5 p = 0.9493
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0281 df = 5 p = 0.5454
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.35 df = 5 p = 0.1959
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3168 df = 5 p = 0.6513
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.9487 df = 5 p = 0.0767
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.6521 df = 5 p = 0.4598
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5131 df = 5 p = 0.6214
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7904 df = 5 p = 0.8773
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.7355 df = 5 p = 0.1714
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2929 df = 5 p = 0.5081
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5582 df = 5 p = 0.6146
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6463 df = 5 p = 0.8956
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.8113 df = 5 p = 0.4393
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.3813 df = 5 p = 0.0443
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4585 df = 5 p = 0.6297
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.4745 df = 5 p = 0.0916
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5584 df = 5 p = 0.6146
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.5984 df = 5 p = 0.06
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5853 df = 5 p = 0.7636
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.8092 df = 5 p = 0.4396
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.8398 df = 5 p = 0.4357
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9728 df = 5 p = 0.7042
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.7262 df = 5 p = 0.0571
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.2979 df = 5 p = 0.2783
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.9712 df = 5 p = 0.2228
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4021 df = 5 p = 0.6383
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8369 df = 5 p = 0.9746
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3134 df = 5 p = 0.6518
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8765 df = 5 p = 0.719
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9151 df = 5 p = 0.8608
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.2215 df = 5 p = 0.0692
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0811 df = 5 p = 0.6875
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7575 df = 5 p = 0.8816
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8341 df = 5 p = 0.5735
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1764 df = 5 p = 0.9471
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.114 df = 5 p = 0.4021
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.0539 df = 5 p = 0.2167
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.3027 df = 5 p = 0.2779
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3512 df = 5 p = 0.9296
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8597 df = 5 p = 0.5698
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.719 df = 5 p = 0.3345
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4956 df = 5 p = 0.4805
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.7204 df = 5 p = 0.3344
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2258 df = 5 p = 0.8171
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3794 df = 5 p = 0.7945
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.0827 df = 5 p = 0.4059
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6344 df = 5 p = 0.6031
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.9956 df = 5 p = 0.221
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1388 df = 5 p = 0.6786
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7977 df = 5 p = 0.5789
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8894 df = 5 p = 0.717
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4289 df = 5 p = 0.7872
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2056 df = 5 p = 0.6683
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8238 df = 5 p = 0.5751
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4996 df = 5 p = 0.6235
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1946 df = 5 p = 0.8216
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.3585 df = 5 p = 0.0447
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5934 df = 5 p = 0.7624
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.2269 df = 5 p = 0.0691
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1752 df = 5 p = 0.5245
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.688 df = 5 p = 0.1222
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.1266 df = 5 p = 0.4006
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.696 df = 5 p = 0.0578
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2996 df = 5 p = 0.8063
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.8236 df = 5 p = 0.4378
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.584 df = 5 p = 0.7638
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0012 df = 5 p = 0.9625
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3582 df = 5 p = 0.7977
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.7175 df = 5 p = 0.3347
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.0174 df = 5 p = 0.0045
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4063 df = 5 p = 0.6376
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0685 df = 5 p = 0.5396
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9191 df = 5 p = 0.9688
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.6728 df = 5 p = 0.3394
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.2817 df = 5 p = 0.2005
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.3183 df = 5 p = 0.2765
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2381 df = 5 p = 0.6633
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5963 df = 5 p = 0.7619
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.6553 df = 5 p = 0.1763
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.428 df = 5 p = 0.6343
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.49 df = 5 p = 0.9142
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8253 df = 5 p = 0.8727
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7404 df = 5 p = 0.5874
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1381 df = 5 p = 0.5297
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3337 df = 5 p = 0.9314
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2022 df = 5 p = 0.8205
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5307 df = 5 p = 0.7719
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1208 df = 5 p = 0.5322
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 19.7205 df = 5 p = 0.0014
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0449 df = 5 p = 0.6931
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4988 df = 5 p = 0.7767
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.8829 df = 5 p = 0.3178
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0887 df = 5 p = 0.6863
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.695 df = 5 p = 0.0264
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.7022 df = 5 p = 0.4533
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1678 df = 5 p = 0.6741
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9744 df = 5 p = 0.5531
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6464 df = 5 p = 0.8956
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7973 df = 5 p = 0.8764
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.2904 df = 5 p = 0.3815
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8115 df = 5 p = 0.8746
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8075 df = 5 p = 0.5775
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3848 df = 5 p = 0.6409
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.7565 df = 5 p = 0.3306
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.5533 df = 5 p = 0.4728
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.895 df = 5 p = 0.4288
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4312 df = 5 p = 0.4891
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4446 df = 5 p = 0.9194
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6086 df = 5 p = 0.9002
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.238 df = 5 p = 0.6634
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.065 df = 5 p = 0.2999
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2578 df = 5 p = 0.6603
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.3454 df = 5 p = 0.274
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.852 df = 5 p = 0.4342
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1659 df = 5 p = 0.5258
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2076 df = 5 p = 0.668
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0302 df = 5 p = 0.5451
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1666 df = 5 p = 0.6743
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.7848 df = 5 p = 0.3277
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.0373 df = 5 p = 0.4113
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.777 df = 5 p = 0.056
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.6957 df = 5 p = 0.2443
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.2069 df = 5 p = 0.1452
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.4539 df = 5 p = 0.2645
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.1736 df = 5 p = 0.0218
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9459 df = 5 p = 0.5572
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.656 df = 5 p = 0.7528
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4914 df = 5 p = 0.7778
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.9623 df = 5 p = 0.3099
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4101 df = 5 p = 0.9232
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.887 df = 5 p = 0.8645
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9831 df = 5 p = 0.8515
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1813 df = 5 p = 0.5236
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.8907 df = 5 p = 0.1624
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2834 df = 5 p = 0.5094
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.3715 df = 5 p = 0.4973
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.6191 df = 5 p = 0.1785
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.0658 df = 5 p = 0.1526
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.0254 df = 5 p = 0.1548
#> 
#>  White's test for heteroscedasticity (robust)
#> 
#> data:  model
#> X-squared = 11.822, df = 5, p-value = 0.0373
#> alternative hypothesis: heteroscedasticity present
#> 
```
