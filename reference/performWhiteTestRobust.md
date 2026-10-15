# Robust White test with bootstrap and effect sizes

Extends
[`performWhiteTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTest.md)
with optional bootstrap resampling, confidence intervals, effect size
reporting, and power analysis.

## Usage

``` r
performWhiteTestRobust(
  model,
  data,
  method = c("standard", "reduced"),
  bootstrap = FALSE,
  B = 1000,
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
  be checked for missing values.

- method:

  Character string selecting the auxiliary specification. "standard"
  retains squares and cross-products, while "reduced" excludes
  cross-products for high-dimensional designs.

- bootstrap:

  Logical, compute bootstrap diagnostics for the statistic and p-value?

- B:

  Number of bootstrap replications when `bootstrap = TRUE`.

- ci_level:

  Confidence level for reported intervals.

- parallel:

  Logical, allow parallel bootstrap evaluation when the `parallel`
  package is available.

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
#> [INFO] White test completed: statistic = 4.1767 df = 5 p = 0.5243
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.0809 df = 5 p = 0.2984
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2523 df = 5 p = 0.9398
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.6931 df = 5 p = 0.2445
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.918 df = 5 p = 0.8604
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0071 df = 5 p = 0.5484
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1534 df = 5 p = 0.8275
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.7347 df = 5 p = 0.1715
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.2991 df = 5 p = 0.1993
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.4501 df = 5 p = 0.1893
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.0159 df = 5 p = 0.4139
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.8661 df = 5 p = 0.2308
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1635 df = 5 p = 0.5261
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.579 df = 5 p = 0.3494
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.0149 df = 5 p = 0.3048
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4289 df = 5 p = 0.4895
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3186 df = 5 p = 0.8035
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7419 df = 5 p = 0.5871
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0753 df = 5 p = 0.8386
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.477 df = 5 p = 0.2625
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5982 df = 5 p = 0.7616
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8615 df = 5 p = 0.7213
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.622 df = 5 p = 0.2503
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.3353 df = 5 p = 0.1387
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.606 df = 5 p = 0.2516
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.477 df = 5 p = 0.2625
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.0425 df = 5 p = 0.1539
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.897 df = 5 p = 0.1132
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.763 df = 5 p = 0.2389
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.8873 df = 5 p = 0.2292
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7811 df = 5 p = 0.5813
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.7303 df = 5 p = 0.3334
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2091 df = 5 p = 0.944
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.0323 df = 5 p = 0.3031
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4925 df = 5 p = 0.7776
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.7703 df = 5 p = 0.1694
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.9617 df = 5 p = 0.31
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.0401 df = 5 p = 0.411
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.5743 df = 5 p = 0.1273
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.511 df = 5 p = 0.1853
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 16.2019 df = 5 p = 0.0063
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2043 df = 5 p = 0.6685
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9169 df = 5 p = 0.5614
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7393 df = 5 p = 0.7401
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0214 df = 5 p = 0.8462
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3601 df = 5 p = 0.7974
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5255 df = 5 p = 0.9101
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.6327 df = 5 p = 0.1246
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1921 df = 5 p = 0.5221
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.4546 df = 5 p = 0.2645
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.2667 df = 5 p = 0.2016
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.3407 df = 5 p = 0.0962
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4697 df = 5 p = 0.628
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9137 df = 5 p = 0.5619
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9094 df = 5 p = 0.5625
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3211 df = 5 p = 0.9327
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6198 df = 5 p = 0.6053
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.0505 df = 5 p = 0.0044
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.776 df = 5 p = 0.5821
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5799 df = 5 p = 0.7644
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8022 df = 5 p = 0.5782
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5673 df = 5 p = 0.9052
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.1876 df = 5 p = 0.0478
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8519 df = 5 p = 0.7228
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4725 df = 5 p = 0.4836
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.3638 df = 5 p = 0.3731
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.4215 df = 5 p = 0.2673
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8322 df = 5 p = 0.7258
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9934 df = 5 p = 0.9631
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.4471 df = 5 p = 0.0925
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.4629 df = 5 p = 0.1325
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.3381 df = 5 p = 0.1386
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.1474 df = 5 p = 0.3982
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6314 df = 5 p = 0.6036
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.8092 df = 5 p = 0.1671
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.954 df = 5 p = 0.0159
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.9582 df = 5 p = 0.421
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8924 df = 5 p = 0.8638
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.8284 df = 5 p = 0.3233
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.9002 df = 5 p = 0.2282
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.1329 df = 5 p = 0.3999
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0347 df = 5 p = 0.5444
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4592 df = 5 p = 0.7826
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.7969 df = 5 p = 0.2362
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9575 df = 5 p = 0.5555
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2667 df = 5 p = 0.8112
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6153 df = 5 p = 0.8994
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3384 df = 5 p = 0.648
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.9048 df = 5 p = 0.4276
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.7732 df = 5 p = 0.2381
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7647 df = 5 p = 0.7362
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.6891 df = 5 p = 0.3377
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3546 df = 5 p = 0.7982
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.1588 df = 5 p = 0.2911
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.17 df = 5 p = 0.1025
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2025 df = 5 p = 0.8205
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.9139 df = 5 p = 0.2271
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.4368 df = 5 p = 0.3649
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.6308 df = 5 p = 0.4626
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5989 df = 5 p = 0.7615
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.4488 df = 5 p = 0.3636
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1464 df = 5 p = 0.5285
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.9026 df = 5 p = 0.228
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9435 df = 5 p = 0.5576
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6266 df = 5 p = 0.6043
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.4537 df = 5 p = 0.2645
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2265 df = 5 p = 0.5173
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3779 df = 5 p = 0.6419
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.1516 df = 5 p = 0.2917
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.5335 df = 5 p = 0.3543
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7997 df = 5 p = 0.8761
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5202 df = 5 p = 0.6203
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.3786 df = 5 p = 0.0949
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.2734 df = 5 p = 0.2805
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.8067 df = 5 p = 0.117
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5553 df = 5 p = 0.9066
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4 df = 5 p = 0.7915
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2586 df = 5 p = 0.5128
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.8074 df = 5 p = 0.0809
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6529 df = 5 p = 0.7533
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.7948 df = 5 p = 0.3267
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.7307 df = 5 p = 0.1203
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.1046 df = 5 p = 0.105
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6269 df = 5 p = 0.6043
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.3419 df = 5 p = 0.2744
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.0381 df = 5 p = 0.3025
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8921 df = 5 p = 0.5651
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3237 df = 5 p = 0.6502
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.5892 df = 5 p = 0.468
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8667 df = 5 p = 0.7205
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6435 df = 5 p = 0.7547
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.3285 df = 5 p = 0.5032
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4567 df = 5 p = 0.4857
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.5274 df = 5 p = 0.355
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1142 df = 5 p = 0.5331
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4519 df = 5 p = 0.6307
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.2859 df = 5 p = 0.2794
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5797 df = 5 p = 0.6114
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9347 df = 5 p = 0.5589
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.1721 df = 5 p = 0.0146
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.611 df = 5 p = 0.9875
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4925 df = 5 p = 0.4809
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9229 df = 5 p = 0.7119
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9737 df = 5 p = 0.5532
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7254 df = 5 p = 0.8857
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6509 df = 5 p = 0.895
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8507 df = 5 p = 0.723
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5346 df = 5 p = 0.7713
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2405 df = 5 p = 0.663
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.9087 df = 5 p = 0.2275
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.3416 df = 5 p = 0.1384
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2685 df = 5 p = 0.5114
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.257 df = 5 p = 0.282
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.1846 df = 5 p = 0.2073
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8569 df = 5 p = 0.5702
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2654 df = 5 p = 0.6591
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9463 df = 5 p = 0.8565
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.698 df = 5 p = 0.3367
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8464 df = 5 p = 0.7237
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1464 df = 5 p = 0.5285
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.4331 df = 5 p = 0.2663
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.0383 df = 5 p = 0.1541
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7375 df = 5 p = 0.5878
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.8084 df = 5 p = 0.4397
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.3841 df = 5 p = 0.1363
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1429 df = 5 p = 0.829
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0997 df = 5 p = 0.8352
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.3836 df = 5 p = 0.3709
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.9156 df = 5 p = 0.227
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.3496 df = 5 p = 0.2737
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4359 df = 5 p = 0.6331
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.2655 df = 5 p = 0.1422
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5519 df = 5 p = 0.2562
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.2113 df = 5 p = 0.145
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.6273 df = 5 p = 0.463
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.562 df = 5 p = 0.4716
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.3304 df = 5 p = 0.3769
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1483 df = 5 p = 0.5283
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4834 df = 5 p = 0.779
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0321 df = 5 p = 0.5448
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3426 df = 5 p = 0.9305
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.7542 df = 5 p = 0.4466
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3901 df = 5 p = 0.6401
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.244 df = 5 p = 0.2832
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.2579 df = 5 p = 0.2022
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9805 df = 5 p = 0.5522
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7762 df = 5 p = 0.7344
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.3421 df = 5 p = 0.3756
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.1147 df = 5 p = 0.2123
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0189 df = 5 p = 0.5467
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.7546 df = 5 p = 0.4466
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.4592 df = 5 p = 0.1887
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.631 df = 5 p = 0.4626
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7232 df = 5 p = 0.5899
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8306 df = 5 p = 0.7261
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.3172 df = 5 p = 0.0307
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7105 df = 5 p = 0.5918
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.9682 df = 5 p = 0.4198
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8066 df = 5 p = 0.5776
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3544 df = 5 p = 0.6455
#> 
#>  White's test for heteroscedasticity (robust)
#> 
#> data:  model
#> X-squared = 11.822, df = 5, p-value = 0.0373
#> alternative hypothesis: heteroscedasticity present
#> 
```
