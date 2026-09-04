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
#> [INFO] White test completed: statistic = 4.9771 df = 5 p = 0.4187
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.0417 df = 5 p = 0.4108
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6524 df = 5 p = 0.6005
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1572 df = 5 p = 0.527
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.2677 df = 5 p = 0.2015
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.6104 df = 5 p = 0.1791
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6122 df = 5 p = 0.7595
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.9717 df = 5 p = 0.0352
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5032 df = 5 p = 0.2603
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0494 df = 5 p = 0.6924
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9092 df = 5 p = 0.5626
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3857 df = 5 p = 0.7936
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0109 df = 5 p = 0.5479
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8693 df = 5 p = 0.7201
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8251 df = 5 p = 0.5749
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.354 df = 5 p = 0.6456
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0948 df = 5 p = 0.5358
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6829 df = 5 p = 0.5959
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.6188 df = 5 p = 0.0868
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5533 df = 5 p = 0.256
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.5337 df = 5 p = 0.0896
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.982 df = 5 p = 0.1572
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5869 df = 5 p = 0.6103
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9344 df = 5 p = 0.7101
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.9969 df = 5 p = 0.4163
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0353 df = 5 p = 0.6945
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.7763 df = 5 p = 0.0818
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0768 df = 5 p = 0.6881
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0579 df = 5 p = 0.6911
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.1222 df = 5 p = 0.1043
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2135 df = 5 p = 0.6671
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.3639 df = 5 p = 0.4983
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0321 df = 5 p = 0.9599
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.012 df = 5 p = 0.4144
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.1663 df = 5 p = 0.2904
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.5299 df = 5 p = 0.1841
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9182 df = 5 p = 0.8603
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4072 df = 5 p = 0.7904
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.0852 df = 5 p = 0.1057
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.2391 df = 5 p = 0.3874
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7736 df = 5 p = 0.7348
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.4644 df = 5 p = 0.3619
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.3487 df = 5 p = 0.5004
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1858 df = 5 p = 0.8229
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4487 df = 5 p = 0.4868
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7115 df = 5 p = 0.5917
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8173 df = 5 p = 0.7281
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.1672 df = 5 p = 0.3958
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6405 df = 5 p = 0.6022
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.649 df = 5 p = 0.601
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.104 df = 5 p = 0.8346
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2807 df = 5 p = 0.6568
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.7809 df = 5 p = 0.0817
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.4783 df = 5 p = 0.3603
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.3524 df = 5 p = 0.0448
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6944 df = 5 p = 0.5942
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1356 df = 5 p = 0.6791
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1811 df = 5 p = 0.8236
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4238 df = 5 p = 0.9217
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4342 df = 5 p = 0.4887
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8339 df = 5 p = 0.7256
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.3233 df = 5 p = 0.1977
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.3107 df = 5 p = 0.0308
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.7659 df = 5 p = 0.0114
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.272 df = 5 p = 0.0679
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.1216 df = 5 p = 0.2118
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0125 df = 5 p = 0.5476
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.3786 df = 5 p = 0.3714
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3091 df = 5 p = 0.6524
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.2234 df = 5 p = 0.1005
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.8138 df = 5 p = 0.0807
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.4413 df = 5 p = 0.2656
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1292 df = 5 p = 0.531
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5324 df = 5 p = 0.2578
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1322 df = 5 p = 0.8306
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0143 df = 5 p = 0.6978
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0627 df = 5 p = 0.5404
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.1158 df = 5 p = 0.4019
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1072 df = 5 p = 0.8341
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.391 df = 5 p = 0.4946
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4233 df = 5 p = 0.635
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.7472 df = 5 p = 0.2401
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.5268 df = 5 p = 0.4763
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0486 df = 5 p = 0.8424
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1339 df = 5 p = 0.8303
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.1814 df = 5 p = 0.2075
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.4575 df = 5 p = 0.1888
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.731 df = 5 p = 0.5888
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7671 df = 5 p = 0.5834
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.7444 df = 5 p = 0.3319
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9494 df = 5 p = 0.5567
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0531 df = 5 p = 0.8417
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9612 df = 5 p = 0.8545
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.2924 df = 5 p = 0.2788
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.06 df = 5 p = 0.8408
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4983 df = 5 p = 0.4801
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2172 df = 5 p = 0.5186
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.3467 df = 5 p = 0.0303
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9416 df = 5 p = 0.5578
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.5254 df = 5 p = 0.3552
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.9943 df = 5 p = 0.1093
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2634 df = 5 p = 0.6595
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5053 df = 5 p = 0.9125
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5136 df = 5 p = 0.2594
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9955 df = 5 p = 0.8498
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.838 df = 5 p = 0.436
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.3556 df = 5 p = 0.374
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.6027 df = 5 p = 0.1795
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.904 df = 5 p = 0.7148
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9348 df = 5 p = 0.8581
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8668 df = 5 p = 0.5687
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7695 df = 5 p = 0.5831
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.354 df = 5 p = 0.1956
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.7977 df = 5 p = 0.0169
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2964 df = 5 p = 0.8068
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4343 df = 5 p = 0.9205
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.7119 df = 5 p = 0.3353
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.1459 df = 5 p = 0.2923
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4381 df = 5 p = 0.7858
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2212 df = 5 p = 0.6659
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.3608 df = 5 p = 0.0656
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6077 df = 5 p = 0.6072
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3073 df = 5 p = 0.8052
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.2154 df = 5 p = 0.1448
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9223 df = 5 p = 0.5607
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 16.5813 df = 5 p = 0.0054
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3275 df = 5 p = 0.9321
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.8238 df = 5 p = 0.055
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.1859 df = 5 p = 0.2072
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.0642 df = 5 p = 0.2159
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7843 df = 5 p = 0.978
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.294 df = 5 p = 0.5079
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.2765 df = 5 p = 0.3831
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4986 df = 5 p = 0.9132
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7472 df = 5 p = 0.5864
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 16.4514 df = 5 p = 0.0057
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.03 df = 5 p = 0.1546
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3804 df = 5 p = 0.6416
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9806 df = 5 p = 0.703
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.955 df = 5 p = 0.4214
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3444 df = 5 p = 0.6471
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9477 df = 5 p = 0.7081
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.1093 df = 5 p = 0.1503
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.1925 df = 5 p = 0.2067
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5094 df = 5 p = 0.622
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 16.4882 df = 5 p = 0.0056
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.2041 df = 5 p = 0.0215
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4544 df = 5 p = 0.9183
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0832 df = 5 p = 0.8375
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.1886 df = 5 p = 0.1018
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.2353 df = 5 p = 0.2037
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.6303 df = 5 p = 0.4626
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.5853 df = 5 p = 0.3487
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.1839 df = 5 p = 0.2887
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8567 df = 5 p = 0.9733
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9255 df = 5 p = 0.7115
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.5556 df = 5 p = 0.4725
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0853 df = 5 p = 0.5372
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.6528 df = 5 p = 0.4597
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9403 df = 5 p = 0.8573
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7091 df = 5 p = 0.592
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.0317 df = 5 p = 0.412
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.4742 df = 5 p = 0.3608
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.4769 df = 5 p = 0.2625
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.8105 df = 5 p = 0.0808
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3174 df = 5 p = 0.8037
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.5359 df = 5 p = 0.354
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1049 df = 5 p = 0.6838
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.4189 df = 5 p = 0.1346
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.67 df = 5 p = 0.8927
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9639 df = 5 p = 0.8541
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7791 df = 5 p = 0.5816
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5169 df = 5 p = 0.6208
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9433 df = 5 p = 0.8569
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9802 df = 5 p = 0.703
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1344 df = 5 p = 0.5302
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.0004 df = 5 p = 0.3062
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2213 df = 5 p = 0.518
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.2626 df = 5 p = 0.2018
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3807 df = 5 p = 0.6415
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.1908 df = 5 p = 0.07
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0634 df = 5 p = 0.9573
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.9809 df = 5 p = 0.3081
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4326 df = 5 p = 0.489
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2037 df = 5 p = 0.5205
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6271 df = 5 p = 0.7572
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0862 df = 5 p = 0.6867
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.1997 df = 5 p = 0.2062
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.484 df = 5 p = 0.9149
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6512 df = 5 p = 0.895
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3192 df = 5 p = 0.6509
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2876 df = 5 p = 0.5088
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.3286 df = 5 p = 0.0967
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5139 df = 5 p = 0.7744
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.9848 df = 5 p = 0.2218
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4259 df = 5 p = 0.4899
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.5766 df = 5 p = 0.4697
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5908 df = 5 p = 0.7628
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0013 df = 5 p = 0.849
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3112 df = 5 p = 0.6521
#> 
#>  White's test for heteroscedasticity (robust)
#> 
#> data:  model
#> X-squared = 11.822, df = 5, p-value = 0.0373
#> alternative hypothesis: heteroscedasticity present
#> 
```
