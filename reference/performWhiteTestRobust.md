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
#> [INFO] White test completed: statistic = 12.5507 df = 5 p = 0.028
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.8773 df = 5 p = 0.0365
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.1552 df = 5 p = 0.0028
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.6789 df = 5 p = 0.0034
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.4577 df = 5 p = 0.3626
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.1518 df = 5 p = 0.0042
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.9969 df = 5 p = 0.2209
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.4925 df = 5 p = 0.0624
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.2145 df = 5 p = 0.0694
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.3193 df = 5 p = 0.0091
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.3829 df = 5 p = 0.0088
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.1226 df = 5 p = 0.0148
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.5806 df = 5 p = 0.0081
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 16.0449 df = 5 p = 0.0067
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.7915 df = 5 p = 0.0557
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.5969 df = 5 p = 0.0122
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 16.3072 df = 5 p = 0.006
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 19.6322 df = 5 p = 0.0015
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.3968 df = 5 p = 0.0133
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.3868 df = 5 p = 0.02
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 20.5344 df = 5 p = 0.001
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.7326 df = 5 p = 0.0077
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 16.8795 df = 5 p = 0.0047
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.3093 df = 5 p = 0.1986
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.7425 df = 5 p = 0.2405
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.6214 df = 5 p = 0.1252
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 23.0002 df = 5 p = 3e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.5358 df = 5 p = 0.0036
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.1764 df = 5 p = 0.1468
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.6664 df = 5 p = 0.0584
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.3227 df = 5 p = 0.5039
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.1062 df = 5 p = 0.0099
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.6348 df = 5 p = 0.0181
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.9681 df = 5 p = 0.0237
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.115 df = 5 p = 0.0491
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 19.7088 df = 5 p = 0.0014
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.8434 df = 5 p = 0.0021
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.1109 df = 5 p = 0.1047
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.6751 df = 5 p = 0.1227
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 19.4621 df = 5 p = 0.0016
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.1469 df = 5 p = 0.0098
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 23.1392 df = 5 p = 3e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.2108 df = 5 p = 0.0027
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.008 df = 5 p = 0.0347
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.3311 df = 5 p = 0.5028
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.8957 df = 5 p = 0.0163
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 20.6782 df = 5 p = 9e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.3274 df = 5 p = 0.0039
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.954 df = 5 p = 0.1588
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.0793 df = 5 p = 0.01
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8965 df = 5 p = 0.5644
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.5721 df = 5 p = 0.0035
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.1648 df = 5 p = 0.0146
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.6651 df = 5 p = 0.0584
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.5434 df = 5 p = 0.0416
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.2502 df = 5 p = 0.0684
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.3298 df = 5 p = 0.0664
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.3883 df = 5 p = 0.0298
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.4465 df = 5 p = 0.1895
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.7933 df = 5 p = 0.0813
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.1769 df = 5 p = 0.0704
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.4839 df = 5 p = 0.0037
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.3 df = 5 p = 0.0457
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.3318 df = 5 p = 0.0026
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.033 df = 5 p = 0.0154
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.1445 df = 5 p = 0.0147
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.4051 df = 5 p = 0.3685
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.8523 df = 5 p = 0.0031
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.0938 df = 5 p = 0.0225
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 21.4412 df = 5 p = 7e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 22.7609 df = 5 p = 4e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.5617 df = 5 p = 0.0887
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.8164 df = 5 p = 0.1666
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 25.4106 df = 5 p = 1e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.9784 df = 5 p = 0.0351
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.2703 df = 5 p = 0.0679
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.8037 df = 5 p = 0.1674
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.6142 df = 5 p = 0.0596
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.7571 df = 5 p = 0.0076
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.7215 df = 5 p = 0.0175
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.1305 df = 5 p = 0.0043
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.1151 df = 5 p = 0.2122
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.5925 df = 5 p = 0.0023
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.4642 df = 5 p = 0.0194
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.3031 df = 5 p = 0.0091
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.7216 df = 5 p = 0.0175
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.1632 df = 5 p = 0.2088
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.8247 df = 5 p = 0.0168
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 20.8028 df = 5 p = 9e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 21.322 df = 5 p = 7e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.2116 df = 5 p = 0.0095
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.5463 df = 5 p = 0.0416
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.476 df = 5 p = 0.1319
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 22.5097 df = 5 p = 4e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.5061 df = 5 p = 0.0127
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.121 df = 5 p = 0.049
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.8215 df = 5 p = 0.0032
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.0371 df = 5 p = 0.2179
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.9372 df = 5 p = 0.016
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.6938 df = 5 p = 0.0022
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.4152 df = 5 p = 0.0295
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.4723 df = 5 p = 0.0428
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.8545 df = 5 p = 0.0795
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.6335 df = 5 p = 0.0022
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.6926 df = 5 p = 0.0264
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.3738 df = 5 p = 0.0134
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.7536 df = 5 p = 0.0383
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 19.6229 df = 5 p = 0.0015
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.9228 df = 5 p = 0.0775
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.7155 df = 5 p = 0.0175
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.1554 df = 5 p = 0.0097
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 16.4823 df = 5 p = 0.0056
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.4265 df = 5 p = 0.0435
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.7005 df = 5 p = 0.1216
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 21.2903 df = 5 p = 7e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 16.2953 df = 5 p = 0.006
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.9501 df = 5 p = 0.0355
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 19.857 df = 5 p = 0.0013
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.1442 df = 5 p = 0.0042
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 22.6968 df = 5 p = 4e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 21.5567 df = 5 p = 6e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 20.4262 df = 5 p = 0.001
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.9058 df = 5 p = 0.0361
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.1399 df = 5 p = 0.0028
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.8995 df = 5 p = 0.0108
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.2697 df = 5 p = 0.2013
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.7891 df = 5 p = 0.2368
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.6952 df = 5 p = 0.0117
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.2402 df = 5 p = 0.0027
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 24.3295 df = 5 p = 2e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.4251 df = 5 p = 0.0087
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.5042 df = 5 p = 0.3575
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.2166 df = 5 p = 0.0027
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.3066 df = 5 p = 0.004
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.1619 df = 5 p = 0.1475
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 24.2465 df = 5 p = 2e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 16.8116 df = 5 p = 0.0049
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.1785 df = 5 p = 0.0096
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.9877 df = 5 p = 0.0069
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 20.1142 df = 5 p = 0.0012
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.601 df = 5 p = 0.0599
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.6865 df = 5 p = 0.058
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.0956 df = 5 p = 0.4043
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 21.6998 df = 5 p = 6e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.7223 df = 5 p = 0.2421
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 20.248 df = 5 p = 0.0011
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.0369 df = 5 p = 0.2179
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.7962 df = 5 p = 0.0254
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 21.6279 df = 5 p = 6e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.4812 df = 5 p = 0.0128
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.8306 df = 5 p = 0.1658
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.0501 df = 5 p = 0.0738
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.8071 df = 5 p = 0.0032
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.0605 df = 5 p = 0.0044
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.1219 df = 5 p = 0.1496
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.5643 df = 5 p = 0.0413
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.2438 df = 5 p = 0.0041
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.0485 df = 5 p = 0.41
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.8966 df = 5 p = 0.0163
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.3803 df = 5 p = 0.0089
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.8733 df = 5 p = 0.0789
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.4649 df = 5 p = 0.0194
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 19.7548 df = 5 p = 0.0014
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 24.3761 df = 5 p = 2e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.0562 df = 5 p = 0.0029
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5646 df = 5 p = 0.2551
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.5313 df = 5 p = 0.0418
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 24.866 df = 5 p = 1e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.1721 df = 5 p = 0.0097
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 20.2225 df = 5 p = 0.0011
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 19.1877 df = 5 p = 0.0018
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.3151 df = 5 p = 0.0668
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.5852 df = 5 p = 0.0409
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.149 df = 5 p = 0.5282
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.9847 df = 5 p = 0.0019
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.286 df = 5 p = 0.0208
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 16.1841 df = 5 p = 0.0063
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.7038 df = 5 p = 0.0263
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.0068 df = 5 p = 0.0347
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.4844 df = 5 p = 0.0128
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.4973 df = 5 p = 0.0424
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 20.0108 df = 5 p = 0.0012
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 19.978 df = 5 p = 0.0013
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.9677 df = 5 p = 0.052
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 19.0614 df = 5 p = 0.0019
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 20.6046 df = 5 p = 0.001
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.6471 df = 5 p = 0.0022
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 18.3719 df = 5 p = 0.0025
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 14.2086 df = 5 p = 0.0143
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.0234 df = 5 p = 0.1081
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.2152 df = 5 p = 0.0095
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.7888 df = 5 p = 0.0378
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.9391 df = 5 p = 0.024
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.1086 df = 5 p = 0.0043
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 12.4635 df = 5 p = 0.029
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.2058 df = 5 p = 0.0474
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.4637 df = 5 p = 0.0919
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 15.379 df = 5 p = 0.0089
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 16.9389 df = 5 p = 0.0046
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 17.2524 df = 5 p = 0.004
#> 
#>  White's test for heteroscedasticity (robust)
#> 
#> data:  model
#> X-squared = 11.822, df = 5, p-value = 0.0373
#> alternative hypothesis: heteroscedasticity present
#> 
```
