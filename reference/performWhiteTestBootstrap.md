# Bootstrap White test

Approximates the finite-sample distribution of White's LM statistic by
resampling the fitted residuals, refitting the model, and recalculating
the test statistic across many bootstrap replications.

## Usage

``` r
performWhiteTestBootstrap(model, data, B = 1000, parallel = FALSE)
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

- B:

  Integer scalar giving the number of bootstrap replications. Larger
  values yield smoother p-value estimates at the cost of additional
  runtime.

- parallel:

  Logical scalar; if `TRUE` the bootstrap loop is executed with
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html)
  when the `parallel` package and forked processing are available.

## Value

A `htest` object reporting both the asymptotic White statistic and its
bootstrap p-value. The returned object also stores the simulated test
statistics in `boot_statistics` for further inspection.

## Details

For a fitted model \\\hat{y} = X\hat{\beta}\\, the algorithm proceeds as
follows:

1.  Compute White's LM statistic \\n R^2\\ from the auxiliary regression
    on squares and cross-products of the regressors.

2.  Generate \\B\\ bootstrap samples by resampling the centred residuals
    with replacement, forming \\y^{\*(b)} = \hat{y} + \hat{e}^{\*(b)}\\.

3.  Refit the model to each bootstrap sample and recompute White's
    statistic \\T^{\*(b)}\\ using the same auxiliary specification.

4.  Estimate the bootstrap p-value as \\\hat{p} = B^{-1} \sum\_{b = 1}^B
    I\\T^{\*(b)} \ge T\_{\text{obs}}\\\\.

The bootstrap distribution offers improved size control for moderate
sample sizes or high-dimensional designs where the chi-squared
approximation may be inaccurate. When `parallel = TRUE` the resampling
step exploits available CPU cores to reduce computation time.

## References

Efron, B., & Tibshirani, R. J. (1993). *An Introduction to the
Bootstrap*. Chapman & Hall/CRC.

Davidson, R., & MacKinnon, J. G. (2006). The power of bootstrap and
asymptotic tests. *Journal of Econometrics, 133*(2), 421–441.
[doi:10.1016/j.jeconom.2005.02.002](https://doi.org/10.1016/j.jeconom.2005.02.002)

## See also

[`performWhiteTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTest.md)
for the classical statistic and
[performWhiteTestRobust()](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestRobust.md)
for enriched diagnostics.

## Examples

``` r
# The bootstrap needs at least 50 observations, so mtcars (32) is too small.
set.seed(42)
n <- 120
sim <- data.frame(x = runif(n, 1, 5))
sim$y <- 1 + 2 * sim$x + rnorm(n, sd = 0.3 + 0.6 * sim$x)
mod <- lm(y ~ x, data = sim)
performWhiteTestBootstrap(mod, sim, B = 199)
#> [INFO] Running Bootstrap White test
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.8014 df = 2 p = 0.001
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3446 df = 2 p = 0.8417
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2837 df = 2 p = 0.1936
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3391 df = 2 p = 0.5119
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.3425 df = 2 p = 0.114
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2326 df = 2 p = 0.8902
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8126 df = 2 p = 0.1486
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0022 df = 2 p = 0.9989
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0196 df = 2 p = 0.9903
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2459 df = 2 p = 0.8843
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3111 df = 2 p = 0.8559
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8817 df = 2 p = 0.1436
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0204 df = 2 p = 0.3642
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2161 df = 2 p = 0.5444
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0329 df = 2 p = 0.9837
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0131 df = 2 p = 0.9935
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3472 df = 2 p = 0.8406
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2298 df = 2 p = 0.8915
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9291 df = 2 p = 0.3812
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2165 df = 2 p = 0.5443
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.8608 df = 2 p = 0.0072
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.6006 df = 2 p = 0.0369
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9695 df = 2 p = 0.3735
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0509 df = 2 p = 0.9749
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3438 df = 2 p = 0.8421
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2669 df = 2 p = 0.8751
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0216 df = 2 p = 0.9893
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5935 df = 2 p = 0.2734
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.3695 df = 2 p = 0.1125
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2488 df = 2 p = 0.3248
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3767 df = 2 p = 0.3047
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3948 df = 2 p = 0.8209
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.717 df = 2 p = 0.257
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9433 df = 2 p = 0.624
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3313 df = 2 p = 0.5139
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1961 df = 2 p = 0.9066
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1901 df = 2 p = 0.3345
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.976 df = 2 p = 0.6139
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.7063 df = 2 p = 0.035
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7958 df = 2 p = 0.6717
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3729 df = 2 p = 0.8299
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0386 df = 2 p = 0.9809
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8084 df = 2 p = 0.6675
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.069 df = 2 p = 0.586
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3402 df = 2 p = 0.5117
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.866 df = 2 p = 0.6486
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3051 df = 2 p = 0.5207
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.1977 df = 2 p = 0.0274
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.552 df = 2 p = 0.7588
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1139 df = 2 p = 0.3475
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3545 df = 2 p = 0.508
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6617 df = 2 p = 0.2642
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0408 df = 2 p = 0.9798
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.394 df = 2 p = 0.0674
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8958 df = 2 p = 0.639
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9377 df = 2 p = 0.1396
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0087 df = 2 p = 0.2222
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.8406 df = 2 p = 0.0539
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8321 df = 2 p = 0.4001
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1602 df = 2 p = 0.3396
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9351 df = 2 p = 0.6265
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5704 df = 2 p = 0.2766
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4317 df = 2 p = 0.8059
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6341 df = 2 p = 0.7283
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0935 df = 2 p = 0.3511
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3635 df = 2 p = 0.3067
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0528 df = 2 p = 0.2173
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.6037 df = 2 p = 0.1001
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.0781 df = 2 p = 0.0789
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3043 df = 2 p = 0.316
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.247 df = 2 p = 0.5361
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3873 df = 2 p = 0.8239
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0273 df = 2 p = 0.9864
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9375 df = 2 p = 0.1396
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0141 df = 2 p = 0.993
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4861 df = 2 p = 0.175
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7028 df = 2 p = 0.7037
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0702 df = 2 p = 0.9655
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7504 df = 2 p = 0.6872
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8487 df = 2 p = 0.6542
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.9788 df = 2 p = 0.083
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2445 df = 2 p = 0.3255
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8242 df = 2 p = 0.6623
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3464 df = 2 p = 0.841
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1308 df = 2 p = 0.209
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1867 df = 2 p = 0.9109
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.336 df = 2 p = 0.8453
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6805 df = 2 p = 0.1588
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0811 df = 2 p = 0.3533
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4613 df = 2 p = 0.1772
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4808 df = 2 p = 0.7863
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6652 df = 2 p = 0.7171
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1094 df = 2 p = 0.2113
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.714 df = 2 p = 0.6998
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2647 df = 2 p = 0.876
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9194 df = 2 p = 0.383
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8729 df = 2 p = 0.6463
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4724 df = 2 p = 0.7896
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8234 df = 2 p = 0.4018
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4619 df = 2 p = 0.4814
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2021 df = 2 p = 0.3325
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.9248 df = 2 p = 0.0517
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2903 df = 2 p = 0.3182
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3617 df = 2 p = 0.5062
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 19.323 df = 2 p = 1e-04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3711 df = 2 p = 0.5038
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.6322 df = 2 p = 0.0363
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4734 df = 2 p = 0.7892
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2135 df = 2 p = 0.8987
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4698 df = 2 p = 0.2909
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1132 df = 2 p = 0.5732
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.729 df = 2 p = 0.6945
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1631 df = 2 p = 0.559
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.869 df = 2 p = 0.3928
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0499 df = 2 p = 0.9754
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4626 df = 2 p = 0.4813
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.333 df = 2 p = 0.5135
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6118 df = 2 p = 0.4467
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4333 df = 2 p = 0.8052
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2834 df = 2 p = 0.1175
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1807 df = 2 p = 0.9136
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5441 df = 2 p = 0.7618
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5106 df = 2 p = 0.4699
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6232 df = 2 p = 0.7323
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5378 df = 2 p = 0.4635
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1565 df = 2 p = 0.9247
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6747 df = 2 p = 0.7137
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.295 df = 2 p = 0.8629
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.165 df = 2 p = 0.2055
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6772 df = 2 p = 0.4323
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7635 df = 2 p = 0.4141
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1968 df = 2 p = 0.5497
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3865 df = 2 p = 0.3032
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6458 df = 2 p = 0.4392
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3062 df = 2 p = 0.5204
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4605 df = 2 p = 0.7944
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2312 df = 2 p = 0.8908
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4729 df = 2 p = 0.2904
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.2189 df = 2 p = 0.0164
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.514 df = 2 p = 0.7734
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5715 df = 2 p = 0.2764
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7288 df = 2 p = 0.4213
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3422 df = 2 p = 0.5111
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.739 df = 2 p = 0.1542
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2764 df = 2 p = 0.3204
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0887 df = 2 p = 0.2135
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9965 df = 2 p = 0.1356
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0816 df = 2 p = 0.96
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2934 df = 2 p = 0.8635
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0018 df = 2 p = 0.9991
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1926 df = 2 p = 0.5509
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.9461 df = 2 p = 0.031
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6326 df = 2 p = 0.4421
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0059 df = 2 p = 0.1349
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.516 df = 2 p = 0.1046
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7496 df = 2 p = 0.4169
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5417 df = 2 p = 0.4626
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2715 df = 2 p = 0.8731
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.576 df = 2 p = 0.1015
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0999 df = 2 p = 0.9513
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9765 df = 2 p = 0.6137
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.804 df = 2 p = 0.669
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5574 df = 2 p = 0.7568
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1413 df = 2 p = 0.5651
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2204 df = 2 p = 0.8957
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8408 df = 2 p = 0.3984
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2889 df = 2 p = 0.5249
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8255 df = 2 p = 0.6618
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6314 df = 2 p = 0.7293
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6262 df = 2 p = 0.4435
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0082 df = 2 p = 0.3664
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.702 df = 2 p = 0.0953
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2486 df = 2 p = 0.8831
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0176 df = 2 p = 0.6012
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1708 df = 2 p = 0.9181
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0795 df = 2 p = 0.3535
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2251 df = 2 p = 0.542
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3173 df = 2 p = 0.5175
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7174 df = 2 p = 0.4237
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5102 df = 2 p = 0.285
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1498 df = 2 p = 0.207
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0216 df = 2 p = 0.6
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7324 df = 2 p = 0.2551
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1358 df = 2 p = 0.9344
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0209 df = 2 p = 0.9896
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7581 df = 2 p = 0.6845
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.552 df = 2 p = 0.4602
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3622 df = 2 p = 0.5061
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7445 df = 2 p = 0.6892
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3095 df = 2 p = 0.8566
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4532 df = 2 p = 0.7972
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2992 df = 2 p = 0.8611
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3343 df = 2 p = 0.3113
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.5872 df = 2 p = 0.1009
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3228 df = 2 p = 0.851
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1882 df = 2 p = 0.9102
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1922 df = 2 p = 0.9084
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.26 df = 2 p = 0.1959
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5071 df = 2 p = 0.0386
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7897 df = 2 p = 0.4087
#> 
#>  Bootstrap White test
#> 
#> data:  mod
#> X-squared = 13.801, B = 199, p-value = 0.01
#> 

# \donttest{
# More replications give a smoother p-value estimate
performWhiteTestBootstrap(mod, sim, B = 999)
#> [INFO] Running Bootstrap White test
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.8014 df = 2 p = 0.001
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1122 df = 2 p = 0.9455
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5489 df = 2 p = 0.2796
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5436 df = 2 p = 0.4622
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1299 df = 2 p = 0.3447
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5301 df = 2 p = 0.7672
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6918 df = 2 p = 0.2603
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.3419 df = 2 p = 0.042
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4966 df = 2 p = 0.7801
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4611 df = 2 p = 0.4816
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7545 df = 2 p = 0.153
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7888 df = 2 p = 0.6741
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.316 df = 2 p = 0.8539
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5299 df = 2 p = 0.1712
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5749 df = 2 p = 0.1674
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9193 df = 2 p = 0.383
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2744 df = 2 p = 0.8718
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3293 df = 2 p = 0.5144
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1286 df = 2 p = 0.9377
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0036 df = 2 p = 0.2227
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0597 df = 2 p = 0.5887
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6483 df = 2 p = 0.266
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7658 df = 2 p = 0.2509
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1754 df = 2 p = 0.2044
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4842 df = 2 p = 0.4761
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.5186 df = 2 p = 0.1044
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9914 df = 2 p = 0.6091
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.3304 df = 2 p = 0.0422
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1013 df = 2 p = 0.3497
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.9277 df = 2 p = 0.0851
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3861 df = 2 p = 0.5
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.9322 df = 2 p = 0.0849
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8695 df = 2 p = 0.6474
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5559 df = 2 p = 0.7573
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2857 df = 2 p = 0.3189
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1402 df = 2 p = 0.9323
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2051 df = 2 p = 0.9025
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1762 df = 2 p = 0.9157
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4039 df = 2 p = 0.8171
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3123 df = 2 p = 0.5188
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7577 df = 2 p = 0.4153
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0092 df = 2 p = 0.9954
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1268 df = 2 p = 0.3453
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2756 df = 2 p = 0.1944
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7727 df = 2 p = 0.1516
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3754 df = 2 p = 0.8289
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8326 df = 2 p = 0.6595
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9094 df = 2 p = 0.3849
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7963 df = 2 p = 0.4073
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7574 df = 2 p = 0.2519
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4611 df = 2 p = 0.1075
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8801 df = 2 p = 0.3906
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.2207 df = 2 p = 0.0735
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3097 df = 2 p = 0.8566
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.025 df = 2 p = 0.599
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0817 df = 2 p = 0.96
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8473 df = 2 p = 0.3971
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3493 df = 2 p = 0.8397
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5682 df = 2 p = 0.4565
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1463 df = 2 p = 0.3419
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2211 df = 2 p = 0.1212
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9396 df = 2 p = 0.6251
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2316 df = 2 p = 0.3277
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6097 df = 2 p = 0.4472
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1518 df = 2 p = 0.9269
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.655 df = 2 p = 0.2651
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9958 df = 2 p = 0.3687
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1508 df = 2 p = 0.5625
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7373 df = 2 p = 0.6917
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3516 df = 2 p = 0.3086
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.4921 df = 2 p = 0.0087
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7459 df = 2 p = 0.2534
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0338 df = 2 p = 0.3617
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.036 df = 2 p = 0.5957
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.743 df = 2 p = 0.4183
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4329 df = 2 p = 0.1797
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8368 df = 2 p = 0.1468
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4659 df = 2 p = 0.7922
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1577 df = 2 p = 0.2062
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9445 df = 2 p = 0.2294
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1321 df = 2 p = 0.9361
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3459 df = 2 p = 0.8412
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4131 df = 2 p = 0.2992
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9473 df = 2 p = 0.6227
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6932 df = 2 p = 0.7071
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3518 df = 2 p = 0.5087
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8515 df = 2 p = 0.2403
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1465 df = 2 p = 0.9294
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1116 df = 2 p = 0.5736
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3233 df = 2 p = 0.313
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8404 df = 2 p = 0.3984
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5441 df = 2 p = 0.7618
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.461 df = 2 p = 0.1075
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6065 df = 2 p = 0.7384
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8705 df = 2 p = 0.6471
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0747 df = 2 p = 0.3544
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5234 df = 2 p = 0.7698
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3092 df = 2 p = 0.8568
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8506 df = 2 p = 0.6536
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0587 df = 2 p = 0.3572
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.1335 df = 2 p = 0.0014
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9368 df = 2 p = 0.1397
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3932 df = 2 p = 0.4983
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4861 df = 2 p = 0.175
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.167 df = 2 p = 0.2053
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.183 df = 2 p = 0.9126
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4898 df = 2 p = 0.1059
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9738 df = 2 p = 0.6145
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9183 df = 2 p = 0.6318
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3666 df = 2 p = 0.8325
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5206 df = 2 p = 0.2836
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0764 df = 2 p = 0.2148
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0159 df = 2 p = 0.6017
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1605 df = 2 p = 0.9229
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5087 df = 2 p = 0.7754
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0939 df = 2 p = 0.9541
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5672 df = 2 p = 0.7531
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2044 df = 2 p = 0.9029
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7385 df = 2 p = 0.4193
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0982 df = 2 p = 0.2124
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.272 df = 2 p = 0.1948
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3445 df = 2 p = 0.8417
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.574 df = 2 p = 0.7505
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8716 df = 2 p = 0.6468
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1461 df = 2 p = 0.9295
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2255 df = 2 p = 0.5419
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.526 df = 2 p = 0.0232
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.245 df = 2 p = 0.8847
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2379 df = 2 p = 0.5385
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4491 df = 2 p = 0.7989
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4409 df = 2 p = 0.2951
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8975 df = 2 p = 0.6384
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5382 df = 2 p = 0.7641
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0546 df = 2 p = 0.5902
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9884 df = 2 p = 0.2244
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7717 df = 2 p = 0.6799
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8234 df = 2 p = 0.4018
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9611 df = 2 p = 0.6184
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3163 df = 2 p = 0.8537
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6218 df = 2 p = 0.4445
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3323 df = 2 p = 0.5137
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1422 df = 2 p = 0.3426
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3999 df = 2 p = 0.8188
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8387 df = 2 p = 0.2419
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0848 df = 2 p = 0.9585
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1536 df = 2 p = 0.9261
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.467 df = 2 p = 0.1072
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6024 df = 2 p = 0.1651
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4786 df = 2 p = 0.4774
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3386 df = 2 p = 0.5121
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0001 df = 2 p = 0.3679
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1427 df = 2 p = 0.9311
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4524 df = 2 p = 0.7976
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3027 df = 2 p = 0.8596
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5451 df = 2 p = 0.4618
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.54 df = 2 p = 0.1703
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.838 df = 2 p = 0.6577
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0446 df = 2 p = 0.5931
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.137 df = 2 p = 0.5664
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5563 df = 2 p = 0.2786
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4367 df = 2 p = 0.4876
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9144 df = 2 p = 0.6331
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2254 df = 2 p = 0.3287
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1526 df = 2 p = 0.562
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7359 df = 2 p = 0.2546
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6554 df = 2 p = 0.7206
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3763 df = 2 p = 0.8285
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3343 df = 2 p = 0.3113
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2846 df = 2 p = 0.8673
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1153 df = 2 p = 0.5725
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5712 df = 2 p = 0.4558
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9667 df = 2 p = 0.1376
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7526 df = 2 p = 0.2525
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.691 df = 2 p = 0.4293
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8331 df = 2 p = 0.6593
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6503 df = 2 p = 0.4382
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6068 df = 2 p = 0.7383
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0913 df = 2 p = 0.3515
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6958 df = 2 p = 0.7062
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2617 df = 2 p = 0.5321
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.3832 df = 2 p = 0.1117
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3043 df = 2 p = 0.8589
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3743 df = 2 p = 0.3051
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5084 df = 2 p = 0.2853
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1447 df = 2 p = 0.9302
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4334 df = 2 p = 0.8052
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6421 df = 2 p = 0.1619
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.9498 df = 2 p = 0.031
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8361 df = 2 p = 0.3993
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4436 df = 2 p = 0.4859
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0034 df = 2 p = 0.3673
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5795 df = 2 p = 0.2753
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6599 df = 2 p = 0.719
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7069 df = 2 p = 0.2583
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1243 df = 2 p = 0.3457
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4626 df = 2 p = 0.1074
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0931 df = 2 p = 0.3511
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.1755 df = 2 p = 0.0277
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7419 df = 2 p = 0.6901
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6213 df = 2 p = 0.733
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5067 df = 2 p = 0.4708
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.729 df = 2 p = 0.2555
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4796 df = 2 p = 0.2894
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7355 df = 2 p = 0.4199
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1404 df = 2 p = 0.208
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0492 df = 2 p = 0.5918
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6132 df = 2 p = 0.1642
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.678 df = 2 p = 0.0964
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2602 df = 2 p = 0.878
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9049 df = 2 p = 0.3858
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5968 df = 2 p = 0.742
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2699 df = 2 p = 0.53
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1057 df = 2 p = 0.9485
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6894 df = 2 p = 0.7084
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7945 df = 2 p = 0.6722
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1963 df = 2 p = 0.3335
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5666 df = 2 p = 0.2771
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7126 df = 2 p = 0.4247
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5259 df = 2 p = 0.7688
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2288 df = 2 p = 0.541
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4224 df = 2 p = 0.4911
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7414 df = 2 p = 0.2539
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3079 df = 2 p = 0.52
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5426 df = 2 p = 0.7624
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1742 df = 2 p = 0.3372
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2032 df = 2 p = 0.9034
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.729 df = 2 p = 0.094
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0596 df = 2 p = 0.5887
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6653 df = 2 p = 0.4349
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9577 df = 2 p = 0.2279
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8144 df = 2 p = 0.4037
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9629 df = 2 p = 0.6179
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4283 df = 2 p = 0.1801
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9143 df = 2 p = 0.6331
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.7301 df = 2 p = 0.0346
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8736 df = 2 p = 0.1442
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1643 df = 2 p = 0.9211
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9352 df = 2 p = 0.6265
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4298 df = 2 p = 0.18
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.985 df = 2 p = 0.1364
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9125 df = 2 p = 0.3843
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8775 df = 2 p = 0.6448
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2436 df = 2 p = 0.537
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5166 df = 2 p = 0.7724
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 10.6911 df = 2 p = 0.0048
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0732 df = 2 p = 0.9641
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2078 df = 2 p = 0.122
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.2049 df = 2 p = 0.0741
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.769 df = 2 p = 0.2505
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0094 df = 2 p = 0.9953
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.499 df = 2 p = 0.7792
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9797 df = 2 p = 0.3716
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.934 df = 2 p = 0.6269
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2142 df = 2 p = 0.3305
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4465 df = 2 p = 0.7999
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.5288 df = 2 p = 0.0085
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3062 df = 2 p = 0.858
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6305 df = 2 p = 0.7296
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6281 df = 2 p = 0.163
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3092 df = 2 p = 0.3152
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.0054 df = 2 p = 0.0301
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0781 df = 2 p = 0.3538
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8715 df = 2 p = 0.6468
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8852 df = 2 p = 0.6424
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0141 df = 2 p = 0.993
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9488 df = 2 p = 0.6222
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0804 df = 2 p = 0.9606
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8244 df = 2 p = 0.6622
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7113 df = 2 p = 0.425
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8168 df = 2 p = 0.6647
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6908 df = 2 p = 0.4294
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8061 df = 2 p = 0.6683
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8584 df = 2 p = 0.1453
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4687 df = 2 p = 0.291
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6171 df = 2 p = 0.2702
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5353 df = 2 p = 0.2815
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7935 df = 2 p = 0.4079
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3367 df = 2 p = 0.5126
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.047 df = 2 p = 0.9768
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1751 df = 2 p = 0.5557
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.018 df = 2 p = 0.2211
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1944 df = 2 p = 0.3338
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3104 df = 2 p = 0.315
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5659 df = 2 p = 0.7536
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6561 df = 2 p = 0.7203
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0074 df = 2 p = 0.3665
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7201 df = 2 p = 0.6976
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.407 df = 2 p = 0.3001
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.349 df = 2 p = 0.1137
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1955 df = 2 p = 0.5501
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.2805 df = 2 p = 0.0713
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7265 df = 2 p = 0.6954
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8867 df = 2 p = 0.2361
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4564 df = 2 p = 0.4828
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6147 df = 2 p = 0.446
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0487 df = 2 p = 0.1321
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6346 df = 2 p = 0.7281
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6931 df = 2 p = 0.7071
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3036 df = 2 p = 0.8591
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4477 df = 2 p = 0.7994
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8703 df = 2 p = 0.3925
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.4365 df = 2 p = 0.04
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0361 df = 2 p = 0.9821
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3692 df = 2 p = 0.8315
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4855 df = 2 p = 0.4758
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0382 df = 2 p = 0.5951
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.3531 df = 2 p = 0.0093
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6773 df = 2 p = 0.2622
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2221 df = 2 p = 0.1997
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1914 df = 2 p = 0.5512
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7965 df = 2 p = 0.6715
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3335 df = 2 p = 0.5134
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1209 df = 2 p = 0.21
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5362 df = 2 p = 0.4639
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0053 df = 2 p = 0.6049
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6782 df = 2 p = 0.7124
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6498 df = 2 p = 0.7226
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3607 df = 2 p = 0.835
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5784 df = 2 p = 0.0373
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.549 df = 2 p = 0.4609
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3524 df = 2 p = 0.8385
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4198 df = 2 p = 0.4917
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3844 df = 2 p = 0.3035
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7168 df = 2 p = 0.1559
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4588 df = 2 p = 0.4822
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.7573 df = 2 p = 0.0927
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.3283 df = 2 p = 0.1148
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.428 df = 2 p = 0.8073
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.656 df = 2 p = 0.1607
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2328 df = 2 p = 0.3275
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2625 df = 2 p = 0.3226
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4131 df = 2 p = 0.4933
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.686 df = 2 p = 0.2611
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2138 df = 2 p = 0.8986
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0575 df = 2 p = 0.5893
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5931 df = 2 p = 0.1659
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5751 df = 2 p = 0.7501
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1856 df = 2 p = 0.3353
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8309 df = 2 p = 0.6601
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.7024 df = 2 p = 0.035
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6832 df = 2 p = 0.7106
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2657 df = 2 p = 0.3221
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2833 df = 2 p = 0.8679
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4436 df = 2 p = 0.8011
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7918 df = 2 p = 0.6731
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2465 df = 2 p = 0.884
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3606 df = 2 p = 0.835
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2472 df = 2 p = 0.8837
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2083 df = 2 p = 0.3315
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6679 df = 2 p = 0.4343
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0152 df = 2 p = 0.6019
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.96 df = 2 p = 0.3753
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.8532 df = 2 p = 0.0883
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1797 df = 2 p = 0.9141
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1231 df = 2 p = 0.9403
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0956 df = 2 p = 0.3507
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.503 df = 2 p = 0.7776
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.6003 df = 2 p = 0.1002
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7351 df = 2 p = 0.42
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1704 df = 2 p = 0.1243
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9935 df = 2 p = 0.6085
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5538 df = 2 p = 0.4598
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0179 df = 2 p = 0.2211
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3597 df = 2 p = 0.8354
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6773 df = 2 p = 0.7127
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1265 df = 2 p = 0.2095
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2996 df = 2 p = 0.5222
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2949 df = 2 p = 0.5234
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8885 df = 2 p = 0.389
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8119 df = 2 p = 0.2451
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0407 df = 2 p = 0.9798
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0175 df = 2 p = 0.9913
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.458 df = 2 p = 0.7953
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1447 df = 2 p = 0.5642
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2862 df = 2 p = 0.1934
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9199 df = 2 p = 0.2323
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4887 df = 2 p = 0.7832
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2066 df = 2 p = 0.547
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7962 df = 2 p = 0.4073
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5433 df = 2 p = 0.7621
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8449 df = 2 p = 0.3975
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.952 df = 2 p = 0.1386
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3774 df = 2 p = 0.3046
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8521 df = 2 p = 0.6531
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3977 df = 2 p = 0.4971
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4821 df = 2 p = 0.1063
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7924 df = 2 p = 0.4081
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0018 df = 2 p = 0.1352
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6271 df = 2 p = 0.1631
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.0131 df = 2 p = 0.0015
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5421 df = 2 p = 0.7626
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9646 df = 2 p = 0.6174
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.175 df = 2 p = 0.3371
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.246 df = 2 p = 0.8843
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3559 df = 2 p = 0.837
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8093 df = 2 p = 0.6672
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9933 df = 2 p = 0.2239
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3472 df = 2 p = 0.8406
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.407 df = 2 p = 0.0149
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5291 df = 2 p = 0.4656
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3641 df = 2 p = 0.8336
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3925 df = 2 p = 0.3023
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3356 df = 2 p = 0.311
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7454 df = 2 p = 0.4178
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4535 df = 2 p = 0.1779
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7386 df = 2 p = 0.4192
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9725 df = 2 p = 0.6149
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3177 df = 2 p = 0.8531
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8764 df = 2 p = 0.2374
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5353 df = 2 p = 0.7652
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1434 df = 2 p = 0.9308
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9106 df = 2 p = 0.6343
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3762 df = 2 p = 0.8285
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1729 df = 2 p = 0.1241
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4462 df = 2 p = 0.4853
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1012 df = 2 p = 0.9506
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1217 df = 2 p = 0.9409
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2645 df = 2 p = 0.8761
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.3767 df = 2 p = 0.0412
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.786 df = 2 p = 0.1506
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1276 df = 2 p = 0.9382
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1021 df = 2 p = 0.3496
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2065 df = 2 p = 0.2012
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.1527 df = 2 p = 0.0761
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.7884 df = 2 p = 0.0075
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1354 df = 2 p = 0.9345
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1198 df = 2 p = 0.2102
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9485 df = 2 p = 0.3775
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0842 df = 2 p = 0.5815
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.1516 df = 2 p = 0.0462
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5604 df = 2 p = 0.1686
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1006 df = 2 p = 0.951
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.528 df = 2 p = 0.2825
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.932 df = 2 p = 0.6275
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5146 df = 2 p = 0.2844
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2697 df = 2 p = 0.53
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5059 df = 2 p = 0.7765
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1033 df = 2 p = 0.9497
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.025 df = 2 p = 0.599
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4466 df = 2 p = 0.7999
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6472 df = 2 p = 0.7235
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.6375 df = 2 p = 0.0362
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4317 df = 2 p = 0.2965
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0787 df = 2 p = 0.9614
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4663 df = 2 p = 0.2914
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.3665 df = 2 p = 0.0683
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6791 df = 2 p = 0.7121
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6159 df = 2 p = 0.735
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7984 df = 2 p = 0.6709
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4204 df = 2 p = 0.1097
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2847 df = 2 p = 0.8673
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3087 df = 2 p = 0.5198
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2626 df = 2 p = 0.877
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4728 df = 2 p = 0.7895
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8754 df = 2 p = 0.6455
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.266 df = 2 p = 0.8755
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5587 df = 2 p = 0.4587
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6339 df = 2 p = 0.4418
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8639 df = 2 p = 0.6492
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8726 df = 2 p = 0.6464
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.351 df = 2 p = 0.3087
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0172 df = 2 p = 0.9914
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0965 df = 2 p = 0.3505
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6308 df = 2 p = 0.7295
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0883 df = 2 p = 0.9568
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6118 df = 2 p = 0.4467
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1313 df = 2 p = 0.9364
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6873 df = 2 p = 0.4301
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3575 df = 2 p = 0.8363
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1087 df = 2 p = 0.3484
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8554 df = 2 p = 0.652
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9536 df = 2 p = 0.1385
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.625 df = 2 p = 0.7316
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6808 df = 2 p = 0.7115
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.187 df = 2 p = 0.335
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9719 df = 2 p = 0.6151
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0746 df = 2 p = 0.3544
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.748 df = 2 p = 0.688
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0845 df = 2 p = 0.5814
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4799 df = 2 p = 0.1065
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8919 df = 2 p = 0.2355
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8462 df = 2 p = 0.655
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4184 df = 2 p = 0.2984
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.468 df = 2 p = 0.065
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1943 df = 2 p = 0.9074
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5919 df = 2 p = 0.4512
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2537 df = 2 p = 0.3241
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.4419 df = 2 p = 0.0399
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0393 df = 2 p = 0.5947
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2974 df = 2 p = 0.1923
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3254 df = 2 p = 0.3126
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8509 df = 2 p = 0.3964
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3722 df = 2 p = 0.5035
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.011 df = 2 p = 0.6032
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0092 df = 2 p = 0.2221
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.912 df = 2 p = 0.3844
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0467 df = 2 p = 0.5925
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.639 df = 2 p = 0.2673
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1734 df = 2 p = 0.9169
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4641 df = 2 p = 0.2917
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6835 df = 2 p = 0.2614
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5558 df = 2 p = 0.0377
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4585 df = 2 p = 0.7951
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.584 df = 2 p = 0.7468
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9931 df = 2 p = 0.6086
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7537 df = 2 p = 0.686
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4254 df = 2 p = 0.8084
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5529 df = 2 p = 0.0378
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.6103 df = 2 p = 0.0082
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4017 df = 2 p = 0.4962
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1579 df = 2 p = 0.9241
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5526 df = 2 p = 0.4601
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6582 df = 2 p = 0.7196
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1606 df = 2 p = 0.9228
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1709 df = 2 p = 0.9181
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5896 df = 2 p = 0.274
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4698 df = 2 p = 0.7907
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8674 df = 2 p = 0.6481
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4702 df = 2 p = 0.1764
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.175 df = 2 p = 0.5557
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4319 df = 2 p = 0.4887
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8641 df = 2 p = 0.6492
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6628 df = 2 p = 0.4354
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.047 df = 2 p = 0.2179
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4091 df = 2 p = 0.815
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7234 df = 2 p = 0.6965
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3542 df = 2 p = 0.5081
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0654 df = 2 p = 0.9678
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.898 df = 2 p = 0.0193
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1663 df = 2 p = 0.3385
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.514 df = 2 p = 0.0635
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6408 df = 2 p = 0.162
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0965 df = 2 p = 0.3505
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9689 df = 2 p = 0.3736
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9694 df = 2 p = 0.1374
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3394 df = 2 p = 0.8439
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5704 df = 2 p = 0.1678
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9675 df = 2 p = 0.6165
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7608 df = 2 p = 0.4146
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3747 df = 2 p = 0.8292
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.8144 df = 2 p = 0.0901
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6035 df = 2 p = 0.7395
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4284 df = 2 p = 0.2969
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0474 df = 2 p = 0.9766
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9717 df = 2 p = 0.6152
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2745 df = 2 p = 0.1945
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.6494 df = 2 p = 0.036
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4052 df = 2 p = 0.8166
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4146 df = 2 p = 0.493
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1538 df = 2 p = 0.926
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4013 df = 2 p = 0.1826
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7561 df = 2 p = 0.2521
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5923 df = 2 p = 0.7437
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8218 df = 2 p = 0.6631
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3694 df = 2 p = 0.5042
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0941 df = 2 p = 0.351
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1601 df = 2 p = 0.3396
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5681 df = 2 p = 0.7527
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1006 df = 2 p = 0.3498
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.327 df = 2 p = 0.0697
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1456 df = 2 p = 0.564
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4144 df = 2 p = 0.493
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5249 df = 2 p = 0.4665
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.421 df = 2 p = 0.8102
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6358 df = 2 p = 0.7277
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.5384 df = 2 p = 0.0231
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8761 df = 2 p = 0.2374
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8211 df = 2 p = 0.244
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2767 df = 2 p = 0.1943
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.7966 df = 2 p = 0.0334
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.7858 df = 2 p = 0.0554
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3747 df = 2 p = 0.5029
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1407 df = 2 p = 0.5653
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9003 df = 2 p = 0.2345
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4193 df = 2 p = 0.1809
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4476 df = 2 p = 0.4849
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9969 df = 2 p = 0.6075
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8957 df = 2 p = 0.3876
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.08 df = 2 p = 0.5827
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2041 df = 2 p = 0.5477
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7078 df = 2 p = 0.2582
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2473 df = 2 p = 0.1196
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0995 df = 2 p = 0.5771
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5745 df = 2 p = 0.276
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6002 df = 2 p = 0.1653
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8338 df = 2 p = 0.6591
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1706 df = 2 p = 0.2049
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4208 df = 2 p = 0.2981
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.4553 df = 2 p = 0.0146
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7122 df = 2 p = 0.4248
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2668 df = 2 p = 0.5308
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7931 df = 2 p = 0.6726
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9534 df = 2 p = 0.3765
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4326 df = 2 p = 0.8055
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1002 df = 2 p = 0.9511
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.646 df = 2 p = 0.724
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.2179 df = 2 p = 0.1214
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6553 df = 2 p = 0.2651
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1284 df = 2 p = 0.5688
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7054 df = 2 p = 0.7028
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7342 df = 2 p = 0.4202
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5676 df = 2 p = 0.7529
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2863 df = 2 p = 0.5256
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1167 df = 2 p = 0.9433
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7134 df = 2 p = 0.7
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.831 df = 2 p = 0.2428
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.0213 df = 2 p = 0.0493
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4953 df = 2 p = 0.7806
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.119 df = 2 p = 0.5715
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1276 df = 2 p = 0.3451
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1415 df = 2 p = 0.9317
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5416 df = 2 p = 0.7628
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4118 df = 2 p = 0.1102
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0871 df = 2 p = 0.3522
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6189 df = 2 p = 0.7338
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3847 df = 2 p = 0.825
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7785 df = 2 p = 0.2493
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0103 df = 2 p = 0.6034
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1116 df = 2 p = 0.5736
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1015 df = 2 p = 0.3497
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3742 df = 2 p = 0.8294
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6753 df = 2 p = 0.7134
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7049 df = 2 p = 0.4264
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9704 df = 2 p = 0.6156
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7768 df = 2 p = 0.2495
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3712 df = 2 p = 0.8306
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2372 df = 2 p = 0.5387
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.4005 df = 2 p = 0.0408
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6363 df = 2 p = 0.7275
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7804 df = 2 p = 0.151
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.9825 df = 2 p = 0.1365
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0158 df = 2 p = 0.365
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8016 df = 2 p = 0.2464
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.451 df = 2 p = 0.4841
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.5679 df = 2 p = 0.0618
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4119 df = 2 p = 0.2994
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.664 df = 2 p = 0.7175
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2294 df = 2 p = 0.5408
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1597 df = 2 p = 0.56
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6716 df = 2 p = 0.2629
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2463 df = 2 p = 0.3252
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0381 df = 2 p = 0.9811
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4647 df = 2 p = 0.2916
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4689 df = 2 p = 0.791
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1241 df = 2 p = 0.9399
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2695 df = 2 p = 0.5301
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6742 df = 2 p = 0.433
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2966 df = 2 p = 0.3172
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5385 df = 2 p = 0.7639
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.584 df = 2 p = 0.7468
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1661 df = 2 p = 0.2053
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3953 df = 2 p = 0.1831
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5385 df = 2 p = 0.281
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8096 df = 2 p = 0.1489
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2324 df = 2 p = 0.3275
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8116 df = 2 p = 0.6665
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.496 df = 2 p = 0.4733
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2702 df = 2 p = 0.3214
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.4713 df = 2 p = 0.1069
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7054 df = 2 p = 0.2585
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0517 df = 2 p = 0.1319
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.149 df = 2 p = 0.3415
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9068 df = 2 p = 0.6355
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5387 df = 2 p = 0.4633
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4926 df = 2 p = 0.7817
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.7852 df = 2 p = 0.0914
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8365 df = 2 p = 0.3992
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.3674 df = 2 p = 0.1126
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5941 df = 2 p = 0.4507
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.767 df = 2 p = 0.4133
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7547 df = 2 p = 0.6857
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.274 df = 2 p = 0.118
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2708 df = 2 p = 0.8734
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0126 df = 2 p = 0.9937
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1144 df = 2 p = 0.5728
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9893 df = 2 p = 0.6098
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9274 df = 2 p = 0.2314
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4566 df = 2 p = 0.7959
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3987 df = 2 p = 0.8193
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0441 df = 2 p = 0.3599
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2696 df = 2 p = 0.53
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.7113 df = 2 p = 0.0212
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6552 df = 2 p = 0.2651
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.222 df = 2 p = 0.3292
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2368 df = 2 p = 0.3268
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8432 df = 2 p = 0.2413
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2317 df = 2 p = 0.1987
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4153 df = 2 p = 0.4928
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1387 df = 2 p = 0.5659
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.2456 df = 2 p = 0.0726
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0153 df = 2 p = 0.6019
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.2444 df = 2 p = 0.1975
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0586 df = 2 p = 0.9711
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.621 df = 2 p = 0.4446
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3457 df = 2 p = 0.1877
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6147 df = 2 p = 0.7354
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7625 df = 2 p = 0.2513
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.323 df = 2 p = 0.5161
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5841 df = 2 p = 0.7467
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6586 df = 2 p = 0.7194
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9258 df = 2 p = 0.6295
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0165 df = 2 p = 0.6015
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4013 df = 2 p = 0.8182
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.54 df = 2 p = 0.2808
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.781 df = 2 p = 0.6767
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3096 df = 2 p = 0.1911
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5612 df = 2 p = 0.4581
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2902 df = 2 p = 0.8649
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.91 df = 2 p = 0.3848
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1081 df = 2 p = 0.9474
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6377 df = 2 p = 0.727
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9937 df = 2 p = 0.6085
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.0498 df = 2 p = 0.0801
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2192 df = 2 p = 0.5436
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.9232 df = 2 p = 0.0517
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0865 df = 2 p = 0.5809
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4325 df = 2 p = 0.4886
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9612 df = 2 p = 0.3751
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7253 df = 2 p = 0.6958
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9688 df = 2 p = 0.3737
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0347 df = 2 p = 0.5961
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0987 df = 2 p = 0.9518
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1202 df = 2 p = 0.9417
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1055 df = 2 p = 0.2117
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1645 df = 2 p = 0.5586
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7423 df = 2 p = 0.1539
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1261 df = 2 p = 0.2095
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.1858 df = 2 p = 0.0748
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8213 df = 2 p = 0.6632
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 7.5589 df = 2 p = 0.0228
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3938 df = 2 p = 0.3021
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.37 df = 2 p = 0.1854
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1563 df = 2 p = 0.9248
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4525 df = 2 p = 0.7975
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8594 df = 2 p = 0.3947
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8815 df = 2 p = 0.2367
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7295 df = 2 p = 0.1549
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6959 df = 2 p = 0.4283
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.541 df = 2 p = 0.2807
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4528 df = 2 p = 0.2934
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0518 df = 2 p = 0.9744
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3557 df = 2 p = 0.5077
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8495 df = 2 p = 0.2406
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8326 df = 2 p = 0.6595
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5504 df = 2 p = 0.2794
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1467 df = 2 p = 0.9293
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8646 df = 2 p = 0.3936
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0523 df = 2 p = 0.3584
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5285 df = 2 p = 0.7678
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7554 df = 2 p = 0.4157
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9591 df = 2 p = 0.3755
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2535 df = 2 p = 0.5343
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9018 df = 2 p = 0.3864
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7478 df = 2 p = 0.6881
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1945 df = 2 p = 0.5503
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5714 df = 2 p = 0.7515
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2604 df = 2 p = 0.8779
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6883 df = 2 p = 0.7088
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.7905 df = 2 p = 0.0911
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0159 df = 2 p = 0.6017
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0255 df = 2 p = 0.9873
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.1061 df = 2 p = 0.2116
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1903 df = 2 p = 0.5515
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1877 df = 2 p = 0.9104
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7473 df = 2 p = 0.2532
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4159 df = 2 p = 0.8123
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4225 df = 2 p = 0.1806
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7912 df = 2 p = 0.6733
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0263 df = 2 p = 0.9869
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6041 df = 2 p = 0.4484
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.615 df = 2 p = 0.2705
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8594 df = 2 p = 0.2394
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9594 df = 2 p = 0.619
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8471 df = 2 p = 0.3971
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5144 df = 2 p = 0.7732
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.0499 df = 2 p = 0.0801
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.121 df = 2 p = 0.5709
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.129 df = 2 p = 0.3449
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.66 df = 2 p = 0.0973
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7601 df = 2 p = 0.4148
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7114 df = 2 p = 0.425
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2071 df = 2 p = 0.9016
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3248 df = 2 p = 0.5156
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3913 df = 2 p = 0.8223
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2459 df = 2 p = 0.3253
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0168 df = 2 p = 0.9916
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.3518 df = 2 p = 0.0418
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3928 df = 2 p = 0.3023
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3178 df = 2 p = 0.8531
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2452 df = 2 p = 0.3254
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2197 df = 2 p = 0.896
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.8662 df = 2 p = 0.0532
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9256 df = 2 p = 0.3818
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4626 df = 2 p = 0.7935
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9113 df = 2 p = 0.2332
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4391 df = 2 p = 0.8029
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5279 df = 2 p = 0.4658
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1426 df = 2 p = 0.126
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.8143 df = 2 p = 0.0331
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0883 df = 2 p = 0.352
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5044 df = 2 p = 0.1734
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5813 df = 2 p = 0.2751
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.252 df = 2 p = 0.5347
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.027 df = 2 p = 0.5984
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1757 df = 2 p = 0.3369
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2698 df = 2 p = 0.8738
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2819 df = 2 p = 0.5268
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0827 df = 2 p = 0.9595
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.89 df = 2 p = 0.3887
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.307 df = 2 p = 0.8577
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.4953 df = 2 p = 0.2872
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2934 df = 2 p = 0.5238
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8182 df = 2 p = 0.4029
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.163 df = 2 p = 0.9218
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0419 df = 2 p = 0.9793
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.3855 df = 2 p = 0.184
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6017 df = 2 p = 0.7402
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7377 df = 2 p = 0.2544
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.7041 df = 2 p = 0.0952
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9845 df = 2 p = 0.3707
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5574 df = 2 p = 0.7568
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6464 df = 2 p = 0.7238
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3793 df = 2 p = 0.5017
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.913 df = 2 p = 0.6335
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0554 df = 2 p = 0.1316
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9305 df = 2 p = 0.628
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1809 df = 2 p = 0.1236
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7014 df = 2 p = 0.4271
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9797 df = 2 p = 0.6127
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8072 df = 2 p = 0.6679
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7552 df = 2 p = 0.2522
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.613 df = 2 p = 0.2708
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.448 df = 2 p = 0.0656
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.8872 df = 2 p = 0.1432
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2751 df = 2 p = 0.3206
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.9013 df = 2 p = 0.2344
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1202 df = 2 p = 0.9417
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4158 df = 2 p = 0.1812
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0155 df = 2 p = 0.1343
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5725 df = 2 p = 0.7511
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8731 df = 2 p = 0.392
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3825 df = 2 p = 0.3038
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0952 df = 2 p = 0.9535
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4173 df = 2 p = 0.4923
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5053 df = 2 p = 0.4711
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2293 df = 2 p = 0.5408
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.1919 df = 2 p = 0.123
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2955 df = 2 p = 0.8626
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1608 df = 2 p = 0.9228
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0792 df = 2 p = 0.9612
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.43 df = 2 p = 0.8065
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1567 df = 2 p = 0.5608
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5669 df = 2 p = 0.7532
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.5063 df = 2 p = 0.1051
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1891 df = 2 p = 0.9098
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6135 df = 2 p = 0.7358
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5271 df = 2 p = 0.7683
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6716 df = 2 p = 0.7148
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3188 df = 2 p = 0.5172
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.125 df = 2 p = 0.9394
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4511 df = 2 p = 0.7981
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2809 df = 2 p = 0.869
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5548 df = 2 p = 0.4596
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0388 df = 2 p = 0.9808
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6053 df = 2 p = 0.7389
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3259 df = 2 p = 0.3126
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1077 df = 2 p = 0.9476
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2191 df = 2 p = 0.8962
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4162 df = 2 p = 0.8121
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6762 df = 2 p = 0.2623
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.8699 df = 2 p = 0.3926
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9742 df = 2 p = 0.3726
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4783 df = 2 p = 0.4775
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0827 df = 2 p = 0.9595
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0061 df = 2 p = 0.6047
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9959 df = 2 p = 0.6078
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7239 df = 2 p = 0.6963
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.6853 df = 2 p = 0.4306
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.058 df = 2 p = 0.2168
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0415 df = 2 p = 0.9794
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.428 df = 2 p = 0.8074
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.303 df = 2 p = 0.1918
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.408 df = 2 p = 0.4946
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.0857 df = 2 p = 0.1297
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5883 df = 2 p = 0.7451
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0865 df = 2 p = 0.2137
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3318 df = 2 p = 0.8471
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2271 df = 2 p = 0.8927
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0244 df = 2 p = 0.9879
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6924 df = 2 p = 0.7074
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.307 df = 2 p = 0.5202
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.7319 df = 2 p = 0.1548
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0599 df = 2 p = 0.9705
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.899 df = 2 p = 0.0318
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4095 df = 2 p = 0.8148
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.983 df = 2 p = 0.225
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7221 df = 2 p = 0.697
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2197 df = 2 p = 0.5434
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7548 df = 2 p = 0.6856
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7717 df = 2 p = 0.2501
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.2454 df = 2 p = 0.0726
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5653 df = 2 p = 0.7538
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5801 df = 2 p = 0.167
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2745 df = 2 p = 0.5287
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0695 df = 2 p = 0.9659
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7483 df = 2 p = 0.4172
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0936 df = 2 p = 0.2129
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 9.1132 df = 2 p = 0.0105
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.3825 df = 2 p = 0.3038
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5641 df = 2 p = 0.4575
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3613 df = 2 p = 0.5063
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7027 df = 2 p = 0.7037
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9896 df = 2 p = 0.3698
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0715 df = 2 p = 0.5852
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5715 df = 2 p = 0.7514
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8657 df = 2 p = 0.6486
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.71 df = 2 p = 0.7012
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.7275 df = 2 p = 0.0571
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3007 df = 2 p = 0.8604
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9105 df = 2 p = 0.6343
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7052 df = 2 p = 0.7029
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2979 df = 2 p = 0.8616
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1905 df = 2 p = 0.3344
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.639 df = 2 p = 0.2673
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9029 df = 2 p = 0.3862
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5908 df = 2 p = 0.2738
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6685 df = 2 p = 0.7159
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4673 df = 2 p = 0.4801
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.7816 df = 2 p = 0.0916
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9421 df = 2 p = 0.6243
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.2169 df = 2 p = 0.8972
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3035 df = 2 p = 0.8592
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6211 df = 2 p = 0.733
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0512 df = 2 p = 0.9747
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7342 df = 2 p = 0.4202
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3349 df = 2 p = 0.513
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1048 df = 2 p = 0.3491
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2094 df = 2 p = 0.3313
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7144 df = 2 p = 0.6996
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2686 df = 2 p = 0.3216
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6767 df = 2 p = 0.7129
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5261 df = 2 p = 0.4663
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1029 df = 2 p = 0.3494
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1096 df = 2 p = 0.5742
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.464 df = 2 p = 0.7929
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7287 df = 2 p = 0.6946
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8227 df = 2 p = 0.2438
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9191 df = 2 p = 0.6316
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.1265 df = 2 p = 0.5694
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.4444 df = 2 p = 0.8007
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0888 df = 2 p = 0.5802
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.9082 df = 2 p = 0.3852
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7893 df = 2 p = 0.2479
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.7035 df = 2 p = 0.7035
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0002 df = 2 p = 0.6065
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4767 df = 2 p = 0.4779
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.6659 df = 2 p = 0.1599
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.2024 df = 2 p = 0.3325
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3727 df = 2 p = 0.83
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.6734 df = 2 p = 0.2627
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1638 df = 2 p = 0.9214
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2976 df = 2 p = 0.5227
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0701 df = 2 p = 0.3552
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.027 df = 2 p = 0.5984
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.0347 df = 2 p = 0.5961
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.8798 df = 2 p = 0.237
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1876 df = 2 p = 0.9105
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3643 df = 2 p = 0.8335
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1314 df = 2 p = 0.3445
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.523 df = 2 p = 0.0632
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.9415 df = 2 p = 0.0114
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0443 df = 2 p = 0.9781
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.5538 df = 2 p = 0.2789
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0751 df = 2 p = 0.9632
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.2563 df = 2 p = 0.5336
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1805 df = 2 p = 0.9137
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.0532 df = 2 p = 0.2173
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.6405 df = 2 p = 0.726
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.3548 df = 2 p = 0.8375
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.9961 df = 2 p = 0.6077
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.4167 df = 2 p = 0.4925
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7612 df = 2 p = 0.2514
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8429 df = 2 p = 0.6561
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1794 df = 2 p = 0.9142
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.5074 df = 2 p = 0.4706
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.7093 df = 2 p = 0.4254
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.259 df = 2 p = 0.5329
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 5.5756 df = 2 p = 0.0616
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.2549 df = 2 p = 0.0438
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.0529 df = 2 p = 0.9739
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 1.3859 df = 2 p = 0.5001
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.7859 df = 2 p = 0.2483
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.4924 df = 2 p = 0.1744
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 3.5051 df = 2 p = 0.1733
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.1056 df = 2 p = 0.9486
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.8831 df = 2 p = 0.643
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 4.8115 df = 2 p = 0.0902
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.1746 df = 2 p = 0.3371
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 2.0677 df = 2 p = 0.3556
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 0.5514 df = 2 p = 0.759
#> 
#>  Bootstrap White test
#> 
#> data:  mod
#> X-squared = 13.801, B = 999, p-value = 0.001
#> 

# Enable parallel processing when supported by the operating system
if (.Platform$OS.type != "windows") {
  performWhiteTestBootstrap(mod, sim, B = 199, parallel = TRUE)
}
#> [INFO] Running Bootstrap White test
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 13.8014 df = 2 p = 0.001
#> 
#>  Bootstrap White test
#> 
#> data:  mod
#> X-squared = 13.801, B = 199, p-value = 0.01
#> 
# }
```
