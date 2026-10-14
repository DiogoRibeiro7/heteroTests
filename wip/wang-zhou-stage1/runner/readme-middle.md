What follows from the reproduction:

- **With the Epanechnikov kernel the table is reproduced.** All 24 rates of
  `T_n` and all 6 of the Wald test are within the 99% interval of their
  difference from the paper's; the paper's 400 replications make that
  interval wide, about 6.5 percentage points either way at a power of 0.7.
  Under the null hypothesis the rates are 0.044 to 0.053. Under the
  alternatives they are lower than the paper's in every cell, by 2.7 points
  on average and at most 6.2.
- **With the Gaussian kernel it is not.** The power against the oscillating
  alternative (3) falls as the bandwidth grows, to 0.084 and 0.145 at
  `h = 0.25` where the paper has 0.513 and 0.903, and the null rate falls to
  0.022. The paper's kernel had a compact support, and the study below uses
  the Epanechnikov kernel for that reason, a choice made before it ran.
- **The Wald test of the table holds here as there:** 0.103 and 0.071 under
  the null hypothesis at 50 and 100 observations, above 0.05 as in the paper,
  and 0.177 to 0.186 against (3), which it does not detect.

**Size and power in the package's designs.** The designs of the first
prototype's study: `y = 1 + 2 x + 0.5 z + sigma(x) e`, `x` and `z` uniform on
(1, 5), the model `y ~ x + z`, 50, 150 and 400 observations, Gaussian and `t5`
errors, six true variances including the wave, and the exponential and power
forms in `x`. The prototype fits the linear mean and the variance function by
pseudo-likelihood, smooths in `x` with the Epanechnikov kernel and
`h = 0.15 (n / 100)^(-1/5)` times the range of `x`, and uses `B = 199` with the
package's p-value. At 50 and 150 observations it also runs with the constant
0.10 and 0.25 in place of 0.15. Beside it, on the same samples, run the same
statistic referred to the standard normal with a plug-in variance, which is
not in the paper, and `performVarianceFormTest()` with `var_formula = ~ x` and
with its defaults. The rates of the first prototype in the same designs, on
other samples, come from `variance-form-dnvk-size-power.csv`. The twelve null
cells with Gaussian errors decide criterion 2 of the release gate and have
5000 replications; the others have 1000, where the 99% interval around 0.05
is [0.032, 0.068].
