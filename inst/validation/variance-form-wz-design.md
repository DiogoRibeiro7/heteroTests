# A consistent test of the variance function, second candidate: design note

A second stage 1 of the item "A consistent test of a variance function" in
`ROADMAP.md`. The first candidate, the test of Dette, Neumeyer and Van
Keilegom (2007), did not hold its level when the variance is not constant
(`variance-form-dnvk-design.md`). The author chose to try the test of Wang and
Zhou (2007) next, because its theory assumes a parametric mean, which is the
package's setting. Nothing is exported and nothing in `R/` changes. The
evidence is in `README.md`, in the section on this test, and in these files:

| File | Contents |
| --- | --- |
| `variance-form-wz-prototype.R` | The statistic, the pseudo-likelihood fit and the bootstrap, as functions in plain R. |
| `variance-form-wz-reproduction.R` | Table 1 of the working paper, reproduced with the prototype. |
| `variance-form-wz-size-power.R` | The prototype in the designs of the first candidate's study, beside `performVarianceFormTest()`. |

## The source, and what was read

The article is Wang, L. and Zhou, X.-H. (2007), "Assessing the adequacy of
variance function in heteroscedastic regression models", *Biometrics* 63(4),
1218-1225, <https://doi.org/10.1111/j.1541-0420.2007.00805.x> (published online
2 May 2007, in the December 2007 issue). **It could not be obtained.** On
2026-10-12 the publisher's page (Oxford Academic) offered it for purchase only,
Unpaywall reported no open copy, Europe PMC holds the record (PMID 17484775)
without a full text, and the University of Minnesota's repository holds a
record without a file.

What was read is the authors' working paper: Wang, L. and Zhou, X.-H. (2006),
"Assessing the adequacy of variance function in heteroscedastic regression
models", UW Biostatistics Working Paper Series, Working Paper 299, dated 7
September 2006, <https://biostats.bepress.com/uwbiostat/paper299>. It has 27
pages: the text, an appendix with sketches of the proofs, and Tables 1 to 3.
Equation, section and table numbers below are the working paper's. **Only the
working paper could be obtained, not the published article. The author
accepted the working paper as the source for this stage 1 on 2026-10-12;**
this note, the prototype and the study rest on it. The comparison with the
published article is still to be made before any stage 2.

How it relates to the article, as far as can be told without the article:

- **It is a revised version.** Its acknowledgements thank "the AE, a referee
  and the Co-editor, whose comments led to significant improvement of the
  paper", so it postdates review. The article is dated as published online in
  May 2007.
- **The summaries agree in substance.** The article's summary, on Crossref,
  describes a kernel-smoothing test of a parametric variance structure that
  needs no error distribution, is asymptotically normal under the null
  hypothesis, uses "a simple bootstrap algorithm to approximate the
  distribution of the test statistic in finite sample size", and is applied to
  a radioimmunoassay data set. The working paper has each of these. The
  article's keywords include pseudolikelihood, as the working paper's do.
- **Not checked:** whether the article keeps Section 6 (a nonparametric mean)
  and Table 3, which the 8-page article may have shortened; whether its
  tables equal the working paper's; and whether it corrects the misprint in
  (7) described below. **The author should compare the two before any
  stage 2.**

## 1. The statistic, its estimators and the null distribution

The model is `Y_i = f(X_i, beta) + eps_i` with `Var(eps_i | Z_i) =
g(Z_i, beta, theta)` (1), a mean of known parametric form, and `Z` a vector of
`q` explanatory variables that may contain some or all of `X`. The variance
may depend on the mean through `beta`, as in a power-of-the-mean model. The
errors are independent with mean zero; no distribution is assumed. The null
hypothesis is `sigma_i^2 = g(Z_i, beta, theta)` for some `(beta, theta)`.

1. **The estimators**, Section 3.1: pseudo-likelihood. Given a current
   `beta*`, `theta` maximises (7), which has the form of a normal
   log-likelihood of the squared residuals; given `theta*`, `beta` solves the
   generalized least-squares equation (8); the two steps are repeated until
   they converge, and the estimators are root-n consistent.
2. **The residuals**: `r-hat_i = (Y_i - f(X_i, beta-hat))^2 -
   g(Z_i, beta-hat, theta-hat)` (2), which have mean zero under the null
   hypothesis.
3. **The statistic** (3):
   `T_n = sum_i sum_{j != i} K((Z_i - Z_j) / h) r-hat_i r-hat_j / (n (n - 1) h^q)`,
   a leave-one-out kernel estimate of `E[r E(r | Z) p(Z)] = E[(E(r | Z))^2 p(Z)]`,
   which is zero under the null hypothesis and positive under any alternative.
   `K` is a bounded, symmetric, non-negative density; the paper does not name
   one. Large values reject.
4. **The asymptotic null distribution** (5)-(6): `n h^(q/2) T_n` tends to
   `N(0, tau^2)` with `tau^2 = 2 int K^2 int [xi^4(z) - g^2(z)]^2 p^2(z) dz`,
   `xi^4(z) = E(eps^4 | Z = z)`, as `h -> 0` and `n h^q -> infinity`; reject when
   `n h^(q/2) T_n / tau` exceeds the normal quantile. The working paper gives
   no estimate of `tau^2`.
5. **The bootstrap**, Section 3.2, which the paper uses in practice because the
   normal approximation of smoothing tests is poor in small samples:
   - fit `(beta-hat, theta-hat)` under the null hypothesis;
   - form `eps-hat_i = (Y_i - f(X_i, beta-hat)) / sqrt(g(Z_i, beta-hat,
     theta-hat))`, and centre and scale them to mean 0 and variance 1;
   - draw `eps*_i` from them and set `Y*_i = f(X_i, beta-hat) +
     sqrt(g(Z_i, beta-hat, theta-hat)) eps*_i`, on the same design;
   - fit the null model to `(X_i, Y*_i, Z_i)` and compute `T*_n` from its
     residuals (9);
   - take the critical value from the quantile of the `T*_n`.
6. **Settings of the simulations**, Section 4: 400 replications, 200 bootstrap
   samples, the 5% level, `h` = 0.10, 0.15, 0.20 and 0.25 at both 50 and 100
   observations, `x` uniform on (0, 1). The kernel and the bootstrap quantile
   are not stated.

**A misprint in (7).** The working paper prints the pseudo-likelihood as
`-sum ln g - (1/2) sum (Y - f)^2 / g`. Read literally, its maximiser is half the
variance, and `r-hat` would not have mean zero under the null hypothesis. The
paper calls (7) the form of a normal likelihood and cites Carroll and Ruppert
(1988) for the method, whose pseudo-likelihood is the normal log-likelihood,
`-(1/2) sum ln g - (1/2) sum (Y - f)^2 / g`. The prototype uses that.

**What the prototype does.** It takes the mean `X beta` of the `lm`, a
log-linear variance function, `g = exp(theta' w)` with `w = (1, x)` for the
exponential form and `w = (1, log x)` for the power form of
`performVarianceFormTest()`, and one smoothing variable, `x`. The fixed point
of (7) and (8) is the joint normal maximum-likelihood estimate; the prototype
reaches it by joint Fisher scoring, which is (7) and (8) taken one step at a
time and is valid because the information is block diagonal, and checks it
against a general-purpose optimiser. In the bootstrap it starts each fit from
`(beta-hat, theta-hat)`, which changes the number of steps and not the result.
It uses the package's p-value, `(1 + #{T* >= T}) / (B + 1)`. Beside the
bootstrap it computes the standardized statistic with the usual variance of
a degenerate U-statistic,
`2 sum_{i != j} K_ij^2 r-hat_i^2 r-hat_j^2 / (n (n - 1))^2` with `K_ij` the
kernel weights, and refers it to the standard normal. That estimate is not in
the working paper and is labelled as an addition.

## 2. What the parametric mean buys

The working paper's main body assumes the mean has a known parametric form,
and its Appendix A sketches the proof that estimating `(beta, theta)` does not
change the limit: `r-hat_i - r_i = 2 eps_i (f(X_i, beta) - f(X_i, beta-hat)) +
(f(X_i, beta) - f(X_i, beta-hat))^2 + (g(Z_i, beta, theta) -
g(Z_i, beta-hat, theta-hat))`, and each of the nine terms this adds to `T_n` is
`o_p(n^(-1) h^(-q/2))` because `beta-hat` and `theta-hat` are root-n
consistent (5). So the package's setting, a linear mean fitted with the
variance function, is the paper's own setting; no argument has to be carried
over, as it had to for the first candidate.

Two consequences matter.

- **The estimation of the variance function does not enter the limit.** The
  statistic is a degenerate U-statistic whose leading term involves the true
  `r_i` only. In the first candidate the fitted variance function entered the
  first-order term, and the bootstrap's dependence on it was what made that
  prototype conservative. Here the bootstrap still generates its data from
  the fitted variance function, and the null distribution of `T_n` still
  depends on the variance function through `tau^2`, so the same mechanism can
  act at second order; the study measures whether it does.
- **The mean must be right.** Section 6 shows that with a misspecified
  parametric mean the test rejects far too often (its Table 3, a linear mean
  for a quadratic truth), and the paper advises testing the mean first.

## 3. What the variance function may depend on

The theory allows `q` variables in `Z`, some or all of the mean's, and a
variance that depends on the mean. The cost is in the smoothing: `T_n` detects
local alternatives at the rate `n^(-1/2) h^(-q/4)` (Section 2.3), slower than
root-n, and slower as `q` grows. The working paper's second study, a
power-of-the-mean model with two regressors, does not say which variables it
smooths in.

**Recommendation for a first version: one named variable**, as for the first
candidate, and the same restriction on the forms (a continuous variable,
positive for the power form). A power-of-the-mean form, where `Z` would be the
fitted mean or the regressors, is the natural second step, because this paper
covers it; the package's forms are not functions of the mean today.

## 4. The bandwidth

The theory asks only for `h -> 0` and `n h^q -> infinity`. The simulations use
the same four bandwidths at 50 and 100 observations, and Table 1 shows the
level insensitive to them (0.048 to 0.055) and the power sensitive in a way
that depends on the alternative: against the log-quadratic departure (2) it
rises with `h`, from 0.633 to 0.810 at 50 observations, and against the
oscillating (3) it peaks at `h = 0.15` (0.690) and falls to 0.513 at
`h = 0.25`. Table 2, under a power-of-the-mean model, has null rates from
0.025 to 0.063 across bandwidths and error distributions.

The prototype's rule is `h = c (n / 100)^(-1/5)` times the range of the
variable, with `c = 0.15`: the paper's middle bandwidth at 100 observations on
(0, 1). The study also runs `c = 0.10` and `c = 0.25`, the paper's extremes,
at 50 and 150 observations.

## 5. How it relates to `performVarianceFormTest()` and to the first candidate

- **The same null hypothesis and the same `form`.** With one variance
  regressor the exponential and power forms are log-linear in `x` or `log x`,
  which is what the prototype fits.
- **A different estimate of the variance function.** The test needs
  `r-hat` to have mean zero under the null hypothesis, so the level of the
  fitted variance matters, not only its shape. `fitWLS()` regresses `log(e^2)`
  on the variance regressors, whose constant is shifted by `E[log eps^2]`, and
  the weighted refit it makes is one step of (8). A stage 2 would need the
  pseudo-likelihood fit, or the log-regression with its constant corrected and
  iterated.
- **Different alternatives.** `performVarianceFormTest()` is a score test
  against the terms it adds. This test is consistent against any smooth
  departure in the smoothing variable, at the price of a bandwidth, a
  bootstrap, and a slower local rate.
- **Against the first candidate.** Both smooth in one variable and resample
  under the null hypothesis. They differ in what they compare (here the
  squared residuals against the fitted variance, there two empirical
  distributions of standardized residuals), in the estimator of the variance
  function (pseudo-likelihood here, unweighted least squares there), in the
  residuals resampled (standardized by the fitted variance function here, by
  the kernel estimate there), and in whether the fit enters the limit
  (section 2).
- **Where it would belong** is the question the first design note answers:
  a function of its own, sharing `form`, `var_formula` and the handling of a
  `fitWLS()` fit with `performVarianceFormTest()`, rather than
  `against = "any"`, which would change the meaning of the returned statistic
  with an argument and carry arguments (`h`, `B`, the kernel) that apply to
  one value only.

## 6. What stage 1 found, and a recommendation

[TO BE WRITTEN FROM THE RESULTS]

## References

Carroll, R. J. and Ruppert, D. (1988). *Transformation and Weighting in
Regression*. Chapman and Hall.

Dette, H., Neumeyer, N. and Van Keilegom, I. (2007). A new test for the
parametric form of the variance function in non-parametric regression.
*Journal of the Royal Statistical Society: Series B* 69(5), 903-917.
<https://doi.org/10.1111/j.1467-9868.2007.00616.x>

Wang, L. and Zhou, X.-H. (2007). Assessing the adequacy of variance function in
heteroscedastic regression models. *Biometrics* 63(4), 1218-1225.
<https://doi.org/10.1111/j.1541-0420.2007.00805.x> (not read)

Wang, L. and Zhou, X.-H. (2006). Assessing the adequacy of variance function in
heteroscedastic regression models. UW Biostatistics Working Paper Series,
Working Paper 299. <https://biostats.bepress.com/uwbiostat/paper299> (read)
