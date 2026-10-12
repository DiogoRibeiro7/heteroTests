# A consistent test of the variance function: design note

Stage 1 of the item "A consistent test of a variance function" in `ROADMAP.md`:
the test of Dette, Neumeyer and Van Keilegom (2007), a prototype of it in plain
R, and what it would take to add it to the package. Nothing is exported and
nothing in `R/` changes. The evidence is in `README.md`, in the section on this
test, and in these files:

| File | Contents |
| --- | --- |
| `variance-form-dnvk-prototype.R` | The statistic and the bootstrap, as functions in plain R. |
| `variance-form-dnvk-reproduction.R` | Tables 4.1 and 4.5 of the manuscript, reproduced with the prototype. |
| `variance-form-dnvk-size-power.R` | The prototype in the designs of `variance-form-size-power.R`, beside `performVarianceFormTest()`. |
| `variance-form-dnvk-bootstrap-check.R` | The same null designs, bootstrapped from the fitted and from the true variance function, to find why the prototype is conservative. |

## The source, and what was read

The article is Dette, H., Neumeyer, N. and Van Keilegom, I. (2007), "A new test
for the parametric form of the variance function in non-parametric
regression", *Journal of the Royal Statistical Society: Series B* 69(5),
903-917, <https://doi.org/10.1111/j.1467-9868.2007.00616.x>. **It could not be
obtained.** On 2026-10-11 the publisher's page (Oxford Academic) offered it for
purchase only; the PDF link Unpaywall lists as free returned a Cloudflare
challenge to a script and a paywall to a fetch; the Wiley addresses returned
403; the Wayback Machine has no copy; the UCLouvain repository (DIAL) holds a
file behind a login.

What was read is the authors' submitted manuscript: Dette, H. and Van Keilegom,
I. (2005), "A new test for the parametric form of the variance function in
nonparametric regression", technical report dated 9 September 2005, TU Dortmund
(SFB 475), <https://doi.org/10.17877/DE290R-14489>, and the same text as IAP
Statistics Network technical report 0544. The two files are identical word for
word apart from the cover page, and Unpaywall lists the first as the submitted
version of the article. Equation, table and theorem numbers below are the
manuscript's.

The manuscript is not the article. The article's summary, on Crossref, differs
from the manuscript's abstract in three ways: the article has a third author,
it states that "the consistency of a bootstrap approximation is established",
which the manuscript does not do, and it has no data example. Pardo-Fernández
and Jiménez-Gamero (2019), who compare their test with the published one,
describe it as the manuscript does: the estimator (2.11), the parametric fit
smoothed with the same weights, and a smooth residual bootstrap
`eps* + v N` from the standardized residuals; they cite its result under
local alternatives as Theorem 2, which is the manuscript's Theorem 3.4. So the
statistic and the resampling scheme appear to be unchanged. What could not be
checked is the article's simulation tables, the conditions of its bootstrap
theorem (for instance on `v`), and any change to the simulation settings.
**The author should compare the manuscript with the article before stage 2.**

The other two papers the roadmap names:

- Wang, L. and Zhou, X.-H. (2007), "Assessing the adequacy of variance function
  in heteroscedastic regression models", *Biometrics* 63(4), 1218-1225,
  <https://doi.org/10.1111/j.1541-0420.2007.00805.x>. Read as UW Biostatistics
  Working Paper 299 (September 2006); the article itself is behind a paywall.
- Samarakoon, N. and Song, W. (2011), "Minimum distance conditional variance
  function checking in heteroscedastic regression models", *Journal of
  Multivariate Analysis* 102(3), 579-600,
  <https://doi.org/10.1016/j.jmva.2010.11.003>. Not obtained: ScienceDirect
  refused the download, and no preprint was found. Only its abstract was read.

## 1. The statistic, its estimators and the bootstrap

The model is `Y = m(X) + sigma(X) eps`, with one covariate `X`, `eps`
independent of `X` with mean 0 and variance 1, and `m` and `sigma` smooth and
unknown. The null hypothesis is that `sigma^2` belongs to a parametric family
`{sigma_theta^2 : theta in Theta}`, with `Theta` compact.

1. **Mean and variance**, (2.8)-(2.9): Nadaraya-Watson estimates with kernel
   `K` and bandwidth `h`,
   `m-hat(x) = sum_i W_i(x, h) Y_i` and
   `sigma-hat^2(x) = sum_i W_i(x, h) (Y_i - m-hat(X_i))^2`, where
   `W_i(x, h) = K((x - X_i) / h) / sum_j K((x - X_j) / h)`. The same kernel
   and bandwidth serve both.
2. **The parametric fit**, (2.11): `theta-hat` minimises
   `sum_i [(Y_i - m-hat(X_i))^2 - sigma_theta^2(X_i)]^2`, an unweighted least
   squares fit of the variance function to the squared residuals. It is then
   smoothed with the same weights,
   `sigma-hat_0^2(x) = sum_i W_i(x, h) sigma_theta-hat^2(X_i)`, so that under
   the null hypothesis it carries the same smoothing bias as
   `sigma-hat^2`.
3. **Two sets of standardized residuals**:
   `eps-hat_i = (Y_i - m-hat(X_i)) / sigma-hat(X_i)` and
   `eps-hat_i0 = (Y_i - m-hat(X_i)) / sigma-hat_0(X_i)`, with empirical
   distribution functions `F-hat` and `F-hat_0`, (2.10) and (2.12). Under the
   null hypothesis they estimate the same distribution; Lemma 2.1 shows that
   the two distributions they estimate coincide only under the null
   hypothesis, when every moment of `eps` exists.
4. **The statistics**:
   `T_KS = sqrt(n) sup_y |F-hat(y) - F-hat_0(y)|` and
   `T_CM = n int [F-hat(y) - F-hat_0(y)]^2 dF-hat(y)`, the second being
   `sum_i [F-hat(eps-hat_i) - F-hat_0(eps-hat_i)]^2`. The manuscript's
   simulations use `T_CM` only. `T_KS` takes its values on the lattice
   `k / sqrt(n)`, so a bootstrap value often equals the observed one, and a
   p-value that counts ties as exceedances, as the package's convention and
   the rule (4.2) both do, is conservative for it; a version of `T_KS` for
   the package would need ties broken at random.
5. **The bootstrap**, Section 4.1: draw `eps~_1, ..., eps~_n` from `F-hat`,
   set `eps*_i = eps~_i + v N_i` with `N_i` standard normal (4.1), generate
   `Y*_i = m-hat(X_i) + sigma_theta-hat(X_i) eps*_i` on the same `X_i`, and
   compute the statistic on `(X_i, Y*_i)` by steps 1 to 4. With `B` bootstrap
   statistics the null hypothesis is rejected when
   `T_CM > T*_CM,(floor(B (1 - alpha)))` (4.2). The resampling is from the
   residuals of the general model, `eps-hat_i`, not from those under the null
   hypothesis, and the bootstrap data are generated with the unsmoothed
   parametric variance function. The residuals are not centred or rescaled.
   With `B = 100`, the rule (4.2) rejects a statistic whose bootstrap
   distribution is exact 6 times in 101 at the 5% level, not 5: it rejects
   when at least 95 of the 100 bootstrap values lie below the observed one.
   The package's convention, `(1 + #{T* >= T}) / (B + 1)` with `B = 999` or
   `B = 199`, has no such excess.
6. **Settings of the simulations**: `X` uniform on `[0, 1]`, `h = 0.3` and
   `h = 0.5`, `v = 0.2`, `B = 100`, 1000 replications; `B = 500` and a
   least-squares cross-validated `h` in the data example. The kernel is not
   named. Assumption (A1) asks for a compact support and two continuous
   derivatives; the prototype uses the Epanechnikov kernel, which has the
   compact support and is not differentiable at its ends.

**The asymptotics.** Under the null hypothesis (Theorem 3.1 and Corollary 3.2)
`sqrt(n) (F-hat - F-hat_0)` converges to `y f(y) W`, with `f` the density of
`eps` and `W` a single normal variable with variance
`(1/4) E[eps^4 - 1] E[a(X)^2]`, where
`a(X) = 1 - sigma^2(X) c' Omega^(-1) d sigma_theta^2(X) / d theta`,
`c = E[sigma^(-2)(X) d sigma_theta^2(X) / d theta]` and `Omega` the
expectation of the outer product of the gradient. The limit is a fixed
function times one random variable, so `T_CM` tends to
`W^2 int y^2 f(y)^2 dF(y)` and `T_KS` to `|W| sup |y f(y)|`. They depend on the
density of the errors, which is why the paper resamples. The estimation of `m`
does not enter the limit: the same `m-hat` appears in both sets of residuals,
and its contribution cancels (proof of Theorem 3.1).

Two consequences matter for this package.

- **The limit is degenerate for constant variance.** When the true variance is
  constant and the family contains the constants, `a(X)` is identically zero
  (Example 3.1): the root-n term vanishes, the statistic is of smaller order,
  and the theory says nothing about its distribution beyond that. Both forms
  the package offers contain constant variance, so the null cells with
  `sigma^2 = 1` are cells the theory does not describe; whether the bootstrap
  follows the statistic there is an empirical question. The manuscript's
  homoscedastic rows (Table 4.1, `c = 0`) suggest that it does, the
  reproduction confirms those rows, and in the package's designs the cells
  with constant variance are the ones where the prototype holds its level from
  150 observations. For a non-constant truth `a(X)` is not zero (Example 3.2),
  and the limit is the one above.
- **Local power at the root-n rate**, in the directions where the drift of
  Theorem 3.4 is not zero. Pardo-Fernández and Jiménez-Gamero (2019) point out
  that local alternatives with zero drift exist; against a fixed alternative
  the test is consistent by Lemma 2.1, whose proof assumes that every moment
  of `eps` exists. The `t5` errors of the package's designs have finite
  moments only below order 5, so for them the consistency is not covered by
  the proof as written; the assumptions of the expansion itself ask only for
  `E eps^4 < infinity`, which `t5` satisfies.

## 2. A linear mean instead of a nonparametric one

The package tests the residuals of an `lm`: the mean is `x' beta`, possibly in
variables other than the one the variance depends on (`z` in the designs), and
`beta-hat` is a least-squares estimate. The prototype replaces
`Y_i - m-hat(X_i)` by the least-squares residuals, keeps everything else, and
in the bootstrap generates `Y* = X beta-hat + sigma_theta-hat(x) eps*` and
refits the linear model to every bootstrap sample.

**The manuscript's theory does not cover this.** Its mean is a kernel smooth of
the single covariate, its proofs (Lemma A.1 to A.3, and Theorem 3.1 through
Akritas and Van Keilegom 2001) are written for that estimator, and assumption
(A2)(iii) and the bandwidth conditions of (A1) concern it.

An argument that the result carries over, which is not a proof: write
`e_i = Y_i - x_i' beta-hat = sigma(X_i) eps_i - x_i'(beta-hat - beta)`. With a
correctly specified mean, errors independent of all the regressors, and a
compact design, `sup |x'(beta-hat - beta)| = O_p(n^(-1/2))`, which is smaller
than the uniform error of the Nadaraya-Watson mean the proofs control.
(i) In `F-hat - F-hat_0` the mean enters both terms identically, as in the
proof of Theorem 3.1, and its contribution is of order
`sup |x'(beta-hat - beta)| sup |sigma-hat - sigma-hat_0| = o_p(n^(-1/2))` under
the null hypothesis. (ii) In `sigma-hat^2` and `theta-hat` the squared
residuals become
`sigma^2 eps^2 - 2 sigma eps x'(beta-hat - beta) + (x'(beta-hat - beta))^2`.
The cross term enters the linear functionals of Lemmas A.1 and A.3 as
`(beta-hat - beta)' n^(-1) sum_i sigma(X_i) eps_i x_i a(X_i)`, a product of two
`O_p(n^(-1/2))` factors, and the last term is `O_p(n^(-1))`. So the main term
of Theorem 3.1 should be unchanged. The step that would need to be redone is
the uniform empirical-process argument of Akritas and Van Keilegom (2001) with
the class `{x'b}` in place of the smooth functions it uses; that class is
simpler, but the work has not been done. The bootstrap with a linear mean is
covered by no result read here (the article's bootstrap theorem, which was not
read, is for its own setting); the simulation is the evidence.

**Wang and Zhou (2007) do cover it.** The main body of their paper assumes a
mean of known parametric form, `f(X, beta)`, with a variance `g(Z, beta, theta)`
that may depend on the mean, and treats the nonparametric mean only in a
section of its own (Section 6 of the working paper). Their statistic is a
kernel-weighted U-statistic of the residuals `r_i = (Y_i - f(X_i, beta-hat))^2 -
g(Z_i, beta-hat, theta-hat)`, `T_n = sum_{i != j} K((Z_i - Z_j) / h) r_i r_j /
(n (n - 1) h^q)`, asymptotically normal after scaling by `n h^(q/2)`, with a
residual bootstrap for the critical values; it allows several variance
regressors `Z`. It detects local alternatives at the rate `n^(-1/2) h^(-q/4)`,
slower than the root-n rate of the test above. Whether Samarakoon and Song
(2011) assume a parametric mean could not be checked.

## 3. What the variance function may depend on

The manuscript has one continuous covariate, used by the mean, the variance and
the smoother alike, with a density bounded away from zero on a compact
interval (A2)(i). Two choices are open for a first version.

- **A single named variable** (`var_formula = ~ x`). This is the manuscript's
  setting with the mean replaced by a linear model, and it is the setting of
  the prototype. The variable must be continuous, and positive for the power
  form.
- **The fitted values.** One index whatever the number of regressors, and the
  usual way to read a funnel in a residual plot. But the fitted values are
  estimated, which the theory does not cover, and the package's two forms are
  functions of named variables, not of the mean: a variance that depends on
  `x` alone is not a function of `x + 0.25 z`, so in the package's own designs
  every null cell with a non-constant variance would become an alternative. A
  power-of-the-mean form is the setting of Wang and Zhou (2007), not of this
  paper.

**Recommendation: a single named variable.** Several variables would need a
multivariate smoother, with the loss of power that brings, and are not in the
paper.

## 4. The bandwidth

The manuscript gives no rule for the test. Its theory asks for `n h^4 -> 0`
and `n h^(3 + 2 delta) / log(1 / h) -> infinity` (A1), that is undersmoothing
with `h` between the orders `n^(-1/3)` and `n^(-1/4)`; a cross-validated
bandwidth is of order `n^(-1/5)`, outside that window, although it is what the
data example uses. The simulations fix `h = 0.3` and `h = 0.5` on `[0, 1]`.

In its own tables the level is insensitive to the choice and the power is not.
At the 5% level the null rows of Table 4.1 (`h = 0.3`) run from 0.049 to 0.061
and those of Table 4.2 (`h = 0.5`) from 0.039 to 0.060. Against the monotone
alternatives (I) and (III) the power barely moves: 0.502 and 0.523 for (I)
with `c = 1` at 50 observations, 0.700 and 0.731 for (III) with `c = 0.5` at
100. Against the oscillating (II) it collapses at the larger bandwidth: 0.600
against 0.124 with `c = 0.5` at 100 observations, and 0.993 against 0.525 with
`c = 1` at 200. In Example 4.3 the power falls from 0.723 to 0.473 with
`c = 0.5` at 200 observations. A bandwidth wider than the scale on which the
variance departs from the family smooths the departure away.

The prototype's rule is `h = 0.3 (n / 100)^(-0.3)` times the range of the
variable. It gives the manuscript's `h = 0.3` at 100 observations on `[0, 1]`
and shrinks at the rate `n^(-0.3)`, inside the theory's window. It is a
scaling of the paper's choice, not a data-driven rule; block 2 of the study
halves and doubles the constant at 150 observations. As in the manuscript, the
level under constant variance does not move with it. Under a non-constant
variance the larger bandwidth makes the test more conservative still, and the
power against the wave falls as the bandwidth grows.

## 5. How it relates to `performVarianceFormTest()`

- **The same null hypothesis.** With one variance regressor, `form =
  "exponential"` is `sigma^2 = exp(a + b x)` and `form = "power"` is
  `sigma^2 = a x^b`, and these are the families the prototype tests. The `form`
  argument carries over unchanged.
- **A different estimate of the variance function.** `fitWLS()` and
  `performVarianceFormTest()` regress `log(e^2)` on the variance regressors,
  whose constant is shifted by `E[log eps^2]`. The test here compares the
  residuals scaled by the parametric fit with those scaled by a nonparametric
  one, so the level of the fit matters as well as its shape, and a shifted
  constant makes `F-hat_0` a rescaled `F-hat` under the null hypothesis. The
  prototype therefore fits by (2.11), least squares on the squared residuals,
  by Gauss-Newton from the log-regression start. On some samples that
  least-squares problem has no minimum: one large squared residual near an end
  of the range makes the fit a spike there. The paper's compact `Theta`
  excludes this; the prototype counts such samples as failures, and the README
  section says how often they occur. They are most frequent when the true
  variance is large at both ends of the range.
- **Different alternatives.** `performVarianceFormTest()` is a score test
  against the terms it adds, with power along them and little elsewhere. This
  test has power against any departure in the one variable that a smoother of
  bandwidth `h` can see, at the price of a bandwidth, a bootstrap, and one
  variable. The study adds a departure that the default added terms hardly
  span, to measure that difference.
- **Cost.** One test of the prototype with `B = 199` took a median of 0.19,
  0.22 and 0.31 seconds at 50, 150 and 400 observations (15 samples each, R
  4.5.1 on Windows 11); `B = 999` takes about five times as long.
  `performVarianceFormTest()` takes milliseconds.

**Where it belongs.** As `against = "any"`, the function would return an F
statistic with two degrees of freedom for one value of an argument and a
Cramér-von Mises statistic with a bootstrap p-value for another; it would carry
arguments that apply to one value only (the bandwidth, `B`, `v`, the kernel,
`parallel`), restrict `var_formula` to one variable for that value only, and
differ in running time by two orders of magnitude. A function of its own,
sharing `form`, `var_formula` and the handling of a `fitWLS()` fit with
`performVarianceFormTest()` and cross-referenced from its help page, keeps both
interfaces plain. That is the proposal; the choice is the author's.

## 6. What stage 1 found, and a recommendation

The figures are those of the section on this test in `README.md`, generated
from the three CSV files by `make-table.R`.

- **The manuscript's tables.** The prototype reproduces the level of Tables
  4.1 and 4.5 (34 of 35 null rates within the 99% interval of the difference)
  and their power to within a few points, with a sign that is consistent
  within each table, which points to a detail the manuscript does not state.
- **The level in the package's designs.** With Gaussian errors, at 5000
  replications, the Cramér-von Mises statistic is inside the release-gate
  interval in 4 of the 12 null cells: the cells with constant variance at 150
  and 400 observations. It rejects 0.026 to 0.034 of the time at 50
  observations and 0.019 to 0.036 whenever the true variance function is not
  constant. The Kolmogorov-Smirnov statistic is inside in none.
- **Why.** The bootstrap generates its data from the least-squares fit (2.11),
  and the statistic's null distribution depends on the variance function, so
  the error in the fit widens the bootstrap distribution. On the same samples
  and random numbers, generating the bootstrap data from the true shape of the
  variance function instead brings the rejection rate from 0.015 to 0.047 in
  the power cells and from 0.024 to 0.051 for the exponential form at 400
  observations (`variance-form-dnvk-bootstrap-check.R`); for the exponential
  form at 150 observations it moves only from 0.033 to 0.037, so the fit is
  not the whole story there. The true shape is not available in practice; the
  check locates the fault, it does not repair it.
- **Power.** Against the departure the default added terms do not span (the
  wave) the prototype does what a consistent test is for: 0.750 against 0.089
  for `performVarianceFormTest(var_formula = ~ x)` at 400 observations, with
  the exponential form. Against the two log-linear forms tested against each
  other and against the quadratic log-variance it has less power than the
  existing test on the same variable, by 2.0 to 65.8 percentage points, and
  part of that is its conservatism; against `sigma^2 = 0.5 + x`, which neither
  detects well, the two are within 4 points of each other.
- **Failures.** The fit (2.11) has no minimum in up to 5.2% of the samples of
  a cell, and in up to 0.9% of those of a null cell.

**Recommendation: do not ship the test as the manuscript defines it.** It fails
criterion 2 of the release gate in 8 of its 12 cells, and the failure is
structural rather than a matter of sample size: it does not shrink from 150 to
400 observations under a non-constant variance. Two ways forward, each a new
stage 1 rather than a stage 2:

1. A modified procedure: a better-behaved estimate of the variance function
   for the bootstrap data (weighted least squares on the squared residuals, or
   Gaussian pseudo-likelihood; the log-regression of `fitWLS()` gets the level
   of the variance wrong, see section 5), or a studentized statistic whose
   null distribution does not depend on the variance function. Either is a
   different test from the paper's and needs its own argument and study.
2. Wang and Zhou (2007), whose theory assumes a parametric mean, which is the
   package's setting, and allows several variance regressors.

The published article should be compared with the manuscript before either.

## References

Akritas, M. G. and Van Keilegom, I. (2001). Non-parametric estimation of the
residual distribution. *Scandinavian Journal of Statistics* 28(3), 549-567.
<https://doi.org/10.1111/1467-9469.00254>

Dette, H., Neumeyer, N. and Van Keilegom, I. (2007). A new test for the
parametric form of the variance function in non-parametric regression.
*Journal of the Royal Statistical Society: Series B* 69(5), 903-917.
<https://doi.org/10.1111/j.1467-9868.2007.00616.x> (not read)

Dette, H. and Van Keilegom, I. (2005). A new test for the parametric form of
the variance function in nonparametric regression. Technical report, TU
Dortmund, SFB 475. <https://doi.org/10.17877/DE290R-14489> (the submitted
manuscript, read)

Pardo-Fernández, J. C. and Jiménez-Gamero, M. D. (2019). A model specification
test for the variance function in nonparametric regression. *AStA Advances in
Statistical Analysis* 103(3), 387-410.
<https://doi.org/10.1007/s10182-018-00336-y> (read as arXiv:1807.08702v1)

Samarakoon, N. and Song, W. (2011). Minimum distance conditional variance
function checking in heteroscedastic regression models. *Journal of
Multivariate Analysis* 102(3), 579-600.
<https://doi.org/10.1016/j.jmva.2010.11.003> (abstract only)

Wang, L. and Zhou, X.-H. (2007). Assessing the adequacy of variance function in
heteroscedastic regression models. *Biometrics* 63(4), 1218-1225.
<https://doi.org/10.1111/j.1541-0420.2007.00805.x> (read as UW Biostatistics
Working Paper 299, 2006)
