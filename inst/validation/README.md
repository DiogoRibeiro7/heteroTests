# Statistical validation matrix

This directory holds the reproducible evidence behind the package's correctness
claims. A test is called **validated** only when it has been carried through all
five steps below.

    definition -> reference -> implementation -> numerical equivalence -> size/power

For each method that means checking the statistic itself, its degrees of
freedom, its null distribution, the direction of the alternative, the p-value,
the treatment of the intercept, and the handling of rank deficiency.

The work is split into three passes and a closing sweep:

- **Pass A — classical regression diagnostics.** Complete as of 0.7.0.
- **Pass B — group-variance tests.** Levene, Brown-Forsythe, Bartlett,
  Fligner-Killeen, Hartley F-max, O'Brien, modified Bartlett. Complete as of
  0.7.1.
- **Pass C — the remainder.** Cameron-Trivedi, ordered LM, Davidian-Carroll,
  Rice, Curry-Walsh, wild bootstrap, rank permutation, quantile regression,
  high-dimensional, spatial and panel diagnostics. Complete as of 0.7.2.
- **Full sweep — every exported test.** All 32 `perform*Test()` functions, each
  driven by the null and alternative appropriate to what it tests rather than a
  single process for all of them. Complete as of 0.11.0.
- **Weighted fits and the variance function.** Every test again, on a weighted
  fit, which no earlier pass had tried; and `performVarianceFormTest()`, whose
  null hypothesis is a variance function. Complete as of 0.12.0.

These lists name what each pass examined, not what the package still exports.
Six of the package's diagnostics were removed in 0.8.0, either because their
statistics could not detect heteroscedasticity or because they duplicated a
test that remains.

## Files

| File | Contents |
| --- | --- |
| `pass-a-size-power.R` | The Monte Carlo study. Run from the repository root: `Rscript inst/validation/pass-a-size-power.R [n_mc]`. |
| `pass-a-size-power.csv` | Its output, in long format, one row per test and scenario. |
| `full-sweep-size-power.R` | The sweep over every exported test. `Rscript inst/validation/full-sweep-size-power.R`, with `N_MC` overridable. |
| `full-sweep-size-power.csv` | Its output, one row per test, with the alternative each was driven against. |
| `weighted-fits-size.R` | The tests on weighted fits, with weights known and with weights estimated by `fitWLS()`. `Rscript inst/validation/weighted-fits-size.R`, with `N_MC` overridable. |
| `weighted-fits-size.csv` | Its output, one row per test, block, error distribution and sample size. |
| `variance-form-size-power.R` | `performVarianceFormTest()` for each form against five true variance functions. `Rscript inst/validation/variance-form-size-power.R`, with `N_MC` overridable. |
| `variance-form-size-power.csv` | Its output, one row per form, truth, error distribution and sample size. |
| `survey-designs-size.R` | The tests `runSurveyHeteroTests()` runs on the data of a survey design, beside three ways of using the design that were not adopted. `Rscript inst/validation/survey-designs-size.R`, with `N_MC` overridable. Needs `survey`. |
| `survey-designs-size.csv` | Its output, one row per test, procedure, design and sample size. |
| `glejser-skewed-errors.R` | `performGlejserTest()` with and without `robust = TRUE`, beside Koenker's test, under symmetric and skewed errors. `Rscript inst/validation/glejser-skewed-errors.R`, with `N_MC` and `MC_CORES` overridable. |
| `glejser-skewed-errors.csv` | Its output, one row per test, block, error distribution and sample size. |
| `make-table.R` | Renders the CSV as the Markdown tables below. |

Reference equivalence is asserted separately, and exactly, in
`tests/testthat/test-pass-a-reference.R`. That file is the authority on *what*
each test is supposed to compute; this directory is the authority on *how it
behaves*.

## Release gate

A test may be listed as validated only if:

1. it reproduces an established implementation, or an independent
   reconstruction of its primary reference, to within `1e-8`; and
2. its empirical size under the Gaussian null lies within Monte Carlo error of
   the nominal 5%; and
3. it has non-trivial power against at least one alternative it is designed to
   detect.

With `n_mc = 5000` the Monte Carlo standard error at the nominal level is
`sqrt(0.05 * 0.95 / 5000) = 0.0031`, so criterion 2 amounts to an approximate
99% interval of `[0.042, 0.058]`.

## Designs

Cross-sectional: `y = 1 + 2 x1 + 0.5 x2 + sigma_i e_i` with `x1 ~ U(1, 5)`
(positive, so the log and inverse transforms used by Park and Glejser are
defined) and `x2 ~ N(0, 1)`.

| Scenario | Variance |
| --- | --- |
| `size_gaussian` | `sigma_i^2 = 1`, Gaussian errors |
| `size_t5` | `sigma_i^2 = 1`, `t_5` errors scaled to unit variance |
| `power_exp` | `sigma_i^2 = exp(gamma x1)`, `gamma = 0.4` |
| `power_quad` | `sigma_i^2 = 1 + gamma x1^2`, `gamma = 0.15` |

Time-series: a mean-zero series fitted by `lm(v ~ 1)`, with the null being
i.i.d. Gaussian or `t_5` innovations, and the alternative an ARCH(1) process
with `alpha = 0.6`.

The `size_t5` column is a robustness probe rather than a pass/fail criterion.
Several of these tests are derived under normality and are expected to
over-reject under heavy tails; the column records which ones, so users can be
pointed to a robust alternative.

## Results

### Cross-sectional block

| Test | Size, Gaussian n=100 | Size, Gaussian n=40 | Size, t5 n=100 | Power, exp n=100 | Power, quad n=100 |
| --- | ---: | ---: | ---: | ---: | ---: |
| Breusch-Pagan | 0.041 | 0.045 | 0.219 | 0.774 | 0.703 |
| Koenker | 0.046 | 0.049 | 0.042 | 0.727 | 0.649 |
| White | 0.047 | 0.049 | 0.059 | 0.518 | 0.436 |
| Goldfeld-Quandt | 0.050 | 0.048 | 0.131 | 0.886 | 0.835 |
| Harvey | 0.052 | 0.057 | 0.070 | 0.424 | 0.354 |
| Harvey (studentized) | 0.051 | 0.057 | 0.052 | 0.423 | 0.356 |
| Park | 0.041 | 0.060 | 0.055 | 0.514 | 0.437 |
| Glejser | 0.046 | 0.056 | 0.053 | 0.814 | 0.743 |
| Szroeter | 0.045 | 0.048 | 0.126 | 0.915 | 0.870 |
| Cook-Weisberg | 0.044 | 0.047 | 0.173 | 0.830 | 0.772 |
| NCV | 0.044 | 0.047 | 0.173 | 0.830 | 0.772 |

### Time-series block

| Test | Size, Gaussian n=300 | Size, t5 n=300 | Power, ARCH(1) n=300 |
| --- | ---: | ---: | ---: |
| ARCH LM (q = 3) | 0.045 | 0.051 | 0.993 |
| McLeod-Li (m = 10) | 0.054 | 0.068 | 0.969 |

Replications: 5000. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0031.

## Reading the table

Under the Gaussian null every Pass A test sits within Monte Carlo error of the
nominal 5%, so all of them clear criterion 2 of the release gate. Two entries are
worth noting: Park reaches 0.060 at `n = 40`, marginally above the interval, which
is consistent with its auxiliary error being a strongly skewed `log(chi^2_1)`
variate in small samples; and Harvey is at the upper edge (0.057) at the same
sample size.

The `t5` column separates the tests derived under normality from those that are
not. Breusch-Pagan (0.219), Cook-Weisberg and NCV (0.173), Goldfeld-Quandt
(0.131) and Szroeter (0.126) all over-reject substantially when the errors are
heavy-tailed, because each relies on a null moment that holds only for Gaussian
errors. Koenker (0.042), Harvey with `studentize = TRUE` (0.052), Glejser
(0.053), Park (0.055) and White (0.059) hold their level. This is the basis for
the cross-references in the help pages: where a test is normality-dependent, its
documentation names the robust alternative.

On power, Szroeter and Goldfeld-Quandt lead against both alternatives, which is
expected since both exploit the ordering in `x1` that the alternatives are built
on. Harvey is the least powerful of the group here; its multiplicative variance
model is a poorer match for the additive `quad` alternative than the
Breusch-Pagan family.

### Group-variance block (Pass B)

| Test | gaussian_null_n30 | gaussian_null_n15 | t5_null_n30 | moderate_hetero | strong_hetero |
| --- | ---: | ---: | ---: | ---: | ---: |
| Levene | 0.057 | 0.055 | 0.054 | 0.723 | 0.941 |
| Brown-Forsythe | 0.043 | 0.027 | 0.038 | 0.677 | 0.922 |
| Bartlett | 0.050 | 0.047 | 0.242 | 0.792 | 0.974 |
| Fligner-Killeen | 0.045 | 0.024 | 0.038 | 0.633 | 0.894 |
| Hartley Fmax | 0.052 | 0.045 | 0.236 | 0.788 | 0.974 |
| O'Brien | 0.048 | 0.035 | 0.032 | 0.693 | 0.926 |

Replications: 5000. Nominal level: 0.05.

The table used to carry a seventh row, `Modified Bartlett alias`, whose five
figures were identical to Bartlett's in every digit: 0.050, 0.047, 0.242,
0.792, 0.974. That is what established `performModifiedBartlettTest()` as an
exact duplicate rather than a distinct correction, and it was removed in 0.8.0.
The row is gone from the table and the CSV because the function it called no
longer exists, so re-running the script could not reproduce it.

Every Pass B test holds its nominal level under the Gaussian null at n=30.
At n=15 that is no longer true of all of them: Brown-Forsythe (0.027) and
Fligner-Killeen (0.024) reject at about half the nominal rate, which is the
expected small-sample conservatism of median-centred and rank-based
statistics rather than a defect, but it is conservatism, not calibration.

The `t5` column separates the normal-theory tests from the robust ones, and
does so sharply: Bartlett and Hartley reject about 24% of the time
against a nominal 5% when the errors are heavy-tailed, while Levene,
Brown-Forsythe, Fligner-Killeen and O'Brien stay near 0.05. That is the basis
for the cross-references in their help pages.

On power, Bartlett and Hartley lead, which is what normal-theory tests buy
when their assumption holds; Fligner-Killeen pays the most for its robustness.

Before 0.7.1, `performOBrienTest()` rejected 100% of the time in every column,
including the null ones, and `performHartleyFmaxTest()` rejected about 35% of
the time under the null at four groups. See `NEWS.md`.

### Full sweep over every exported test

| Test | Alternative | Size | Size, t5 | Power |
| --- | --- | ---: | ---: | ---: |
| `performCookWeisbergTest()` | sd = x^2 | 0.072 | 0.205 | 0.980 |
| `performDavidianCarrollTest()` | sd = x^2 | 0.042 | 0.045 | 0.975 |
| `performHarveyTest()` | sd = x^2 | 0.058 | 0.060 | 1.000 |
| `performNCVTest()` | sd = x^2 | 0.072 | 0.205 | 0.980 |
| `performSpearmanTest()` | sd = x^2 | 0.070 | 0.032 | 0.980 |
| `performSpreadLevelTest()` | sd = x^2 | 0.055 | 0.042 | 0.975 |
| `performBPTest()` | sd = x^2 | 0.058 | 0.287 | 1.000 |
| `performBreuschPaganTest()` | sd = x^2 | 0.058 | 0.287 | 1.000 |
| `performKoenkerTest()` | sd = x^2 | 0.045 | 0.048 | 1.000 |
| `performStudentizedBPTest()` | sd = x^2 | 0.045 | 0.048 | 1.000 |
| `performWhiteTest()` | sd = x^2 | 0.032 | 0.048 | 1.000 |
| `performQuantileRegressionTest()` | sd = x^2 | 0.048 | 0.020 | 1.000 |
| `performHighDimensionalTest()` | sd = x^2 | 0.045 | 0.048 | 1.000 |
| `performRankPermutationTest()` | sd = x^2 | 0.050 | 0.048 | 1.000 |
| `performWildBootstrapTest()` | sd = x^2 | 0.052 | 0.042 | 1.000 |
| `performBartlettTest()` | sd = x^2 | 0.060 | 0.268 | 1.000 |
| `performBrownForsytheTest()` | sd = x^2 | 0.055 | 0.035 | 1.000 |
| `performFlignerKilleenTest()` | sd = x^2 | 0.048 | 0.022 | 1.000 |
| `performHartleyFmaxTest()` | sd = x^2 | 0.062 | 0.280 | 1.000 |
| `performLeveneTest()` | sd = x^2 | 0.068 | 0.048 | 1.000 |
| `performOBrienTest()` | sd = x^2 | 0.048 | 0.028 | 1.000 |
| `performBoxMTest()` | sd = x^2 | 0.012 (low) | 0.070 | 1.000 |
| `performGlejserTest()` | sd = x^2 | 0.060 | 0.058 | 1.000 |
| `performParkTest()` | sd = x^2 | 0.062 | 0.058 | 1.000 |
| `performGQTest()` | sd = x^2 | 0.082 | 0.152 | 1.000 |
| `performSzroeterTest()` | sd = x^2 | 0.058 | 0.163 | 1.000 |
| `performArchLMTest()` | ARCH(1), alpha = 0.6 | 0.048 | -- | 0.830 |
| `performMcLeodLiTest()` | ARCH(1), alpha = 0.6 | 0.040 | -- | 0.848 |
| `performRESETTest()` | omitted quadratic | 0.045 | -- | 1.000 |
| `performBPRandomEffectsTest()` | random intercepts | 0.030 | -- | 1.000 |
| `performPesaranTest()` | common time factor | 0.037 | -- | 1.000 |
| `performSpatialHeteroTest()` | variance clustered in space | 0.045 | -- | 0.362 |
| `performVarianceFormTest()` | log-variance quadratic in x | 0.058 | -- | 0.978 |

Replications: 400. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0109.

<!-- generated by make-table.R; do not edit the numbers by hand -->

The sweep covers 33 exported tests: 26 heteroscedasticity diagnostics and 7 with other nulls (arch, csdep, effect, form, spatial, variance_form). 25 of the 26 heteroscedasticity tests fall within three Monte Carlo standard errors of the nominal 0.05.

Outside that band:

- `performBoxMTest()`, size 0.012, 6.75 standard errors below nominal.

Under a homoscedastic t5 null, 16 of the 26 heteroscedasticity tests hold their level, meaning they land within three standard errors of nominal in either direction. These do not:

- `performBPTest()`, 0.287 against a nominal 0.05, over-rejecting.
- `performBreuschPaganTest()`, 0.287 against a nominal 0.05, over-rejecting.
- `performHartleyFmaxTest()`, 0.280 against a nominal 0.05, over-rejecting.
- `performBartlettTest()`, 0.268 against a nominal 0.05, over-rejecting.
- `performCookWeisbergTest()`, 0.205 against a nominal 0.05, over-rejecting.
- `performNCVTest()`, 0.205 against a nominal 0.05, over-rejecting.
- `performSzroeterTest()`, 0.163 against a nominal 0.05, over-rejecting.
- `performGQTest()`, 0.152 against a nominal 0.05, over-rejecting.
- `performFlignerKilleenTest()`, 0.022 against a nominal 0.05, under-rejecting.
- `performQuantileRegressionTest()`, 0.020 against a nominal 0.05, under-rejecting.

Inside the band but worth naming, at more than two standard errors:

- `performGQTest()`, size 0.082, 2.36 standard errors above nominal.

Each test is driven against its own alternative, so the power column compares
tests only within a family. The heteroscedasticity diagnostics face
`sd = x^2`; the ARCH pair faces an ARCH(1) process, which is a harder problem
at this sample size and is why their power sits near 0.84. The spatial test
faces error variance rising with distance from the centre of a 10 by 15 grid,
which holds the same 150 observations as the rest of the sweep, and reaches
0.36 against it.

The `Size, t5` column is a second homoscedastic null with \eqn{t_5} errors
scaled to unit variance, so only the tails differ. It applies to the
heteroscedasticity family alone; the other families test different things and a
heavy-tailed null would not isolate tail sensitivity for them.

That column is the one to read before choosing a test. The split it shows is
not a defect in any implementation -- it is the assumption each statistic
carries, made visible:

- The classical Breusch-Pagan statistic divides by \eqn{2\sigma^4}, which is
  the variance of \eqn{e_i^2} only under normality. At 0.287 it rejects a true
  null more than five times too often. `performKoenkerTest()` and
  `performStudentizedBPTest()` are the studentized form that exists to remove
  exactly this dependence, and they sit at 0.048.
- Bartlett's and Hartley's tests assume normal errors in the same way, at 0.268
  and 0.280. The median- and rank-based alternatives to them --
  `performBrownForsytheTest()`, `performFlignerKilleenTest()`,
  `performLeveneTest()`, `performOBrienTest()` -- stay between 0.022 and 0.048.
- `performCookWeisbergTest()` and `performNCVTest()` are the same score test
  and inherit the same normality assumption, at 0.205.

Two tests fail the column by being too conservative rather than too liberal:
`performFlignerKilleenTest()` at 0.022 and `performQuantileRegressionTest()` at
0.020. That costs power rather than validity, but the count above is two-sided,
so they are named alongside the over-rejecting ones rather than passed over.

So the heavy-tailed column mostly reproduces the textbook advice, from this
package's own implementations. Where it does not, prefer the measurement.

Two remarks on the tests the summary names under the Gaussian null.

- `performBoxMTest()` is conservative there. Box's M tests equality of
  covariance matrices rather than of scalar variances, so it is answering a
  broader question, and it is known to be sensitive to non-normality in the
  opposite direction. It is left as it is.
- `performGQTest()` is inside the band but at the high end, and the reason is
  the design rather than the implementation: `lmtest::gqtest()` gives 0.070 on
  the same samples, and with a single regressor both give 0.025. The package
  reproduces `lmtest::gqtest()` exactly at matched `fraction` -- the difference
  is zero at 0.1, 0.2 and 0.3 -- though it cannot reproduce that function's
  default, since `lmtest` omits no central observations and `performGQTest()`
  requires `fraction` strictly between 0 and 1.

Six of the swept tests are not heteroscedasticity diagnostics, and are included
because they are exported and were unvalidated: `performRESETTest()`
(functional form), `performBPRandomEffectsTest()` (presence of an individual
effect), `performPesaranTest()` (cross-sectional dependence),
`performSpatialHeteroTest()` (spatial clustering of variance) and the two
ARCH-type tests. Reading their power as power against heteroscedasticity would
be a mistake: the corrected `performBPRandomEffectsTest()` rejects at 3.2%
against `sd = x^2`, which is its nominal level rather than power.

A seventh, `performVarianceFormTest()`, is a heteroscedasticity diagnostic of a
different kind and has a family of its own. Its null hypothesis is a variance
function, so the data it is sized on are heteroscedastic: the log-variance is
linear in `x`, which is the exponential form it assumes by default. Its
alternative is a log-variance that is quadratic in `x`. It has its own study
below.

`size_effective` and `power_effective` in the CSV record how many replications
actually produced a p-value. They equal the requested count in this run; a
lower number would mean the corresponding rate rests on fewer samples than
`replications` suggests, and the script warns when that happens.

### Weighted fits

`lm(..., weights = w)` states that the error variance is `sigma^2 / w_i`. No
pass before 0.12.0 fitted a weighted model, and on one the tests read the raw
residuals, which are heteroscedastic by assumption. Three blocks follow,
because weights that are known and weights that were estimated are different
cases, and estimated weights can come from the wrong variance function.

**Known weights.** The cross-sectional design of the full sweep under its
alternative, `sd = x^2`, fitted with weights `1 / x^4`. The model is correctly
weighted, so the rejection rate is a size. The ARCH pair faces a standard
deviation that rises along the sample, with the weights that undo it.

| Test | Rejection rate |
| --- | ---: |
| `performCookWeisbergTest()` | 0.052 |
| `performDavidianCarrollTest()` | 0.070 |
| `performHarveyTest()` | 0.043 |
| `performNCVTest()` | 0.052 |
| `performSpearmanTest()` | 0.048 |
| `performSpreadLevelTest()` | 0.050 |
| `performBPTest()` | 0.033 |
| `performKoenkerTest()` | 0.030 |
| `performStudentizedBPTest()` | 0.030 |
| `performWhiteTest()` | 0.022 |
| `performWhiteTestRobust()` | 0.022 |
| `performBPTestRobust()` | 0.030 |
| `performWhiteTestStreaming()` | 0.022 |
| `performBPTestStreaming()` | 0.033 |
| `performKoenkerTestStreaming()` | 0.030 |
| `performHighDimensionalTest()` | 0.030 |
| `performRankPermutationTest()` | 0.052 |
| `performBartlettTest()` | 0.048 |
| `performBrownForsytheTest()` | 0.040 |
| `performFlignerKilleenTest()` | 0.050 |
| `performHartleyFmaxTest()` | 0.050 |
| `performLeveneTest()` | 0.050 |
| `performOBrienTest()` | 0.040 |
| `performGlejserTest()` | 0.048 |
| `performParkTest()` | 0.058 |
| `performGQTest()` | 0.045 |
| `performSzroeterTest()` | 0.077 |
| `performArchLMTest()` | 0.040 |
| `performMcLeodLiTest()` | 0.048 |
| `performRESETTest()` | 0.055 |
| `performBPRandomEffectsTest()` | 0.055 |
| `performPesaranTest()` | 0.072 |
| `performWildBootstrapTest()` | refused |
| `performWhiteTestBootstrap()` | refused |
| `performQuantileRegressionTest()` | refused |

Replications: 400. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0109.

The 27 heteroscedasticity diagnostics that accept a weighted fit reject between
0.022 and 0.077 of the time. Run against 0.11.2, this script gives 1.000 for
every one of the 27 that ran there, 0.723 and 0.797 for the ARCH pair, and
0.230 for RESET; `performStudentizedBPTest()`, `performBPTestRobust()` and
`performWildBootstrapTest()` could not run at all when the weights were a
column of the data.

The White family is conservative here at 0.022, more so than its 0.032 on an
unweighted fit in the full sweep, and Szroeter is at the high end at 0.077
against 0.058. Both are inside three standard errors of what the same tests do
without weights.

The three tests marked `refused` regenerate the response or refit under a
different loss, and refit without the weights. They stop with an error on a
weighted fit instead of testing a different model.

**Weights estimated by `fitWLS()`.** The log-variance is linear in `x`, so the
variance function `fitWLS()` fits by default is correctly specified, and a test
of the weighted fit should reject at its nominal level.

| Test | Gaussian, n = 150 | Gaussian, n = 600 | t5, n = 150 | t5, n = 600 |
| --- | ---: | ---: | ---: | ---: |
| `performKoenkerTest()` | 0.077 | 0.110 | 0.042 | 0.042 |
| `performBPTest()` | 0.106 | 0.118 | 0.243 | 0.304 |
| `performWhiteTest()` | 0.063 | 0.083 | 0.051 | 0.050 |
| `performNCVTest()` | 0.096 | 0.109 | 0.198 | 0.225 |
| `performHarveyTest()` | 0.000 | 0.000 | 0.002 | 0.000 |
| `performVarianceFormTest()` | 0.048 | 0.052 | 0.051 | 0.053 |

Replications: 2000. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0049.

It does not, for any of the tests of constant variance. Their reference
distributions take the weights as given, and here the weights were estimated
from the same residuals. The discrepancy grows with the sample rather than
shrinking: Koenker moves from 0.077 to 0.110 and White from 0.063 to 0.083
under Gaussian errors. Harvey's test never rejects, because it repeats the
regression of `log(e^2)` on the regressors that the weights were estimated
from. Under `t5` errors Koenker and White are near 0.05 in this design.
Nothing guarantees that: the size of the discrepancy depends on the error
distribution. Breusch-Pagan and the score test carry their usual heavy-tail
inflation on top.

`performVarianceFormTest()` keeps the regressors of the variance function in
its auxiliary regression and tests only the terms added to them, which removes
the effect of the estimation to first order. It is within Monte Carlo error of
0.05 in all four columns.

**The wrong variance function.** The variance is `x^3` and `fitWLS()` fits an
exponential. This is what a test of the weighted fit is usually asked to
catch.

| Test | n = 150 | n = 600 |
| --- | ---: | ---: |
| `performKoenkerTest()` | 0.083 | 0.069 |
| `performBPTest()` | 0.118 | 0.092 |
| `performWhiteTest()` | 0.177 | 0.879 |
| `performNCVTest()` | 0.082 | 0.056 |
| `performHarveyTest()` | 0.017 | 0.004 |
| `performVarianceFormTest()` | 0.214 | 0.929 |

Replications: 2000. Gaussian errors.

The tests that ask about the variance regressors themselves have almost
nothing left to find, because the estimation has fitted those directions:
Koenker rejects 0.069 at 600 observations, less than the 0.110 it rejects when
the form is right. White and the variance-form test look at squares and
products as well, and both find the misfit; only the second has a known level.

### The variance-form test

`performVarianceFormTest()` for each form it offers, against five true variance
functions. A cell is a null cell when the form under test nests the truth:
constant variance is nested in both. The model is `y ~ x + z`, the variance
depends on `x` alone, and the test is run with its defaults, so the variance
regressors are `x` and `z` and the added terms are their squares and product.

Gaussian errors:

| Form tested | True variance | Hypothesis | n = 50 | n = 150 | n = 400 |
| --- | --- | --- | ---: | ---: | ---: |
| exponential | `sigma^2 = 1` | null | 0.048 | 0.051 | 0.046 |
| exponential | `sigma^2 = exp(0.6 x)` | null | 0.048 | 0.054 | 0.052 |
| exponential | `sigma^2 = x^3` | alternative | 0.059 | 0.224 | 0.741 |
| exponential | `sigma^2 = 0.5 + x` | alternative | 0.043 | 0.054 | 0.066 |
| exponential | `sigma^2 = exp(0.5 (x - 3)^2)` | alternative | 0.517 | 0.980 | 1.000 |
| power | `sigma^2 = 1` | null | 0.057 | 0.052 | 0.054 |
| power | `sigma^2 = exp(0.6 x)` | alternative | 0.079 | 0.158 | 0.347 |
| power | `sigma^2 = x^3` | null | 0.044 | 0.045 | 0.046 |
| power | `sigma^2 = 0.5 + x` | alternative | 0.051 | 0.054 | 0.055 |
| power | `sigma^2 = exp(0.5 (x - 3)^2)` | alternative | 0.434 | 0.951 | 1.000 |

`t5` errors, scaled to unit variance:

| Form tested | True variance | Hypothesis | n = 50 | n = 150 | n = 400 |
| --- | --- | --- | ---: | ---: | ---: |
| exponential | `sigma^2 = 1` | null | 0.048 | 0.051 | 0.049 |
| exponential | `sigma^2 = exp(0.6 x)` | null | 0.052 | 0.047 | 0.048 |
| exponential | `sigma^2 = x^3` | alternative | 0.042 | 0.089 | 0.293 |
| exponential | `sigma^2 = 0.5 + x` | alternative | 0.044 | 0.049 | 0.044 |
| exponential | `sigma^2 = exp(0.5 (x - 3)^2)` | alternative | 0.361 | 0.803 | 0.982 |
| power | `sigma^2 = 1` | null | 0.057 | 0.057 | 0.054 |
| power | `sigma^2 = exp(0.6 x)` | alternative | 0.076 | 0.121 | 0.188 |
| power | `sigma^2 = x^3` | null | 0.042 | 0.049 | 0.051 |
| power | `sigma^2 = 0.5 + x` | alternative | 0.056 | 0.053 | 0.052 |
| power | `sigma^2 = exp(0.5 (x - 3)^2)` | alternative | 0.304 | 0.717 | 0.971 |

Replications: 5000. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0031.

<!-- generated by make-table.R; do not edit the numbers by hand -->

12 of the 12 null cells with Gaussian errors, and 12 of the 12 with t5 errors, fall inside the release-gate interval [0.042, 0.058]. Across all 24 null cells the rejection rate runs from 0.042 to 0.057.

Against a log-variance that is quadratic in `x`, which neither form can
follow, the test has power 0.98 at 150 observations for the exponential form
and 0.95 for the power form. Telling the two log-linear forms from each other
is harder, since `0.6 x` and `3 log(x)` are not far apart on `(1, 5)`: 0.224
and 0.158 at 150 observations, 0.741 and 0.347 at 400.

The additive truth, `sigma^2 = 0.5 + x`, is the limiting case. Over this range
it is close to both forms, and the test rejects it between 0.043 and 0.066 of
the time. That is not a defect of the statistic. Two variance functions that
the data cannot separate will both be accepted, and a non-rejection is not
evidence for the form.

Heavy tails cost power, 0.293 against 0.741 for the exponential form at 400
observations, without moving the level.

### Survey designs

`runSurveyHeteroTests()` takes the data out of a survey design, fits the model
by ordinary least squares and runs the ordinary tests. It does not use the
sampling weights, strata or clusters. Its help page said otherwise up to
0.12.0; the weights never reached the fit. This study says when the function
holds its level as it is, and why none of the three ways of bringing the
design in replaced it.

The population model is `y = 1 + 2 x + e` with `x` standard normal. The error
variance is constant in every design but the last, so every column but
**Power** is a size.

| Design | Sampling |
| --- | --- |
| Ignorable | The chance of selection depends on `x` alone, as `exp(0.4 x)`. Weights vary about 5 to 1. |
| Ignorable, strong | The same with `exp(0.9 x)`. Weights vary about 36 to 1. |
| Stratified | Three strata cut on `x`, sampled at 15%, 35% and 50% of the sample, with finite-population corrections. |
| Informative | Units with `x > 0` and `abs(e) > 1` are four times as likely to be selected. The errors are homoscedastic in the population and not in the sample. |
| Clustered | Clusters of ten with equal weights, a cluster-level component in `x`, and a cluster effect in the error whose scale varies between clusters. |
| Power | The ignorable design with `sd(e) = exp(0.2 x)`. |

The first row of each table is the function, called as a user would call it.
The second is the same test on `lm(weights = w)` with the sampling weights,
which reads the residuals multiplied by `sqrt(w)`; a test applied to a
`survey::svyglm()` fit computes the same statistic. The last two are not in
the package. Both regress the squared residuals of the `svyglm()` fit on the
variance regressors using the design: the Wald version is
`survey::regTermTest()` on that regression, and the score version estimates
the variance of the weighted score from the design under the null.

**Koenker regressors, n = 300.**

| Procedure | Ignorable | Ignorable, strong | Stratified | Informative | Clustered | Power |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `runSurveyHeteroTests()` | 0.047 | 0.053 | 0.048 | 0.830 | 0.371 | 0.997 |
| Sampling weights as precision weights | 0.988 | 1.000 | 0.998 | 0.930 | 0.371 | 0.062 |
| Design-based Wald test (not in the package) | 0.077 | 0.177 | 0.085 | 0.073 | 0.037 | 0.999 |
| Design-based score test (not in the package) | 0.068 | 0.130 | 0.084 | 0.075 | 0.028 | 0.999 |

**Koenker regressors, n = 1000.**

| Procedure | Ignorable | Ignorable, strong | Stratified | Informative | Clustered | Power |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `runSurveyHeteroTests()` | 0.045 | 0.057 | 0.046 | 1.000 | 0.428 | 1.000 |
| Sampling weights as precision weights | 1.000 | 1.000 | 1.000 | 1.000 | 0.428 | 0.086 |
| Design-based Wald test (not in the package) | 0.068 | 0.102 | 0.076 | 0.058 | 0.029 | 1.000 |
| Design-based score test (not in the package) | 0.064 | 0.089 | 0.076 | 0.056 | 0.026 | 1.000 |

**White regressors, n = 300.**

| Procedure | Ignorable | Ignorable, strong | Stratified | Informative | Clustered | Power |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `runSurveyHeteroTests()` | 0.046 | 0.049 | 0.043 | 0.772 | 0.488 | 0.981 |
| Sampling weights as precision weights | 0.970 | 1.000 | 0.984 | 0.888 | 0.488 | 0.082 |
| Design-based Wald test (not in the package) | 0.139 | 0.254 | 0.166 | 0.102 | 0.077 | 0.991 |
| Design-based score test (not in the package) | 0.093 | 0.097 | 0.137 | 0.089 | 0.017 | 0.986 |

**White regressors, n = 1000.**

| Procedure | Ignorable | Ignorable, strong | Stratified | Informative | Clustered | Power |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `runSurveyHeteroTests()` | 0.047 | 0.053 | 0.051 | 0.999 | 0.595 | 1.000 |
| Sampling weights as precision weights | 1.000 | 1.000 | 1.000 | 1.000 | 0.595 | 0.120 |
| Design-based Wald test (not in the package) | 0.080 | 0.172 | 0.120 | 0.077 | 0.062 | 1.000 |
| Design-based score test (not in the package) | 0.067 | 0.095 | 0.112 | 0.069 | 0.034 | 1.000 |

Replications: 2000. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0049.

Reading the tables:

- **The function holds its level when the design is ignorable given the
  regressors.** Over the twelve cells of the first three designs it rejects
  between 0.043 and 0.057 of the time, all inside the 99% band around 0.05 at
  2000 replications, [0.037, 0.063]. Unequal weights do not matter by
  themselves, however unequal, as long as selection depends only on variables
  in the model.
- **It does not when selection depends on the response or the sample is
  clustered.** It rejects a model with constant error variance 0.772 to 1.000
  of the time under informative sampling and 0.371 to 0.595 of the time in the
  clustered design, and more often in the larger sample. The function warns
  about unequal weights and about clusters for this reason; it cannot tell
  whether unequal weights are informative.
- **Sampling weights are not precision weights.** Treated as such they make
  the test reject a homoscedastic model 0.970 to 1.000 of the time in the
  first three designs, because the residuals it reads are multiplied by
  `sqrt(w)` and `w` varies with `x`. In the power column the same test rejects
  0.062 to 0.120 of the time, where the unweighted one rejects 0.981 to 1.000:
  the weights fall with `x` while the variance rises, and the two nearly
  cancel. This is what `runHeteroTests(formula, design)` computed up to
  0.12.0, through a `svyglm()` fit, and why such fits are now refused.
- **The design-based tests do what they are for and still reject too often.**
  They are the only rows that come near the nominal level under informative sampling
  (0.056 to 0.102) and in the clustered design (0.017 to 0.077). In the first
  three designs the Wald test rejects 0.068 to 0.254 of the time and the score
  test 0.064 to 0.137, and of their forty size cells three fall inside the
  band. Their variance estimate makes no use of the fact that the squared
  errors have a common variance under the null, which the ordinary tests rely
  on, and where the weights vary a few observations carry most of it. Neither
  was adopted. A design-based test needs a small-sample correction before it
  can clear the release gate.

### The Glejser test under asymmetric errors

Glejser's statistic regresses the absolute residuals on a transformation of a
regressor. An absolute residual differs from the absolute error by
`sign(e) x'(b - beta)`, and the mean of that term is zero only when positive
and negative errors are equally likely (Godfrey 1996). `robust = TRUE`
regresses `abs(e) - m e` instead, where `m` is the proportion of positive
residuals less the proportion of negative ones (Im 2000; Machado and Santos
Silva 2000).

The design is the cross-sectional one of Pass A,
`y = 1 + 2 x1 + 0.5 x2 + sigma e`, with `x1` uniform on (1, 5) and the test
applied to `x1`. The errors have mean 0 and variance 1 in every row. Their
skewness is in the second column: a chi-squared with 5 degrees of freedom, an
exponential, the same exponential with its sign changed, and a lognormal, all
centred.

Koenker's test is in the tables twice, because the help page of
`performGlejserTest()` used to send the reader to it when the errors are
skewed. `performKoenkerTest()` is the call that reader makes, and it tests both
regressors of the mean equation. "Koenker's statistic on the same regressor"
is `n R^2` of the squared residuals on the one regressor the Glejser test is
given, with one degree of freedom. The package has no call for it and the
script computes it. That row is the like-for-like comparison.

**Glejser test, size by error distribution.**

| Errors | Skewness | Test | n = 50 | n = 150 | n = 400 | n = 1000 |
| --- | ---: | --- | ---: | ---: | ---: | ---: |
| Gaussian | 0 | `performGlejserTest()` | 0.054 | 0.049 | 0.045 | 0.050 |
| Gaussian | 0 | `performGlejserTest(robust = TRUE)` | 0.049 | 0.047 | 0.044 | 0.050 |
| Gaussian | 0 | Koenker's statistic on the same regressor | 0.046 | 0.048 | 0.048 | 0.051 |
| Gaussian | 0 | `performKoenkerTest()` | 0.046 | 0.053 | 0.048 | 0.049 |
| t5 | 0 | `performGlejserTest()` | 0.054 | 0.050 | 0.053 | 0.055 |
| t5 | 0 | `performGlejserTest(robust = TRUE)` | 0.046 | 0.048 | 0.053 | 0.054 |
| t5 | 0 | Koenker's statistic on the same regressor | 0.048 | 0.046 | 0.049 | 0.053 |
| t5 | 0 | `performKoenkerTest()` | 0.045 | 0.048 | 0.051 | 0.050 |
| chi-squared(5) | 1.26 | `performGlejserTest()` | 0.094 | 0.096 | 0.089 | 0.088 |
| chi-squared(5) | 1.26 | `performGlejserTest(robust = TRUE)` | 0.057 | 0.056 | 0.048 | 0.047 |
| chi-squared(5) | 1.26 | Koenker's statistic on the same regressor | 0.058 | 0.060 | 0.053 | 0.050 |
| chi-squared(5) | 1.26 | `performKoenkerTest()` | 0.058 | 0.058 | 0.052 | 0.050 |
| exponential | 2 | `performGlejserTest()` | 0.133 | 0.134 | 0.132 | 0.123 |
| exponential | 2 | `performGlejserTest(robust = TRUE)` | 0.059 | 0.054 | 0.049 | 0.046 |
| exponential | 2 | Koenker's statistic on the same regressor | 0.061 | 0.049 | 0.045 | 0.046 |
| exponential | 2 | `performKoenkerTest()` | 0.063 | 0.055 | 0.046 | 0.049 |
| exponential, mirrored | -2 | `performGlejserTest()` | 0.133 | 0.134 | 0.139 | 0.126 |
| exponential, mirrored | -2 | `performGlejserTest(robust = TRUE)` | 0.058 | 0.057 | 0.046 | 0.047 |
| exponential, mirrored | -2 | Koenker's statistic on the same regressor | 0.059 | 0.051 | 0.044 | 0.046 |
| exponential, mirrored | -2 | `performKoenkerTest()` | 0.058 | 0.051 | 0.044 | 0.046 |
| lognormal | 6.18 | `performGlejserTest()` | 0.166 | 0.177 | 0.169 | 0.174 |
| lognormal | 6.18 | `performGlejserTest(robust = TRUE)` | 0.057 | 0.054 | 0.047 | 0.051 |
| lognormal | 6.18 | Koenker's statistic on the same regressor | 0.048 | 0.041 | 0.039 | 0.045 |
| lognormal | 6.18 | `performKoenkerTest()` | 0.054 | 0.046 | 0.047 | 0.044 |

**Glejser test, size with `transformation = "inverse"`.**

| Errors | Skewness | Test | n = 50 | n = 400 |
| --- | ---: | --- | ---: | ---: |
| Gaussian | 0 | `performGlejserTest()` | 0.055 | 0.053 |
| Gaussian | 0 | `performGlejserTest(robust = TRUE)` | 0.049 | 0.053 |
| Gaussian | 0 | Koenker's statistic on the same regressor | 0.049 | 0.053 |
| Gaussian | 0 | `performKoenkerTest()` | 0.048 | 0.057 |
| exponential | 2 | `performGlejserTest()` | 0.118 | 0.120 |
| exponential | 2 | `performGlejserTest(robust = TRUE)` | 0.063 | 0.048 |
| exponential | 2 | Koenker's statistic on the same regressor | 0.061 | 0.048 |
| exponential | 2 | `performKoenkerTest()` | 0.062 | 0.053 |
| lognormal | 6.18 | `performGlejserTest()` | 0.155 | 0.158 |
| lognormal | 6.18 | `performGlejserTest(robust = TRUE)` | 0.066 | 0.053 |
| lognormal | 6.18 | Koenker's statistic on the same regressor | 0.071 | 0.054 |
| lognormal | 6.18 | `performKoenkerTest()` | 0.066 | 0.043 |

**Glejser test, size after a correctly weighted fit.**

| Errors | Skewness | Test | n = 150 | n = 400 |
| --- | ---: | --- | ---: | ---: |
| Gaussian | 0 | `performGlejserTest()` | 0.051 | 0.058 |
| Gaussian | 0 | `performGlejserTest(robust = TRUE)` | 0.050 | 0.057 |
| Gaussian | 0 | Koenker's statistic on the same regressor | 0.051 | 0.053 |
| Gaussian | 0 | `performKoenkerTest()` | 0.053 | 0.050 |
| exponential | 2 | `performGlejserTest()` | 0.119 | 0.116 |
| exponential | 2 | `performGlejserTest(robust = TRUE)` | 0.053 | 0.052 |
| exponential | 2 | Koenker's statistic on the same regressor | 0.051 | 0.048 |
| exponential | 2 | `performKoenkerTest()` | 0.052 | 0.057 |
| lognormal | 6.18 | `performGlejserTest()` | 0.153 | 0.148 |
| lognormal | 6.18 | `performGlejserTest(robust = TRUE)` | 0.054 | 0.049 |
| lognormal | 6.18 | Koenker's statistic on the same regressor | 0.044 | 0.039 |
| lognormal | 6.18 | `performKoenkerTest()` | 0.043 | 0.043 |

**Glejser test, power.**

| Errors | Skewness | gamma | Test | n = 50 | n = 150 |
| --- | ---: | ---: | --- | ---: | ---: |
| Gaussian | 0 | 0.2 | `performGlejserTest()` | 0.163 | 0.441 |
| Gaussian | 0 | 0.2 | `performGlejserTest(robust = TRUE)` | 0.152 | 0.434 |
| Gaussian | 0 | 0.2 | Koenker's statistic on the same regressor | 0.167 | 0.478 |
| Gaussian | 0 | 0.2 | `performKoenkerTest()` | 0.118 | 0.366 |
| Gaussian | 0 | 0.4 | `performGlejserTest()` | 0.476 | 0.941 |
| Gaussian | 0 | 0.4 | `performGlejserTest(robust = TRUE)` | 0.456 | 0.940 |
| Gaussian | 0 | 0.4 | Koenker's statistic on the same regressor | 0.485 | 0.953 |
| Gaussian | 0 | 0.4 | `performKoenkerTest()` | 0.346 | 0.900 |
| exponential | 2 | 0.2 | `performGlejserTest()` | 0.203 | 0.365 |
| exponential | 2 | 0.2 | `performGlejserTest(robust = TRUE)` | 0.105 | 0.209 |
| exponential | 2 | 0.2 | Koenker's statistic on the same regressor | 0.096 | 0.179 |
| exponential | 2 | 0.2 | `performKoenkerTest()` | 0.083 | 0.135 |
| exponential | 2 | 0.4 | `performGlejserTest()` | 0.409 | 0.778 |
| exponential | 2 | 0.4 | `performGlejserTest(robust = TRUE)` | 0.251 | 0.622 |
| exponential | 2 | 0.4 | Koenker's statistic on the same regressor | 0.221 | 0.520 |
| exponential | 2 | 0.4 | `performKoenkerTest()` | 0.167 | 0.398 |

Replications: 5000. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0031.

<!-- generated by make-table.R; do not edit the numbers by hand -->

Under symmetric errors the default statistic rejects 0.045 to 0.058 of the time and the corrected one 0.044 to 0.057. Under asymmetric errors the default rejects 0.088 to 0.177 and the corrected one 0.046 to 0.066. Of the 36 null cells of the corrected statistic, 32 fall inside the release-gate interval [0.0421, 0.0579]. Outside it:

- exponential errors, n = 50, size: 0.0592.
- exponential, mirrored errors, n = 50, size: 0.0580.
- exponential errors, n = 50, size, regressor outside the mean equation: 0.0626.
- lognormal errors, n = 50, size, regressor outside the mean equation: 0.0658.

Five things follow.

- **The default statistic is not a 5% test when the errors are skewed.** With
  `x1` as the auxiliary regressor it rejects 0.088 to 0.096 of the time at a
  skewness of 1.26, 0.123 to 0.139 at a skewness of 2 in either direction, and
  0.166 to 0.177 for the lognormal. With `1 / x1`, and after a weighted fit,
  the rates are 0.116 to 0.120 for the exponential and 0.148 to 0.158 for the
  lognormal. At 1000 observations each rate is within about one percentage
  point of its value at 50. The term the statistic ignores is of the same
  order as the statistic, so the excess is not a small-sample effect.
- **The excess is the one the asymptotic theory gives.** The variance of the
  numerator of the slope is
  `Var(z) {Var|u| + rho^2 [m^2 Var(u) - 2 m E(u|u|)]}`, with `m = E sign(u)`
  and `rho^2` the `R^2` of the auxiliary regressor on the regressors of the
  mean equation, and the t statistic assumes `Var(z) Var|u|`. The table below
  puts the size this implies beside the simulated one. It also shows what the
  excess depends on: the shape of the errors and how far the auxiliary
  regressor is explained by the mean equation, 1 for `x1` and 0.844 for
  `1 / x1`. Under other error distributions the bracket can be negative, and
  the default statistic then rejects too seldom.
- **The corrected statistic holds its level from 150 observations.** All 27
  of its null cells at 150 observations or more are inside the release-gate
  interval, with `x1` or `1 / x1` as the auxiliary regressor and after a
  weighted fit. At 50 observations four of its nine cells are above the
  interval, at 0.058 to 0.066, all with skewed errors. In the same four cells
  Koenker's statistic on the same regressor is at 0.059 to 0.071 and
  `performKoenkerTest()` at 0.058 to 0.066.
- **Under symmetric errors the correction costs little, and it is not free.**
  The corrected statistic rejects less often than the default in ten of the
  twelve symmetric null cells and as often in the other two: by 0.5 to 0.7
  percentage points at 50 observations and by 0.2 or less from 150. With
  Gaussian errors it loses 1.1 and 1.9 points of power at 50 observations and
  under one point at 150.
- **Against Koenker's statistic on the same regressor the ranking depends on
  the errors.** With Gaussian errors Koenker's statistic has 1.3 to 4.4 points
  more power than the corrected Glejser statistic. With exponential errors the
  corrected Glejser statistic has 0.9 to 10.3 points more. `performKoenkerTest()`
  is below both in every power cell, because it spends a degree of freedom on
  `x2`, which the variance does not depend on. The rejection rates of the
  default Glejser statistic under exponential errors are not powers, because
  its size there is 0.13.

**Glejser test, size of the default statistic against its asymptotic value.**

| Errors | Regressor | Asymptotic | Simulated |
| --- | --- | ---: | ---: |
| chi-squared(5) | `x1` | 0.090 | 0.088 to 0.096 |
| exponential | `x1` | 0.132 | 0.123 to 0.134 |
| exponential | `1 / x1` | 0.120 | 0.118 to 0.120 |
| lognormal | `x1` | 0.170 | 0.166 to 0.177 |
| lognormal | `1 / x1` | 0.153 | 0.155 to 0.158 |

The default of `robust` is `FALSE`, so `performGlejserTest()` returns what it
returned in every earlier release. Whether the default should change is listed
under *Decisions needed* in `ROADMAP.md`.

## History

Before 0.7.0, `Szroeter` rejected in 0.0% of samples in every one of these
scenarios, including all the power columns. The full before-and-after is in
`NEWS.md`.

Before 0.12.0, every heteroscedasticity diagnostic rejected a correctly
weighted fit in 100% of samples. The weighted-fits tables above are the
after; the before is the same script run against 0.11.2.

Up to 0.12.0, `runHeteroTests(formula, design)` tested a `survey::svyglm()`
fit and so read the sampling weights as precision weights. The second row of
each survey table is that statistic.

The variance-form study once had a third form, an additive variance function
`sigma^2 = a + z'b`, estimated by regressing the squared residuals on the
regressors. It was withdrawn before 0.12.0 was released and its rows are gone
from the table and the CSV, because the code that produced them no longer
exists. Under its own null, `sigma^2 = 0.5 + x` with Gaussian errors, it
rejected 0.038 of the time at 50 observations, 0.061 at 150 and 0.061 at 400,
the last two outside the release gate. Its fitted variances were not all
positive, so that no test could be computed, in 17.3% of those samples at 50
observations, 5.9% at 150 and 0.7% at 400, and in 62% to 100% of samples when
the truth was exponential or a power.
