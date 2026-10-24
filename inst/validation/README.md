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
| `weighted-fits-size.R` | The tests on weighted fits, with weights known and with weights estimated by `fitWLS()`, and the resampling tests with weights exact, true, estimated and too weak. `Rscript inst/validation/weighted-fits-size.R`, with `N_MC` and `MC_CORES` overridable. |
| `weighted-fits-size.csv` | Its output, one row per test, block, error distribution and sample size. |
| `weighted-fits-refit-reproduction.R` | The rows of `weighted-fits-size.R` whose weights are estimated again in each replicate, measured by a prototype in 0.12.0, against the package on the same samples. `Rscript inst/validation/weighted-fits-refit-reproduction.R`, with `STEP`, `N_MC`, `N_BOOT`, `CELLS`, `PART`, `MERGE` and `CSV` overridable. Reads the prototype and the design from `weighted-fits-size.R`. |
| `weighted-fits-refit-reproduction.csv` | Its output, one row per cell, with the samples checked and how many agree. |
| `offset-refits-size.R` | The three resampling tests on an unweighted fit with an offset given as an argument, as corrected and as they computed it before, and RESET on the same fit. `Rscript inst/validation/offset-refits-size.R`, with `N_MC`, `N_MC_BEFORE` and `MC_CORES` overridable. |
| `offset-refits-size.csv` | Its output, one row per test, version, terms and order of the rows. |
| `white-bootstrap-transformed-size.R` | `performWhiteTestBootstrap()` on a model of `log(y)`: its size, and its power against a variance that grows with a regressor. `Rscript inst/validation/white-bootstrap-transformed-size.R`, with `N_MC`, `N_MC_POWER`, `MC_CORES` and `MIN_FREE_GB` overridable. |
| `white-bootstrap-transformed-size.csv` | Its output, one row per cell. |
| `variance-form-size-power.R` | `performVarianceFormTest()` for each form against five true variance functions. `Rscript inst/validation/variance-form-size-power.R`, with `N_MC` overridable. |
| `variance-form-size-power.csv` | Its output, one row per form, truth, error distribution and sample size. |
| `survey-designs-size.R` | The tests `runSurveyHeteroTests()` runs on the data of a survey design, beside three ways of using the design that were not adopted. `Rscript inst/validation/survey-designs-size.R`, with `N_MC` overridable. Needs `survey`. |
| `survey-designs-size.csv` | Its output, one row per test, procedure, design and sample size. |
| `glejser-skewed-errors.R` | `performGlejserTest()` with `robust = FALSE`, Glejser's statistic, and with `robust = TRUE`, the default since 0.15.0, beside Koenker's test, under symmetric and skewed errors. `Rscript inst/validation/glejser-skewed-errors.R`, with `N_MC` and `MC_CORES` overridable. |
| `glejser-skewed-errors.csv` | Its output, one row per test, block, error distribution and sample size. |
| `modified-wald-size-power.R` | The modified Wald test for groupwise heteroscedasticity of Stata's `xttest3`, which the package does not export, on fixed-effects panels. `Rscript inst/validation/modified-wald-size-power.R`, with `N_MC` and `MC_CORES` overridable. Checks the statistic against the example in Baum (2001) when `plm` is installed. |
| `modified-wald-size-power.csv` | Its output, one row per block, reference distribution, error distribution and panel shape. |
| `harrison-mccabe-size-power.R` | `performHarrisonMcCabeTest()` beside the Goldfeld-Quandt and Szroeter tests on the same ordering: its p-value on samples drawn given the design, its size under Gaussian and t5 errors and after a weighted fit, and its power. `Rscript inst/validation/harrison-mccabe-size-power.R`, with `N_MC`, `N_NULL` and `MC_CORES` overridable. |
| `harrison-mccabe-size-power.csv` | Its output, one row per test, block, error distribution, sample size and level. |
| `variance-form-dnvk-design.md` | Design note for the test of Dette, Neumeyer and Van Keilegom (2007), which the package does not export: the statistic, its scope, the bandwidth, and its relation to `performVarianceFormTest()`. |
| `variance-form-dnvk-prototype.R` | A prototype of that test in plain R, sourced by the next two scripts. It defines functions only. |
| `variance-form-dnvk-reproduction.R` | Tables 4.1 and 4.5 of the authors' submitted manuscript, reproduced with the prototype. `Rscript inst/validation/variance-form-dnvk-reproduction.R`, with `N_MC`, `MC_CORES`, `SCENARIOS` and `CSV` overridable. Does not load the package. |
| `variance-form-dnvk-reproduction.csv` | Its output, one row per variant, model, `c`, sample size and level, with the manuscript's rate beside each. |
| `variance-form-dnvk-size-power.R` | The prototype in the designs of `variance-form-size-power.R`, beside `performVarianceFormTest()`. `Rscript inst/validation/variance-form-dnvk-size-power.R`, with `N_MC`, `N_MC_GATE`, `N_BOOT`, `MC_CORES`, `SCENARIOS` and `CSV` overridable. |
| `variance-form-dnvk-size-power.csv` | Its output, one row per test, block, bandwidth, form, truth, error distribution and sample size. |
| `variance-form-dnvk-bootstrap-check.R` | The null cells of that study bootstrapped twice on the same samples, from the fitted and from the true variance function. `Rscript inst/validation/variance-form-dnvk-bootstrap-check.R`, with `N_MC`, `N_BOOT`, `MC_CORES`, `SCENARIOS` and `CSV` overridable. Does not load the package. |
| `variance-form-dnvk-bootstrap-check.csv` | Its output, one row per form, truth, sample size and source of the bootstrap data, with the mean fitted slope and the mean statistics. |
| `wild-multiplier-size.R` | `performWildBootstrapTest()` with Rademacher and with Mammen multipliers on the same samples: size under Gaussian, `t5` and skewed errors at 30, 50 and 100 observations, and power. `Rscript inst/validation/wild-multiplier-size.R`, with `N_MC`, `N_MC_POWER`, `N_BOOT`, `CHUNK_SIZE`, `MC_CORES`, `UNITS`, `PART`, `MERGE` and `CSV` overridable. |
| `wild-multiplier-size.csv` | Its output, one row per multiplier, block, error distribution and sample size, with the counts of samples only that multiplier rejected. |
| `fixed-effects-size.R` | White's, Koenker's and the Breusch-Pagan tests on `lm(y ~ x + subject)` with G subjects of T observations: their size on a grid of G and T, and on unbalanced groups, two regressors and a regressor correlated with the subject effects, each checked against the package. `Rscript inst/validation/fixed-effects-size.R`, with `N_MC`, `N_MC_LARGE`, `CHECK_REPS`, `MC_CORES`, `MIN_FREE_GB`, `CSV` and `RDS` overridable. |
| `fixed-effects-size.csv` | Its output with `N_MC = 2000` and `CHECK_REPS = 5`, one row per block, design and test, with the rates at 1, 5 and 10%, the mean and variance of the statistic, and the check against the package. |
| `fixed-effects-size-10000.csv` | The same with `N_MC = 10000` and `CHECK_REPS = 3`, on the same seeds. |
| `analyse-fixed-effects-size.R` | Renders the two CSVs as Markdown tables and searches for a rule on G and T that separates the designs whose size is within 2.5% to 7.5% from the others. `Rscript inst/validation/analyse-fixed-effects-size.R [results.csv [sensitivity.csv]]`. Does not load the package. |
| `fixed-effects-dummies.R` | Koenker's and the Breusch-Pagan statistics of those designs split into the regression on the subject dummies and the regression on `x` alone. `Rscript inst/validation/fixed-effects-dummies.R`, with `N_MC` and `CSV` overridable. Does not load the package. |
| `fixed-effects-dummies.csv` | Its output, one row per design. |
| `fixed-effects-variants-size.R` | The other routes to the three statistics on two of those designs: the robust and streaming forms, the deprecated names, the registered names, and the White bootstrap, the wild bootstrap and `bootstrapTestStatistic()`, with the warnings each call gives. `N_MC_BOOT_LARGE=1000 Rscript inst/validation/fixed-effects-variants-size.R`, with `N_MC`, `N_MC_BOOT_LARGE`, `CHUNK_SIZE`, `N_BOOT`, `MC_CORES`, `UNITS`, `PART`, `MERGE`, `LIST` and `CSV` overridable. |
| `fixed-effects-variants-size.csv` | Its output, one row per design and route, with the rates at 1, 5 and 10%, the warnings per call and the refusals. |
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
| Glejser (`robust = FALSE`) | 0.046 | 0.056 | 0.053 | 0.814 | 0.743 |
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

The Glejser row is Glejser's (1969) statistic,
`performGlejserTest(robust = FALSE)`, which was the default up to 0.14.0. The
default since 0.15.0, `robust = TRUE`, corrects the absolute residuals for
asymmetric errors. It is measured in the same design, at 50 to 1000
observations, in *The Glejser test under asymmetric errors* below, and is not
in this table.

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
| `performGlejserTest()` | sd = x^2 | 0.058 | 0.055 | 1.000 |
| `performParkTest()` | sd = x^2 | 0.062 | 0.058 | 1.000 |
| `performGQTest()` | sd = x^2 | 0.082 | 0.152 | 1.000 |
| `performSzroeterTest()` | sd = x^2 | 0.058 | 0.163 | 1.000 |
| `performArchLMTest()` | ARCH(1), alpha = 0.6 | 0.048 | -- | 0.830 |
| `performMcLeodLiTest()` | ARCH(1), alpha = 0.6 | 0.040 | -- | 0.848 |
| `performRESETTest()` | omitted quadratic | 0.045 | -- | 1.000 |
| `performBPRandomEffectsTest()` | random intercepts | 0.030 | -- | 1.000 |
| `performPesaranTest()` | common time factor | 0.037 | -- | 1.000 |
| `performSpatialHeteroTest()` | variance clustered in space | 0.050 | -- | 0.350 |
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
0.35 against it with its default of 999 permutations (0.36 with the 499 it
used before 0.15.0).

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
residuals, which are heteroscedastic by assumption. Four blocks follow:
weights that are known and weights that were estimated are different cases,
estimated weights can come from the wrong variance function, and the
resampling tests refit the model in every replicate.

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
| `performGlejserTest()` | 0.045 |
| `performParkTest()` | 0.058 |
| `performGQTest()` | 0.045 |
| `performSzroeterTest()` | 0.077 |
| `performArchLMTest()` | 0.040 |
| `performMcLeodLiTest()` | 0.048 |
| `performRESETTest()` | 0.055 |
| `performBPRandomEffectsTest()` | 0.055 |
| `performPesaranTest()` | 0.072 |
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
against 0.058. At 400 replications both are inside three standard errors of
what the same tests do without weights, so this table cannot tell them apart.
For Szroeter a larger study does: at 5000 replications, on the weighted design
of `harrison-mccabe-size-power.R`, it rejects 0.067 of the time at 40
observations, outside the release gate, and 0.056 and 0.055 at 100 and 400.
See the section on the Harrison-McCabe test.

The test marked `refused` fits quantile regressions without the weights, and
stops with an error on a weighted fit instead of testing a different model;
its help page says why it was not adapted. The wild and the White bootstrap
refused a weighted fit in 0.12.0 as well. They now resample on the weighted
scale, and are measured in the fourth block, at 2000 replications.

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

These studies ran before 0.14.0 renamed `rbootstrap_test_statistic()` to
`bootstrapTestStatistic()`; the tables keep the name it had, and the scripts
call the new one.

**The resampling tests.** `performWildBootstrapTest()`,
`performWhiteTestBootstrap()` and `rbootstrap_test_statistic()`, the last with
`performKoenkerTest()` as its test function, each with `B = 499`. On a
weighted fit they resample in the regression of `sqrt(w) y` on `sqrt(w) X`,
where the null hypothesis is constant variance, and refit with the weights.
The data are those of the first two blocks: `sd = x^2` with the exact weights
`1 / x^4`, and the `fitWLS()` design, `sd = exp(0.3 x)`. The second is fitted
three ways: with the weights of its true variance function, `exp(-0.6 x)`;
with the weights `fitWLS()` estimates, held fixed, `reestimate_weights =
FALSE`, as the tests of the second block take them as known and as the three
did by default before 0.15.0; and with the weights estimated again in every
replicate, `reestimate_weights = TRUE`, the default on a `fitWLS()` fit since
0.15.0. The rows with the weights estimated again were measured in 0.12.0,
before the package could estimate them again, by a prototype: for the wild
and the White bootstrap the script repeated their resampling with that one
change, after checking that without it the repetition reproduced the
package's replicates, and `rbootstrap_test_statistic()` was given a test
function that calls `fitWLS()`. In 0.15.0 their design text in the CSV, which
said "not in the package", was changed to the text the script writes now, and
nothing else in them. The script now calls the package for them, and the
rows were not run again: on 200 of the 2000 samples of each, the package gives
the p-values, the replicates and the random draws of the prototype (the
table after this block). The wild bootstrap, the cheapest, also runs at 600
observations.
The last table is a power: `sd = x^2` with the weights `1 / x`, which leave
the weighted fit heteroscedastic.

**The resampling tests, size.**

| Test | Weights | n = 150 | n = 600 |
| --- | --- | ---: | ---: |
| `performWildBootstrapTest()` | exact, `sd = x^2` | 0.053 | -- |
| `performWhiteTestBootstrap()` | exact, `sd = x^2` | 0.052 | -- |
| `rbootstrap_test_statistic()`, Koenker | exact, `sd = x^2` | 0.050 | -- |
| `performWildBootstrapTest()` | true, `fitWLS()` design | 0.054 | 0.048 |
| `performWhiteTestBootstrap()` | true, `fitWLS()` design | 0.054 | -- |
| `rbootstrap_test_statistic()`, Koenker | true, `fitWLS()` design | 0.045 | -- |
| `performWildBootstrapTest()` | `fitWLS()`, held fixed | 0.086 | 0.105 |
| `performWhiteTestBootstrap()` | `fitWLS()`, held fixed | 0.064 | -- |
| `rbootstrap_test_statistic()`, Koenker | `fitWLS()`, held fixed | 0.083 | -- |
| `performWildBootstrapTest()` | `fitWLS()`, estimated again in each replicate | 0.060 | 0.051 |
| `performWhiteTestBootstrap()` | `fitWLS()`, estimated again in each replicate | 0.042 | -- |
| `rbootstrap_test_statistic()`, Koenker | `fitWLS()`, estimated again in each replicate | 0.045 | -- |

Replications: 2000, with B = 499. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0049.

**The resampling tests, power against weights that are too weak.**

| Test | n = 150 |
| --- | ---: |
| `performWildBootstrapTest()` | 1.000 |
| `performWhiteTestBootstrap()` | 1.000 |
| `rbootstrap_test_statistic()`, Koenker | 1.000 |

Replications: 400, with B = 499.

<!-- generated by make-table.R; do not edit the numbers by hand -->

With exact weights in the design `sd = x^2` the three resampling tests reject 0.050 to 0.053 of the time, and with the true weights of the `fitWLS()` design 0.045 to 0.054. With the weights of `fitWLS()` held fixed they reject 0.064 to 0.105, where `performKoenkerTest()` on the same design rejects 0.077 at 150 observations and 0.110 at 600; with the weights estimated again in each replicate they reject 0.042 to 0.060. 11 of the 15 size cells fall inside [0.037, 0.063]. Outside it:

- `performWildBootstrapTest()`, weights `fitWLS()`, held fixed, n = 150: 0.0865.
- `performWhiteTestBootstrap()`, weights `fitWLS()`, held fixed, n = 150: 0.0640.
- `rbootstrap_test_statistic()`, Koenker, weights `fitWLS()`, held fixed, n = 150: 0.0825.
- `performWildBootstrapTest()`, weights `fitWLS()`, held fixed, n = 600: 0.1050.

Inside the band but worth naming, at more than two standard errors:

- `performWildBootstrapTest()`, weights `fitWLS()`, estimated again in each replicate, n = 150: 0.0605, 2.2 standard errors above nominal.

Against the weights 1 / x they reject 1.000 of the time.

At 2000 replications the 99% interval around 0.05 is `[0.037, 0.063]`. A
bootstrap test whose level is exact rejects 0.048 of the time at `p < 0.05`
with `B = 499`, because its p-value takes the values `k / 500`. `B` is not a
column of the CSV; every row of this block uses `n_boot = 499`, set in the
script.

Four things follow.

- **With the right weights the three hold their level.** They reject 0.050
  to 0.053 of the time in the design `sd = x^2`, where they refused the fit
  in 0.12.0, and 0.045 to 0.054 with the true weights of the `fitWLS()`
  design, 0.048 for the wild bootstrap at 600 observations. The wild
  bootstrap and `rbootstrap_test_statistic()` with `performKoenkerTest()`
  compute the statistic of Koenker's test; the White bootstrap that of
  White's test.
- **With the weights of `fitWLS()` held fixed they are no better than the
  tests of the second block.** In the same design, where the true weights
  give 0.054 and 0.048, the wild bootstrap rejects 0.086 at 150 observations and
  0.105 at 600, beside 0.077 and 0.110 for `performKoenkerTest()`, the same
  statistic with its chi-squared reference. `rbootstrap_test_statistic()`
  rejects 0.083 at 150 observations against 0.045 with the true weights, and
  the White bootstrap 0.064 against 0.054, where `performWhiteTest()` rejects
  0.063. All four cells are outside the interval. A replicate that keeps the
  weights is a sample whose weights are known, so the reference distribution
  leaves out the estimation that the observed statistic went through, and the
  discrepancy does not shrink with the sample.
- **Estimating the weights again in each replicate holds the level at this
  number of replications.** The same three resampling schemes, with
  `fitWLS()` run on every replicate, reject 0.042 to 0.060 of the time at 150
  observations, and the wild bootstrap 0.051 at 600, all inside the
  interval. The wild bootstrap at 150 observations, 0.0605, is 2.2 standard
  errors above 0.05, against 0.054 for the same test with the true weights.
  Since 0.15.0 the three do this by default on a `fitWLS()` fit
  (`reestimate_weights`), and `performVarianceFormTest()` allows for the
  estimation too. These rates are at 2000 replications, not at the 5000 of
  the release gate. Against the gate's interval, `[0.042, 0.058]`, the wild
  bootstrap at 150 observations, 0.0605, is outside, above it, and the White
  bootstrap, 0.0415, just below it; against `[0.041, 0.057]`, the interval
  around 0.049, the rate of an exact test with `B = 999`, the wild bootstrap
  is outside and the other three inside. No study of the default at 5000
  replications with `B = 999` has been run; `ROADMAP.md` lists it under
  0.16.0.
- **The weights `1 / x` are rejected every time** by all three, at 400
  replications. Resampling on the weighted scale imposes the null hypothesis
  of the weighted model and keeps the power against weights that do not undo
  the heteroscedasticity.

**The rows with the weights estimated again, prototype and package.** The
four rows "estimated, re-estimated" were produced in 0.12.0 by the prototype
described above, before the package could estimate the weights again. Since
0.15.0 the three functions do it themselves, and `weighted-fits-size.R` calls
them, but the rows were not run again. `weighted-fits-refit-reproduction.R`
checks instead that the package gives what the prototype gave, on every
tenth sample of each row: the 10th, 20th, and so on to the 2000th, drawn from
the seeds of `weighted-fits-size.R`, with `B = 499` as recorded. On each it
runs the prototype and the package from the same state of the random number
generator and compares their p-values, their replicates, the replicates that
fail and the state each leaves the generator in. The samples in between are
skipped by drawing the random numbers they would draw, which the script checks
against a sample it runs.

| Test | n | Samples | Decisions agree | p-values equal | Max abs. difference of p | Max rel. difference of a replicate | Samples with a failed replicate, prototype / package | Same random draws |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `performWildBootstrapTest()` | 150 | 200 | 200 | 200 | 0 | 0 | 0 / 0 | 200 |
| `performWhiteTestBootstrap()` | 150 | 200 | 200 | 200 | 0 | 0 | 0 / 0 | 200 |
| `rbootstrap_test_statistic()`, Koenker | 150 | 200 | 200 | 200 | 0 | 0 | 0 / 0 | 200 |
| `performWildBootstrapTest()` | 600 | 200 | 200 | 200 | 0 | 0 | 0 / 0 | 200 |

Every 10th sample of the 2000 of each cell, with B = 499, as recorded.

<!-- generated by make-table.R; do not edit the numbers by hand -->

On all 800 samples checked, 200 in each of the 4 cells, the package returns the p-value of the repetition, so its decision at the 5% level is the same, no replicate fails in either, and leaves the generator in the same state. The largest relative difference between two replicates is 0. Both reject 12, 11, 7 and 12 of the 200 samples of the cells, in the order of the table.

A relative difference of 0 means the replicates are equal to the last bit:
the package runs the same computations as the prototype, in the same order.
The numbers of the four rows of the CSV are therefore kept as they are. That a run of
`weighted-fits-size.R` would write them again rests on these 800 samples,
not on a run of all 8000.

### The resampling tests on a fit with an offset

These studies ran before 0.14.0 renamed `rbootstrap_test_statistic()` to
`bootstrapTestStatistic()`; the tables keep the name it had, and the scripts
call the new one.

`lm(..., offset = o)` keeps `o` in the model frame and not in the formula.
Until the correction recorded in `NEWS.md`, the resampling tests refitted
each replicate of an unweighted fit from the formula alone, and three things
went wrong. `performWhiteTestBootstrap()` and `rbootstrap_test_statistic()`
dropped the offset. `performWildBootstrapTest()` evaluated the call of the
fit again in its model frame, where the offset is `(offset)` and a
transformed regressor is `log(x)` rather than `x`, and stopped with an
error. `rbootstrap_test_statistic()` took the fitted values by position, so
data holding the fitted rows in another order regenerated each row from
another row's fitted value. A weighted fit was already resampled correctly.

The design is `y = 1 + 2 x + 0.5 z + o + e` with `o = 0.5 x^2`, `x` and `z`
uniform on (1, 5), Gaussian errors and 100 observations, fitted as
`lm(y ~ x + z, data, offset = o)`. The variance is constant, so every rate is
a size, and the offset is not in the span of the regressors.
`rbootstrap_test_statistic()` resamples under the null hypothesis with
`performKoenkerTest()` as its test function; its second row has the same
rows in a random order. The fifth row fits `log(x)` in place of `x`. The
last row is the third again with `B = 999`, the default of
`rbootstrap_test_statistic()`. The `Before` column repeats the refits as the
package made them, without the offset and with the fitted values taken by
position. The script writes them out after checking that, with the offset
kept and the rows matched by name, they reproduce the package's replicates.
The corrected cells run at 5000 replications, the release standard. The
cells before the correction run at 2000: they describe code that is gone and
are not evidence for the release gate.

| Test | Terms | Rows of the data | B | Corrected | Before |
| --- | --- | --- | ---: | ---: | ---: |
| `performWildBootstrapTest()` | `x + z` | in the order of the fit | 199 | 0.044 | -- |
| `performWhiteTestBootstrap()` | `x + z` | in the order of the fit | 199 | 0.044 | 0.012 |
| `rbootstrap_test_statistic()`, Koenker | `x + z` | in the order of the fit | 199 | 0.040 | 0.047 |
| `rbootstrap_test_statistic()`, Koenker | `x + z` | in another order | 199 | 0.049 | 0.257 |
| `performWildBootstrapTest()` | `log(x) + z` | in the order of the fit | 199 | 0.048 | -- |
| `rbootstrap_test_statistic()`, Koenker | `x + z` | in the order of the fit | 999 | 0.046 | -- |

Replications: 5000 for the corrected cells and 2000 for those before the correction. Gaussian errors, 100 observations. Nominal level: 0.05.

<!-- generated by make-table.R; do not edit the numbers by hand -->

With B = 199 a test whose level is exact gives p < 0.05 with probability 0.045, and the 5 corrected cells with B = 199 are judged against the 99% interval around it at 5000 replications, [0.037, 0.053]: they reject 0.040 to 0.049 of the time, and 5 of them fall inside it. With B = 999 the exact rate is 0.049, and the cell with B = 999, `rbootstrap_test_statistic()`, Koenker, `x + z`, rows in the order of the fit, B = 999, is judged against the release gate, [0.042, 0.058]: it rejects 0.0460 of the time, inside the gate, and inside the 99% interval around its exact rate, [0.041, 0.057]. Before the correction the White bootstrap rejected 0.012, and `rbootstrap_test_statistic()` 0.047 on the data in the order of the fit and 0.257 on the same rows in another order; the wild bootstrap stopped with an error.

A bootstrap test whose level is exact gives `p < 0.05`, the rule of every
study in this directory, with probability `(ceiling(0.05 (B + 1)) - 1) / (B + 1)`,
because its p-value takes the values `k / (B + 1)`: 0.045 with `B = 199` and
0.049 with `B = 999`. Each cell is judged against the reference that says
which of those it is. The cells with `B = 199` are held to the 99% interval
around 0.045; the author approved that reference for them, in place of the
gate around 0.05, on 2026-10-12. The cell with `B = 999`, whose exact rate is
close to 0.05, is held to the release gate around 0.05, `[0.042, 0.058]` at
5000 replications, and is inside the interval around 0.049 as well.

- **The corrected tests hold their level.** The five cells with `B = 199`
  reject 0.040 to 0.049 of the time, inside the interval around 0.045. The
  lowest, `rbootstrap_test_statistic()` with `performKoenkerTest()` on the
  data in the order of the fit, rejects 0.0402: 1.6 standard errors below
  0.045, and 3.2 below 0.05, which put it outside the gate when it was read
  against 0.05. Run again with `B = 999`, it rejects 0.0460, inside the
  release gate and inside the interval around 0.049.
- **The dropped offset made the White bootstrap conservative.** It rejected
  0.012 of the time. The residuals of every replicate kept the part of
  `0.5 x^2` that `x` and `z` do not explain, and its square varies with
  `x^2`, a term of White's auxiliary regression, so the replicates were too
  large and the observed statistic looked small against them.
- **Koenker's statistic happened not to see it.** `rbootstrap_test_statistic()`
  with `performKoenkerTest()` rejected 0.047 of the time before the
  correction. The square of that remainder is close to symmetric about the
  middle of the range of `x`, and a regression on `x` and `z`, without their
  squares, finds little of it. The replicates were nonetheless those of a
  model without the offset, which `tests/testthat/test-resampling-refits.R`
  checks one by one.
- **Rows in another order were far off.** With the rows in a random order,
  `rbootstrap_test_statistic()` rejected 0.257 of the time; corrected, it
  rejects 0.049. Every replicate added its draws to the same scrambled fitted
  values, whose residuals dominate those of the draws, so the replicates
  gathered around one value instead of following the null distribution of
  the statistic.
- **The wild bootstrap could not run** on any fit of this design, nor on the
  fit with `log(x)`; corrected, it rejects 0.044 and 0.048.

#### RESET on a fit with an offset

Since 0.16.0 `performRESETTest()` on a fit with an offset is the nested F
test of the model with its offset against the model augmented with the
squares and cubes of its fitted values, `X b + o`, both fitted with the offset
(decision 19 of the design of the model context). Before, it fitted the
augmented model without the offset, so the two models were not nested, and
the development of 0.16.0 refused such a model. The tenth scenario of
`offset-refits-size.R` runs it on the fit above, whose mean is linear in `x`
and `z`, so that the null hypothesis of RESET, a correct form of the mean,
holds and the rejection rate is its size. The added columns are functions of
the fitted values, so under Gaussian errors the statistic has its F
distribution exactly, as RESET without an offset has (Milliken and Graybill,
1970); the cell has no bootstrap, and is judged against the release gate
around 0.05. It ran in 10 seconds, after the others, with its own seeds, so
that theirs stay as they were.

<!-- generated by make-table.R; do not edit the numbers by hand -->

`performRESETTest()` on the same fit, `x + z` with the offset given as an argument, rows in the order of the fit, Gaussian errors and 100 observations, as the nested F test of the model with its offset against the model augmented with the squares and cubes of its fitted values: it rejects 0.0492 of the time at 5000 replications (Monte Carlo standard error 0.0031), inside the release gate, [0.042, 0.058].

### The White bootstrap on a transformed response

`performWhiteTestBootstrap()` regenerates the response and refits the model.
For a transformed response, such as `log(y)` in `log(y) ~ x + z`, it wrote the
regenerated values into a column named `log(y)`, which the refit did not
read: it computed `log(y)` from `y` again, so every replicate was the data of
the fit, every replicate statistic was the observed one, and the
p-value was 1 whatever the data. Such a test never rejects, so there is
nothing to simulate for the code before the correction;
`tests/testthat/test-resampling-refits.R` shows it on one sample. The design
is `log(y) = 1 + 0.3 x + 0.1 z + s(x) e`, with `x` and `z` uniform on (1, 5),
standard normal `e` and 100 observations, fitted as `lm(log(y) ~ x + z)`,
with `B = 199`. The size cell runs at 5000 replications and the power cell at
400.

| Cell | Errors | Replications | Rejection rate |
| --- | --- | ---: | ---: |
| size | sd = 0.3 on the log scale | 5000 | 0.047 |
| power | sd = 0.1 x on the log scale | 400 | 0.892 |

`performWhiteTestBootstrap()` on `lm(log(y) ~ x + z)`, B = 199, 100 observations. Nominal level: 0.05.

<!-- generated by make-table.R; do not edit the numbers by hand -->

With constant variance on the scale of the model the test rejects 0.0474 of the time at the 5% level. With B = 199 a test whose level is exact gives p < 0.05 with probability 0.045, and the 99% interval around that at 5000 replications is [0.037, 0.053]; the rate is inside that interval, 0.8 standard errors above 0.045. Against a standard deviation of 0.1 x on the log scale it rejects 0.892 of the time.

- **The corrected test holds its level on a transformed response.** It
  rejects 0.0474 of the time, inside the 99% interval around 0.045, the rate
  of an exact test with `B = 199`. That is the reference the author approved
  for cells with `B = 199` on 2026-10-12, as in the section above.
- **It has power where it had none.** Against a standard deviation that
  grows with `x` on the log scale it rejects 0.892 of the time, where the
  code before the correction returned p = 1 on every sample.

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
and negative errors are equally likely (Godfrey 1996). `robust = TRUE`, the
default since 0.15.0, regresses `abs(e) - m e` instead, where `m` is the
proportion of positive residuals less the proportion of negative ones (Im
2000; Machado and Santos Silva 2000).

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

The script gives `robust` in both calls, so each row means the same whatever
the default: `performGlejserTest(robust = FALSE)` is Glejser's statistic, the
default up to 0.14.0, and `performGlejserTest(robust = TRUE)` the corrected
statistic, the default since 0.15.0.

**Glejser test, size by error distribution.**

| Errors | Skewness | Test | n = 50 | n = 150 | n = 400 | n = 1000 |
| --- | ---: | --- | ---: | ---: | ---: | ---: |
| Gaussian | 0 | `performGlejserTest(robust = FALSE)` | 0.054 | 0.049 | 0.045 | 0.050 |
| Gaussian | 0 | `performGlejserTest(robust = TRUE)` | 0.049 | 0.047 | 0.044 | 0.050 |
| Gaussian | 0 | Koenker's statistic on the same regressor | 0.046 | 0.048 | 0.048 | 0.051 |
| Gaussian | 0 | `performKoenkerTest()` | 0.046 | 0.053 | 0.048 | 0.049 |
| t5 | 0 | `performGlejserTest(robust = FALSE)` | 0.054 | 0.050 | 0.053 | 0.055 |
| t5 | 0 | `performGlejserTest(robust = TRUE)` | 0.046 | 0.048 | 0.053 | 0.054 |
| t5 | 0 | Koenker's statistic on the same regressor | 0.048 | 0.046 | 0.049 | 0.053 |
| t5 | 0 | `performKoenkerTest()` | 0.045 | 0.048 | 0.051 | 0.050 |
| chi-squared(5) | 1.26 | `performGlejserTest(robust = FALSE)` | 0.094 | 0.096 | 0.089 | 0.088 |
| chi-squared(5) | 1.26 | `performGlejserTest(robust = TRUE)` | 0.057 | 0.056 | 0.048 | 0.047 |
| chi-squared(5) | 1.26 | Koenker's statistic on the same regressor | 0.058 | 0.060 | 0.053 | 0.050 |
| chi-squared(5) | 1.26 | `performKoenkerTest()` | 0.058 | 0.058 | 0.052 | 0.050 |
| exponential | 2 | `performGlejserTest(robust = FALSE)` | 0.133 | 0.134 | 0.132 | 0.123 |
| exponential | 2 | `performGlejserTest(robust = TRUE)` | 0.059 | 0.054 | 0.049 | 0.046 |
| exponential | 2 | Koenker's statistic on the same regressor | 0.061 | 0.049 | 0.045 | 0.046 |
| exponential | 2 | `performKoenkerTest()` | 0.063 | 0.055 | 0.046 | 0.049 |
| exponential, mirrored | -2 | `performGlejserTest(robust = FALSE)` | 0.133 | 0.134 | 0.139 | 0.126 |
| exponential, mirrored | -2 | `performGlejserTest(robust = TRUE)` | 0.058 | 0.057 | 0.046 | 0.047 |
| exponential, mirrored | -2 | Koenker's statistic on the same regressor | 0.059 | 0.051 | 0.044 | 0.046 |
| exponential, mirrored | -2 | `performKoenkerTest()` | 0.058 | 0.051 | 0.044 | 0.046 |
| lognormal | 6.18 | `performGlejserTest(robust = FALSE)` | 0.166 | 0.177 | 0.169 | 0.174 |
| lognormal | 6.18 | `performGlejserTest(robust = TRUE)` | 0.057 | 0.054 | 0.047 | 0.051 |
| lognormal | 6.18 | Koenker's statistic on the same regressor | 0.048 | 0.041 | 0.039 | 0.045 |
| lognormal | 6.18 | `performKoenkerTest()` | 0.054 | 0.046 | 0.047 | 0.044 |

**Glejser test, size with `transformation = "inverse"`.**

| Errors | Skewness | Test | n = 50 | n = 400 |
| --- | ---: | --- | ---: | ---: |
| Gaussian | 0 | `performGlejserTest(robust = FALSE)` | 0.055 | 0.053 |
| Gaussian | 0 | `performGlejserTest(robust = TRUE)` | 0.049 | 0.053 |
| Gaussian | 0 | Koenker's statistic on the same regressor | 0.049 | 0.053 |
| Gaussian | 0 | `performKoenkerTest()` | 0.048 | 0.057 |
| exponential | 2 | `performGlejserTest(robust = FALSE)` | 0.118 | 0.120 |
| exponential | 2 | `performGlejserTest(robust = TRUE)` | 0.063 | 0.048 |
| exponential | 2 | Koenker's statistic on the same regressor | 0.061 | 0.048 |
| exponential | 2 | `performKoenkerTest()` | 0.062 | 0.053 |
| lognormal | 6.18 | `performGlejserTest(robust = FALSE)` | 0.155 | 0.158 |
| lognormal | 6.18 | `performGlejserTest(robust = TRUE)` | 0.066 | 0.053 |
| lognormal | 6.18 | Koenker's statistic on the same regressor | 0.071 | 0.054 |
| lognormal | 6.18 | `performKoenkerTest()` | 0.066 | 0.043 |

**Glejser test, size after a correctly weighted fit.**

| Errors | Skewness | Test | n = 150 | n = 400 |
| --- | ---: | --- | ---: | ---: |
| Gaussian | 0 | `performGlejserTest(robust = FALSE)` | 0.051 | 0.058 |
| Gaussian | 0 | `performGlejserTest(robust = TRUE)` | 0.050 | 0.057 |
| Gaussian | 0 | Koenker's statistic on the same regressor | 0.051 | 0.053 |
| Gaussian | 0 | `performKoenkerTest()` | 0.053 | 0.050 |
| exponential | 2 | `performGlejserTest(robust = FALSE)` | 0.119 | 0.116 |
| exponential | 2 | `performGlejserTest(robust = TRUE)` | 0.053 | 0.052 |
| exponential | 2 | Koenker's statistic on the same regressor | 0.051 | 0.048 |
| exponential | 2 | `performKoenkerTest()` | 0.052 | 0.057 |
| lognormal | 6.18 | `performGlejserTest(robust = FALSE)` | 0.153 | 0.148 |
| lognormal | 6.18 | `performGlejserTest(robust = TRUE)` | 0.054 | 0.049 |
| lognormal | 6.18 | Koenker's statistic on the same regressor | 0.044 | 0.039 |
| lognormal | 6.18 | `performKoenkerTest()` | 0.043 | 0.043 |

**Glejser test, power.**

| Errors | Skewness | gamma | Test | n = 50 | n = 150 |
| --- | ---: | ---: | --- | ---: | ---: |
| Gaussian | 0 | 0.2 | `performGlejserTest(robust = FALSE)` | 0.163 | 0.441 |
| Gaussian | 0 | 0.2 | `performGlejserTest(robust = TRUE)` | 0.152 | 0.434 |
| Gaussian | 0 | 0.2 | Koenker's statistic on the same regressor | 0.167 | 0.478 |
| Gaussian | 0 | 0.2 | `performKoenkerTest()` | 0.118 | 0.366 |
| Gaussian | 0 | 0.4 | `performGlejserTest(robust = FALSE)` | 0.476 | 0.941 |
| Gaussian | 0 | 0.4 | `performGlejserTest(robust = TRUE)` | 0.456 | 0.940 |
| Gaussian | 0 | 0.4 | Koenker's statistic on the same regressor | 0.485 | 0.953 |
| Gaussian | 0 | 0.4 | `performKoenkerTest()` | 0.346 | 0.900 |
| exponential | 2 | 0.2 | `performGlejserTest(robust = FALSE)` | 0.203 | 0.365 |
| exponential | 2 | 0.2 | `performGlejserTest(robust = TRUE)` | 0.105 | 0.209 |
| exponential | 2 | 0.2 | Koenker's statistic on the same regressor | 0.096 | 0.179 |
| exponential | 2 | 0.2 | `performKoenkerTest()` | 0.083 | 0.135 |
| exponential | 2 | 0.4 | `performGlejserTest(robust = FALSE)` | 0.409 | 0.778 |
| exponential | 2 | 0.4 | `performGlejserTest(robust = TRUE)` | 0.251 | 0.622 |
| exponential | 2 | 0.4 | Koenker's statistic on the same regressor | 0.221 | 0.520 |
| exponential | 2 | 0.4 | `performKoenkerTest()` | 0.167 | 0.398 |

Replications: 5000. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0031.

<!-- generated by make-table.R; do not edit the numbers by hand -->

Under symmetric errors Glejser's statistic, `robust = FALSE`, rejects 0.045 to 0.058 of the time and the corrected one, `robust = TRUE`, the default since 0.15.0, 0.044 to 0.057. Under asymmetric errors Glejser's statistic rejects 0.088 to 0.177 and the corrected one 0.046 to 0.066. Of the 36 null cells of the corrected statistic, 32 fall inside the release-gate interval [0.0421, 0.0579]. Outside it:

- exponential errors, n = 50, size: 0.0592.
- exponential, mirrored errors, n = 50, size: 0.0580.
- exponential errors, n = 50, size, regressor outside the mean equation: 0.0626.
- lognormal errors, n = 50, size, regressor outside the mean equation: 0.0658.

Five things follow.

- **Glejser's statistic is not a 5% test when the errors are skewed.** With
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
  Glejser's statistic then rejects too seldom.
- **The corrected statistic holds its level from 150 observations.** All 27
  of its null cells at 150 observations or more are inside the release-gate
  interval, with `x1` or `1 / x1` as the auxiliary regressor and after a
  weighted fit. At 50 observations four of its nine cells are above the
  interval, at 0.058 to 0.066, all with skewed errors. In the same four cells
  Koenker's statistic on the same regressor is at 0.059 to 0.071 and
  `performKoenkerTest()` at 0.058 to 0.066.
- **Under symmetric errors the correction costs little, and it is not free.**
  The corrected statistic rejects less often than Glejser's in ten of the
  twelve symmetric null cells and as often in the other two: by 0.5 to 0.7
  percentage points at 50 observations and by 0.2 or less from 150. With
  Gaussian errors it loses 1.1 and 1.9 points of power at 50 observations and
  under one point at 150.
- **Against Koenker's statistic on the same regressor the ranking depends on
  the errors.** With Gaussian errors Koenker's statistic has 1.3 to 4.4 points
  more power than the corrected Glejser statistic. With exponential errors the
  corrected Glejser statistic has 0.9 to 10.3 points more. `performKoenkerTest()`
  is below both in every power cell, because it spends a degree of freedom on
  `x2`, which the variance does not depend on. The rejection rates of
  Glejser's statistic under exponential errors are not powers, because its
  size there is 0.13.

**Glejser test, size of Glejser's statistic against its asymptotic value.**

| Errors | Regressor | Asymptotic | Simulated |
| --- | --- | ---: | ---: |
| chi-squared(5) | `x1` | 0.090 | 0.088 to 0.096 |
| exponential | `x1` | 0.132 | 0.123 to 0.134 |
| exponential | `1 / x1` | 0.120 | 0.118 to 0.120 |
| lognormal | `x1` | 0.170 | 0.166 to 0.177 |
| lognormal | `1 / x1` | 0.153 | 0.155 to 0.158 |

Up to 0.14.0 the default of `robust` was `FALSE`, Glejser's statistic. Since
0.15.0 it is `TRUE`: `performGlejserTest()` returns the corrected statistic,
and with `robust = FALSE` what its default returned up to 0.14.0. The
release gate holds for the new default from 150 observations on, in all 27
of its null cells, and does not at 50 observations, where four of its nine
null cells, all with skewed errors, are above the interval, at 0.0580 to
0.0658. The full sweep and the weighted fits above call
`performGlejserTest()` with its default, and their rows were regenerated in
0.15.0 (*History*, below).

### The modified Wald test for groupwise heteroscedasticity

The test Stata users run as `xttest3` after a fixed-effects regression, and the
first candidate in the roadmap's order of work. It asks whether the error
variance differs between the units of a panel. The package does not export it,
and this study is the reason.

For unit `i` with `T_i` residuals `e_it` from the within (fixed-effects) fit,
Baum (2001) defines

- `s2_i = sum_t e_it^2 / T_i`, the estimate of the unit's error variance;
- `V_i = sum_t (e_it^2 - s2_i)^2 / (T_i (T_i - 1))`, the estimate of the
  variance of `s2_i`;
- `W = sum_i (s2_i - s2)^2 / V_i`, referred to a chi-squared distribution with
  as many degrees of freedom as there are units.

The article does not define `s2`. The code of `xttest3` (version 1.0.8, 4
October 2024) takes the variance of all the residuals with divisor `n`, which
is `sum e_it^2 / n` because the within residuals have mean zero. With that
choice the script reproduces the article's example, `chi2(4) = 279.13` on
Greene's Grunfeld data without firm 2, as 279.1279; the divisor `n - 1` would
give 287.0876. Baum says he follows Greene (2000, p. 598), generalised to
unbalanced panels. That page could not be obtained for this study, so any
difference between Greene's form and Baum's is not recorded here.

**Reference.** No R implementation was found (CRAN and GitHub, searched on
2026-10-10). The Python package `panelbox` has a `ModifiedWaldTest`. It was
run in version 1.0.2, with Python 3.13.5, NumPy 2.5.3, pandas 3.0.6 and SciPy
1.18.1, on the data of Baum's example written to a file, and returned 30.181
where the statistic above is 279.128. The residuals agree: `W` computed from
panelbox's own residuals differs from the R value by less than `1e-11`. The
difference is the statistic. Under that name panelbox computes
`sum_i T_i log(s2 / s2_i)`, with `s2 = e'e / (n - N - k)` and `s2_i` the
sample variance of unit `i`'s residuals. That formula, computed in R on the
same file, matches panelbox's value to `1e-12`, and the script prints it for
the example as 30.1809. It is a likelihood-ratio form, not Baum's statistic,
so it is not a reference for this test. The comparison with panelbox itself
was run once for this study; the Python environment is not part of the
repository.

**Designs.** `y_it = a_i + 0.5 x1_it - 0.25 x2_it + sigma_i e_it`, where
`x1 = 0.5 a_i + N(0, 1)` is correlated with the unit effect and `x2` is
uniform, so the fixed effects are needed. The errors are Gaussian, or `t5`
scaled to unit variance. The residuals are those of
`lm(y ~ x1 + x2 + factor(id))`, computed by the within transformation; the
script checks that the two agree. Every cell reports the published test,
referred to a chi-squared with `N` degrees of freedom, and the same statistic
referred to `N - 1`, which `xttest3` does not use and which is there to
separate the two sources of error. Under the alternative the variance of unit `i` is
`exp(tau z_i - tau^2 / 2)` with `z_i` standard normal, drawn in each
replication, so the unit variances are lognormal with mean 1.

**Modified Wald test, size in balanced panels.**

| Errors | Units | Reference | T = 5 | T = 10 | T = 30 | T = 100 |
| --- | ---: | --- | ---: | ---: | ---: | ---: |
| Gaussian | 10 | chi-squared, N df (xttest3) | 0.927 | 0.681 | 0.282 | 0.100 |
| Gaussian | 10 | chi-squared, N - 1 df | 0.937 | 0.714 | 0.324 | 0.137 |
| Gaussian | 30 | chi-squared, N df (xttest3) | 1.000 | 0.966 | 0.584 | 0.183 |
| Gaussian | 30 | chi-squared, N - 1 df | 1.000 | 0.970 | 0.612 | 0.211 |
| Gaussian | 100 | chi-squared, N df (xttest3) | 1.000 | 1.000 | 0.928 | 0.361 |
| Gaussian | 100 | chi-squared, N - 1 df | 1.000 | 1.000 | 0.934 | 0.383 |
| t5 | 10 | chi-squared, N df (xttest3) | 0.966 | 0.876 | 0.621 | 0.367 |
| t5 | 10 | chi-squared, N - 1 df | 0.971 | 0.895 | 0.662 | 0.415 |
| t5 | 30 | chi-squared, N df (xttest3) | 1.000 | 0.999 | 0.944 | 0.717 |
| t5 | 30 | chi-squared, N - 1 df | 1.000 | 0.999 | 0.951 | 0.739 |
| t5 | 100 | chi-squared, N df (xttest3) | 1.000 | 1.000 | 1.000 | 0.982 |
| t5 | 100 | chi-squared, N - 1 df | 1.000 | 1.000 | 1.000 | 0.985 |

**Modified Wald test, size in unbalanced panels.**

| Errors | Units | Periods per unit | Mean periods | chi-squared, N df (xttest3) | chi-squared, N - 1 df |
| --- | ---: | --- | ---: | ---: | ---: |
| Gaussian | 30 | 10, 30, 100 | 46.67 | 0.809 | 0.823 |
| Gaussian | 100 | 5, 10, 20, 30 | 16.25 | 1.000 | 1.000 |
| t5 | 30 | 10, 30, 100 | 46.67 | 0.980 | 0.983 |
| t5 | 100 | 5, 10, 20, 30 | 16.25 | 1.000 | 1.000 |

**Modified Wald test, size in long panels.**

| Errors | Units | Reference | T = 300 | T = 1000 |
| --- | ---: | --- | ---: | ---: |
| Gaussian | 2 | chi-squared, N df (xttest3) | 0.020 | 0.014 |
| Gaussian | 2 | chi-squared, N - 1 df | 0.054 | 0.052 |
| Gaussian | 10 | chi-squared, N df (xttest3) | 0.052 | 0.036 |
| Gaussian | 10 | chi-squared, N - 1 df | 0.074 | 0.053 |
| Gaussian | 30 | chi-squared, N df (xttest3) | 0.078 | 0.049 |
| Gaussian | 30 | chi-squared, N - 1 df | 0.095 | 0.060 |
| t5 | 2 | chi-squared, N df (xttest3) | 0.051 | 0.039 |
| t5 | 2 | chi-squared, N - 1 df | 0.110 | 0.090 |
| t5 | 10 | chi-squared, N df (xttest3) | 0.211 | 0.129 |
| t5 | 10 | chi-squared, N - 1 df | 0.257 | 0.166 |
| t5 | 30 | chi-squared, N df (xttest3) | 0.426 | 0.235 |
| t5 | 30 | chi-squared, N - 1 df | 0.462 | 0.270 |

**Modified Wald test, rejection rate under lognormal unit variances.**

| Units | Periods | Size, Gaussian | tau = 0.25 | tau = 0.50 |
| ---: | ---: | ---: | ---: | ---: |
| 10 | 10 | 0.681 | 0.809 | 0.943 |
| 30 | 10 | 0.966 | 0.994 | 1.000 |
| 100 | 10 | 1.000 | 1.000 | 1.000 |
| 10 | 100 | 0.100 | 0.914 | 0.998 |
| 30 | 100 | 0.183 | 1.000 | 1.000 |
| 100 | 100 | 0.361 | 1.000 | 1.000 |

Replications: 5000. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0031.

<!-- generated by make-table.R; do not edit the numbers by hand -->

In the balanced panels of 10 to 100 units and 5 to 100 periods the test rejects a true null hypothesis 0.100 to 1.000 of the time with Gaussian errors and 0.367 to 1.000 with t5 errors. Of its 40 null cells, 3 fall inside the release-gate interval [0.0421, 0.0579]:

- t5 errors, 2 units, 300 periods: 0.0514.
- Gaussian errors, 10 units, 300 periods: 0.0524.
- Gaussian errors, 30 units, 1000 periods: 0.0492.

Below it:

- Gaussian errors, 2 units, 300 periods: 0.0198.
- Gaussian errors, 2 units, 1000 periods: 0.0144.
- t5 errors, 2 units, 1000 periods: 0.0390.
- Gaussian errors, 10 units, 1000 periods: 0.0356.

The other 33 are above it.

What follows from the tables:

- **The test does not hold its level in any panel of 5 to 100 periods.** It
  rejects a true null hypothesis more often the more units there are and the
  fewer periods: with 10 units and 100 periods it rejects 0.100 of the time
  under Gaussian errors, and with 100 units and 5 or 10 periods it rejects in
  every replication. In the unbalanced panels it rejects 0.809 of the time
  with 30 units and 46.67 periods on average, and 1.000 with 100 units and
  16.25.
- **That pattern is what the denominators lead one to expect.** `V_i`
  estimates a fourth moment from `T_i` residuals, and it is small when `s2_i`
  is small, so a unit whose residuals happen to be small contributes a term
  that is too large. The excess in each term shrinks as `T_i` grows, and the
  `N` terms add up. Unbalanced panels carry a second error: with divisor
  `T_i`, the expectation of `s2_i` is about `sigma^2 (T_i - 1) / T_i` for
  within residuals, so units of different length differ in expectation even
  under the null. The balanced panels, which do not have that error, are far
  from the level too. The `xttest3` documentation warns that the test's power
  is very low in panels with many units and few periods; in these designs the
  problem there is its size.
- **In long panels the degrees of freedom are wrong the other way.** `s2` is a
  weighted mean of the `s2_i`, so the `N` terms of `W` satisfy one linear
  restriction, and as the periods grow `W` tends to a chi-squared with `N - 1`
  degrees of freedom. With 2 units and Gaussian errors the published test
  rejects 0.020 and 0.014 of the time at 300 and 1000 periods; referred to
  `N - 1`, 0.054 and 0.052. The three cells inside the release-gate interval
  are places where the two errors cancel on the way down, not a range: with
  10 units the published test goes from 0.052 at 300 periods to 0.036 at 1000.
- **Heavy tails make it worse.** Under `t5` errors it still rejects 0.129 of
  the time with 10 units and 0.235 with 30 at 1000 periods. The statistic is
  described as robust to non-normality in large samples, and these samples are
  not large enough.
- **The rejection rates under the alternative are not powers.** They run from
  0.809 to 1.000, in panels where the same test rejects a true null 0.100 to
  1.000 of the time.

The test was not adopted. The roadmap records it under *Candidate tests*.

### The Harrison-McCabe test

`performHarrisonMcCabeTest()` orders the observations by a variable and takes
the share of the residual sum of squares that falls in the first `m` of them,
`b = sum(e[1:m]^2) / sum(e^2)`, with `m = floor(n / 2)` by default (Harrison
and McCabe 1979). A variance that increases along the ordering makes `b`
small. Under normal errors `b` is a ratio of quadratic forms in normal
variables whose distribution depends on the regressors, and the test computes
its p-value from that distribution exactly, by Imhof's (1961) formula.

**The source.** Harrison and McCabe (1979) is not open access and was not
read. The definition is taken from Harrison (1981), by its first author, in
*The Economic and Social Review* 12(4), 235-252, which is open access at
<https://hdl.handle.net/2262/68690>. Its equation (2) is the statistic above,
with `m` the integer part of `n / 2` and the observations ordered so that the
variance rises under the alternative, and the test rejects for small `b`. It
computes the exact test by Imhof's method, and its Table 3 prints the exact 5%
critical values for a constant and a linear trend: 0.147, 0.243, 0.289 and
0.317 at 10, 20, 30 and 40 observations. The null distribution of this package
gives 0.1467, 0.2429, 0.2890 and 0.3168, and
`tests/testthat/test-harrison-mccabe.R` asserts each to within `5e-4`. The same
file checks that the p-value lies between those of the two beta-distributed
bounds that Harrison (1981) restates from the 1979 article.

The design is the cross-sectional one of Pass A, ordered by `x1`. The
Goldfeld-Quandt and Szroeter tests are run on the same samples with the same
ordering, each with its default split and its one-sided alternative of a
variance that increases with `x1`, which is `alternative = "less"` for this
test and `"greater"` for those two. The Glejser test of the absolute residuals
on `x1` with `robust = TRUE` is in the tables because the help page recommends
it when the errors may be heavy-tailed. It is two-sided, so its rejection
rates under the alternatives are not powers to rank against the others.

The first of the three tables of the study holds one design of each size fixed
and draws 100000 error vectors on it, so it measures the p-value itself. The
other two redraw the design in every sample, as a user's data would vary.

**Reference implementations.** The statistic is that of `lmtest::hmctest()`,
and `tests/testthat/test-harrison-mccabe.R` asserts the agreement to `1e-8`.
Neither reference computes this p-value.

- `lmtest::hmctest()` (0.9-40) simulates its p-value from standard normal
  vectors that are centred and scaled, which are the residuals of a regression
  on a constant alone, whatever the model. With `simulate.p = FALSE` the
  p-value is `NA`. The rows "Null of `lmtest::hmctest()`" in the first table
  are that null computed exactly, on the same samples as the test's own.
- `skedastic::harrison_mccabe()` (2.0.3) evaluates Imhof's formula through
  `CompQuadForm::imhof()` (1.4.4), and its p-value differs from this one for
  two reasons. It passes the eigenvalues through `almost.unique()` first,
  which keeps one copy of eigenvalues that round to the same multiple of
  `sqrt(.Machine$double.eps)`, about `1.5e-8`. Here they come in two large
  groups, `1 - b` repeated `m - k` times and `-b` repeated `n - m - k` times,
  so the p-value it reports is that of a quadratic form with at most `k + 2`
  terms in place of `n - k`. And with `deflator` it orders
  the residuals by that variable but computes the null distribution from the
  design in the order of the rows. It also puts `round(m * n)` observations in
  the first part where `lmtest` puts `floor(point * n)`.

On four data sets, with skedastic given `m` as a count, R 4.5.1:

| Data | n | m | Here | skedastic, rows sorted | skedastic, rows as given, `deflator` | skedastic's matrices, every eigenvalue kept | Simulated given the design | `lmtest::hmctest()`, `nsim = 100000` |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `mtcars`, `mpg ~ wt + qsec`, by `wt` | 32 | 16 | 0.6929 | 0.6515 | 0.5589 | 0.6929 | 0.6923 | 0.6802 |
| `cars`, `dist ~ speed`, by `speed` | 50 | 25 | 0.1320 | 0.3790 | 0.3790 | 0.1320 | 0.1311 | 0.1286 |
| `trees`, `Volume ~ Girth + Height`, by `Height` | 31 | 15 | 0.4694 | 0.3988 | 0.4189 | 0.4694 | 0.4698 | 0.4770 |
| A sample of the Pass A design, `y ~ x1 + x2`, by `x1` | 43 | 21 | 0.0351 | 0.1702 | 0.1023 | 0.0351 | 0.0350 | 0.0367 |

These figures are not in the CSV, because skedastic is not a dependency of the
package; they were computed with it installed in a separate library. The
statistic agrees with both references to `7e-16` on every row. Keeping every
eigenvalue of skedastic's own matrices, `CompQuadForm::imhof()` gives this
package's p-value to `4e-15`, and 400000 error vectors drawn on each design
are within 1.7 standard errors of it. `cars` is sorted by `speed`, so the two
skedastic columns agree there. The computation for the first row:

    fit <- lm(mpg ~ wt + qsec, data = mtcars[order(mtcars$wt), ])
    X <- model.matrix(fit); n <- nrow(X); m <- 16
    sk <- skedastic::harrison_mccabe(fit, m = m)           # p.value 0.6515
    M <- diag(n) - X %*% solve(crossprod(X), t(X))
    A <- diag(rep(c(1, 0), c(m, n - m)))
    lambda <- eigen(M %*% A %*% M - sk$statistic * M, only.values = TRUE)$values
    lambda <- lambda[abs(lambda) > .Machine$double.eps]
    1 - CompQuadForm::imhof(0, lambda)$Qq                  # 0.6929

**Harrison-McCabe test, the p-value on samples drawn given the design.**

| Design | P-value | 1% | 5% | 10% |
| --- | --- | ---: | ---: | ---: |
| n = 40 | `alternative = "less"` | 0.011 | 0.051 | 0.101 |
| n = 40 | `alternative = "greater"` | 0.010 | 0.049 | 0.100 |
| n = 40 | `alternative = "two.sided"` | 0.010 | 0.050 | 0.100 |
| n = 40 | Null of `lmtest::hmctest()`, regressors ignored | 0.012 | 0.054 | 0.105 |
| n = 100 | `alternative = "less"` | 0.010 | 0.049 | 0.098 |
| n = 100 | `alternative = "greater"` | 0.010 | 0.049 | 0.099 |
| n = 100 | `alternative = "two.sided"` | 0.010 | 0.050 | 0.098 |
| n = 100 | Null of `lmtest::hmctest()`, regressors ignored | 0.011 | 0.052 | 0.103 |
| n = 400 | `alternative = "less"` | 0.011 | 0.052 | 0.102 |
| n = 400 | `alternative = "greater"` | 0.010 | 0.050 | 0.100 |
| n = 400 | `alternative = "two.sided"` | 0.010 | 0.051 | 0.102 |
| n = 400 | Null of `lmtest::hmctest()`, regressors ignored | 0.011 | 0.053 | 0.103 |
| n = 100, weighted | `alternative = "less"` | 0.010 | 0.050 | 0.100 |
| n = 100, weighted | `alternative = "greater"` | 0.010 | 0.049 | 0.099 |
| n = 100, weighted | `alternative = "two.sided"` | 0.010 | 0.049 | 0.099 |

Samples per design: 100000. Monte Carlo standard error at the 5% level: 0.0007.

**Harrison-McCabe test, size.**

| Errors | Test | n = 40 | n = 100 | n = 400 |
| --- | --- | ---: | ---: | ---: |
| Gaussian | `performHarrisonMcCabeTest()` | 0.047 | 0.043 | 0.052 |
| Gaussian | `performGQTest()` | 0.048 | 0.048 | 0.052 |
| Gaussian | `performSzroeterTest()` | 0.044 | 0.048 | 0.049 |
| Gaussian | `performGlejserTest(robust = TRUE)`, two-sided | 0.045 | 0.044 | 0.047 |
| t5 | `performHarrisonMcCabeTest()` | 0.117 | 0.142 | 0.163 |
| t5 | `performGQTest()` | 0.110 | 0.125 | 0.158 |
| t5 | `performSzroeterTest()` | 0.096 | 0.129 | 0.153 |
| t5 | `performGlejserTest(robust = TRUE)`, two-sided | 0.049 | 0.047 | 0.047 |
| Gaussian, weighted fit | `performHarrisonMcCabeTest()` | 0.048 | 0.050 | 0.053 |
| Gaussian, weighted fit | `performGQTest()` | 0.050 | 0.048 | 0.048 |
| Gaussian, weighted fit | `performSzroeterTest()` | 0.067 | 0.056 | 0.055 |
| Gaussian, weighted fit | `performGlejserTest(robust = TRUE)`, two-sided | 0.050 | 0.046 | 0.054 |

**Harrison-McCabe test, power.**

| Variance | Test | n = 40 | n = 100 |
| --- | --- | ---: | ---: |
| `sigma^2 = exp(0.4 x1)` | `performHarrisonMcCabeTest()` | 0.468 | 0.848 |
| `sigma^2 = exp(0.4 x1)` | `performGQTest()` | 0.491 | 0.881 |
| `sigma^2 = exp(0.4 x1)` | `performSzroeterTest()` | 0.530 | 0.915 |
| `sigma^2 = exp(0.4 x1)` | `performGlejserTest(robust = TRUE)`, two-sided | 0.367 | 0.805 |
| `sigma^2 = 1 + 0.15 x1^2` | `performHarrisonMcCabeTest()` | 0.422 | 0.787 |
| `sigma^2 = 1 + 0.15 x1^2` | `performGQTest()` | 0.447 | 0.835 |
| `sigma^2 = 1 + 0.15 x1^2` | `performSzroeterTest()` | 0.477 | 0.866 |
| `sigma^2 = 1 + 0.15 x1^2` | `performGlejserTest(robust = TRUE)`, two-sided | 0.311 | 0.730 |

Replications: 5000. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0031.

<!-- generated by make-table.R; do not edit the numbers by hand -->

On samples drawn given the design, 35 of the 36 rejection rates of the p-value fall inside the 99% interval around their level. Outside it: n = 400, `alternative = "less"`, 5% level, 0.0520 (2.8 standard errors). The null of `lmtest::hmctest()` rejects 0.054, 0.052, 0.053 of the time at the 5% level in the same samples (n = 40, 100, 400).

Of the 6 null cells of `performHarrisonMcCabeTest()` with Gaussian errors, with and without weights, 6 fall inside the release-gate interval [0.0421, 0.0579], at 0.043 to 0.053. Under t5 errors it rejects 0.117 to 0.163 of the time, `performGQTest()` 0.110 to 0.158, `performSzroeterTest()` 0.096 to 0.153 and `performGlejserTest(robust = TRUE)` 0.047 to 0.049. The asymptotic rate of the three one-sided tests under t5 errors is 0.205.

In the 4 power cells, `performGQTest()` has 2.3 to 4.8 percentage points more power than `performHarrisonMcCabeTest()`, and `performSzroeterTest()` 5.4 to 7.9.

Five things follow.

- **The p-value is the exact one.** On samples drawn given the design it
  rejects at its level at 1%, 5% and 10%, in each direction, with and without
  weights. One rate in 36 is outside its 99% interval, by 2.8 standard
  errors; were the 36 independent, chance would put at least one outside about
  30% of the time. In
  `tests/testthat/test-harrison-mccabe.R` the p-value equals the beta
  distribution function to `1e-10` in two designs where the statistic has
  one, and a reconstruction from the full projection matrix to `1e-8`.
- **The null of `lmtest::hmctest()` is close to the exact one in this
  design, and it is not the same.** It rejects 5.2% to 5.4% of the time at
  the 5% level on the samples above. On the four data sets of the previous
  table its p-value differs from the exact one by 0.002 to 0.013, which
  includes its own simulation error.
- **The test holds its level under Gaussian errors, with and without
  weights.** All six null cells are inside the release-gate interval, at
  0.043 to 0.053.
- **Under heavy tails it does not, and a larger sample makes it worse.** With
  `t5` errors it rejects 0.117 of the time at 40 observations and 0.163 at
  400, and the Goldfeld-Quandt and Szroeter tests do the same: 0.125 and
  0.129 at 100 observations, beside 0.131 and 0.126 in Pass A. All three
  statistics are linear in the squared errors to first order, so their
  variance under the null is proportional to the kurtosis of the errors less
  1, which is 8 for `t5` against 2 for normal errors, and each one-sided test
  at the 5% level tends to `pnorm(-qnorm(0.95) / 2)`, 0.205. The robust
  Glejser test on `x1` rejects 0.047 to 0.049 of the time under the same
  errors, and the help page sends the reader to it.
- **It is the least powerful of the three ordered tests here.** In each of
  the four power cells the Goldfeld-Quandt test has 2.3 to 4.8 percentage
  points more power and the Szroeter test 5.4 to 7.9. The statistic weights
  every observation in each half equally, where Szroeter's weights each by its
  rank, and the variance in these alternatives rises smoothly along `x1`. What
  the test adds to the interface is a p-value that is exact for any design and
  any split under normal errors.

One further result concerns `performSzroeterTest()` rather than this test. On
the weighted fit at 40 observations it rejects 0.067 of the time, outside the
release-gate interval, and 0.056 and 0.055 at 100 and 400. Its null
distribution does not depend on the design. In the weighted design the rows at
the start of the ordering, where `x1` is small, carry the largest weights and
have the largest leverages, so their residuals are the smallest and the
variance appears to rise along the ordering. The Harrison-McCabe and
Goldfeld-Quandt tests, whose null distributions use the weighted design, are
at 0.048 to 0.053 in the same cells. The finding is an open item in
`ROADMAP.md`.

### A consistent test of the variance function: a prototype

`performVarianceFormTest()` tests a variance function against the terms it
adds, and has little power against a departure those terms do not span.
Dette, Neumeyer and Van Keilegom (2007) test the same null hypothesis against
any smooth departure in one covariate. They standardize the residuals twice,
by a kernel estimate of the variance and by the fitted parametric variance
function smoothed with the same kernel, compare the two empirical distribution
functions by a Kolmogorov-Smirnov and a Cramér-von Mises distance, and take
the null distribution from a smooth residual bootstrap. This section is stage
1 of that item of the roadmap: a prototype in plain R, which the package does
not export, and a study of it. `variance-form-dnvk-design.md` defines the test
step by step, says what adopting it would take, and ends with a
recommendation. The published article could not be obtained, so the prototype
follows the authors' submitted manuscript of 2005, and the tables reproduced
below are the manuscript's.

**The manuscript's tables.** The prototype is run on the designs of Tables 4.1
and 4.5 of the manuscript as it describes them: `X` uniform on [0, 1],
standard normal errors, Nadaraya-Watson estimates of the mean and the variance
with `h = 0.3`, `B = 100` bootstrap samples with `v = 0.2`, the Cramér-von
Mises statistic, and its rejection rule (4.2). Table 4.1 tests constant
variance in three models and Table 4.5 the family `sigma^2 = 1 + theta x^2`;
`c = 0` is the null hypothesis in each. Each cell gives the rate here and, in
brackets, the manuscript's. The manuscript names no kernel and does not say
whether the resampled residuals are rescaled, so the script also runs the
Gaussian kernel and, for Table 4.5, residuals rescaled to unit variance. The
reproduction proper is the Epanechnikov kernel with the residuals as they are.

**Manuscript Table 4.1, constant variance, h = 0.3: here (manuscript).**

| Model | c | Level | n = 50 | n = 100 | n = 200 |
| --- | ---: | ---: | ---: | ---: | ---: |
| (I) | 0.0 | 2.5% | 0.040 (0.038) | 0.038 (0.038) | 0.042 (0.032) |
| (I) | 0.0 | 5.0% | 0.053 (0.058) | 0.058 (0.057) | 0.063 (0.052) |
| (I) | 0.0 | 10.0% | 0.100 (0.101) | 0.104 (0.103) | 0.116 (0.106) |
| (I) | 0.5 | 2.5% | 0.152 (0.136) | 0.324 (0.292) | 0.624 (0.564) |
| (I) | 0.5 | 5.0% | 0.204 (0.173) | 0.388 (0.352) | 0.696 (0.638) |
| (I) | 0.5 | 10.0% | 0.306 (0.246) | 0.509 (0.471) | 0.796 (0.736) |
| (I) | 1.0 | 2.5% | 0.469 (0.431) | 0.851 (0.822) | 0.995 (0.991) |
| (I) | 1.0 | 5.0% | 0.539 (0.502) | 0.887 (0.863) | 0.998 (0.997) |
| (I) | 1.0 | 10.0% | 0.664 (0.614) | 0.940 (0.922) | 1.000 (0.999) |
| (II) | 0.0 | 2.5% | 0.042 (0.033) | 0.039 (0.039) | 0.034 (0.037) |
| (II) | 0.0 | 5.0% | 0.062 (0.061) | 0.064 (0.054) | 0.052 (0.055) |
| (II) | 0.0 | 10.0% | 0.103 (0.109) | 0.110 (0.099) | 0.107 (0.109) |
| (II) | 0.5 | 2.5% | 0.237 (0.219) | 0.522 (0.497) | 0.955 (0.949) |
| (II) | 0.5 | 5.0% | 0.304 (0.279) | 0.620 (0.600) | 0.977 (0.968) |
| (II) | 0.5 | 10.0% | 0.431 (0.414) | 0.777 (0.740) | 0.995 (0.992) |
| (II) | 1.0 | 2.5% | 0.487 (0.466) | 0.811 (0.789) | 0.998 (0.988) |
| (II) | 1.0 | 5.0% | 0.548 (0.539) | 0.865 (0.838) | 1.000 (0.993) |
| (II) | 1.0 | 10.0% | 0.656 (0.650) | 0.931 (0.902) | 1.000 (0.999) |
| (III) | 0.0 | 2.5% | 0.044 (0.035) | 0.035 (0.038) | 0.047 (0.038) |
| (III) | 0.0 | 5.0% | 0.064 (0.057) | 0.051 (0.061) | 0.066 (0.049) |
| (III) | 0.0 | 10.0% | 0.113 (0.108) | 0.102 (0.099) | 0.116 (0.098) |
| (III) | 0.5 | 2.5% | 0.322 (0.285) | 0.650 (0.631) | 0.953 (0.948) |
| (III) | 0.5 | 5.0% | 0.388 (0.351) | 0.714 (0.700) | 0.969 (0.967) |
| (III) | 0.5 | 10.0% | 0.511 (0.471) | 0.799 (0.797) | 0.985 (0.984) |
| (III) | 1.0 | 2.5% | 0.728 (0.651) | 0.975 (0.959) | 1.000 (1.000) |
| (III) | 1.0 | 5.0% | 0.777 (0.708) | 0.985 (0.973) | 1.000 (1.000) |
| (III) | 1.0 | 10.0% | 0.854 (0.810) | 0.993 (0.991) | 1.000 (1.000) |

**Manuscript Table 4.5, sigma^2 = 1 + theta x^2, h = 0.3: here (manuscript).**

| Model | c | Level | n = 50 | n = 100 | n = 200 |
| --- | ---: | ---: | ---: | ---: | ---: |
| (VII) | 0.0 | 2.5% | 0.031 (0.034) | 0.038 (0.034) | 0.043 (0.023) |
| (VII) | 0.0 | 5.0% | 0.050 (0.055) | 0.058 (0.049) | 0.064 (0.095) |
| (VII) | 0.0 | 10.0% | 0.096 (0.110) | 0.114 (0.096) | 0.108 (0.093) |
| (VII) | 0.5 | 2.5% | 0.127 (0.200) | 0.314 (0.368) | 0.675 (0.662) |
| (VII) | 0.5 | 5.0% | 0.182 (0.248) | 0.387 (0.440) | 0.746 (0.723) |
| (VII) | 0.5 | 10.0% | 0.281 (0.337) | 0.516 (0.544) | 0.843 (0.827) |
| (VII) | 1.0 | 2.5% | 0.274 (0.383) | 0.690 (0.715) | 0.973 (0.956) |
| (VII) | 1.0 | 5.0% | 0.361 (0.456) | 0.766 (0.794) | 0.983 (0.976) |
| (VII) | 1.0 | 10.0% | 0.537 (0.604) | 0.870 (0.888) | 0.996 (0.990) |

**Manuscript tables, by the choices the manuscript does not state.**

| Choice | Table | Null rates within the interval | Alternative rates within the interval | Alternatives, here less the manuscript, percentage points |
| --- | --- | ---: | ---: | --- |
| as described | 4.1 | 27 of 27 | 43 of 54 | 2.3 on average, 0.0 to 7.7 |
| as described | 4.5 | 7 of 8 | 10 of 18 | -3.3 on average, -10.9 to 2.3 |
| Gaussian kernel | 4.1 | 17 of 27 | 10 of 54 | -7.1 on average, -69.3 to 15.2 |
| Gaussian kernel | 4.5 | 8 of 8 | 14 of 18 | -1.8 on average, -7.8 to 1.1 |
| unit-variance residuals | 4.5 | 8 of 8 | 11 of 18 | -3.1 on average, -8.9 to 2.0 |

Replications: 2000 here and 1000 in the manuscript, with B = 100 bootstrap samples and the rule (4.2). A rate is within the interval when it is within the 99% interval of the difference between two independent estimates.

<!-- generated by make-table.R; do not edit the numbers by hand -->

As described, of the 107 rates of the two tables, leaving out the misprinted one, 87 are within the 99% interval of the difference: 34 of the 35 under the null hypothesis (c = 0) and 53 of the 72 under the alternatives. Under the null hypothesis the rates here run from 0.050 to 0.066 at 5%, where the rule (4.2) with B = 100 rejects an exactly calibrated statistic 0.059 of the time (0.040 at 2.5%, 0.109 at 10%).
Under the alternatives of Table 4.1 the rate here is higher than the manuscript's in 51 of 54 cells.
Under the alternatives of Table 4.5 the rate here is higher than the manuscript's in 6 of 18 cells.

Outside the interval, as described:

- (I), c = 0.5, n = 50, 10.0% level: 0.306 here, 0.246 in the manuscript.
- (I), c = 1.0, n = 50, 10.0% level: 0.664 here, 0.614 in the manuscript.
- (I), c = 0.5, n = 200, 2.5% level: 0.624 here, 0.564 in the manuscript.
- (I), c = 0.5, n = 200, 5.0% level: 0.696 here, 0.638 in the manuscript.
- (I), c = 0.5, n = 200, 10.0% level: 0.796 here, 0.736 in the manuscript.
- (II), c = 1.0, n = 100, 10.0% level: 0.931 here, 0.902 in the manuscript.
- (II), c = 1.0, n = 200, 2.5% level: 0.998 here, 0.988 in the manuscript.
- (II), c = 1.0, n = 200, 5.0% level: 1.000 here, 0.993 in the manuscript.
- (III), c = 1.0, n = 50, 2.5% level: 0.728 here, 0.651 in the manuscript.
- (III), c = 1.0, n = 50, 5.0% level: 0.777 here, 0.708 in the manuscript.
- (III), c = 1.0, n = 50, 10.0% level: 0.854 here, 0.810 in the manuscript.
- (VII), c = 0.5, n = 50, 2.5% level: 0.127 here, 0.200 in the manuscript.
- (VII), c = 0.5, n = 50, 5.0% level: 0.182 here, 0.248 in the manuscript.
- (VII), c = 0.5, n = 50, 10.0% level: 0.281 here, 0.337 in the manuscript.
- (VII), c = 1.0, n = 50, 2.5% level: 0.274 here, 0.383 in the manuscript.
- (VII), c = 1.0, n = 50, 5.0% level: 0.361 here, 0.456 in the manuscript.
- (VII), c = 1.0, n = 50, 10.0% level: 0.537 here, 0.604 in the manuscript.
- (VII), c = 0.5, n = 100, 2.5% level: 0.314 here, 0.368 in the manuscript.
- (VII), c = 0.5, n = 100, 5.0% level: 0.387 here, 0.440 in the manuscript.
- (VII), c = 0.0, n = 200, 2.5% level: 0.043 here, 0.023 in the manuscript.

What follows from the reproduction:

- **The level is reproduced.** Under the null hypothesis 34 of the 35 rates
  are within the 99% interval of their difference from the manuscript's. Both
  sit a little above the nominal level at 5%, because the rule (4.2) with
  `B = 100` rejects an exactly calibrated statistic 0.059 of the time.
- **The power is close, and not reproduced to Monte Carlo accuracy.** 53 of
  the 72 rates under the alternatives are within the interval. Here the rate
  is higher than the manuscript's in 51 of the 54 alternative cells of Table
  4.1, by 2.3 percentage points on average and at most 7.7, and lower in
  Table 4.5 at 50 and 100 observations, by up to 10.9. A difference with a
  sign that holds across a table points to a detail the manuscript does not
  state, such as the kernel. It is not the Gaussian kernel, which puts the
  null rates of Table 4.1 too high (17 of 27 within the interval) and loses
  most of the power against the oscillating model (II); rescaling the
  residuals changes Table 4.5 little.
- **One cell of the manuscript is misprinted.** (VII) with `n = 200` and
  `c = 0` reads 0.095 at 5% and 0.093 at 10%. It is left out of the counts.

**Size and power in the package's designs.** Block 1 of
`variance-form-dnvk-size-power.R` runs the designs of
`variance-form-size-power.R`: `y = 1 + 2 x + 0.5 z + sigma(x) e`, with `x` and
`z` uniform on (1, 5), the model `y ~ x + z`, 50, 150 and 400 observations, and
Gaussian and `t5` errors. It adds a sixth true variance, a log-variance with a
sine wave of one period over the range of `x` on top of the exponential one.
The wave is antisymmetric about the middle of the range, so the terms
`performVarianceFormTest()` adds hardly span it; it was fixed before any result
was seen. The prototype takes the least-squares fit of the linear model in
place of the kernel mean, refits it to every bootstrap sample, smooths in `x`
with the Epanechnikov kernel and `h = 0.3 (n / 100)^(-0.3)` times the range of
`x`, and uses `B = 199` with the package's p-value `(1 + #) / (B + 1)`. Beside
it, on the same samples, run `performVarianceFormTest()` with its defaults, as
`variance-form-size-power.R` runs it, and with `var_formula = ~ x`, which is
the like-for-like comparison: one variable, tested against its square. The
twelve null cells with Gaussian errors decide criterion 2 of the release gate
and have 5000 replications; the others have 1000, where the 99% interval
around 0.05 is [0.032, 0.068]. Block 2 halves and doubles the constant of the
bandwidth at 150 observations.

**Prototype beside performVarianceFormTest(), size.**

| Form tested | True variance | Test | Gaussian, n = 50 | Gaussian, n = 150 | Gaussian, n = 400 | t5, n = 50 | t5, n = 150 | t5, n = 400 |
| --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| exponential | `sigma^2 = 1` | Cramér-von Mises, prototype | 0.027 | 0.047 | 0.049 | 0.039 | 0.056 | 0.076 |
| exponential | `sigma^2 = 1` | Kolmogorov-Smirnov, prototype | 0.007 | 0.023 | 0.027 | 0.013 | 0.027 | 0.041 |
| exponential | `sigma^2 = 1` | `performVarianceFormTest(var_formula = ~ x)` | 0.046 | 0.052 | 0.053 | 0.052 | 0.046 | 0.043 |
| exponential | `sigma^2 = 1` | `performVarianceFormTest()` | 0.042 | 0.050 | 0.051 | 0.052 | 0.047 | 0.052 |
| exponential | `sigma^2 = exp(0.6 x)` | Cramér-von Mises, prototype | 0.034 | 0.030 | 0.036 | 0.036 | 0.024 | 0.035 |
| exponential | `sigma^2 = exp(0.6 x)` | Kolmogorov-Smirnov, prototype | 0.013 | 0.015 | 0.026 | 0.016 | 0.014 | 0.027 |
| exponential | `sigma^2 = exp(0.6 x)` | `performVarianceFormTest(var_formula = ~ x)` | 0.054 | 0.053 | 0.050 | 0.035 | 0.038 | 0.043 |
| exponential | `sigma^2 = exp(0.6 x)` | `performVarianceFormTest()` | 0.053 | 0.050 | 0.050 | 0.070 | 0.039 | 0.048 |
| power | `sigma^2 = 1` | Cramér-von Mises, prototype | 0.026 | 0.044 | 0.051 | 0.026 | 0.064 | 0.088 |
| power | `sigma^2 = 1` | Kolmogorov-Smirnov, prototype | 0.007 | 0.021 | 0.029 | 0.009 | 0.034 | 0.054 |
| power | `sigma^2 = 1` | `performVarianceFormTest(var_formula = ~ x)` | 0.047 | 0.051 | 0.048 | 0.049 | 0.050 | 0.058 |
| power | `sigma^2 = 1` | `performVarianceFormTest()` | 0.051 | 0.060 | 0.053 | 0.063 | 0.043 | 0.070 |
| power | `sigma^2 = x^3` | Cramér-von Mises, prototype | 0.027 | 0.019 | 0.023 | 0.034 | 0.024 | 0.031 |
| power | `sigma^2 = x^3` | Kolmogorov-Smirnov, prototype | 0.015 | 0.014 | 0.020 | 0.020 | 0.020 | 0.021 |
| power | `sigma^2 = x^3` | `performVarianceFormTest(var_formula = ~ x)` | 0.035 | 0.042 | 0.053 | 0.027 | 0.031 | 0.039 |
| power | `sigma^2 = x^3` | `performVarianceFormTest()` | 0.043 | 0.049 | 0.051 | 0.042 | 0.053 | 0.051 |

**Prototype beside performVarianceFormTest(), power, Gaussian errors.**

| Form tested | True variance | Test | n = 50 | n = 150 | n = 400 |
| --- | --- | --- | ---: | ---: | ---: |
| exponential | `sigma^2 = x^3` | Cramér-von Mises, prototype | 0.045 | 0.104 | 0.248 |
| exponential | `sigma^2 = x^3` | Kolmogorov-Smirnov, prototype | 0.023 | 0.083 | 0.212 |
| exponential | `sigma^2 = x^3` | `performVarianceFormTest(var_formula = ~ x)` | 0.115 | 0.472 | 0.906 |
| exponential | `sigma^2 = x^3` | `performVarianceFormTest()` | 0.055 | 0.243 | 0.745 |
| exponential | `sigma^2 = 0.5 + x` | Cramér-von Mises, prototype | 0.029 | 0.056 | 0.102 |
| exponential | `sigma^2 = 0.5 + x` | Kolmogorov-Smirnov, prototype | 0.011 | 0.032 | 0.054 |
| exponential | `sigma^2 = 0.5 + x` | `performVarianceFormTest(var_formula = ~ x)` | 0.059 | 0.059 | 0.120 |
| exponential | `sigma^2 = 0.5 + x` | `performVarianceFormTest()` | 0.050 | 0.040 | 0.076 |
| exponential | `sigma^2 = exp(0.5 (x - 3)^2)` | Cramér-von Mises, prototype | 0.158 | 0.581 | 0.894 |
| exponential | `sigma^2 = exp(0.5 (x - 3)^2)` | Kolmogorov-Smirnov, prototype | 0.058 | 0.416 | 0.873 |
| exponential | `sigma^2 = exp(0.5 (x - 3)^2)` | `performVarianceFormTest(var_formula = ~ x)` | 0.679 | 0.996 | 1.000 |
| exponential | `sigma^2 = exp(0.5 (x - 3)^2)` | `performVarianceFormTest()` | 0.518 | 0.982 | 1.000 |
| exponential | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | Cramér-von Mises, prototype | 0.041 | 0.183 | 0.750 |
| exponential | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | Kolmogorov-Smirnov, prototype | 0.012 | 0.069 | 0.453 |
| exponential | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | `performVarianceFormTest(var_formula = ~ x)` | 0.057 | 0.075 | 0.089 |
| exponential | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | `performVarianceFormTest()` | 0.052 | 0.077 | 0.079 |
| power | `sigma^2 = exp(0.6 x)` | Cramér-von Mises, prototype | 0.035 | 0.010 | 0.024 |
| power | `sigma^2 = exp(0.6 x)` | Kolmogorov-Smirnov, prototype | 0.012 | 0.004 | 0.027 |
| power | `sigma^2 = exp(0.6 x)` | `performVarianceFormTest(var_formula = ~ x)` | 0.078 | 0.194 | 0.457 |
| power | `sigma^2 = exp(0.6 x)` | `performVarianceFormTest()` | 0.080 | 0.134 | 0.328 |
| power | `sigma^2 = 0.5 + x` | Cramér-von Mises, prototype | 0.027 | 0.022 | 0.024 |
| power | `sigma^2 = 0.5 + x` | Kolmogorov-Smirnov, prototype | 0.013 | 0.012 | 0.017 |
| power | `sigma^2 = 0.5 + x` | `performVarianceFormTest(var_formula = ~ x)` | 0.057 | 0.047 | 0.046 |
| power | `sigma^2 = 0.5 + x` | `performVarianceFormTest()` | 0.063 | 0.053 | 0.049 |
| power | `sigma^2 = exp(0.5 (x - 3)^2)` | Cramér-von Mises, prototype | 0.153 | 0.553 | 0.826 |
| power | `sigma^2 = exp(0.5 (x - 3)^2)` | Kolmogorov-Smirnov, prototype | 0.062 | 0.381 | 0.811 |
| power | `sigma^2 = exp(0.5 (x - 3)^2)` | `performVarianceFormTest(var_formula = ~ x)` | 0.597 | 0.988 | 1.000 |
| power | `sigma^2 = exp(0.5 (x - 3)^2)` | `performVarianceFormTest()` | 0.444 | 0.949 | 1.000 |
| power | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | Cramér-von Mises, prototype | 0.052 | 0.199 | 0.753 |
| power | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | Kolmogorov-Smirnov, prototype | 0.020 | 0.070 | 0.459 |
| power | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | `performVarianceFormTest(var_formula = ~ x)` | 0.055 | 0.078 | 0.132 |
| power | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | `performVarianceFormTest()` | 0.058 | 0.065 | 0.089 |

**Prototype beside performVarianceFormTest(), power, t5 errors.**

| Form tested | True variance | Test | n = 50 | n = 150 | n = 400 |
| --- | --- | --- | ---: | ---: | ---: |
| exponential | `sigma^2 = x^3` | Cramér-von Mises, prototype | 0.051 | 0.067 | 0.153 |
| exponential | `sigma^2 = x^3` | Kolmogorov-Smirnov, prototype | 0.027 | 0.058 | 0.139 |
| exponential | `sigma^2 = x^3` | `performVarianceFormTest(var_formula = ~ x)` | 0.071 | 0.196 | 0.518 |
| exponential | `sigma^2 = x^3` | `performVarianceFormTest()` | 0.041 | 0.070 | 0.332 |
| exponential | `sigma^2 = 0.5 + x` | Cramér-von Mises, prototype | 0.036 | 0.074 | 0.105 |
| exponential | `sigma^2 = 0.5 + x` | Kolmogorov-Smirnov, prototype | 0.012 | 0.040 | 0.063 |
| exponential | `sigma^2 = 0.5 + x` | `performVarianceFormTest(var_formula = ~ x)` | 0.041 | 0.045 | 0.066 |
| exponential | `sigma^2 = 0.5 + x` | `performVarianceFormTest()` | 0.043 | 0.047 | 0.050 |
| exponential | `sigma^2 = exp(0.5 (x - 3)^2)` | Cramér-von Mises, prototype | 0.100 | 0.362 | 0.654 |
| exponential | `sigma^2 = exp(0.5 (x - 3)^2)` | Kolmogorov-Smirnov, prototype | 0.030 | 0.204 | 0.595 |
| exponential | `sigma^2 = exp(0.5 (x - 3)^2)` | `performVarianceFormTest(var_formula = ~ x)` | 0.503 | 0.904 | 0.991 |
| exponential | `sigma^2 = exp(0.5 (x - 3)^2)` | `performVarianceFormTest()` | 0.376 | 0.807 | 0.982 |
| exponential | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | Cramér-von Mises, prototype | 0.039 | 0.102 | 0.324 |
| exponential | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | Kolmogorov-Smirnov, prototype | 0.015 | 0.049 | 0.184 |
| exponential | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | `performVarianceFormTest(var_formula = ~ x)` | 0.055 | 0.050 | 0.072 |
| exponential | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | `performVarianceFormTest()` | 0.067 | 0.061 | 0.054 |
| power | `sigma^2 = exp(0.6 x)` | Cramér-von Mises, prototype | 0.029 | 0.012 | 0.010 |
| power | `sigma^2 = exp(0.6 x)` | Kolmogorov-Smirnov, prototype | 0.008 | 0.005 | 0.007 |
| power | `sigma^2 = exp(0.6 x)` | `performVarianceFormTest(var_formula = ~ x)` | 0.071 | 0.114 | 0.249 |
| power | `sigma^2 = exp(0.6 x)` | `performVarianceFormTest()` | 0.079 | 0.101 | 0.199 |
| power | `sigma^2 = 0.5 + x` | Cramér-von Mises, prototype | 0.019 | 0.039 | 0.059 |
| power | `sigma^2 = 0.5 + x` | Kolmogorov-Smirnov, prototype | 0.011 | 0.021 | 0.044 |
| power | `sigma^2 = 0.5 + x` | `performVarianceFormTest(var_formula = ~ x)` | 0.037 | 0.049 | 0.056 |
| power | `sigma^2 = 0.5 + x` | `performVarianceFormTest()` | 0.050 | 0.043 | 0.062 |
| power | `sigma^2 = exp(0.5 (x - 3)^2)` | Cramér-von Mises, prototype | 0.108 | 0.337 | 0.596 |
| power | `sigma^2 = exp(0.5 (x - 3)^2)` | Kolmogorov-Smirnov, prototype | 0.051 | 0.208 | 0.535 |
| power | `sigma^2 = exp(0.5 (x - 3)^2)` | `performVarianceFormTest(var_formula = ~ x)` | 0.415 | 0.840 | 0.986 |
| power | `sigma^2 = exp(0.5 (x - 3)^2)` | `performVarianceFormTest()` | 0.285 | 0.703 | 0.962 |
| power | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | Cramér-von Mises, prototype | 0.062 | 0.129 | 0.380 |
| power | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | Kolmogorov-Smirnov, prototype | 0.021 | 0.067 | 0.229 |
| power | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | `performVarianceFormTest(var_formula = ~ x)` | 0.049 | 0.062 | 0.080 |
| power | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | `performVarianceFormTest()` | 0.046 | 0.046 | 0.061 |

**Prototype, constant of the bandwidth, Gaussian errors, n = 150.**

| Form tested | True variance | Hypothesis | Test | c = 0.15 | c = 0.30 | c = 0.60 |
| --- | --- | --- | --- | ---: | ---: | ---: |
| exponential | `sigma^2 = 1` | null | Cramér-von Mises, prototype | 0.046 | 0.047 | 0.048 |
| exponential | `sigma^2 = 1` | null | Kolmogorov-Smirnov, prototype | 0.029 | 0.023 | 0.019 |
| exponential | `sigma^2 = exp(0.6 x)` | null | Cramér-von Mises, prototype | 0.031 | 0.030 | 0.007 |
| exponential | `sigma^2 = exp(0.6 x)` | null | Kolmogorov-Smirnov, prototype | 0.014 | 0.015 | 0.003 |
| exponential | `sigma^2 = exp(0.5 (x - 3)^2)` | alternative | Cramér-von Mises, prototype | 0.551 | 0.581 | 0.601 |
| exponential | `sigma^2 = exp(0.5 (x - 3)^2)` | alternative | Kolmogorov-Smirnov, prototype | 0.385 | 0.416 | 0.397 |
| exponential | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | alternative | Cramér-von Mises, prototype | 0.259 | 0.183 | 0.095 |
| exponential | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | alternative | Kolmogorov-Smirnov, prototype | 0.124 | 0.069 | 0.036 |
| power | `sigma^2 = 1` | null | Cramér-von Mises, prototype | 0.053 | 0.044 | 0.040 |
| power | `sigma^2 = 1` | null | Kolmogorov-Smirnov, prototype | 0.021 | 0.021 | 0.023 |
| power | `sigma^2 = x^3` | null | Cramér-von Mises, prototype | 0.030 | 0.019 | 0.010 |
| power | `sigma^2 = x^3` | null | Kolmogorov-Smirnov, prototype | 0.029 | 0.014 | 0.004 |
| power | `sigma^2 = exp(0.5 (x - 3)^2)` | alternative | Cramér-von Mises, prototype | 0.528 | 0.553 | 0.477 |
| power | `sigma^2 = exp(0.5 (x - 3)^2)` | alternative | Kolmogorov-Smirnov, prototype | 0.386 | 0.381 | 0.311 |
| power | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | alternative | Cramér-von Mises, prototype | 0.279 | 0.199 | 0.141 |
| power | `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))` | alternative | Kolmogorov-Smirnov, prototype | 0.125 | 0.070 | 0.052 |

Replications: 1000 and 5000, with B = 199 bootstrap samples for the prototype. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0069 and 0.0031.

<!-- generated by make-table.R; do not edit the numbers by hand -->

- Cramér-von Mises, prototype: 4 of the 12 Gaussian null cells inside the 99% interval at their replications (rates 0.019 to 0.051); 6 of the 12 t5 null cells (rates 0.024 to 0.088).
- Kolmogorov-Smirnov, prototype: 0 of the 12 Gaussian null cells inside the 99% interval at their replications (rates 0.007 to 0.029); 3 of the 12 t5 null cells (rates 0.009 to 0.054).
- `performVarianceFormTest(var_formula = ~ x)`: 10 of the 12 Gaussian null cells inside the 99% interval at their replications (rates 0.035 to 0.054); 10 of the 12 t5 null cells (rates 0.027 to 0.058).
- `performVarianceFormTest()`: 10 of the 12 Gaussian null cells inside the 99% interval at their replications (rates 0.042 to 0.060); 10 of the 12 t5 null cells (rates 0.039 to 0.070).

Gaussian null cells of the Cramér-von Mises prototype outside that interval:

- exponential form, `sigma^2 = 1`, n = 50: 0.0274.
- power form, `sigma^2 = 1`, n = 50: 0.0264.
- exponential form, `sigma^2 = exp(0.6 x)`, n = 50: 0.0337.
- power form, `sigma^2 = x^3`, n = 50: 0.0266.
- exponential form, `sigma^2 = exp(0.6 x)`, n = 150: 0.0304.
- power form, `sigma^2 = x^3`, n = 150: 0.0194.
- exponential form, `sigma^2 = exp(0.6 x)`, n = 400: 0.0360.
- power form, `sigma^2 = x^3`, n = 400: 0.0226.

In the 48 alternative cells of block 1 the Cramér-von Mises prototype rejects more often than `performVarianceFormTest(var_formula = ~ x)` in 12; the difference runs from -65.8 to 66.1 percentage points. By true variance:

- `sigma^2 = exp(0.6 x)`: -43.3 to -4.2 points.
- `sigma^2 = x^3`: -65.8 to -2.0 points.
- `sigma^2 = 0.5 + x`: -3.0 to 3.9 points.
- `sigma^2 = exp(0.5 (x - 3)^2)`: -54.2 to -10.6 points.
- `sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))`: -1.6 to 66.1 points.

The variance function had no least-squares fit, so that the prototype could not be computed, in 761 of the 136000 samples: at most 0.9% of the samples of a null cell and 5.2% of those of any cell (exponential form, `sigma^2 = exp(0.5 (x - 3)^2)`, t5 errors, n = 400). It had none in 0.26% of all bootstrap samples, at most 1.37% in a cell (exponential form, `sigma^2 = exp(0.5 (x - 3)^2)`, t5 errors, n = 150); those samples are left out of the bootstrap distribution.

**Where the conservatism comes from.** `variance-form-dnvk-bootstrap-check.R`
bootstraps the Gaussian null cells at 150 and 400 observations twice, on the
same samples and with the same random numbers: from the least-squares fit of
the variance function, as the prototype does, and from the true shape of the
variance function scaled to the fitted level, which is not available in
practice. The slope is `b` in `exp(a + b x)` or in `a x^b`.

**Prototype, bootstrap data from the fitted and from the true variance function.**

| Form tested | True variance | n | True slope | Mean fitted slope | Mean T | Mean T*, fitted | Mean T*, true shape | Size, fitted | Size, true shape |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| exponential | `sigma^2 = 1` | 150 | 0.0 | 0.005 | 0.0103 | 0.0105 | 0.0103 | 0.044 | 0.052 |
| exponential | `sigma^2 = exp(0.6 x)` | 150 | 0.6 | 0.646 | 0.0638 | 0.1083 | 0.0799 | 0.033 | 0.037 |
| exponential | `sigma^2 = 1` | 400 | 0.0 | -0.001 | 0.0068 | 0.0069 | 0.0069 | 0.043 | 0.046 |
| exponential | `sigma^2 = exp(0.6 x)` | 400 | 0.6 | 0.607 | 0.0299 | 0.0456 | 0.0332 | 0.024 | 0.051 |
| power | `sigma^2 = 1` | 150 | 0.0 | 0.011 | 0.0103 | 0.0109 | 0.0104 | 0.036 | 0.043 |
| power | `sigma^2 = x^3` | 150 | 3.0 | 3.419 | 0.2124 | 0.2780 | 0.2161 | 0.015 | 0.047 |
| power | `sigma^2 = 1` | 400 | 0.0 | 0.007 | 0.0071 | 0.0070 | 0.0070 | 0.050 | 0.050 |
| power | `sigma^2 = x^3` | 400 | 3.0 | 3.088 | 0.1636 | 0.2118 | 0.1661 | 0.015 | 0.047 |

Replications: 1000, with B = 199 bootstrap samples drawn from the same random numbers in both schemes. Gaussian errors.

What follows:

- **The prototype does not hold its level when the variance function is not
  constant.** With Gaussian errors, 4 of its 12 null cells are inside the
  release-gate interval [0.042, 0.058], all four with constant variance at 150
  and 400 observations, at 0.044 to 0.051. Under `exp(0.6 x)` and `x^3` it
  rejects 0.019 to 0.036 of the time at every sample size, still 0.036 and
  0.023 at 400 observations, and at 50 observations every null cell is at
  0.026 to 0.034. The Kolmogorov-Smirnov statistic is inside in no
  cell: its values lie on a lattice, the bootstrap often reproduces the
  observed value, and a p-value that counts ties as exceedances is
  conservative.
- **The cause is the fitted variance function the bootstrap data come from.**
  The statistic's null distribution depends on the variance function. Where
  the variance is not constant, the mean bootstrap statistic exceeds the mean
  observed one by 29% to 70%, and the prototype rejects too seldom. Generated
  from the true shape on the same samples, the bootstrap statistics come
  within 2% of the observed ones for `x^3` and 11% for `exp(0.6 x)` at 400
  observations, and the rejection rate rises from 0.015 to 0.047 in both cells
  of the power form and from 0.024 to 0.051 in that of the exponential form.
  For the exponential form at 150 observations the true shape leaves a gap of
  25% and moves the rate only from 0.033 to 0.037, so the fit is not the whole
  story there.
- **Heavy tails move it the other way.** With `t5` errors and constant
  variance it rejects 0.076 and 0.088 of the time at 400 observations.
- **Its power is where a consistent test should have it, and little
  elsewhere.** Against the wave, with Gaussian errors at 400 observations, it
  rejects 0.750 and 0.753 of the time, where
  `performVarianceFormTest(var_formula = ~ x)` rejects 0.089 and 0.132.
  Against the two log-linear forms tested against each other and against the
  quadratic log-variance it has less power than that test, by 2.0 to 65.8
  percentage points: 0.581 against 0.996 for the quadratic log-variance at 150
  observations, and 0.024 against 0.457 when the power form is tested against
  `exp(0.6 x)` at 400. Part of that is its conservatism. Against
  `sigma^2 = 0.5 + x`, which neither test detects well, the two are within 4
  points of each other. A smaller bandwidth raises the power against the
  wave, 0.259 and 0.279 at half the constant against 0.183 and 0.199, and a
  larger one makes the null cells with a non-constant variance more
  conservative still, 0.007 and 0.010.
- **`performVarianceFormTest()` in a second run of its own designs.** Its
  twelve Gaussian null cells here repeat those of
  `variance-form-size-power.csv` on new samples, at 5000 replications. At its
  defaults ten are inside the interval. Just outside are the exponential form
  under constant variance at 50 observations, 0.0420, and the power form under
  constant variance at 150, 0.0596, where the earlier run gave 0.048 and 0.052.
  With `var_formula = ~ x`, which no earlier study measured, the power form
  under `x^3` rejects 0.0348 of the time at 50 observations and 0.0416 at 150.
  This stage changes nothing in that function; the numbers are recorded for
  the author.

Stage 1 ends here. `variance-form-dnvk-design.md` gives the recommendation,
not to ship the test as the manuscript defines it, and the two ways forward;
`ROADMAP.md` records the result.

### The multiplier of the wild bootstrap

`performWildBootstrapTest()` resamples the leverage-standardised, centred
residuals with replacement and multiplies each by a draw `v` from the
distribution `distribution` names. Rademacher's is `-1` or `1` with equal
probability; Mammen's is `(1 - sqrt(5)) / 2` with probability
`(sqrt(5) + 1) / (2 sqrt(5))` and `(1 + sqrt(5)) / 2` otherwise. Both have
`E v = 0` and `E v^2 = 1`. Rademacher's has `E v^3 = 0` and `E v^4 = 1`: its
bootstrap errors are symmetric, and their squares are the squares of the
resampled residuals. Mammen's has `E v^3 = 1` and `E v^4 = 2`: its bootstrap
errors keep the skewness of the residuals, and their squares are more
dispersed than those of the residuals. The statistic is Koenker's, `n R^2` of
the squared residuals on the regressors, so the question is which of the two
matters more. The default is `"rademacher"`, and the roadmap asks for a size
study at small `n` with skewed errors to settle it.

The design is the cross-sectional one of Pass A,
`y = 1 + 2 x1 + 0.5 x2 + sigma e`, with `x1` uniform on (1, 5), `x2` standard
normal and the model `lm(y ~ x1 + x2)`, at 30, 50 and 100 observations. The
errors have mean 0 and variance 1: Gaussian, `t5`, and a centred exponential,
`rexp(n) - 1`, with skewness 2. The exponential is the skewed distribution
because skewness is what separates the two multipliers, and in the Glejser
study above it is the error under which the corrected statistic was outside
the gate at 50 observations; the lognormal of that study adds tails heavy
enough to test the statistic rather than the multiplier. Each sample is
tested with both multipliers, so the two rates are paired, and the
difference between them is measured more precisely than either rate. The
size cells run at 5000 replications and the power cells, against
`sigma^2 = exp(0.4 x1)` (the `power_exp` alternative of Pass A) with
Gaussian errors, at 1000. Every test uses `B = 999`.

With `B = 999` a test whose level is exact gives `p < 0.05` with probability
0.049. Each size cell is judged against the release gate, `[0.042, 0.058]`,
and against the 99% interval around 0.049 at 5000 replications,
`[0.041, 0.057]`; the author approved that second reference for these cells
on 2026-10-15. The decision rule was written into the script before the full
run started; the script was first committed with its results. The default
should be the multiplier inside the interval in more of the nine size cells,
under each of the two intervals, provided that in at least one cell where
only it is inside, the paired difference of the two rates is outside its 99%
Monte Carlo interval. Otherwise neither is better within Monte Carlo error,
and the default stays.

**Wild bootstrap, size by multiplier.**

| Errors | n | Rademacher | Mammen | Mammen - Rademacher |
| --- | ---: | ---: | ---: | ---: |
| Gaussian | 30 | 0.0442 [0.0367, 0.0517] | 0.0430 [0.0356, 0.0504] | -0.0012 [-0.0046, +0.0022] |
| t5 | 30 | 0.0472 [0.0395, 0.0549] | 0.0460 [0.0384, 0.0536] | -0.0012 [-0.0043, +0.0019] |
| exponential | 30 | 0.0820 [0.0720, 0.0920] | 0.0782 [0.0684, 0.0880] | -0.0038 [-0.0079, +0.0003] |
| Gaussian | 50 | 0.0476 [0.0398, 0.0554] | 0.0508 [0.0428, 0.0588] | +0.0032 [-0.0002, +0.0066] |
| t5 | 50 | 0.0444 [0.0369, 0.0519] | 0.0454 [0.0378, 0.0530] | +0.0010 [-0.0021, +0.0041] |
| exponential | 50 | 0.0734 [0.0639, 0.0829] | 0.0698 [0.0605, 0.0791] | -0.0036 [-0.0075, +0.0003] |
| Gaussian | 100 | 0.0482 [0.0404, 0.0560] | 0.0488 [0.0410, 0.0566] | +0.0006 [-0.0026, +0.0038] |
| t5 | 100 | 0.0492 [0.0413, 0.0571] | 0.0476 [0.0398, 0.0554] | -0.0016 [-0.0050, +0.0018] |
| exponential | 100 | 0.0608 [0.0521, 0.0695] | 0.0586 [0.0500, 0.0672] | -0.0022 [-0.0057, +0.0013] |

Rejection rates at the 5% level with their 99% Monte Carlo intervals. Replications: 5000 per cell, both multipliers on each sample. B = 999.

**Wild bootstrap, power against `sigma^2 = exp(0.4 x1)` by multiplier.**

| Errors | n | Rademacher | Mammen | Mammen - Rademacher |
| --- | ---: | ---: | ---: | ---: |
| Gaussian | 30 | 0.1940 [0.1618, 0.2262] | 0.1880 [0.1562, 0.2198] | -0.0060 [-0.0191, +0.0071] |
| Gaussian | 50 | 0.3610 [0.3219, 0.4001] | 0.3580 [0.3189, 0.3971] | -0.0030 [-0.0159, +0.0099] |
| Gaussian | 100 | 0.7450 [0.7095, 0.7805] | 0.7490 [0.7137, 0.7843] | +0.0040 [-0.0086, +0.0166] |

Gaussian errors. Rejection rates at the 5% level with their 99% Monte Carlo intervals. Replications: 1000 per cell, both multipliers on each sample. B = 999.

<!-- generated by make-table.R; do not edit the numbers by hand -->

Against the release gate, the 99% interval around 0.05, [0.0421, 0.0579], Rademacher's multiplier is inside in 6 of the 9 size cells and Mammen's in 6. Rademacher is outside in exponential errors, n = 30 (0.0820); exponential errors, n = 50 (0.0734); exponential errors, n = 100 (0.0608). Mammen is outside in exponential errors, n = 30 (0.0782); exponential errors, n = 50 (0.0698); exponential errors, n = 100 (0.0586).

Against the 99% interval around 0.049, the rate of an exact test with B = 999, [0.0411, 0.0569], Rademacher's multiplier is inside in 6 of the 9 size cells and Mammen's in 6. Rademacher is outside in exponential errors, n = 30 (0.0820); exponential errors, n = 50 (0.0734); exponential errors, n = 100 (0.0608). Mammen is outside in exponential errors, n = 30 (0.0782); exponential errors, n = 50 (0.0698); exponential errors, n = 100 (0.0586).

Neither multiplier holds the level in more of the grid by more than Monte Carlo error under both intervals, so by the rule fixed in the script the default, Rademacher, stays. The paired difference, Mammen less Rademacher, is -0.0038 to +0.0032 across the size cells, and its 99% Monte Carlo interval excludes zero in 0 of the 9.

Against `sigma^2 = exp(0.4 x1)` the paired difference in power, Mammen less Rademacher, is -0.006, -0.003, +0.004 at n = 30, 50, 100.

Every one of the 96,000 tests returned a p-value. Five things follow.

- **Neither multiplier holds the level in more cells, and no paired
  difference is outside its 99% interval, so by the rule the default stays
  Rademacher.** The two are inside the release gate in the same six of the
  nine size cells and outside it in the same three, the three with
  exponential errors; the interval around 0.049 gives the same split. In all
  nine cells the two are within Monte Carlo error of each other. Under
  exponential errors Mammen's rate is lower in all three cells, each
  difference inside its 99% interval (next bullet).
- **Under symmetric errors both hold the level from 30 observations.** With
  Gaussian and `t5` errors Rademacher's multiplier rejects 0.0442 to 0.0492
  of the time and Mammen's 0.0430 to 0.0508, all twelve cells inside both
  intervals.
- **Under skewed errors neither holds it, at any of the three sizes.**
  Rademacher's multiplier rejects 0.0820, 0.0734 and 0.0608 of the time at
  30, 50 and 100 observations, Mammen's 0.0782, 0.0698 and 0.0586. Mammen's
  is lower in all three, by 0.22 to 0.38 percentage points, the direction its
  third moment would suggest; but each difference is 1.6 to 2.4 of its
  standard errors, inside the 99% interval, and it brings no cell inside
  either interval. Both rates fall with `n` and are still outside at 100.
- **Neither multiplier brings the level under skewed errors closer to 5%
  than the asymptotic test.** `performWildBootstrapTest()` computes Koenker's
  statistic, the one `performKoenkerTest()` refers to the chi-squared
  distribution. At 50 observations in the same design with the same
  exponential errors, `glejser-skewed-errors.csv` has `performKoenkerTest()`
  at 0.0628, against 0.0734 and 0.0698 for the two multipliers here, on other
  samples: 2.1 and 1.4 standard errors of the difference above it. With
  Gaussian errors the three are at 0.0462, 0.0476 and 0.0508, and with `t5`
  errors at 0.0450, 0.0444 and 0.0454. The help page said the bootstrap
  controls the size for heavy-tailed errors. That holds for the `t5` errors
  here and not for the skewed ones, and the page now says so.
- **Power does not separate them either.** Against `sigma^2 = exp(0.4 x1)`
  the two reject 0.194 and 0.188 of the time at 30 observations, 0.361 and
  0.358 at 50, and 0.745 and 0.749 at 100. Each paired difference is inside
  its 99% interval at 1000 replications. A tie on size broken by power would
  have nothing to go on.

This study changes no value: the default of `distribution` is `"rademacher"`
before and after it.

### Fixed effects for small groups

`fixed-effects-size.R` measures White's, Koenker's and the Breusch-Pagan tests
on fits with fixed effects for small groups: `lm(y ~ x + subject)` with G
subjects of T observations, `y = 1 + 0.5 x + a_i + e`, with `x`, the subject
effects `a_i` and the errors `e` standard normal. The errors are
homoscedastic, so every rejection is a false one. The grid crosses G = 5, 10,
20, 30, 50, 100 and 200 with T = 2, 3, 4, 5, 6, 8, 10, 15, 20 and 30, at 2000
replications per cell (`fixed-effects-size.csv`), and again at 10,000 on the
same seeds (`fixed-effects-size-10000.csv`), used only to recount the rules.
Three more blocks vary the design: unbalanced groups, two regressors, and `x`
correlated with the subject effects (r = 0.71). The statistics are computed on
matrices of replications by the within transformation, and the first five
replications of every cell are fitted with `lm()` and given to the three
functions: of those 1335 calls, the 1285 the package computed agree with the
script to 1.2e-14 relative on the statistic and 1.7e-14 on the p-value, and
the 50 it refused are those the script records as refused or not run. The
study ran on 0.15.0, before the warning; with 2000 replications the Monte
Carlo standard error at 5% is 0.49 points.

**Size at 5%, in %, of G subjects of T observations.** Bold: outside
[2.5%, 7.5%]. n/r: not run (T = 2, an exact fit White's test refuses). ref:
refused by the package (fewer than 20 observations for White, 15 for Koenker
and Breusch-Pagan).

White:

| G \ T | 2 | 3 | 4 | 5 | 6 | 8 | 10 | 15 | 20 | 30 |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 5 | n/r | ref | **1.0** | 3.7 | 5.5 | 6.2 | 5.5 | 7.1 | 5.2 | 5.8 |
| 10 | n/r | **0.0** | 5.2 | 6.6 | 6.4 | **7.8** | 6.4 | 7.3 | 6.6 | 5.8 |
| 20 | n/r | **18.8** | **14.0** | **11.3** | **9.5** | **8.4** | **7.9** | 5.8 | 6.0 | 6.3 |
| 30 | n/r | **50.7** | **22.2** | **17.8** | **13.8** | **11.3** | **10.1** | **7.8** | 5.9 | 6.2 |
| 50 | n/r | **88.3** | **40.5** | **25.3** | **20.1** | **14.8** | **11.6** | **8.3** | **7.8** | 7.0 |
| 100 | n/r | **99.9** | **74.7** | **47.5** | **33.4** | **23.8** | **18.1** | **11.2** | **10.7** | 7.4 |
| 200 | n/r | **100.0** | **97.8** | **79.1** | **59.5** | **36.0** | **28.4** | **16.0** | **12.6** | **9.1** |

Koenker:

| G \ T | 2 | 3 | 4 | 5 | 6 | 8 | 10 | 15 | 20 | 30 |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 5 | ref | **0.9** | 5.4 | 4.5 | 5.4 | 5.3 | 5.1 | 5.5 | 4.8 | 5.0 |
| 10 | **100.0** | **2.2** | **8.8** | 7.4 | **7.8** | 5.9 | 6.2 | 6.3 | 6.5 | 5.2 |
| 20 | **100.0** | **23.1** | **14.8** | **10.8** | **9.9** | **8.8** | **8.2** | 6.8 | 6.4 | 6.3 |
| 30 | **100.0** | **57.3** | **23.9** | **18.7** | **14.0** | **11.3** | **10.3** | 7.4 | 5.9 | 6.2 |
| 50 | **100.0** | **89.9** | **42.0** | **26.5** | **20.5** | **15.0** | **12.2** | **9.1** | **8.0** | 7.0 |
| 100 | **100.0** | **99.9** | **75.6** | **47.9** | **33.9** | **23.8** | **18.4** | **11.7** | **10.6** | **7.5** |
| 200 | **100.0** | **100.0** | **97.6** | **79.5** | **59.9** | **36.4** | **28.4** | **16.1** | **12.7** | **9.1** |

Breusch-Pagan:

| G \ T | 2 | 3 | 4 | 5 | 6 | 8 | 10 | 15 | 20 | 30 |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 5 | ref | **7.6** | 7.0 | 6.7 | 6.8 | 6.1 | 6.8 | 5.4 | 5.5 | 5.3 |
| 10 | **25.8** | **15.2** | **12.8** | **11.5** | **10.3** | **8.0** | 7.3 | 6.6 | 6.3 | 6.3 |
| 20 | **47.9** | **28.5** | **21.1** | **16.7** | **14.4** | **12.0** | **11.1** | **8.6** | **8.0** | 6.8 |
| 30 | **67.5** | **39.6** | **28.7** | **22.9** | **17.0** | **14.1** | **12.4** | **9.5** | **8.0** | 7.1 |
| 50 | **83.9** | **54.9** | **38.6** | **29.8** | **25.4** | **17.9** | **13.2** | **11.8** | **9.6** | **7.5** |
| 100 | **97.5** | **79.4** | **60.7** | **45.8** | **36.1** | **25.2** | **21.3** | **12.9** | **12.6** | **8.7** |
| 200 | **100.0** | **96.5** | **82.5** | **65.8** | **53.5** | **38.4** | **30.7** | **19.1** | **14.1** | **10.8** |

<!-- generated by analyse-fixed-effects-size.R; do not edit the numbers by hand -->

`analyse-fixed-effects-size.R` prints these tables, those at the 1% and 10%
levels, every cell with its degrees of freedom, the variants, the mean
statistic, and the search for a rule.

**Where the excess comes from.** `fixed-effects-dummies.R` splits the
auxiliary regression of Koenker's and the Breusch-Pagan statistics, on the
same 2000 replications per design, into the regression of the squared
residuals on the subject dummies and the regression on `x` alone:

| G | T | Koenker | dummies only | `x` only | Breusch-Pagan | dummies only | `x` only |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 30 | 3 | 52.7 | 57.8 | 5.4 | 36.8 | 37.8 | 4.4 |
| 100 | 3 | 100.0 | 100.0 | 4.7 | 78.3 | 79.1 | 4.3 |
| 100 | 4 | 75.4 | 76.0 | 5.5 | 59.6 | 59.8 | 5.3 |
| 200 | 5 | 79.2 | 80.2 | 5.5 | 67.6 | 67.4 | 5.6 |
| 30 | 8 | 9.8 | 10.4 | 5.0 | 14.0 | 13.9 | 5.1 |
| 100 | 8 | 24.1 | 24.2 | 4.6 | 27.1 | 27.2 | 4.8 |
| 200 | 30 | 9.5 | 9.6 | 5.5 | 10.8 | 10.6 | 5.4 |

Rejection at the 5% level, in %; `fixed-effects-dummies.csv`.

Six things follow.

- **The three tests over-reject with many small groups, and the size goes to
  1 as the groups grow at a fixed size.** On 30 subjects of three
  observations they reject in 50.7%, 57.3% and 39.6% of the draws, on 100 of
  eight in 23.8%, 23.8% and 25.2%, and on 200 of four in 97.8%, 97.6% and
  82.5%. At a fixed number of subjects the size goes to 5% as each is
  observed more often: with 5 subjects every T from 4 on is inside the band
  for Koenker's test.
- **The excess comes from the dummies.** Regressed on `x` alone, the squared
  residuals are rejected in 4.6% to 5.5% of the draws by Koenker's statistic
  and in 4.3% to 5.6% by the Breusch-Pagan statistic, and on the dummies alone
  as often as on both. Under normal errors the squared residuals of a subject
  of T observations, taken from its own mean, sum to `sigma^2` times a
  chi-squared with T - 1 degrees of freedom, so their subject means vary
  T / (T - 1) times as much as the auxiliary regression assumes. Koenker's
  statistic has mean `df + (G - 1) / (T - 1)`, within 2.3% over the 63 cells
  with T of 3 or more, while the spread of its reference distribution grows as
  `sqrt(2 df)`: the excess grows with G at a fixed T.
- **No rule on G, T, n or df / n alone separates the designs.** The best
  single threshold misclassifies 7 to 21 of the 62 to 69 cells of a test,
  because the size depends on both: at T = 30, Koenker's test rejects 5.0% of
  the time with 5 subjects and 9.1% with 200.
- **A rule on (G - 1) / (T - 1), the expected excess, does.** For the three
  tests together the best threshold, 1.14, splits the grid as G <= T does,
  and misclassifies 1 of the 69 cells in each run: the Breusch-Pagan test on
  20 subjects of 20, at 8.0%, in the run of 2000, and on 10 of 10, at 7.9%, in
  the run of 10,000. Fitted to one test, the threshold is 1.74 to 1.90 for
  Koenker's (1 or 2 cells) and 1.94 for White's (4 or 5, most of them where
  White's auxiliary regression holds the products, below).
- **The variants follow the same pattern,** with the mean number of
  observations per subject, n / G, in place of T: unbalanced groups, a second
  regressor, and `x` correlated with the subject effects, to which Koenker's
  and the Breusch-Pagan tests are invariant. On the unbalanced designs of 10 to
  30 observations per subject, Koenker's test rejects in 6.2%, 8.9% and 10.9%
  with 30, 100 and 200 subjects, against 5.9%, 10.6% and 12.7% for 20 each.
- **White's test with the products is a different test.** With at most
  `max_interactions` regressors, here 10 subjects or fewer, its auxiliary
  regression holds the products of `x` with the dummies, a slope for each
  subject, and 2G degrees of freedom. On 5 subjects of 4 it rejects in 1.0% of
  the draws, too rarely, and on 10 of 3 in none.

Decided by the author on 2026-10-23: White's, Koenker's and the Breusch-Pagan
tests warn when a term of the model that holds a factor has L levels among
the n observations of the fit with L > n / L, that is L^2 > n, which is
G > T on a balanced design, with their values unchanged (the help pages,
under *Fixed effects for small groups*). L counts the cells of the term: the
levels of `subject`, or the combinations of the factors of an interaction
such as `a:b` or `school:class`. The rule is a warning, not a correction, and the thresholds
are fitted on this grid, for normal errors, one factor and one or two
regressors; it does not flag under-rejection. Most panels of firms or people
over years fail it: on 100 subjects of 10 the tests reject in 18.1%, 18.4% and
21.3% of the draws.

**The other routes to the three statistics.** `fixed-effects-variants-size.R`
decides which routes give the warning, one that over-rejects on the designs
above giving it and one that holds its level not. It measures every route on
30 subjects of 4 and 100 of 8, `lm(y ~ x + subject)`, and the asymptotic ones
on a factor that enters only through an interaction, a slope for each subject:
`y = 1 + (0.5 + b_i) x + e`, fitted as `lm(y ~ x + x:subject)`. It counts the
warnings each call gives. The rates are at the 5% level, in %, over 2000
replications, and over 1000 for the resampling routes on 100 x 8 (in
brackets, `N_MC_BOOT_LARGE = 1000`, to keep the run to about an hour); the
resampling routes draw B = 199 samples each:

| Route | 30 x 4 | 100 x 8 | slopes 30 x 3 | slopes 30 x 4 | slopes 100 x 8 | Warnings per call |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `performWhiteTest()` | 21.6 | 22.1 | 0.7 | 2.2 | 0.9 | 1 |
| `performKoenkerTest()` | 22.9 | 22.4 | 0.8 | 0.7 | 0.1 | 1 |
| `performBPTest()` | 27.6 | 25.3 | 2.8 | 1.8 | 0.4 | 1 |
| `performWhiteTestRobust()` | 21.6 | 22.1 | 0.7 | 2.2 | 0.9 | 1 |
| `performWhiteTestRobust(method = "reduced")` | 21.6 | 22.1 | 0.7 | 2.2 | 0.9 | 1 |
| `performBPTestRobust()`, `studentize = TRUE` | 22.9 | 22.4 | 0.8 | 0.7 | 0.1 | 1 |
| `performBPTestRobust(studentize = FALSE)` | 27.6 | 25.3 | 2.8 | 1.8 | 0.4 | 1 |
| `performWhiteTestStreaming()` | refused | refused | 0.7 | 2.2 | 0.9 | 0 or 1 |
| `performBPTestStreaming()` | 27.6 | 25.3 | 2.8 | 1.8 | 0.4 | 1 |
| `performKoenkerTestStreaming()` | 22.9 | 22.4 | 0.8 | 0.7 | 0.1 | 1 |
| `performStudentizedBPTest()` (deprecated) | 22.9 | 22.4 | 0.8 | 0.7 | 0.1 | 1 |
| registered `"white"` | 21.6 | 22.1 | 0.7 | 2.2 | 0.9 | 1 |
| registered `"koenker"` | 22.9 | 22.4 | 0.8 | 0.7 | 0.1 | 1 |
| registered `"breusch_pagan"` | 27.6 | 25.3 | 2.8 | 1.8 | 0.4 | 1 |
| registered `"student_bp"` | 22.9 | 22.4 | 0.8 | 0.7 | 0.1 | 1 |
| `performWhiteTestBootstrap()` | 3.7 | 4.2 (1000) |  |  |  | 0 |
| `performWildBootstrapTest()` | 3.8 | 3.9 (1000) |  |  |  | 0 |
| `bootstrapTestStatistic(performKoenkerTest)` | 3.9 | 4.6 (1000) |  |  |  | 0 |

`fixed-effects-variants-size.csv`. The registered names and the robust,
streaming and deprecated forms compute the statistic and the p-value of the
test they wrap on every sample, so they over-reject as it does, and give the
warning, once per call. The streaming White test refuses a factor that is a
main effect, whose dummies and their squares are collinear, and computes on
the slopes, where it refused 2 and 1 of the samples of 30 x 3 and 30 x 4.

- **The bootstraps hold their level, and give no warning.** They resample the
  residuals and refit the same design, so their replicates carry the excess
  the dummies add: 3.7% to 3.9% on 30 x 4, against 21.6% to 27.6% for the
  asymptotic p-values, and 3.9% to 4.6% on 100 x 8. All six are inside
  the study's band, and on 30 x 4 a little below 5%, by 2.3 to 2.7 Monte Carlo
  standard errors.
- **A slope for each level makes the tests reject too rarely.** With a
  slope for each subject, the three tests reject in 0.1% to 2.8% of the
  samples. The rule counts the term `x:subject`, as decided, and the warning
  says that the p-value is unreliable, in the simulations usually far too
  small, and too large with a slope per level. A model with the factor both
  as a main effect and in a slope, such as `lm(y ~ x * subject)`, was not
  measured here.

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

Up to 0.14.0, `performSpatialHeteroTest()` drew 499 permutations by default.
The full sweep calls it with its default, so its row was regenerated in 0.15.0
with 999, from the same seeds: with 499 it rejected 0.045 of the time under
the null and 0.362 against the alternative, and with 999, 0.050 and 0.350. No
other row of the sweep calls a resampling test with its default.

Up to 0.14.0, `performGlejserTest()` computed Glejser's statistic by default,
and since 0.15.0 the statistic corrected for asymmetric errors. The full sweep
and the known-weights block of the weighted fits call it with its default, so
their two rows were regenerated in 0.15.0 from the same seeds, and no other
row: with `robust = FALSE` the same runs reproduce the rows of 0.14.0 byte for
byte. In the full sweep the size moved from 0.0600 to 0.0575, the size under
`t5` errors from 0.0575 to 0.0550, and the power stayed 1.000; with known
weights the rejection rate moved from 0.0475 to 0.0450. Pass A and the
`glejser` rows of `glejser-skewed-errors.R` measure Glejser's statistic by
name; their calls give `robust = FALSE` since 0.15.0, and their CSVs are
unchanged.

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
