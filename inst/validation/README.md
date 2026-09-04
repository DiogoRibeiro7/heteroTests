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

| Test | Alternative | Size | Power |
| --- | --- | ---: | ---: |
| `performCookWeisbergTest()` | sd = x^2 | 0.072 | 0.980 |
| `performDavidianCarrollTest()` | sd = x^2 | 0.042 | 0.975 |
| `performHarveyTest()` | sd = x^2 | 0.058 | 1.000 |
| `performNCVTest()` | sd = x^2 | 0.072 | 0.980 |
| `performSpearmanTest()` | sd = x^2 | 0.070 | 0.980 |
| `performSpreadLevelTest()` | sd = x^2 | 0.055 | 0.975 |
| `performBPTest()` | sd = x^2 | 0.058 | 1.000 |
| `performBreuschPaganTest()` | sd = x^2 | 0.058 | 1.000 |
| `performKoenkerTest()` | sd = x^2 | 0.045 | 1.000 |
| `performStudentizedBPTest()` | sd = x^2 | 0.045 | 1.000 |
| `performWhiteTest()` | sd = x^2 | 0.032 | 1.000 |
| `performQuantileRegressionTest()` | sd = x^2 | 0.048 | 1.000 |
| `performHighDimensionalTest()` | sd = x^2 | 0.045 | 1.000 |
| `performRankPermutationTest()` | sd = x^2 | 0.050 | 1.000 |
| `performWildBootstrapTest()` | sd = x^2 | 0.052 | 1.000 |
| `performBartlettTest()` | sd = x^2 | 0.060 | 1.000 |
| `performBrownForsytheTest()` | sd = x^2 | 0.055 | 1.000 |
| `performFlignerKilleenTest()` | sd = x^2 | 0.048 | 1.000 |
| `performHartleyFmaxTest()` | sd = x^2 | 0.062 | 1.000 |
| `performLeveneTest()` | sd = x^2 | 0.068 | 1.000 |
| `performOBrienTest()` | sd = x^2 | 0.048 | 1.000 |
| `performBoxMTest()` | sd = x^2 | 0.012 (low) | 1.000 |
| `performGlejserTest()` | sd = x^2 | 0.060 | 1.000 |
| `performParkTest()` | sd = x^2 | 0.062 | 1.000 |
| `performGQTest()` | sd = x^2 | 0.082 | 1.000 |
| `performSzroeterTest()` | sd = x^2 | 0.058 | 1.000 |
| `performArchLMTest()` | ARCH(1), alpha = 0.6 | 0.048 | 0.830 |
| `performMcLeodLiTest()` | ARCH(1), alpha = 0.6 | 0.040 | 0.848 |
| `performRESETTest()` | omitted quadratic | 0.045 | 1.000 |
| `performBPRandomEffectsTest()` | random intercepts | 0.030 | 1.000 |
| `performPesaranTest()` | common time factor | 0.037 | 1.000 |

Replications: 400. Nominal level: 0.05. Monte Carlo standard error at the nominal level: 0.0109.

Each test is driven against its own alternative, so the power column compares
tests only within a family. The heteroscedasticity diagnostics face
`sd = x^2`; the ARCH pair faces an ARCH(1) process, which is a harder problem
at this sample size and is why their power sits near 0.84 rather than 1.

Twenty-six of the twenty-eight heteroscedasticity tests fall inside three
Monte Carlo standard errors of the nominal level. Two remarks on the rest:

- `performBoxMTest()` is conservative at 0.012. Box's M is a test of equality
  of covariance matrices rather than of scalar variances, so it is answering a
  broader question here, and it is known to be sensitive to non-normality in
  the opposite direction. It is left as it is.
- `performGQTest()` at 0.082 is 2.9 standard errors above nominal, which is
  within the flagging threshold but worth explaining. It is a property of this
  design rather than of the implementation: `lmtest::gqtest()` gives 0.070 on
  the same samples, and with a single regressor both give 0.025. The package
  reproduces `lmtest::gqtest()` exactly at matched `fraction` -- the difference
  is zero at 0.1, 0.2 and 0.3 -- though it cannot reproduce that function's
  default, since `lmtest` omits no central observations and `performGQTest()`
  requires `fraction` strictly between 0 and 1.

Four tests are not heteroscedasticity diagnostics and are included because they
are exported and were unvalidated: `performRESETTest()` (functional form),
`performBPRandomEffectsTest()` (presence of an individual effect),
`performPesaranTest()` (cross-sectional dependence) and `performBoxMTest()`
(equality of covariance matrices). Reading their power as power against
heteroscedasticity would be a mistake; the corrected
`performBPRandomEffectsTest()` rejects at 3.2% against `sd = x^2`, which is the
nominal level rather than power.

## History

Before 0.7.0, `Szroeter` rejected in 0.0% of samples in every one of these
scenarios, including all the power columns. The full before-and-after is in
`NEWS.md`.
