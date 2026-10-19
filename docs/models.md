<!-- Hand-written page: tools/build-docs.R does not generate it. The table was measured by running every exported test on a fit of each class; update it by hand when a test changes what it accepts, with tests/testthat/test-model-eligibility.R and test-input-validation.R, which check it. -->

# Supported models

The tests of the package examine the residuals of a least-squares fit for a
variance that is not constant. They accept the models whose residuals are
those of a least-squares fit, and refuse the others when they are called,
before they compute anything, each with one message. This page says which
test accepts which class of model, and why; the rules, and the messages, are
in [What every test returns](reference/test_results.md), under *Supported
models* and *Inputs every test refuses*.

The table was measured, not written from the code: every exported test was
called as in `tests/testthat/helper-test-recipes.R`, on a fit of each class to
the 120 observations of `contract_data()`, on 0.15.0 and on the development
version. A cell in bold changed in 0.16.0.

## The classes, and why

One line for each class and each answer in the table.

| Class | Answer | Why |
| --- | --- | --- |
| `lm` | yes | Its residuals are least-squares residuals, the ones the null distributions of the tests are derived for. |
| `lm(..., weights = w)` | yes | `lm()` states that the error variance is $\sigma^2 / w_i$; the tests read the Pearson residuals $\sqrt{w_i}\, e_i$, the residuals of the unweighted regression of $\sqrt{w_i}\, y_i$ on $\sqrt{w_i}\, x_i$, which have constant variance when the weights are right. |
| | no: weighted | `performQuantileRegressionTest()` refits the model without the weights, which would test another model. |
| | no: not `fitWLS()` | `performVarianceFormTest()` tests the variance function `fitWLS()` estimated; weights given by hand have none to test. |
| An `lm` with a zero weight | no: zero weight | The observation is not part of the fit, and its residual is an artificial zero that the test would count as an observation. |
| | yes | `performRESETTest()` compares two weighted least-squares fits, which both leave the observation out: its value is that of the fit to the observations with positive weight. |
| Gaussian `glm`, identity link | yes | A least-squares fit: its coefficients, fitted values and residuals are those of `lm()`, up to rounding, and every `perform*Test()` gives the result of the `lm` (see *A Gaussian glm is a least-squares fit* below), when the two decide the rank of the design alike. `performWhiteTestStreaming()`, `performHarrisonMcCabeTest()` and `performVarianceFormTest()` refused every `glm` until 0.16.0; `bootstrapTestStatistic()` refuses every `glm`. |
| Gaussian `glm` with prior weights | yes | Its prior weights, `prior.weights`, are the weights of `lm(..., weights = w)`, and the tests read them. Its `weights` are the working weights of the last iteration of iteratively reweighted least squares, $w_i (d\mu_i / d\eta_i)^2 / V(\mu_i)$, which for this family and link are the prior weights; the tests do not read them. |
| | no: not `fitWLS()` | `performVarianceFormTest()` refuses prior weights given to `glm()` as it refuses weights given to `lm()`: they have no variance function to test. |
| Gaussian `glm` with a zero prior weight | no: zero weight | As for the `lm`. Before 0.16.0 most tests read no weights for a `glm` and tested the artificial zero. |
| Binomial, Poisson or any other `glm` | no: not least squares | The variance of its residuals depends on the mean by construction, so a test of constant variance tests nothing. |
| `survey::svyglm()` | no: survey | Its residuals are scaled by the sampling weights, which are not precision weights: a homoscedastic model is rejected whenever they vary with the regressors. `runSurveyHeteroTests()` fits the model by least squares to the data of the design instead. |
| `plm::plm()` | no: not `lm` | The panel tests, `performPesaranTest()` and `performBPRandomEffectsTest()`, take the pooled `lm` of the panel and the columns of `data` that name the units and the periods. A `plm` fit is not an `lm`. |
| A rank-deficient fit | yes | An aliased column, a linear combination of the others, does not change the column space of the design, so the residuals and fitted values are those of the model fitted without it. Since 0.16.0 the tests drop the aliased columns from the design they build and take their degrees of freedom from the rank: the value is that of the model fitted without them. |
| | no | A test that refuses the model without the aliased columns, for one of the reasons above, refuses the rank-deficient one with the same message. |
| | yes, not the reduced value | `performVarianceFormTest(form = "power")` takes the logarithms of every column of the design, the aliased ones included; the logarithm of an aliased column is not a combination of the logarithms of the others, so the value is not that of the model without the aliased columns. It does not depend on the order of the terms, as the value of that model does. With the exponential form, the default, the value is that of the model without them. |
| An `NA` from an empty cell | yes | The dummy of a cell of an interaction that no observation falls in is aliased: as for any rank-deficient fit. |
| | error, as reduced | The squares of the dummies are collinear, and `performWhiteTestStreaming()` stops, as it does on the model fitted without the aliased column; `performWhiteTest()` drops such terms. |
| Gaussian `mgcv::gam()` | yes | A `glm` by class. Without smooth terms it is the least-squares fit of its design, and the tests give the value of the Gaussian `glm`. |
| Gaussian `mgcv::gam()` with smooth terms | as today | A smooth term is penalised unless it is fixed (`s(x, fx = TRUE)`), and a penalised fit is not the least-squares fit of its design. White's, the Breusch-Pagan, Koenker's, the rank permutation and the high-dimensional tests test it as before 0.16.0; whether the tests take such a fit is an open decision, settled for every test together in the last step of the model context, with `MASS::rlm()`. |
| | **no: smooth terms** | Since 0.16.0 RESET, Goldfeld-Quandt, the quantile regression test and the studentized Breusch-Pagan statistic refuse it until then, fixed smooths included: each fits the design of the fit again, by least squares or by quantile regression, which would test a regression the model is not. RESET and the studentized statistic returned a value before; Goldfeld-Quandt and the quantile test stopped on the smooth term, which they evaluated outside mgcv. |
| `MASS::rlm()` | as today | An `lm` by class, so 37 tests accept it, as before 0.16.0, and test its M-estimation residuals as if they were least-squares residuals. Whether that is a test of anything is an open decision (below). |
| | no: weighted, no: not `fitWLS()`, no: refit differs | Its case weights, all 1 unless given, are read as weights; `bootstrapTestStatistic()` refits each replicate by least squares, which does not reproduce an M-estimate. |
| `fitWLS()` | yes | A weighted `lm` whose weights were estimated. The reference distributions take the weights as known, so they are approximate; `performVarianceFormTest()` accounts for the estimation. |
| `fitWLS()` of a Gaussian `glm` | yes | Since 0.16.0 it is the `lm` that `fitWLS()` returns for the equivalent `lm`, with the call of that `lm`, and is tested as that. Before, it kept the class and the call of the `glm` without its family, which `summary()` could not read and every test refused; `fitWLS()` refuses any other `glm`, and a `survey::svyglm()` fit, now. |
| A model with an offset | yes | The residuals of the fit include the offset, and the tests read them (see *Offsets* below). |
| | **yes, since 0.16.0** | RESET, the quantile regression test and the studentized Breusch-Pagan statistic left any offset out of a refit, and GQ an offset given as an argument, or any offset on a weighted fit. They refused such a model in the development of 0.16.0 (decided 2026-10-17), until the offset fix, the second step of the model context, made them read the offset from the fit (*Offsets* below). |

Every test also refuses a coefficient that is infinite, `NaN`, or `NA` without
the fit being rank-deficient, and `data` that lack rows or variables of the
fit, or whose rows cannot be shown to be those of the fit. Anything that is
not an `lm` or a `glm`, a `plm` fit among them, is refused by every test with
one message, which names the class received and points to the pooled `lm` the
panel tests take.

### Which model without the aliased columns

The model fitted without the aliased columns is the one `lm()` keeps: its
decomposition keeps a column unless it is a combination of the columns before
it, so of an aliased set the column that comes last, in the order of the
terms, is left out (in `y ~ x1 + x2 + x_sum`, with `x_sum = x1 + x2`, it is
`x_sum`; in `y ~ x_sum + x1 + x2`, it is `x2`). Most statistics depend only on
the space the columns span, and are the same whichever column is left out.
These depend on which:

- `performWhiteTest()`, and so `performWhiteTestRobust()`,
  `performWhiteTestStreaming()` and `performWhiteTestBootstrap()`, when the
  cross products are left out (`cross_products = FALSE`, `method = "reduced"`,
  or more regressors than `max_interactions`): the squares are those of the
  columns kept. With the cross products, the default, they span the same space
  either way.
- `performHighDimensionalTest()` when it keeps fewer principal components than
  there are columns.

### Offsets

An offset, given as `offset = o` or written as `offset(o)`, is part of the fit,
and most tests read the residuals of the fit, which include it: on a model
with an offset they give the value of the same model fitted to the response
minus the offset. Four left it out of a refit, and returned a value that is
not that of the model. Decided on 2026-10-17, they refused exactly the offsets
they left out, in the development of 0.16.0, until the offset fix made them
correct. That fix is the second step of the model context: they read the
response, the design, the weights and the total offset from the fit, and every
refit carries the offset.

| Test | Before the fix | Since 0.16.0 |
| --- | --- | --- |
| `performRESETTest()` | It refitted the model augmented with powers of the fitted values without the offset, so the two fits were not nested. | The nested F test of the model with its offset against the model augmented with the powers of its fitted values, $X\hat\beta + o$, fitted with the same offset and weights (decided 2026-10-18). It is not RESET of the model of `y - o`, which augments that model with the powers of $X\hat\beta$ alone. |
| `performGQTest()` | It refitted each segment of an unweighted fit from the formula, which evaluated an `offset()` term again but left out an argument; on a weighted fit it refitted with `lm.wfit()` and no offset. | Each segment is fitted to its rows of the design, the response and the offset of the fit: the value of the model of `y - o`. |
| `performQuantileRegressionTest()` | `quantreg::rq()` was fitted to the formula, which left the offset out. | The quantile regressions are fitted to the response minus the offset, on the design of the fit: the model of `y - o`, since the offset shifts every conditional quantile as it shifts the mean. |
| `performStudentizedBPTest()`, `performBPTestRobust()` with `studentize = TRUE` (its default), the registered `"student_bp"` | The studentized statistic refitted the design without the offset. | The refit carries the offset: Koenker's statistic of the model, as `performKoenkerTest()` and `performBPTestRobust(studentize = FALSE)` give it. |

An offset that is zero everywhere changes nothing. `runDiagnostics()`, which
runs RESET, returns for a model with an offset. The tests built on the fitted values
(the Cook-Weisberg, Spearman, spread-level and Davidian-Carroll tests, and
Harvey's with `auxiliary = "fitted"`) read $X\hat\beta$ plus the offset, as
the fit defines it, so their value is not that of the model of `y - o`, by
design.

### RESET with a zero weight

Decided on 2026-10-17: `performRESETTest()` keeps accepting a zero weight,
the one exception to the refusal of a zero weight. Its two fits are weighted
least squares, which leave the observation out, and its F statistic and
degrees of freedom are those of the fit to the observations with positive
weight.

### A Gaussian glm is a least-squares fit

Every `perform*Test()` function accepts a `glm` with the Gaussian family and
the identity link, and gives it the value of the `lm` with the same prior
weights. Until 0.16.0 `performWhiteTestStreaming()`,
`performHarrisonMcCabeTest()` and `performVarianceFormTest()` refused every
`glm`; this section is why they need not, and
`tests/testthat/test-model-eligibility.R` is the evidence.
`bootstrapTestStatistic()` still refuses every `glm`, as it refits each
replicate by least squares, which would drop a family and a link; the same
argument would let it accept this one (roadmap).

**The fit.** With prior weights $w_i$, all 1 when none are given, such a
`glm` states that the $y_i$ are independent and normal, with mean
$x_i^\top \beta$ and variance $\phi / w_i$: the normal linear model that
`lm(..., weights = w)` fits, with $\sigma^2 = \phi$. `glm()` fits it by
maximum likelihood, and the log-likelihood

$$
\ell(\beta, \phi) = -\frac{1}{2\phi} \sum_i w_i (y_i - x_i^\top \beta)^2
  - \frac{n}{2} \log \phi + \text{terms free of } \beta \text{ and } \phi
$$

is maximised in $\beta$ by weighted least squares. The algorithm of `glm()`,
iteratively reweighted least squares, regresses the working response
$z_i = \eta_i + (y_i - \mu_i)\, d\eta_i / d\mu_i$ on $x_i$ with the working
weights $w_i (d\mu_i / d\eta_i)^2 / V(\mu_i)$ (Nelder and Wedderburn, 1972;
McCullagh and Nelder, 1989, section 2.5). With the identity link
$d\mu / d\eta = 1$, and for the Gaussian family $V(\mu) = 1$, so
$z_i = y_i$ and the working weights are the prior weights, whatever the
current estimate: the first step is the weighted least-squares fit, and it is
a fixed point. In the terms of Green (1984, section 1.2): Newton-Raphson
converges in one step from any start when the log-likelihood is quadratic in
$\beta$, as it is here; iteratively reweighted least squares is Fisher
scoring, Newton-Raphson with the information replaced by its expectation;
and for the identity link, the canonical link of the Gaussian family, the
information $X^\top W X / \phi$ does not depend on $y$, so it equals its
expectation and scoring is Newton-Raphson, which converges in one step.
`glm.fit()` takes a second iteration to see that the deviance no longer
changes, which moves the fitted values by rounding only. McCullagh and
Nelder (1989) treat the normal linear model as this generalized linear model
in their chapter 3, *Models for continuous data with constant variance*
(section 3.6, *Estimation*; section 3.8, *Algorithms for least squares*).

So the `glm` and `lm(..., weights = w)` have the same design matrix $X$
(`model.matrix()`), response and model frame; the same coefficients, fitted
values $\hat\mu = X \hat\beta$ and response residuals $e = y - \hat\mu$, up
to rounding (about $10^{-14}$ in the tests); the same weights, as the prior
weights of the `glm`, `prior.weights`, are the weights of the `lm`, which
the tests read, and its working weights, `weights`, equal them up to
rounding (`glm.fit()` stores the squares of their square roots, which
differ from them by up to about $2 \times 10^{-15}$); the same rank and
residual degrees of freedom, when the two decide the rank alike (below);
and the same estimate of the error variance, as the dispersion
$\hat\phi = \sum_i w_i e_i^2 / (n - p)$ is $\hat\sigma^2$. Its deviance
residuals, which `residuals()` returns, are
$\operatorname{sign}(e_i) \sqrt{w_i e_i^2} = \sqrt{w_i}\, e_i$, the Pearson
residuals of the `lm`, which the tests read.

**The rank.** `lm()` decides the rank of the design in its QR
decomposition with the tolerance `tol = 1e-7`; `glm.fit()` with
`min(1e-7, epsilon / 1000)`, which is `1e-11` with the default
`epsilon = 1e-8` of `glm.control()`. A column that is a combination of the
others to within that margin is aliased by the `lm` and kept by the `glm`,
and the `glm` then has the residuals, the rank and the test values of
`lm(..., tol = 1e-11)`, not those of the default `lm`. With
`x3 = x1 + 1e-9 * rnorm(120)` added to `contract_data()` (seed 5), the `lm`
aliases `x3` and the `glm` does not: Harrison-McCabe gives 0.3184 for the
`lm` and 0.3199 for the `glm` (that of `lm(..., tol = 1e-11)`), Koenker
9.04 on 2 degrees of freedom against 9.25 on 3, and the White streaming test
refuses the `glm`, whose auxiliary regression is then rank-deficient. The
equivalence above holds when the two decide the rank alike, as they do
unless a column is that close to a combination of the others; the behaviour
is not changed. On such a fit Harrison-McCabe is not consistent with itself:
its residuals are those of the fit of rank 4, and its null distribution is
computed from the design at the default tolerance, of rank 3 (roadmap).

**The tests.** A test is a statistic of $(X, y, w, e)$, and possibly of an
ordering of the observations or of the fitted values, with a null
distribution derived under the assumptions of the linear model. The `glm`
has the same $(X, y, w, e)$ and states the same model, normal errors
included, so the statistic, its null distribution and the p-value are the
same. That holds for a test whose code reads only those quantities, or the
quantities of the `lm` they equal, and never one a `glm` holds with another
value or not at all: `summary()`, whose result for a `glm` reports a
dispersion and no R-squared; `$weights` read as prior weights; the
deviance; the family. For the three tests:

| Test | Statistic and null distribution | What it reads | What had to change |
| --- | --- | --- | --- |
| `performHarrisonMcCabeTest()` | The share $b$ of $\sum_i \tilde e_i^2$ in the first $m$ observations in the order of `order_by`, with $\tilde e_i = \sqrt{w_i}\, e_i$; exact under normal errors of variance $\sigma^2 / w_i$, from the residual-maker of $\sqrt{w_i}\, x_i$ in that order. | The Pearson residuals (the deviance residuals of the `glm`), `model.matrix()`, the prior weights, the ordering column. | It read the weights with `rprior_weights()`, which returns none for a `glm`, so a weighted `glm` would have had the unweighted design under weighted residuals. It reads `rleast_squares_weights()`, the prior weights of an `lm` or of a Gaussian `glm`. |
| `performWhiteTestStreaming()` | $n R^2$ of the regression of $\tilde e_i^2$ on $x_i$, their squares and cross products, accumulated in chunks; $\chi^2$ under a constant variance (White, 1980). | The Pearson residuals; the `model.matrix()` of each chunk, from the `terms`, `contrasts` and `xlevels` of the fit, which a `glm` holds as an `lm` does. | Nothing in the statistic. The checks it runs read the R-squared of `summary()`, which a `glm` does not report, and stopped on a `glm` with "missing value where TRUE/FALSE needed": that is why it refused every `glm`. They read the R-squared of the least-squares fit now (below). |
| `performVarianceFormTest()` | The F test of the added terms in the regression of the squared standardized residuals of the weighted refit on the score direction of the variance function, fitted to $\log e_i^2$ of the unweighted fit (Wooldridge, 1990, 1991). | `$residuals`, which for a `glm` are its working residuals, $y_i - \hat\mu_i$ for the identity link, the residuals of the `lm`; `model.frame()`, `model.matrix()`, `model.offset()`; the weights, to refuse those that did not come from `fitWLS()`. | It read the weights with `rprior_weights()`, so it would have taken a weighted `glm` for an unweighted one. It reads `rleast_squares_weights()`, and refuses a weighted `glm` as it refuses a weighted `lm`. |

Once the `glm` is accepted, the code of the three reads none of its
dispersion, its QR decomposition, its working weights or its family. The
checks they run read its residual degrees of freedom, for a warning, which
are those of the `lm`, and compute the R-squared below with `summary.lm()`,
which reads its working residuals and weights: for this family and link,
the response residuals and the prior weights. The sources of the tests
define them for the linear
regression model fitted by least squares: White (1980) for the disturbances
of a linear regression model, Harrison and McCabe (1979) for ordinary
least-squares residuals, which Harrison (1981), whom the package follows,
restates, and Wooldridge (1990, 1991) for models of conditional means and
variances, of which the least-squares fit and its weighted refit are one. A
Gaussian `glm` with the identity link is that model, fitted by that
estimator; the weighted forms are the package's own, and apply to the `glm`
as to the `lm`.

**The checks of the model.** Every test refuses a near-perfect fit,
$R^2 > 0.999$, with "Model appears perfectly explained". The checks read
$R^2$ from `summary()`, which reports none for a `glm`, so until 0.16.0 a
near-perfect Gaussian `glm` escaped the refusal the same `lm` met, in 30
tests. They read the $R^2$ of the least-squares fit now: for the `glm`,
`summary.lm()` applied to it, which reads its fitted values, residuals,
weights and offset, those of the `lm`, so it reports what `summary()` of the
`lm` reports, in every version of R. (Since R 4.3.0 `summary.lm()` takes the
offset out of the fitted values; before, it did not, and $1 - D / D_0$, the
deviance of the `glm` over its null deviance, which takes it out, gave
another value for a model with an offset on R 4.1.) Five tests do not run the
check and give the `lm` and the `glm` a value: the spread-level, Pesaran,
Breusch-Pagan random effects, Davidian-Carroll and RESET tests.

**`fitWLS()`.** `performVarianceFormTest()` tests the variance function of a
`fitWLS()` fit, and refits any other model as `fitWLS()` would. `fitWLS()`
of a Gaussian `glm` is the `lm` that `fitWLS()` returns for the equivalent
`lm`, with the prior weights of a weighted `glm` recorded as the weights of
the model it refitted and the call of that `lm`, `stats::lm()` without the
arguments only `glm()` takes, so that `update()` fits an `lm` again. The test
gives the two the same value.

**Prior weights of 1.** A `glm` fitted with weights that are all 1 has the
`prior.weights` of a `glm` fitted without weights, as `glm()` records no
weights as weights of 1. Its model frame tells the two apart, as it holds a
`(weights)` column, which `model.weights()` reads, but the tests read the
weights of a `glm` from `prior.weights` (`rleast_squares_weights()`), so
they take it for an unweighted fit, where `lm(..., weights = rep(1, n))` is a
weighted one. The two tests that refuse a weighted fit therefore accept the
`glm` and refuse the `lm`: `performVarianceFormTest()`, whose weights must
come from `fitWLS()`, since 0.16.0, when it accepts a Gaussian `glm`; and
`performQuantileRegressionTest()`, which refits without the weights, since
0.15.0. So does `runHeteroTests()` with `tests = "variance_form"`, which
stops on the `lm`, and with `tests = "quantile_regression"`, which falls
back on another test for the `lm`. `bootstrapTestStatistic()` refuses every
`glm`, so the two are not compared there. The behaviour is kept.

References:

- Green, P. J. (1984). Iteratively reweighted least squares for maximum
  likelihood estimation, and some robust and resistant alternatives.
  *Journal of the Royal Statistical Society, Series B, 46*(2), 149--192.
  [doi:10.1111/j.2517-6161.1984.tb01288.x](https://doi.org/10.1111/j.2517-6161.1984.tb01288.x)
- Harrison, M. J. (1981). A comparison of the bounds, beta-approximate, and
  exact variants of two tests for heteroscedasticity based on ordinary least
  squares residuals. *The Economic and Social Review, 12*(4), 235--252.
- Harrison, M. J., & McCabe, B. P. M. (1979). A test for heteroscedasticity
  based on ordinary least squares residuals. *Journal of the American
  Statistical Association, 74*(366a), 494--499.
  [doi:10.1080/01621459.1979.10482544](https://doi.org/10.1080/01621459.1979.10482544)
- McCullagh, P., & Nelder, J. A. (1989). *Generalized Linear Models* (2nd
  ed.). Chapman & Hall. The section numbers and titles above are those of its
  table of contents.
- Nelder, J. A., & Wedderburn, R. W. M. (1972). Generalized linear models.
  *Journal of the Royal Statistical Society, Series A, 135*(3), 370--384.
  [doi:10.2307/2344614](https://doi.org/10.2307/2344614)
- White, H. (1980). A heteroskedasticity-consistent covariance matrix
  estimator and a direct test for heteroskedasticity. *Econometrica, 48*(4),
  817--838. [doi:10.2307/1912934](https://doi.org/10.2307/1912934)
- Wooldridge, J. M. (1990). A unified approach to robust, regression-based
  specification tests. *Econometric Theory, 6*(1), 17--43.
  [doi:10.1017/S0266466600004898](https://doi.org/10.1017/S0266466600004898)
- Wooldridge, J. M. (1991). On the application of robust, regression-based
  diagnostics to models of conditional means and conditional variances.
  *Journal of Econometrics, 47*(1), 5--46.
  [doi:10.1016/0304-4076(91)90076-P](https://doi.org/10.1016/0304-4076(91)90076-P)

## The table

| Test | lm | lm, weights | lm, a zero weight | Gaussian glm | Gaussian glm, weights | Gaussian glm, a zero weight | binomial glm | Poisson glm | svyglm, Gaussian | svyglm, binomial | plm | rank-deficient lm | rank-deficient lm, weights | rank-deficient Gaussian glm | NA from an empty cell | MASS::rlm | fitWLS() | lm, offset = o | lm, + offset(o) | weighted lm, + offset(o) |
| --- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| `performWhiteTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performWhiteTestStreaming()` | yes | yes | no: zero weight | **yes** | **yes** | **no: zero weight** | **no: not least squares** | **no: not least squares** | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **error, as reduced** | as today | yes | yes | yes | yes |
| `performWhiteTestRobust()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performBPTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performBPTestStreaming()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performBreuschPaganTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performBPTestRobust()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | **yes** | **yes** | **yes** |
| `performGQTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | **yes** | yes | **yes** |
| `performKoenkerTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performKoenkerTestStreaming()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performParkTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performStudentizedBPTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | **yes** | **yes** | **yes** |
| `performWhiteTestBootstrap()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performSzroeterTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performHarrisonMcCabeTest()` | yes | yes | no: zero weight | **yes** | **yes** | **no: zero weight** | **no: not least squares** | **no: not least squares** | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performWildBootstrapTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performQuantileRegressionTest()` | yes | no: weighted | no: weighted | yes | no: weighted | no: weighted | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | no: weighted | **yes** | **yes** | no: weighted | no: weighted | **yes** | **yes** | no: weighted |
| `performRankPermutationTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performHighDimensionalTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performSpatialHeteroTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performSpearmanTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performLeveneTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performGlejserTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performBrownForsytheTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performFlignerKilleenTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performBartlettTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performHarveyTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performArchLMTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performMcLeodLiTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performHartleyFmaxTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performOBrienTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performCookWeisbergTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performNCVTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performSpreadLevelTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performPesaranTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performBPRandomEffectsTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performDavidianCarrollTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | yes | yes | yes |
| `performVarianceFormTest()` | yes | no: not fitWLS() | no: not fitWLS() | **yes** | **no: not fitWLS()** | **no: not fitWLS()** | **no: not least squares** | **no: not least squares** | no: survey | no: survey | no: not lm | **yes** | **no: not fitWLS()** | **yes** | **yes** | no: not fitWLS() | yes | yes | yes | no: not fitWLS() |
| `performRESETTest()` | yes | yes | yes | yes | yes | yes | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes | **yes** | **yes** | **yes** |
| `performScatterDiagnostic()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | **no: not least squares** | **no: not least squares** | no: survey | no: survey | no: not lm | yes | yes | yes | yes | as today | yes | yes | yes | yes |
| `bootstrapTestStatistic()` | yes | yes | no: zero weight | no: any glm | no: any glm | no: any glm | no: any glm | no: any glm | no: any glm | no: any glm | no: not lm | **yes** | **yes** | **no: any glm** | **yes** | no: refit differs | yes | yes | yes | yes |

`performBreuschPaganTest()`, `performStudentizedBPTest()` and
`rbootstrap_test_statistic()` are deprecated names; `bootstrapTestStatistic()`
was given `performKoenkerTest()`. On `MASS::rlm()` the studentized
Breusch-Pagan statistic of `performStudentizedBPTest()` and of
`performBPTestRobust()` refits the design by least squares, so it tests the
least-squares fit (9.04 in this design, the value of the `lm`), where
`performKoenkerTest()` tests the residuals of the M-estimate (8.74).

## The routes

The runners and the registry call the tests, and inherit what they refuse:

- `runHeteroTests()` and `runDiagnostics()` stop with the refusal of the first
  test. `data` whose rows cannot be matched to the fit are refused with no
  fallback. Since 0.16.0 every fallback refuses what the test it stands in
  for refuses because of the model or the rows and variables of `data`, so
  no name holds a test computed regardless of them. A test refused for a
  reason of its own, such as the quantile regression test on a weighted fit,
  still falls back on another test under its name, tagged
  `attr(, "extras")$status == "fallback"` (an open decision for 1.0.0, in the
  roadmap).
- The registry names, `cachedTest()`, `runHeteroTestsParallel()` and
  `test_factory$run_test()` call the same tests. `test_factory` leaves a
  rank-deficient fit to a function of the package, whoever registered it, and
  refuses it, as before 0.16.0, for a function defined outside the package,
  such as one a user writes.
- `runPanelTests()` and `runTimeSeriesTests()` refuse anything but an `lm` or a
  `glm` with the message of the tests, then call them.
- `runHeteroTests()` tests a tidymodels workflow on the model frame of its
  engine's fit, with `data = NULL`. Given `data`, such as the data the
  workflow was fitted to, it refuses them when they lack the variables of that
  fit, which names its response `..y`, and says to pass `data = NULL`.
- `runSurveyHeteroTests()` fits the model by least squares to the data of a
  design, and is never given a `svyglm()` fit.
- `runMultivariateTests()` runs Box's M test on data and a grouping; a test of
  a model run through it is given the data in place of a model, and refuses
  them.
- `checkModel()` checks the class of a model only, `lm` or `glm`: it accepts
  every class in the table but `plm`.

## Not covered

`performVIFDiagnostic()`, `performInfluenceDiagnostics()` and the plots are
diagnostics of the fit, not tests of its variance, and accept any `lm` or
`glm`.

## Open decisions

- `MASS::rlm()`: keep accepting it, as above, or refuse it as 0.15.0 refused
  a `glm` that is not a least-squares fit. Deferred on 2026-10-17 to the work
  on the model context; it is accepted as before until then.
