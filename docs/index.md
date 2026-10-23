---
hide:
  - navigation
  - toc
---

<div class="ht-hero" markdown="1">

<div markdown="1">

<p class="ht-kicker">R package · heteroscedasticity diagnostics</p>

# Is the error variance of your model constant?

heteroTests gathers the tests of that question for linear models in R: the
classical auxiliary-regression tests, tests on ordered observations, group-wise
tests of equal variance, resampling and scalable variants, with plots and
remedies, behind one calling convention and one result type.

[Get started](getting-started.md){ .md-button .md-button--primary }
[Browse the reference](reference/index.md){ .md-button }

</div>

![heteroTests logo](assets/logo.png){ .ht-logo }

</div>

<div class="ht-cards" markdown="1">

<div markdown="1">

### One convention

Every test takes `(model, data, ...)` and returns a base-R `htest` object,
which prints, subsets and composes like the result of `lmtest::bptest()`.
[What every test returns](reference/test_results.md) lists its fields.

</div>

<div markdown="1">

### Checked against references

Where an established implementation exists, the test reproduces it and the
test suite asserts the agreement: `lmtest`, `car`, `stats`, `vartest` and
`plm`. Simulated size and power are in
[`inst/validation/`](https://github.com/DiogoRibeiro7/heteroTests/tree/main/inst/validation).

</div>

<div markdown="1">

### From diagnosis to remedy

[`runHeteroTests()`](reference/runHeteroTests.md) runs a battery of tests by
name, [`fitWLS()`](reference/fitWLS.md) fits weighted least squares with a
choice of variance function, and
[`performVarianceFormTest()`](reference/performVarianceFormTest.md) asks
whether that function had the right shape.

</div>

</div>

## What it looks like

```r
library(heteroTests)

model <- lm(stations ~ mag + depth, data = quakes)
performKoenkerTest(model, quakes)
#>
#>         Koenker studentized Breusch-Pagan test
#>
#> data:  stations ~ mag + depth
#> X-squared = 111.31, df = 2, p-value < 2.2e-16

# Several tests at once, chosen by name; listDiagnostics() returns the names.
runHeteroTests(model, quakes, tests = c("white", "koenker"))

# A remedy, and a test of the variance function it assumed.
wls <- fitWLS(model)
performVarianceFormTest(wls)
```

## What it provides

- **Does the variance depend on the regressors?** White
  ([`performWhiteTest()`](reference/performWhiteTest.md)), the classical
  Breusch–Pagan test ([`performBPTest()`](reference/performBPTest.md)) and
  Koenker's studentized form of it
  ([`performKoenkerTest()`](reference/performKoenkerTest.md)), Harvey
  ([`performHarveyTest()`](reference/performHarveyTest.md)), Park
  ([`performParkTest()`](reference/performParkTest.md)), Glejser
  ([`performGlejserTest()`](reference/performGlejserTest.md)) and the
  Cook–Weisberg score test
  ([`performCookWeisbergTest()`](reference/performCookWeisbergTest.md)).
- **Does the variance change with the fitted values or along an ordering?**
  Goldfeld–Quandt ([`performGQTest()`](reference/performGQTest.md)), Szroeter
  ([`performSzroeterTest()`](reference/performSzroeterTest.md)),
  Harrison–McCabe
  ([`performHarrisonMcCabeTest()`](reference/performHarrisonMcCabeTest.md)),
  Spearman's rank correlation, the spread–level test and Davidian–Carroll.
- **Do groups have equal variances?** Levene, Brown–Forsythe, Bartlett,
  Fligner–Killeen, Hartley's F-max and O'Brien on the residuals of a model,
  and Box's M for equal covariance matrices
  ([`performBoxMTest()`](reference/performBoxMTest.md)).
- **Does the variance of a time series depend on its past?** Engle's ARCH LM
  test ([`performArchLMTest()`](reference/performArchLMTest.md)) and
  McLeod–Li ([`performMcLeodLiTest()`](reference/performMcLeodLiTest.md)).
- **When the asymptotic reference distribution is in doubt:** a null-imposed
  wild bootstrap
  ([`performWildBootstrapTest()`](reference/performWildBootstrapTest.md)), a
  bootstrap White test, a rank-permutation test and a quantile-regression
  test.
- **Panel and spatial data:**
  [`performSpatialHeteroTest()`](reference/performSpatialHeteroTest.md) asks
  whether the squared residuals cluster in space, and
  [`runPanelTests()`](reference/runPanelTests.md) runs the Breusch–Pagan test
  for a random effect and Pesaran's test of cross-sectional dependence.
- **Scalability:** streaming implementations of the White, Breusch–Pagan and
  Koenker tests accumulate the auxiliary cross-products in chunks; the results
  are exact and memory-bounded, and `runHeteroTests()` runs them when asked
  with a finite `chunk_threshold_mb`.
- **The shape of the variance:**
  [`performVarianceFormTest()`](reference/performVarianceFormTest.md) tests
  whether an exponential or a power variance function describes the
  heteroscedasticity, where the other tests only establish that the variance
  is not constant.
- **Model checks that are not tests of the variance:** Ramsey's RESET, variance
  inflation factors, influential observations and the correlations of the
  absolute residuals with chosen variables, listed apart in the
  [reference](reference/index.md#model-checks).
- **Remediation and guidance:** weighted least squares
  ([`fitWLS()`](reference/fitWLS.md)), robust fits
  ([`fitRobust()`](reference/fitRobust.md)), variance-stabilising transforms
  ([`autoTransform()`](reference/autoTransform.md)) and a recommendation engine
  ([`generateHeteroRecommendations()`](reference/generateHeteroRecommendations.md)).
- **Ecosystem integration:** `broom` tidiers, `ggplot2` theming and
  `autoplot()`, and helpers for tidymodels and grouped pipelines.

## Installation

Install the released version from
[CRAN](https://CRAN.R-project.org/package=heteroTests), or the development
version from GitHub:

```r
install.packages("heteroTests")

# install.packages("remotes")
remotes::install_github("DiogoRibeiro7/heteroTests")
```

heteroTests needs R 4.1 or later. [Getting started](getting-started.md) walks
through a first analysis, and the [user guide](guide/using-heterotests.md)
holds the long-form articles.

## Citation and licence

If you use heteroTests in research, cite the CRAN release,
[doi:10.32614/CRAN.package.heteroTests](https://doi.org/10.32614/CRAN.package.heteroTests),
as `citation("heteroTests")` does. Releases are also archived on
[Zenodo](https://doi.org/10.5281/zenodo.22226790). heteroTests is released
under the
[Apache License 2.0](https://github.com/DiogoRibeiro7/heteroTests/blob/main/LICENSE).
