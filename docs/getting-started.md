# Getting started

## Installation

heteroTests is on CRAN, and the development version is on GitHub:

```r
install.packages("heteroTests")

# install.packages("remotes")
remotes::install_github("DiogoRibeiro7/heteroTests")
```

Each release is also on the
[releases page](https://github.com/DiogoRibeiro7/heteroTests/releases) as a
source tarball, for `install.packages(path, repos = NULL)`. heteroTests needs
R 4.1 or later. The tests that reproduce another package's statistic, and the
panel, spatial, survey and tidymodels helpers, use the packages listed under
`Suggests` in `DESCRIPTION` when they are installed.

## Run a test

Fit the model as usual, then pass it to a test with the data it was fitted
to. R's `quakes` data record the number of stations that reported each of
1,000 earthquakes near Fiji; a count's variance tends to grow with its mean.

```r
library(heteroTests)

model <- lm(stations ~ mag + depth, data = quakes)
performKoenkerTest(model, quakes)
#>
#>         Koenker studentized Breusch-Pagan test
#>
#> data:  stations ~ mag + depth
#> X-squared = 111.31, df = 2, p-value < 2.2e-16
```

Every test takes `(model, data, ...)`. `data` may be left out, and the model
frame is then used, so `performKoenkerTest(model)` gives the same result. A
test still needs `data` for a column the model does not use, such as the
grouping variable of a group-wise test:

```r
quakes$depth_band <- cut(quakes$depth, 3, labels = c("shallow", "mid", "deep"))
performLeveneTest(model, quakes, "depth_band")
#>
#>         Levene's test for equality of variances
#>
#> data:  stations ~ mag + depth
#> F = 11.207, df1 = 2, df2 = 997, p-value = 1.537e-05
```

It also needs `data` for a model that transforms a variable (`log(x)`) or has
an `offset()` term. Box's M test, `performBoxMTest(data, group)`, is the one
exception to the convention: it tests the data, not the residuals of a model.

## Read the result

A test returns a base-R `htest` object, so the result prints like that of
`stats::t.test()` and its fields are read with `$`:

```r
result <- performKoenkerTest(model, quakes)
result$statistic
#> X-squared
#>  111.3052
result$p.value
#> [1] 6.76691e-25
```

Every test returns `statistic`, `parameter`, `p.value`, `method` and
`data.name`; some add `alternative`, `estimate` or fields of their own.
[What every test returns](reference/test_results.md) lists them, with the
inputs every test refuses.

## Run several tests

[`runHeteroTests()`](reference/runHeteroTests.md) runs the tests it is given
by name and returns them as a list. The default battery is White's test and
Koenker's:

```r
suite <- runHeteroTests(model, quakes)
names(suite)
#> [1] "white"   "koenker"

runHeteroTests(model, quakes, tests = c("white", "koenker", "cook_weisberg"))
```

[`listDiagnostics()`](reference/listDiagnostics.md) returns the names it
accepts, and [`registerDiagnostic()`](reference/registerDiagnostic.md) adds a
test of your own under a new name.
[`runDiagnostics()`](reference/runDiagnostics.md) runs the same tests and adds
the model checks: variance inflation factors, Ramsey's RESET and influence
measures.

## Look at the residuals

The plotting functions return `ggplot2` objects:

```r
plotResidualsFitted(model)
plotSpreadLevel(model)

hd <- HeteroDiagnostic(model, quakes)
plot(hd)
```

## Apply a remedy, and check it

[`fitWLS()`](reference/fitWLS.md) estimates the variance function and refits
the model by weighted least squares. On a weighted fit the tests are computed
from the standardized residuals, so they ask whether the weights are adequate:

```r
wls <- fitWLS(model)
compareModelDiagnostics(list(model, wls))
#>            white  koenker
#> Model1 125.55798 111.3052
#> Model2  42.15312  35.0896
```

`fitWLS()` assumed that the log of the error variance is linear in the
regressors.
[`performVarianceFormTest()`](reference/performVarianceFormTest.md) tests that
shape against the squares and products of the same regressors, allowing for
the weights having been estimated from the same data:

```r
performVarianceFormTest(wls)
```

A small p-value says that the variance function needs those extra terms, and
that another `form` or `var_formula` in `fitWLS()` is worth trying.

## Where next

- The [user guide](guide/using-heterotests.md) holds the long-form articles,
  from a walkthrough to case studies, the statistical theory and a simulation
  study of size and power.
- The [API reference](reference/index.md) documents every function, grouped by
  what it does.
- [Architecture](architecture.md) explains how the tests, the registry and the
  runners fit together.
