# heteroTests tutorial notebooks

An executable, six-part course on heteroscedasticity — from first diagnosis to
corrected inference, test selection, and special data structures. The notebooks
use an R Jupyter kernel ([IRkernel](https://irkernel.github.io/)); install it
once with `install.packages("IRkernel"); IRkernel::installspec()`, then open them
in Jupyter or VS Code. Every figure and number is computed live, and each
notebook ends with a `sessionInfo()` record and links to the rest of the series.

| Notebook | Theme | Highlights |
|----------|-------|------------|
| [`01-detecting-heteroscedasticity.ipynb`](01-detecting-heteroscedasticity.ipynb) | **Detection** | Quantifies the cost of ignoring non-constant variance (OLS vs HC3 standard errors, with a forest-plot of the intervals), visual diagnosis, the White / Breusch–Pagan / Koenker tests with theory, and machine-precision validation against `lmtest`. |
| [`02-remediating-heteroscedasticity.ipynb`](02-remediating-heteroscedasticity.ipynb) | **Remediation** | The three remedy families — robust (HC) standard errors, weighted least squares, variance-stabilising transforms — each demonstrated where it provably works (known-variance simulations, before/after residual plots) with honest real-data caveats, plus the recommendation engine. |
| [`03-modern-and-scalable.ipynb`](03-modern-and-scalable.ipynb) | **Modern & scalable** | A size-control study showing the classical χ² asymptotics over-rejecting under heavy tails while the studentized and permutation tests stay calibrated; a tour of the rank-permutation, quantile-regression and HC-covariance tests; and exact, memory-bounded streaming for large data. |
| [`04-group-wise-variance.ipynb`](04-group-wise-variance.ipynb) | **Group-wise variance** | Bartlett, Levene, Brown–Forsythe and Fligner–Killeen on `InsectSprays`, validated against `car`, plus a simulation exposing Bartlett's catastrophic over-rejection (~50%) under heavy tails versus the robust alternatives. |
| [`05-time-series-arch.ipynb`](05-time-series-arch.ipynb) | **Time series & ARCH** | Conditional heteroscedasticity and volatility clustering: the level-vs-squared-residual ACF signature, Engle's ARCH LM and McLeod–Li tests, calibration on white noise, and how to choose the lag order. |
| [`06-choosing-a-test.ipynb`](06-choosing-a-test.ipynb) | **Choosing a test** | A Monte-Carlo power study (heatmap) over six tests and several variance shapes, distilled into a practical decision guide: the best test is the one whose auxiliary model matches the variance you actually have. |

`diagnostics.ipynb` is a minimal one-call example kept for backwards
compatibility.

## Regenerating

Each notebook is generated (without outputs) by a builder under `scripts/`
(sharing `scripts/nb_helpers.R`), then executed to populate outputs:

```bash
Rscript scripts/build_nb_01.R    # build_nb_01.R ... build_nb_06.R
jupyter nbconvert --to notebook --execute --inplace \
  --ExecutePreprocessor.kernel_name=ir inst/tutorials/01-detecting-heteroscedasticity.ipynb
```
