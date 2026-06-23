#!/usr/bin/env Rscript
# Builds inst/tutorials/05-time-series-arch.ipynb (without outputs).
# Run from the package root:  Rscript scripts/build_nb_05.R
source("scripts/nb_helpers.R")

cells <- list(

series_nav("05"),

md("# Time Series and ARCH Effects",
   "",
   "**Detecting conditional heteroscedasticity and volatility clustering**",
   "",
   "---",
   "",
   "In cross-sectional data the error variance depends on covariates. In time series",
   "it often depends on the *recent past*: large shocks cluster together, producing",
   "the volatility clustering familiar from financial returns. This is **conditional**",
   "heteroscedasticity, captured by the ARCH (autoregressive conditional",
   "heteroscedasticity) family,",
   "",
   "$$\\varepsilon_t = \\sigma_t z_t, \\qquad \\sigma_t^2 = \\alpha_0 + \\sum_{j=1}^{q} \\alpha_j \\varepsilon_{t-j}^2 .$$",
   "",
   "The diagnostic question is whether the squared residuals are serially correlated.",
   "`heteroTests` provides two complementary tests for this: **Engle's ARCH LM test**",
   "and the **McLeod--Li portmanteau test**. Both take a fitted `lm`/`glm` whose",
   "residuals are in time order."),

code("library(heteroTests)",
     "ht_set_log_level(\"SILENT\")  # keep simulation loops quiet",
     "library(ggplot2)",
     "theme_set(theme_minimal(base_size = 12))",
     "options(digits = 4)"),

md("## 1. Simulating volatility clustering",
   "",
   "We generate an ARCH(1) process. There is no autocorrelation in the level of the",
   "series, only in its variance --- the hallmark that ARCH tests are built to find."),

code("arch1 <- function(n, alpha0 = 0.2, alpha1 = 0.7) {",
     "  e <- rnorm(n); y <- numeric(n); y[1] <- e[1]",
     "  for (t in 2:n) y[t] <- e[t] * sqrt(alpha0 + alpha1 * y[t - 1]^2)",
     "  y",
     "}",
     "set.seed(1)",
     "y <- arch1(500)",
     "ggplot(data.frame(t = seq_along(y), y = y), aes(t, y)) +",
     "  geom_line(colour = \"grey30\") +",
     "  labs(x = \"Time\", y = expression(y[t]),",
     "       title = \"An ARCH(1) series\",",
     "       subtitle = \"Quiet and turbulent stretches alternate: volatility clusters\")"),

md("## 2. The diagnostic signature",
   "",
   "ARCH leaves a clean fingerprint: the residuals themselves look like white noise",
   "(no autocorrelation in the level), but the *squared* residuals are strongly",
   "autocorrelated. Plotting both autocorrelation functions side by side makes the",
   "case before any test is run."),

code("model <- lm(y ~ 1)",
     "r <- residuals(model)",
     "acf_df <- function(x, lab) {",
     "  a <- acf(x, lag.max = 15, plot = FALSE)",
     "  data.frame(lag = a$lag[-1], acf = a$acf[-1], series = lab)",
     "}",
     "both <- rbind(acf_df(r, \"residuals\"), acf_df(r^2, \"squared residuals\"))",
     "ci <- 1.96 / sqrt(length(r))",
     "ggplot(both, aes(lag, acf)) +",
     "  geom_hline(yintercept = c(-ci, ci), linetype = \"dashed\", colour = \"firebrick\") +",
     "  geom_segment(aes(xend = lag, yend = 0)) +",
     "  facet_wrap(~series) +",
     "  labs(x = \"Lag\", y = \"Autocorrelation\",",
     "       title = \"Level vs. squared residuals\",",
     "       subtitle = \"No structure in the level; strong structure in the squares (ARCH)\")"),

md("The level residuals stay inside the white-noise bands, while the squared",
   "residuals breach them at several lags. That contrast *is* conditional",
   "heteroscedasticity."),

md("## 3. Formal tests",
   "",
   "**Engle's ARCH LM test** regresses $e_t^2$ on its own $q$ lags and forms",
   "$nR^2 \\sim \\chi^2_q$. **McLeod--Li** is a portmanteau test on the",
   "autocorrelations of the squared residuals. Both should reject decisively here."),

code("performArchLMTest(model, lags = 5)"),

code("performMcLeodLiTest(model, lags = 5)"),

md("### Calibration on a quiet series",
   "",
   "A test that always fires is useless. On i.i.d. Gaussian noise --- no ARCH ---",
   "neither test should reject."),

code("set.seed(2)",
     "y0 <- rnorm(500); m0 <- lm(y0 ~ 1)",
     "c(ARCH_LM   = signif(performArchLMTest(m0, lags = 5)$p.value, 3),",
     "  McLeod_Li = signif(performMcLeodLiTest(m0, lags = 5)$p.value, 3))"),

md("## 4. Choosing the lag order",
   "",
   "Both tests require a lag order $q$. Too few lags can miss higher-order dynamics;",
   "too many dilute the signal across degrees of freedom and cost power. We trace",
   "the power of the ARCH LM test against $q$ on the ARCH(1) process above."),

code("set.seed(3)",
     "power_at_lag <- function(L, reps = 150, n = 300) {",
     "  rej <- 0",
     "  for (i in seq_len(reps)) rej <- rej + (performArchLMTest(lm(arch1(n) ~ 1), lags = L)$p.value < 0.05)",
     "  100 * rej / reps",
     "}",
     "data.frame(lags = c(1, 2, 5, 10),",
     "           power_pct = sapply(c(1, 2, 5, 10), power_at_lag))"),

md("Power is highest at short lags --- appropriate, since the data-generating",
   "process is ARCH(1) --- and erodes gently as unnecessary lags are added. In",
   "practice, choose $q$ from the apparent persistence of the squared-residual",
   "autocorrelations (the right-hand panel above), or compare a small grid as here."),

md("## 5. Takeaways",
   "",
   "- **Conditional heteroscedasticity is about the squared residuals.** Inspect their",
   "  autocorrelation before testing; the level residuals can look perfectly clean.",
   "- **Engle's ARCH LM and McLeod--Li are complementary.** The first targets a",
   "  specific autoregressive order; the second is an omnibus portmanteau check.",
   "- **Both need time-ordered residuals** from a fitted `lm`/`glm`, and a sensible",
   "  lag order. Detecting ARCH is the cue to model the conditional variance",
   "  explicitly (an ARCH/GARCH model) rather than to reach for a single corrected",
   "  standard error.",
   "",
   "---"),

code("sessionInfo()")
)

write_nb(notebook(cells), "inst/tutorials/05-time-series-arch.ipynb")
