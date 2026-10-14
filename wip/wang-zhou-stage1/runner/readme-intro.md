### A consistent test of the variance function: a second prototype

The first prototype, of the test of Dette, Neumeyer and Van Keilegom, did not
hold its level when the variance is not constant (the previous section). The
author chose to try the test of Wang and Zhou (2007) next, whose theory
assumes a parametric mean, which is the package's setting. With `r` the
squared residual less the fitted variance, the test estimates
`E[r E(r | x) p(x)]`, which is zero exactly when the variance function is
right, by a leave-one-out kernel average of the products `r-hat_i r-hat_j`,
and takes its null distribution from a residual bootstrap under the fitted
model. This section is the stage 1 of that test: a prototype in plain R, which
the package does not export, and a study of it.
`variance-form-wz-design.md` defines the test step by step and ends with a
recommendation. The published article could not be obtained; the prototype
follows the authors' working paper of September 2006, a version revised after
review, which the author accepted as the source for stage 1 on 2026-10-12, and
the table reproduced below is the working paper's.

**The working paper's Table 1.** Simulation study 1 of the paper:
`Y = 1 + 2 x + sigma(x) e` with `x` uniform on (0, 1), standard normal errors,
and the null hypothesis that the variance is log-linear in `x`. The standard
deviation is `exp(-0.5 - 0.25 x)` in (1), the null hypothesis, and has a
log-quadratic term in (2) and a squared sine in (3). The prototype runs with
`B = 200` bootstrap samples at the paper's four bandwidths, which share the
samples and the bootstrap samples, and beside it the Wald test of the paper's
table, of a quadratic term in the log-variance under normal errors. Each cell
gives the rate here and, in brackets, the paper's. The paper names no kernel,
so the script runs the Gaussian and the Epanechnikov kernels.
