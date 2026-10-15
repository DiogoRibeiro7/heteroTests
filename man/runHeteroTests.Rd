\name{runHeteroTests}
\alias{runHeteroTests}
\title{Run multiple heteroscedasticity diagnostics}
\description{Convenience wrapper that executes several heteroscedasticity tests on a fitted linear model.}
\details{By default runs \code{\link{performWhiteTest}} and \code{\link{performKoenkerTest}}, the studentized
Breusch-Pagan test, but additional tests can be requested. Custom diagnostics registered via \code{registerDiagnostic}
are also available.

Up to 0.14.0 the default was \code{c("white", "breusch_pagan")}, whose second test is the classical Breusch-Pagan
statistic of \code{\link{performBPTest}}. That statistic divides by \eqn{2 \hat\sigma^4}{2 sigma^4}, the variance of
the squared errors only when they are normal. In the full sweep of \code{inst/validation/}, with 150 observations and
400 replications, it rejected a true null hypothesis 28.75\% of the time at the 5\% level when the errors had a
\eqn{t_5}{t5} distribution, against 4.75\% for Koenker's statistic. The classical test is still run by name,
\code{tests = "breusch_pagan"}.

When a test stops with an error, the substitute registered for it is run, if one is and it succeeds, and its result
is returned under the name of the test asked for: \code{"white"} falls back on White's test without cross products,
the classical Breusch-Pagan, Koenker and Cook-Weisberg tests, \code{"koenker"} on the Cook-Weisberg test. The
\code{method} of the result names the test that ran, \code{attr(result, "diagnostic")} its registry name and
\code{attr(result, "extras")$status} is \code{"fallback"}. A weighted fit whose weights are a column of the data,
called without \code{data}, is such a case: White's and Koenker's tests stop, and both names hold the Cook-Weisberg
test. Pass \code{data} for such a fit.}
\usage{
runHeteroTests(
  model,
  data = NULL,
  tests = c("white", "koenker"),
  use_cache = TRUE,
  chunk_threshold_mb = 100,
  chunk_size = 10000,
  progress = interactive()
)
}
\arguments{
  \item{model}{a fitted \code{lm}, or a \code{glm} with the Gaussian family and the identity link, a model
  formula, a tidymodels workflow or a parsnip model fit. The tests refuse a \code{glm} of any other family or link
  since 0.15.0.}
  \item{data}{optional data frame used to fit \code{model}. If omitted, \code{model.frame(model)} is used, which
  serves only the tests that find what they need there (see their \code{data} argument). With
  data grouped by \code{dplyr::group_by()} the model is refitted within each group by evaluating its own call on that
  group, so every variable it uses, including weights, offset and subset, must be a column of \code{data}.}
  \item{tests}{character vector of test names to run: any of the names \code{\link{listDiagnostics}()} returns, which
  include those added with \code{registerDiagnostic}.}
  \item{use_cache}{logical indicating whether cached results should be reused when available. Requires the \pkg{digest}
  package and defaults to \code{TRUE}.}
  \item{chunk_threshold_mb}{numeric threshold (in megabytes) above which streaming implementations are preferred for supported
  diagnostics.}
  \item{chunk_size}{integer number of observations processed per chunk when streaming diagnostics.}
  \item{progress}{logical flag controlling whether textual progress bars are displayed when running multiple diagnostics.}
}
\value{A named list of \code{htest} objects.}
\examples{
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
# Default: White and Koenker
runHeteroTests(m, mtcars)
# Specify additional diagnostics, or the classical Breusch-Pagan test
runHeteroTests(m, mtcars, tests = c("white", "koenker", "cook_weisberg"))
runHeteroTests(m, mtcars, tests = "breusch_pagan")
custom <- function(model, data) list(stat = 1)
registerDiagnostic("custom", custom)
runHeteroTests(m, mtcars, tests = c("white", "custom"))
}
\references{White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator and a direct test for heteroskedasticity. \emph{Econometrica}, 48(4), 817--838.
Breusch, T. S., & Pagan, A. R. (1979). A Simple Test for Heteroscedasticity and Random Coefficient Variation. \emph{Econometrica}, 47(5), 1287--1294.
Koenker, R. (1981). A note on studentizing a test for heteroscedasticity. \emph{Journal of Econometrics}, 17(1), 107--112. \doi{10.1016/0304-4076(81)90062-2}}
