\name{runHeteroTests}
\alias{runHeteroTests}
\title{Run multiple heteroscedasticity diagnostics}
\description{Convenience wrapper that executes several heteroscedasticity tests on a fitted linear model.}
\details{By default runs \code{\link{performWhiteTest}} and \code{\link{performBPTest}}, but additional tests can be requested. Custom diagnostics registered via \code{registerDiagnostic} are also available.}
\usage{
runHeteroTests(
  model,
  data = NULL,
  tests = c("white", "breusch_pagan"),
  use_cache = TRUE,
  chunk_threshold_mb = 100,
  chunk_size = 10000,
  progress = interactive()
)
}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{optional data frame used to fit \code{model}. If omitted, \code{model.frame(model)} is used.}
  \item{tests}{character vector of test names to run. Supported values are any names registered via \code{registerDiagnostic}.}
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
# Default: White and Breusch-Pagan
runHeteroTests(m, mtcars)
# Specify additional diagnostics
runHeteroTests(m, mtcars, tests = c("white", "koenker", "ncv"))
custom <- function(model, data) list(stat = 1)
registerDiagnostic("custom", custom)
runHeteroTests(m, mtcars, tests = c("white", "custom"))
}
\references{White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator and a direct test for heteroskedasticity. \emph{Econometrica}, 48(4), 817--838.
Breusch, T. S., & Pagan, A. R. (1979). A Simple Test for Heteroscedasticity and Random Coefficient Variation. \emph{Econometrica}, 47(5), 1287--1294.}
