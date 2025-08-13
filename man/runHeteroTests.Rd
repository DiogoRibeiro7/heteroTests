\name{runHeteroTests}
\alias{runHeteroTests}
\title{Run multiple heteroscedasticity diagnostics}
\description{Convenience wrapper that executes several heteroscedasticity tests on a fitted linear model.}
\details{By default runs \code{\link{performWhiteTest}} and \code{\link{performBPTest}}, but additional tests can be requested. Custom diagnostics registered via \code{registerDiagnostic} are also available.}
\usage{runHeteroTests(model, data = NULL, tests = c("white", "breusch_pagan"))}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{optional data frame used to fit \code{model}. If omitted, \code{model.frame(model)} is used.}
  \item{tests}{character vector of test names to run. Supported values are any names registered via \code{registerDiagnostic}.}
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
