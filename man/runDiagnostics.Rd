\name{runDiagnostics}
\alias{runDiagnostics}
\title{Run a suite of model diagnostics}
\description{Combines heteroscedasticity checks with tests for collinearity, nonlinearity and influential observations.}
\usage{
runDiagnostics(
  model,
  data = NULL,
  tests = c("white", "breusch_pagan"),
  power = 2:3,
  use_cache = TRUE,
  chunk_threshold_mb = 100,
  chunk_size = 10000,
  progress = interactive()
)
}
\arguments{\item{model}{A fitted \code{lm} model or a formula.}\item{data}{Optional data frame if \code{model} is a formula.}\item{tests}{Heteroscedasticity tests to run.}\item{power}{Powers for \code{performRESETTest}.}\item{use_cache}{Logical flag forwarded to \code{\link{runHeteroTests}} controlling whether cached diagnostic results may be reused.}\item{chunk_threshold_mb}{Numeric threshold passed to \code{\link{runHeteroTests}} for enabling streaming diagnostics on large datasets.}\item{chunk_size}{Integer chunk size supplied to \code{\link{runHeteroTests}} when streaming diagnostics.}\item{progress}{Logical flag controlling whether textual progress indicators are displayed during long-running operations.}}
\value{A list with the results of all diagnostics.}
\examples{data(mtcars)
runDiagnostics(mpg ~ wt + qsec, mtcars)}
