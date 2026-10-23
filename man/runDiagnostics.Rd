\name{runDiagnostics}
\alias{runDiagnostics}
\title{Run a suite of model diagnostics}
\description{Combines heteroscedasticity checks with tests for collinearity, nonlinearity and influential observations.}
\usage{
runDiagnostics(
  model,
  data = NULL,
  tests = c("white", "koenker"),
  power = 2:3,
  use_cache = TRUE,
  chunk_threshold_mb = Inf,
  chunk_size = 10000,
  progress = interactive()
)
}
\arguments{\item{model}{A fitted \code{lm}, or a \code{glm} with the Gaussian family and the identity link, or a formula. The tests refuse a \code{glm} of any other family or link since 0.15.0.}\item{data}{Optional data frame if \code{model} is a formula.}\item{tests}{Heteroscedasticity tests to run. Defaults to the White and Koenker tests, the default of \code{\link{runHeteroTests}}; it was the White and the classical Breusch-Pagan tests up to 0.14.0.}\item{power}{Powers for \code{performRESETTest}.}\item{use_cache}{Logical flag forwarded to \code{\link{runHeteroTests}} controlling whether cached diagnostic results may be reused.}\item{chunk_threshold_mb}{Size of the data in megabytes above which \code{\link{runHeteroTests}} runs the streaming forms of the White, Breusch-Pagan and Koenker tests. The default, \code{Inf}, never streams; give a finite value to ask for them. Up to 0.15.0 the default was \code{100}.}\item{chunk_size}{Integer chunk size supplied to \code{\link{runHeteroTests}} when the streaming tests run.}\item{progress}{Logical flag controlling whether textual progress indicators are displayed during long-running operations.}}
\value{A list with the results of all diagnostics.}
\examples{data(mtcars)
runDiagnostics(mpg ~ wt + qsec, mtcars)}
