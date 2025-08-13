\name{runDiagnostics}
\alias{runDiagnostics}
\title{Run a suite of model diagnostics}
\description{Combines heteroscedasticity checks with tests for collinearity, nonlinearity and influential observations.}
\usage{runDiagnostics(model, data = NULL, tests = c("white", "breusch_pagan"), power = 2:3)}
\arguments{\item{model}{A fitted \code{lm} model or a formula.}\item{data}{Optional data frame if \code{model} is a formula.}\item{tests}{Heteroscedasticity tests to run.}\item{power}{Powers for \code{performRESETTest}.}}
\value{A list with the results of all diagnostics.}
\examples{data(mtcars)
runDiagnostics(mpg ~ wt + qsec, mtcars)}
