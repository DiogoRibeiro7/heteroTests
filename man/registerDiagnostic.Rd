\name{registerDiagnostic}
\alias{registerDiagnostic}
\title{Register a custom diagnostic}
\description{Adds a function to the diagnostics registry so it can be called by \code{runHeteroTests}.}
\usage{registerDiagnostic(name, fun)}
\arguments{
  \item{name}{Name of the diagnostic.}
  \item{fun}{Function taking \code{model} and \code{data} arguments.}
}
\details{A diagnostic registered this way is never cached by \code{\link{cachedTest}} or \code{\link{runHeteroTests}},
nor is a test of the package registered again under its name: whether it draws random numbers, or reads anything
besides the model and the data, cannot be known.}
\value{Invisibly returns \code{NULL}.}
\seealso{\code{\link{listDiagnostics}} for the names already registered.}
\examples{
custom <- function(model, data) list(statistic = 0)
registerDiagnostic("custom", custom)
}
