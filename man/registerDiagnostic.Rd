\name{registerDiagnostic}
\alias{registerDiagnostic}
\title{Register a custom diagnostic}
\description{Adds a function to the diagnostics registry so it can be called by \code{runHeteroTests}.}
\usage{registerDiagnostic(name, fun)}
\arguments{
  \item{name}{Name of the diagnostic.}
  \item{fun}{Function taking \code{model} and \code{data} arguments.}
}
\value{Invisibly returns \code{NULL}.}
\examples{
custom <- function(model, data) list(statistic = 0)
registerDiagnostic("custom", custom)
}
