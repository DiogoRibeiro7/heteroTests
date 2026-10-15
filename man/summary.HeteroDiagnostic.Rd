\name{summary.HeteroDiagnostic}
\alias{summary.HeteroDiagnostic}
\title{Summarize diagnostics}
\usage{\method{summary}{HeteroDiagnostic}(object, tests = c("white", "koenker"), ...)}
\arguments{
  \item{object}{A \code{HeteroDiagnostic} object.}
  \item{tests}{Character vector naming the diagnostics to summarise. Defaults to the White and Koenker tests, the
  default of \code{\link{runHeteroTests}}; it was the White and the classical Breusch-Pagan tests up to 0.14.0.}
  \item{...}{Further arguments passed to the individual tests.}
}
\value{Named numeric vector of test statistics.}
\description{Summarize diagnostic test results.}
