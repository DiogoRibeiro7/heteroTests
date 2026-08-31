\name{summary.HeteroDiagnostic}
\alias{summary.HeteroDiagnostic}
\title{Summarize diagnostics}
\usage{\method{summary}{HeteroDiagnostic}(object, tests = c("white", "breusch_pagan"), ...)}
\arguments{
  \item{object}{A \code{HeteroDiagnostic} object.}
  \item{tests}{Character vector naming the diagnostics to summarise.}
  \item{...}{Further arguments passed to the individual tests.}
}
\value{Named numeric vector of test statistics.}
\description{Summarize diagnostic test results.}
