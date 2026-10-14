\name{test.HeteroDiagnostic}
\alias{test}
\alias{test.HeteroDiagnostic}
\title{Run diagnostic tests}
\usage{test(object, ...)}
\arguments{
  \item{object}{A \code{HeteroDiagnostic} object.}
  \item{...}{Further arguments passed to the individual tests.}
}
\value{List of test results.}
\description{Run the registered diagnostics on the model.}
\section{Deprecated}{\code{test()} is deprecated since 0.14.0: its name masks \code{devtools::test()}. Use \code{runDiagnostics(x$model, x$data, tests)}, which returns what \code{test(x, tests)} returns. It still works and warns; \code{summary()} of a \code{HeteroDiagnostic} object is unchanged.}
