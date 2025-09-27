% Generated manually
\name{hetero_test_suite}
\alias{hetero_test_suite}
\alias{hetero_grouped_suite}
\title{S3 containers for heteroscedasticity diagnostics}
\description{
Collections of heteroscedasticity test results returned by \code{runHeteroTests}.
A \code{hetero_test_suite} is a simple named list of \code{htest} objects with
additional metadata. When diagnostics are evaluated on grouped data, the
returned object inherits from \code{hetero_grouped_suite} and contains one
\code{hetero_test_suite} per group along with the grouping keys.
}
\details{
These classes are primarily useful for method dispatch (e.g. the broom tidiers
and \code{autoplot()} methods). Users typically interact with the objects via
\code{generics::tidy()}, \code{generics::glance()}, \code{generics::augment()}
and \code{ggplot2::autoplot()}.
}
