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
\section{Attributes}{
A \code{hetero_test_suite} carries \code{attr(, "tests")}, the names asked for;
\code{attr(, "model")}, the model tested; \code{attr(, "data_source")}, the class of
the data, or \code{NA}; and, since 0.16.0, \code{attr(, "model_origin")} when
\code{runHeteroTests} fitted the model itself: \code{"formula"} when it was given a
formula, \code{"group"} for the refit of the model within a group of a grouped run,
and \code{"survey"} for the fit to the data of a survey design
(\code{\link{runSurveyHeteroTests}}). The call of such a model names variables of
the runner rather than the caller's, so \code{summary()} shows its formula
instead. A suite of a model you fitted has no \code{model_origin}. A
\code{hetero_grouped_suite} carries \code{attr(, "group_keys")} and
\code{attr(, "tests")}.
}
\details{
These classes are primarily useful for method dispatch (e.g. the broom tidiers
and \code{autoplot()} methods). Users typically interact with the objects via
\code{print()} and \code{summary()} (see \code{\link{summary.hetero_test_suite}}),
\code{generics::tidy()}, \code{generics::glance()}, \code{generics::augment()}
and \code{ggplot2::autoplot()}.
}
