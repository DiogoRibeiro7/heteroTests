\name{compareModelDiagnostics}
\alias{compareModelDiagnostics}
\title{Compare diagnostics across models}
\description{Runs \code{runDiagnostics} for multiple models and collects test statistics.
Diagnostics that cannot be computed (for example, due to validation failures)
emit warnings and fill the corresponding entries with \code{NA_real_}. The
original error messages are preserved on the returned data frame via the
\code{diagnostic_errors} attribute.}
\usage{compareModelDiagnostics(models, data = NULL, tests = c("white", "breusch_pagan"))}
\arguments{
  \item{models}{List of fitted models or formulas.}
  \item{data}{Optional data frame when formulas are supplied.}
  \item{tests}{Diagnostics to run.}
}
\value{A data frame of statistics, one row per model. Failed diagnostics are
represented by \code{NA_real_} entries and the underlying error messages can be
retrieved from the \code{diagnostic_errors} attribute.}
\examples{
 data(mtcars)
 m1 <- lm(mpg ~ wt + qsec, mtcars)
 m2 <- lm(mpg ~ wt + hp, mtcars)
 compareModelDiagnostics(list(m1, m2))
}
\seealso{\code{\link{analyzeMLResiduals}}}

