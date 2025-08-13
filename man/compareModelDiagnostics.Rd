\name{compareModelDiagnostics}
\alias{compareModelDiagnostics}
\title{Compare diagnostics across models}
\description{Runs \code{runDiagnostics} for multiple models and collects test statistics.}
\usage{compareModelDiagnostics(models, data = NULL, tests = c("white", "breusch_pagan"))}
\arguments{
  \item{models}{List of fitted models or formulas.}
  \item{data}{Optional data frame when formulas are supplied.}
  \item{tests}{Diagnostics to run.}
}
\value{A data frame of statistics, one row per model.}
\examples{
 data(mtcars)
 m1 <- lm(mpg ~ wt + qsec, mtcars)
 m2 <- lm(mpg ~ wt + hp, mtcars)
 compareModelDiagnostics(list(m1, m2))
}
\seealso{\code{\link{analyzeMLResiduals}}}

