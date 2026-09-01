\name{performModifiedBartlettTest}
\alias{performModifiedBartlettTest}
\title{Bartlett compatibility alias}
\description{Preserves the historical modified-Bartlett entry point while delegating to the canonical Bartlett implementation.}
\usage{performModifiedBartlettTest(model, data, group)}
\arguments{
  \item{model}{A fitted \code{lm} object.}
  \item{data}{Data frame used to fit \code{model}.}
  \item{group}{Grouping variable name.}
}
\details{The pre-0.7.1 implementation manually evaluated the usual finite-sample correction that is already part of the standard Bartlett chi-squared statistic. It did not define a distinct test. This function now delegates to \code{performBartlettTest()} so the two entry points cannot drift while compatibility is retained.}
\value{An object of class \code{htest} with statistic, degrees of freedom and p-value identical to \code{performBartlettTest()}.}
\examples{
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performModifiedBartlettTest(m, mtcars, "cyl")
}
\references{Bartlett, M. S. (1937). Properties of sufficiency and statistical tests. \emph{Proceedings of the Royal Society of London A}, 160(901), 268--282.}
