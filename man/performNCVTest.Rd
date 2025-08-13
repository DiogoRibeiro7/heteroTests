\name{performNCVTest}
\alias{performNCVTest}
\title{Perform NCV test}
\description{Approximates the NCV test by regressing absolute residuals on fitted values.}
\details{The test statistic is computed from the studentized regression of \eqn{|e|} on \eqn{\hat{y}}. It is asymptotically chi-square with one degree of freedom.}
\usage{performNCVTest(model)}
\arguments{
  \item{model}{an object of class \code{lm}.}
}
\value{An object of class \code{htest}.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performNCVTest(m)
}
\references{Fox, J., & Weisberg, S. (2019). \emph{An R Companion to Applied Regression} (3rd ed.). SAGE.}
