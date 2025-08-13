\name{performCameronTrivediTest}
\alias{performCameronTrivediTest}
\title{Perform Cameron-Trivedi decomposition test}
\description{Regresses squared residuals on fitted values and their squares.}
\details{Linear and quadratic terms are jointly tested in an auxiliary regression of \eqn{e^2} on \eqn{\hat{y}} and \eqn{\hat{y}^2}. The resulting \eqn{F} statistic assesses both linear and nonlinear forms of heteroscedasticity.}
\usage{performCameronTrivediTest(model)}
\arguments{
  \item{model}{an object of class \code{lm}.}
}
\value{An object of class \code{htest} containing the F statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performCameronTrivediTest(m)
}
\references{Cameron, A. C., & Trivedi, P. K. (1990). Regression-based tests for heteroskedasticity in the linear model. \emph{Journal of Econometrics}, 47(1), 267--288.}
