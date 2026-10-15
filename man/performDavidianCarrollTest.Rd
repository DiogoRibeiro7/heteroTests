\name{performDavidianCarrollTest}
\alias{performDavidianCarrollTest}
\title{Perform Davidian-Carroll test}
\description{Regresses \code{log(residual^2)} on fitted values using a polynomial.}
\details{A polynomial in the fitted values is fitted to \eqn{\log(e^2)}. Joint significance of the polynomial terms is evaluated with an \eqn{F} statistic.}
\usage{performDavidianCarrollTest(model, data = NULL, degree = 2)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{optional data frame the model was fitted to. When \code{NULL}, the default, the model frame of \code{model} is used. The statistic is computed from the residuals and fitted values of \code{model}, so \code{data} does not change the result; it is accepted so that every test takes \code{(model, data, ...)}.}
  \item{degree}{polynomial degree. Before 0.15.0 \code{degree} was the second argument: a number passed there by position is still read as \code{degree}, with a warning that shows the new call; that form stops working in 1.0.0.}
}
\value{An object of class \code{htest}.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performDavidianCarrollTest(m)
}
\references{Davidian, M., & Carroll, R. J. (1987). Variance function estimation. \emph{Journal of the American Statistical Association}, 82(400), 1079--1091.}
