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
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, unweighted
or with positive weights, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it
tests on the estimable part of its design: it gives the value of the same model fitted without the
aliased columns. It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit, a
fit with a zero weight, a coefficient that is not finite for another reason, and \code{data} that
lack rows or variables of the fit. Since 0.16.0 it is built on the model context, and also refuses
a multivariate \code{lm} ("needs a model with one response"), a fit whose coefficients, residuals or
weights were changed after the fit, and a fit with \code{model = FALSE} whose model frame, rebuilt
from the data it was fitted to, no longer holds its response, because those data changed after the
fit, with or without \code{data}. A fit with \code{model = FALSE} whose data are gone is tested on
\code{data} that give its response and fitted values, and refused without them. The test reads
nothing from \code{data}, and since 0.16.0 matches it to the observations of the fit as every
test does: data with row names of their own in any order, data that number their rows (a tibble,
a data.table) in the order of the fit; other numbered data, rows in front of those of the fit and
repeated row names are refused, where it gave the value of the fit. Which model each test
accepts, and why, is set out in
\code{\link{test_results}}, under \emph{Supported models}.
}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performDavidianCarrollTest(m)
}
\references{Davidian, M., & Carroll, R. J. (1987). Variance function estimation. \emph{Journal of the American Statistical Association}, 82(400), 1079--1091.}
