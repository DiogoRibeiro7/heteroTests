\name{performParkTest}
\alias{performParkTest}
\title{Perform Park test for heteroscedasticity}
\description{Regresses the log of squared residuals on the log of a suspected variable.}
\details{The slope coefficient from \eqn{\log(e^2)} regressed on \eqn{\log x} is tested for zero using a t statistic. A significant coefficient indicates scale depending on \eqn{x}.}
\usage{performParkTest(model, data = NULL, variable)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}. When \code{NULL}, the default, the model frame of \code{model} is used, so \code{variable} must then be a variable of the model. The model frame holds the terms of the model rather than its variables (\code{log(x)} rather than \code{x}), so a model that transforms a variable needs \code{data}, as does a model with an \code{offset()} term.}
  \item{variable}{name of the suspected explanatory variable.}
}
\value{An object of class \code{htest} containing the t statistic, p-value and degrees of freedom.}
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, unweighted
or with positive weights, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it
tests on the estimable part of its design: it gives the value of the same model fitted without the
aliased columns. It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit, a
fit with a zero weight, a coefficient that is not finite for another reason, and \code{data} that
lack rows or variables of the fit. Which model each test accepts, and why, is set out in
\code{\link{test_results}}, under \emph{Supported models}.
}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performParkTest(m, mtcars, "wt")
}
\references{Park, R. E. (1966). Estimation with heteroscedastic error terms. \emph{Econometrica}, 34(4), 888. \doi{10.2307/1910108}}
