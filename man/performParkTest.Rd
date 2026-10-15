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
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performParkTest(m, mtcars, "wt")
}
\references{Park, R. E. (1966). Estimation with heteroscedastic error terms. \emph{Econometrica}, 34(4), 888. \doi{10.2307/1910108}}
