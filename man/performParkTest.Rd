\name{performParkTest}
\alias{performParkTest}
\title{Perform Park test for heteroscedasticity}
\description{Regresses the log of squared residuals on the log of a suspected variable.}
\details{The slope coefficient from \eqn{\log(e^2)} regressed on \eqn{\log x} is tested for zero using a t statistic. A significant coefficient indicates scale depending on \eqn{x}.}
\usage{performParkTest(model, data, variable)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
  \item{variable}{name of the suspected explanatory variable.}
}
\value{An object of class \code{htest} containing the t statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performParkTest(m, mtcars, "wt")
}
\references{Park, R. E. (1966). Estimation with heteroscedastic error terms. \emph{Econometrica}, 34(5), 888.}
