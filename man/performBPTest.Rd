\name{performBPTest}
\alias{performBPTest}
\alias{performBreuschPaganTest}
\title{Perform Breusch-Pagan test for heteroscedasticity}
\description{Implements the classical Breusch-Pagan (1979) test on a fitted linear model, in which the scaled squared residuals are regressed on the original regressors.}
\details{The test statistic is half the explained sum of squares from regressing the scaled squared residuals \eqn{e_i^2/\hat{\sigma}^2 - 1} (with \eqn{\hat{\sigma}^2 = \sum e_i^2 / n}) on the explanatory variables. Under the null hypothesis of homoscedasticity \emph{and normal disturbances} it follows a chi-square distribution with degrees of freedom equal to the number of regressors, matching \code{lmtest::bptest(..., studentize = FALSE)}. For the studentized \eqn{n R^2} form that drops the normality assumption use \code{\link{performKoenkerTest}} or \code{\link{performStudentizedBPTest}}.}
\usage{performBPTest(model, data)
performBreuschPaganTest(model, data)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
}
\value{An object of class \code{htest} containing the test statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performBPTest(m, mtcars)
}
\references{
Breusch, T. S., & Pagan, A. R. (1979). A simple test for heteroscedasticity and random coefficient variation. \emph{Econometrica}, 47(5), 1287--1294. \doi{10.2307/1911963}

Koenker, R. (1981). A note on studentizing a test for heteroscedasticity. \emph{Journal of Econometrics}, 17(1), 107--112. \doi{10.1016/0304-4076(81)90062-2}
}
