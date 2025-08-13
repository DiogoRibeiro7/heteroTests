\name{performBPTest}
\alias{performBPTest}
\title{Perform Breusch-Pagan test for heteroscedasticity}
\description{Implements the Breusch-Pagan test on a fitted linear model, where squared residuals are regressed on the original regressors.}
\details{The test statistic is computed as \eqn{n R^2}, with \eqn{R^2} coming from the auxiliary regression of \eqn{e^2} on the explanatory variables. Under the null hypothesis of homoscedasticity it follows a chi-square distribution with degrees of freedom equal to the number of regressors.}
\usage{performBPTest(model, data)}
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
\references{Breusch, T. S., & Pagan, A. R. (1979). A Simple Test for Heteroscedasticity and Random Coefficient Variation. \emph{Econometrica}, 47(5), 1287--1294.}
