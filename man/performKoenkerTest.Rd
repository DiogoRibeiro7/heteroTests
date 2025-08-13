\name{performKoenkerTest}
\alias{performKoenkerTest}
\title{Perform Koenker studentized Breusch-Pagan test}
\description{Implementation of the Koenker version of the Breusch-Pagan test.}
\details{Squared residuals are regressed on the regressors but the statistic \eqn{n R^2} uses a studentized form that is robust to non-normality.}
\usage{performKoenkerTest(model, data)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
}
\value{An object of class \code{htest} containing the test statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performKoenkerTest(m, mtcars)
}
\references{Koenker, R. (1981). A note on studentizing a test for heteroscedasticity. \emph{Journal of Econometrics}, 17(1), 107--112.}
