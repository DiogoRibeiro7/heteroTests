\name{performKoenkerTest}
\alias{performKoenkerTest}
\title{Perform Koenker studentized Breusch-Pagan test}
\description{Implementation of the Koenker version of the Breusch-Pagan test.}
\details{Squared residuals are regressed on the regressors but the statistic \eqn{n R^2} uses a studentized form that is robust to non-normality.}
\section{Weighted fits}{
\code{lm(..., weights = w)} states that the error variance is \eqn{\sigma^2 / w_i}. On such a fit the test is computed from the Pearson residuals \eqn{\sqrt{w_i}\, e_i}, the residuals of the equivalent unweighted regression, which have constant variance when the weights are right. The null hypothesis is then that the weights are adequate; the variance regressors stay on their original scale. Releases before 0.12.0 used the raw residuals, whose variance differs across observations by assumption, so a correctly weighted fit was rejected as often as an unweighted one. A fit with a zero weight is refused.

The reference distribution takes the weights as known. When they were estimated from the same data, as by \code{\link{fitWLS}()}, it is only approximate.
}
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
