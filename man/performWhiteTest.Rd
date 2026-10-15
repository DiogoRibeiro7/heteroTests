\name{performWhiteTest}
\alias{performWhiteTest}
\title{Perform White's test for heteroscedasticity}
\description{Implements White's test on a fitted linear model.}
\details{An auxiliary regression of \eqn{e^2} on all regressors, their squares and cross-products produces \eqn{R^2}. The statistic \eqn{n R^2} follows a chi-square distribution with degrees of freedom equal to the number of regressors in the auxiliary model.}
\section{Weighted fits}{On a fit with weights the test is computed from the Pearson residuals \eqn{\sqrt{w_i}\, e_i}, so it tests whether the weights are adequate; see the section on weighted fits in \code{\link{performKoenkerTest}()}, which also covers weights estimated by \code{\link{fitWLS}()}.}
\usage{performWhiteTest(model, data = NULL, cross_products = TRUE, max_interactions = 10)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{Data frame used to fit \code{model}. When \code{NULL}, the default, the model frame of \code{model} is used. It holds the terms of the model rather than its variables (\code{log(x)} rather than \code{x}), so a model that transforms a variable needs \code{data}, and so may a weighted fit whose weights are a column of the data, which the model frame holds as \code{(weights)}.}
  \item{cross_products}{Logical. Include cross-product terms in the auxiliary regression?}
  \item{max_interactions}{Maximum number of cross-product terms admitted to the auxiliary
    regression. Guards the auxiliary design against growing quadratically with the number
    of regressors.}
}
\value{An object of class \code{htest} containing the test statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performWhiteTest(m, mtcars)
}
\references{
White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator and a direct test for heteroskedasticity. \emph{Econometrica}, 48(4), 817--838. \doi{10.2307/1912934}

Greene, W. H. (2018). \emph{Econometric Analysis} (8th ed.). Pearson.
}
