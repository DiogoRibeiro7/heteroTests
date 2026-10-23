\name{performKoenkerTest}
\alias{performKoenkerTest}
\title{Perform Koenker studentized Breusch-Pagan test}
\description{Implementation of the Koenker version of the Breusch-Pagan test.}
\details{Squared residuals are regressed on the regressors but the statistic \eqn{n R^2} uses a studentized form that is robust to non-normality.

Like White's test, it is not valid with fixed effects for small groups, a factor with few observations per level, and over-rejects on homoscedastic data, the more the smaller the groups and the more groups there are: in 2,000 draws of \code{lm(y ~ x + subject)} with 30 subjects it rejected at the 5\% level in 54\% with three observations of each, 22\% with four and 12\% with eight, and with 100 subjects of eight in 22\% (see \emph{Too few observations} in \code{\link{performWhiteTest}()}).}
\section{Weighted fits}{
\code{lm(..., weights = w)} states that the error variance is \eqn{\sigma^2 / w_i}. On such a fit the test is computed from the Pearson residuals \eqn{\sqrt{w_i}\, e_i}, the residuals of the equivalent unweighted regression, which have constant variance when the weights are right. The null hypothesis is then that the weights are adequate; the variance regressors stay on their original scale. Releases before 0.12.0 used the raw residuals, whose variance differs across observations by assumption, so a correctly weighted fit was rejected as often as an unweighted one. A fit with a zero weight is refused.

The reference distribution takes the weights as known. When they were estimated from the same data, as by \code{\link{fitWLS}()}, it is only approximate, and not more accurate in larger samples. With a correctly specified variance function and normal errors, the Koenker test rejected 8\% of the time at the 5\% level with 150 observations and 11\% with 600, the White test 6\% and 8\%, and the Harvey test never, because it repeats the regression the weights were estimated from (\code{inst/validation/weighted-fits-size.csv}). \code{\link{performVarianceFormTest}()} accounts for the estimation, held its level in the same designs, and is the test to use on a \code{fitWLS()} fit.
}
\usage{performKoenkerTest(model, data = NULL)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{The data frame \code{model} was fitted to, or \code{NULL}, the default. The response, design, weights and offset are read from the fit itself, so a model that transforms its variables, has an \code{offset()} term, or takes its \code{weights} or \code{offset} from a column of the data needs no \code{data}, and the model frame of \code{model} is accepted as \code{data} (since 0.16.0; before, such models needed \code{data}). Data that are given are matched to the observations of the fit: rows with row names of their own by name, in any order; rows that only number themselves (a tibble, a data.table, a data frame whose row names were reset) by the row numbers of the fit, or in the order of the fit when they hold one row per observation. The rows the fit dropped may be there or not. Data whose rows cannot be shown to hold the observations of the fit are refused; see \code{\link{test_results}}.}
}
\value{An object of class \code{htest} containing the test statistic, p-value and degrees of freedom.}
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
 performKoenkerTest(m, mtcars)
}
\references{Koenker, R. (1981). A note on studentizing a test for heteroscedasticity. \emph{Journal of Econometrics}, 17(1), 107--112.}
