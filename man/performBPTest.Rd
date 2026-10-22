\name{performBPTest}
\alias{performBPTest}
\alias{performBreuschPaganTest}
\title{Perform Breusch-Pagan test for heteroscedasticity}
\description{Implements the classical Breusch-Pagan (1979) test on a fitted linear model, in which the scaled squared residuals are regressed on the original regressors.}
\details{The test statistic is half the explained sum of squares from regressing the scaled squared residuals \eqn{e_i^2/\hat{\sigma}^2 - 1} (with \eqn{\hat{\sigma}^2 = \sum e_i^2 / n}) on the explanatory variables. Under the null hypothesis of homoscedasticity \emph{and normal disturbances} it follows a chi-square distribution with degrees of freedom equal to the number of regressors, matching \code{lmtest::bptest(..., studentize = FALSE)}. For the studentized \eqn{n R^2} form that drops the normality assumption use \code{\link{performKoenkerTest}}.

Like White's test, it is not valid with fixed effects for small groups, a factor with few observations per level, and over-rejects on homoscedastic data, the more the smaller the groups and the more groups there are: in 2,000 draws of \code{lm(y ~ x + subject)} with normal errors and 30 subjects it rejected at the 5\% level in 38\% with three observations of each, 26\% with four and 15\% with eight, and with 100 subjects of eight in 24\% (see \emph{Too few observations} in \code{\link{performWhiteTest}()}).}
\section{Deprecated name}{\code{performBreuschPaganTest()} is deprecated since 0.14.0. It was the same function as \code{performBPTest()} under a second name; it still works, returns what \code{performBPTest()} returns and warns. Use \code{performBPTest()}.}
\section{Weighted fits}{On a fit with weights the test is computed from the Pearson residuals \eqn{\sqrt{w_i}\, e_i}, so it tests whether the weights are adequate; see the section on weighted fits in \code{\link{performKoenkerTest}()}, which also covers weights estimated by \code{\link{fitWLS}()}.}
\usage{performBPTest(model, data = NULL)
performBreuschPaganTest(model, data = NULL)}
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
 performBPTest(m, mtcars)
}
\references{
Breusch, T. S., & Pagan, A. R. (1979). A simple test for heteroscedasticity and random coefficient variation. \emph{Econometrica}, 47(5), 1287--1294. \doi{10.2307/1911963}

Koenker, R. (1981). A note on studentizing a test for heteroscedasticity. \emph{Journal of Econometrics}, 17(1), 107--112. \doi{10.1016/0304-4076(81)90062-2}
}
