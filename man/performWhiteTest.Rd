\name{performWhiteTest}
\alias{performWhiteTest}
\title{Perform White's test for heteroscedasticity}
\description{Implements White's test on a fitted linear model.}
\details{An auxiliary regression of \eqn{e^2} on all regressors, their squares and cross-products produces \eqn{R^2}. The statistic \eqn{n R^2} follows a chi-square distribution whose degrees of freedom are the rank of the auxiliary regression less its intercept (see \emph{Degrees of freedom}).}
\section{Degrees of freedom}{Let \eqn{Z} hold the regressors, their squares and their cross-products. The auxiliary regression of \eqn{e^2} on \eqn{Z} has an intercept, and the degrees of freedom are the rank of \eqn{[1, Z]} minus one, as \code{lmtest::bptest()} counts them given the same terms as \code{varformula}. A column of \eqn{Z} that is a linear combination of the intercept and the columns before it, at the default tolerance of \code{\link{qr}()}, is dropped. That happens when the columns of \eqn{Z} span the constant: the squares of the dummies of \code{contr.sum} or \code{contr.helmert} contrasts, the dummies of every level of a factor in a model without an intercept, an interaction with an empty cell such as that of the baseline levels, or the squares of an orthogonal \code{poly()} basis. In 0.15.0 the degrees of freedom were the rank of \eqn{Z} alone, one more on such a design, with the same statistic and a larger p-value: on \code{lm(y ~ x1 + f, contrasts = list(f = "contr.sum"))}, with a factor of two levels, 5 where \code{lmtest::bptest()} gives 4. On a design whose auxiliary columns do not span the constant, such as continuous regressors or factors with the default treatment contrasts, nothing changed. The same holds with \code{cross_products = FALSE} or more regressors than \code{max_interactions}, and for the tests computed through this function: \code{\link{performWhiteTestRobust}()}, whose interval, effect size and power follow the degrees of freedom, and the registered \code{"white"} of \code{\link{runHeteroTests}()}. The p-value of \code{\link{performWhiteTestBootstrap}()} depends on the statistic alone and does not change on such a design; \code{\link{performWhiteTestStreaming}()} stops on such a design, whose auxiliary columns are collinear with its intercept, and otherwise gives these degrees of freedom. On an auxiliary design whose columns are nearly collinear, as the squares and products of the basis of a smooth term can be, the columns kept are those \code{lm(e^2 ~ Z)} keeps, which can differ from those kept in 0.15.0, so the statistic can change too: there the auxiliary regression of 0.15.0 could silently lose a column to its intercept while its degrees of freedom still counted it, as on a \code{mgcv::gam(y ~ s(x1) + x2)} fit whose statistic moves by 0.28\%, to the value of \code{lmtest::bptest()} with the same terms.}
\section{Weighted fits}{On a fit with weights the test is computed from the Pearson residuals \eqn{\sqrt{w_i}\, e_i}, so it tests whether the weights are adequate; see the section on weighted fits in \code{\link{performKoenkerTest}()}, which also covers weights estimated by \code{\link{fitWLS}()}.}
\usage{performWhiteTest(model, data = NULL, cross_products = TRUE, max_interactions = 10)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{The data frame \code{model} was fitted to, or \code{NULL}, the default. The response, design, weights and offset are read from the fit itself, so a model that transforms its variables, has an \code{offset()} term, or takes its \code{weights} or \code{offset} from a column of the data needs no \code{data}, and the model frame of \code{model} is accepted as \code{data} (since 0.16.0; before, such models needed \code{data}). Data that are given are matched to the observations of the fit: rows with row names of their own by name, in any order; rows that only number themselves (a tibble, a data.table, a data frame whose row names were reset) by the row numbers of the fit, or in the order of the fit when they hold one row per observation. The rows the fit dropped may be there or not. Data whose rows cannot be shown to hold the observations of the fit are refused; see \code{\link{test_results}}.}
  \item{cross_products}{Logical. Include cross-product terms in the auxiliary regression?}
  \item{max_interactions}{Maximum number of cross-product terms admitted to the auxiliary
    regression. Guards the auxiliary design against growing quadratically with the number
    of regressors.}
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
 performWhiteTest(m, mtcars)
}
\references{
White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator and a direct test for heteroskedasticity. \emph{Econometrica}, 48(4), 817--838. \doi{10.2307/1912934}

Greene, W. H. (2018). \emph{Econometric Analysis} (8th ed.). Pearson.
}
