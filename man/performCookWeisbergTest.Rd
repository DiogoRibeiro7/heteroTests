\name{performCookWeisbergTest}
\alias{performCookWeisbergTest}
\title{Perform Cook-Weisberg test for heteroscedasticity}
\description{The Cook-Weisberg (1983) score test, with the fitted values as the sole variance regressor by default, or against a variance model given by \code{var_formula}. Since 0.14.0 it is the one entry point for the test; \code{\link{performNCVTest}} is deprecated.}
\details{
Writing \eqn{\hat{\sigma}^2 = \sum_i \hat{e}_i^2 / n} for the maximum-likelihood error variance, the test regresses the scaled squared residuals \eqn{\hat{e}_i^2 / \hat{\sigma}^2} on the fitted values and refers half the explained sum of squares to a chi-square distribution with one degree of freedom. This is the diagnostic returned by Stata's \code{estat hettest} with the \code{fitted} option, and it is exactly \code{\link{performNCVTest}} with its default variance model.

The chi-square reference distribution follows from the null variance of \eqn{\hat{e}^2 / \sigma^2} being 2, which holds under normal errors. When that assumption is doubtful, prefer \code{\link{performKoenkerTest}}.
}
\usage{performCookWeisbergTest(model, data = NULL, var_formula = NULL)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{optional data frame the model was fitted to, in which \code{var_formula} is evaluated. When \code{NULL}, the default, the model frame of \code{model} is used. Without \code{var_formula} the statistic is computed from the residuals and fitted values of \code{model}, and \code{data} does not change the result. With it, the variables of \code{var_formula} are read at the rows of \code{data} that hold the observations of the fit, matched as the tests that read \code{data} match them: data with row names of their own in any order, data that number their rows (a tibble, a data.table) in the order of the fit. Since 0.16.0 that includes numbered data with one row per observation, such as the complete rows of a fit that dropped rows numbered again, unless two rows of the fit tie on every column of the model frame and \code{var_formula} names a variable beyond it. A variable of \code{var_formula} is read from \code{data}, or without \code{data} from the model frame, which holds the variables of the model under their own names; since 0.16.0 one that neither holds is refused, where it was found in the workspace, whatever the order of its values.}
  \item{var_formula}{optional one-sided formula giving the variance model, evaluated in \code{data} (for example \code{~ x1 + x2}). When \code{NULL}, the default, the fitted values are the variance regressor and the result is what this function returned before 0.14.0; otherwise it is what the deprecated \code{performNCVTest(model, var_formula)} returned, labels included. In 0.14.0 \code{var_formula} was the second argument: a formula passed there by position is still read as \code{var_formula}, with a warning that shows the new call; that form stops working in 1.0.0.}
}
\value{An object of class \code{htest} containing the score statistic, its single degree of freedom and the p-value.}
\section{Validation}{
Reproduces \code{car::ncvTest()} to within \code{1e-8}; see \file{tests/testthat/test-pass-a-reference.R}. Releases before 0.7.0 returned \eqn{n R^2} from regressing the raw squared residuals on the fitted values, which is the studentized (Koenker) statistic rather than the Cook-Weisberg score test.
}
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, unweighted
or with positive weights, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it
tests on the estimable part of its design: it gives the value of the same model fitted without the
aliased columns. It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit, a
fit with a zero weight, a coefficient that is not finite for another reason, and \code{data} that
lack rows or variables of the fit. Since 0.16.0 it is built on the model context, and also refuses
a multivariate \code{lm} ("needs a model with one response"), a fit whose coefficients, residuals or
weights were changed after the fit, and a fit with \code{model = FALSE} whose model frame, rebuilt
from the data it was fitted to, no longer holds its response, because those data changed after the
fit, with or without \code{data}. A fit with \code{model = FALSE} whose data are gone is tested on
\code{data} that give its response and fitted values, and refused without them. Since 0.16.0
\code{data} is matched to the observations of the fit as by every test, with or without
\code{var_formula}: without it the test reads nothing from \code{data}, and gave the value of the
fit on numbered data in another order, rows in front of those of the fit and repeated row names,
which are refused. Which model each test accepts, and why, is set out in
\code{\link{test_results}}, under \emph{Supported models}.
}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performCookWeisbergTest(m)

 # Score test against a specific variance model rather than the fitted values
 performCookWeisbergTest(m, var_formula = ~wt)

 # With the data, the variance model can use variables the mean model does not
 performCookWeisbergTest(m, mtcars, var_formula = ~ wt + hp)
}
\references{
Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity in regression. \emph{Biometrika}, 70(1), 1--10. \doi{10.1093/biomet/70.1.1}
}
\seealso{
  \code{\link{performNCVTest}}, \code{\link{performKoenkerTest}}, \code{\link{performBPTest}}
}
