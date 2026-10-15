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
  \item{data}{optional data frame the model was fitted to, in which \code{var_formula} is evaluated; its rows are matched to the residuals by row name. When \code{NULL}, the default, the model frame of \code{model} is used. Without \code{var_formula} the statistic is computed from the residuals and fitted values of \code{model}, and \code{data} does not change the result.}
  \item{var_formula}{optional one-sided formula giving the variance model, evaluated in \code{data} (for example \code{~ x1 + x2}). When \code{NULL}, the default, the fitted values are the variance regressor and the result is what this function returned before 0.14.0; otherwise it is what the deprecated \code{performNCVTest(model, var_formula)} returned, labels included. In 0.14.0 \code{var_formula} was the second argument: a formula passed there by position is still read as \code{var_formula}, with a warning that shows the new call.}
}
\value{An object of class \code{htest} containing the score statistic, its single degree of freedom and the p-value.}
\section{Validation}{
Reproduces \code{car::ncvTest()} to within \code{1e-8}; see \file{tests/testthat/test-pass-a-reference.R}. Releases before 0.7.0 returned \eqn{n R^2} from regressing the raw squared residuals on the fitted values, which is the studentized (Koenker) statistic rather than the Cook-Weisberg score test.
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
