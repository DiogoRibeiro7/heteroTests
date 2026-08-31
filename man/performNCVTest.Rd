\name{performNCVTest}
\alias{performNCVTest}
\title{Perform the non-constant variance (NCV) score test}
\description{The Cook-Weisberg score test for non-constant error variance, the diagnostic returned by \code{car::ncvTest()}.}
\details{
Let \eqn{\hat{e}_i} denote the OLS residuals and \eqn{\hat{\sigma}^2 = \sum_i \hat{e}_i^2 / n} the maximum-likelihood variance estimate. The test regresses the scaled squared residuals \eqn{u_i = \hat{e}_i^2 / \hat{\sigma}^2} on the variance regressors \eqn{Z} and computes half the explained sum of squares of that auxiliary regression. Under homoscedasticity and normal errors the statistic is asymptotically chi-square with degrees of freedom equal to the number of variance regressors.

With \code{var_formula = NULL} the fitted values are the sole variance regressor, matching the default of \code{car::ncvTest()} and Stata's \code{estat hettest}.

Because the divisor 2 is the null variance of \eqn{\hat{e}^2 / \sigma^2} under normality, the test is sensitive to non-normal errors; \code{\link{performKoenkerTest}} provides the studentized variant, which is robust to non-normal kurtosis.
}
\usage{performNCVTest(model, var_formula = NULL)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{var_formula}{optional one-sided formula giving the variance model, evaluated in the model frame of \code{model} (for example \code{~ x1 + x2}). When \code{NULL} the fitted values are used.}
}
\value{An object of class \code{htest} containing the chi-square score statistic, its degrees of freedom and the p-value.}
\section{Validation}{
Reproduces \code{car::ncvTest()} to within \code{1e-8} for both the default and the \code{var_formula} forms; see \file{tests/testthat/test-pass-a-reference.R}. Releases before 0.7.0 regressed the \emph{absolute} residuals on the fitted values and reported a t statistic, which is a Glejser-type test rather than the Cook-Weisberg score test.
}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performNCVTest(m)

 # Score test against a specific variance model rather than the fitted values
 performNCVTest(m, var_formula = ~wt)
}
\references{
Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity in regression. \emph{Biometrika}, 70(1), 1--10. \doi{10.1093/biomet/70.1.1}

Fox, J., & Weisberg, S. (2019). \emph{An R Companion to Applied Regression} (3rd ed.). SAGE.
}
\seealso{
  \code{\link{performCookWeisbergTest}}, \code{\link{performKoenkerTest}}, \code{\link{performBPTest}}
}
